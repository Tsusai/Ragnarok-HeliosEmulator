(*------------------------------------------------------------------------------
Socket
RaX 2006

Description:
 Unit contains all incoming and outgoing packet procedures to place data in and
	out of a databuffer.  Everything here is self explanitory and should not be
	changed anytime soon.
------------------------------------------------------------------------------*)
unit BufferIO;

interface
uses
	//IDE
	Types,
	//Helios
	PacketTypes,
	CommClient,
	//3rd Party
	IdContext;

	procedure WriteBufferByte(Index:word; ByteIn:byte; var Buffer : TBuffer);
	procedure WriteBufferWord(Index:word; WordIn:word; var Buffer : TBuffer);
	procedure WriteBufferCardinal(Index:word; CardinalIn:cardinal; var Buffer : TBuffer);
	procedure WriteBufferString(Index:word; StringIn:string; Count:word; var Buffer : TBuffer);
	procedure WriteBufferPointAndDirection(
		index:word;
		xy:TPoint;
		var Buffer : TBuffer;
		Dir:byte = 0
	);
	procedure WriteBufferMD5String(
		Index:word;
		MD5String:string;
		var Buffer : TBuffer
	);


	function BufferReadByte(Index:word; var Buffer : TBuffer) : byte;
	function BufferReadWord(Index:word; var Buffer : TBuffer) : word;
	function BufferReadCardinal(Index:word; var Buffer : TBuffer) : cardinal;
	function BufferReadString(Index:word; Count:word; var Buffer : TBuffer) : string;
	function BufferReadMD5(Index : word; var Buffer : TBuffer) : string;

	procedure SendPadding(AClient : TIdContext);

	procedure SendBuffer(var AClient : TIdContext; var Buffer : TBuffer; Size : Cardinal);overload;
	procedure SendBuffer(var AClient : TInterClient; var Buffer : TBuffer; Size : Cardinal);overload;
	procedure RecvBuffer(var AClient : TIdContext; var Buffer; Size : Cardinal); overload;
	procedure RecvBuffer(var AClient : TInterClient; var Buffer; Size : Cardinal);overload;

implementation
uses
	{IDE}
	SysUtils,
	{Third Party}
	IdGlobal;
(*------------------------------------------------------------------------------
PUSHING DATA INTO THE BUFFER METHODS
------------------------------------------------------------------------------*)
	//Socket Method WriteBuffer - Writes a Byte to the buffer.
	procedure WriteBufferByte(Index:word; ByteIn:byte; var Buffer : TBuffer);
	begin
		Assert(Index <= 32767, 'WriteBuffer - Byte: index overflow ' + IntToStr(Index));
		Move(ByteIn, Buffer[Index], 1);
	end;

	//Socket Method WriteBuffer - Writes a Word to the buffer.
	procedure WriteBufferWord(Index : word; WordIn : word; var Buffer : TBuffer);
	begin
		Assert(Index <= 32766, 'WriteBuffer - Word: index overflow ' + IntToStr(Index));
		Move(WordIn, Buffer[Index], 2);
	end;

	//Socket Method WriteBuffer - Writes a Cardinal to the buffer.
	procedure WriteBufferCardinal(index : word; CardinalIn : cardinal; var Buffer : TBuffer);
	begin
		Assert(Index <= 32766, 'WriteBuffer - Cardinal: index overflow ' + IntToStr(Index));
		Move(CardinalIn, Buffer[Index], 4);
	end;

	//Socket Method WriteBuffer - Writes a String to the buffer.
	procedure WriteBufferString(Index:word; StringIn : string; Count : word; var Buffer : TBuffer);
	var
		StrLength :integer;
	begin
		Assert(Index <= 32767, 'WriteBuffer - String: Index overflow ' + IntToStr(Index));
		Assert(Index + Count <= 32767, 'WriteBuffer - String: Index+Count overflow ' + IntToStr(Index+Count));

		FillChar(Buffer[Index], Count, 0);
		StrLength := Length(StringIn);
		if StrLength <> 0 then begin
			if StrLength > Count then begin
				StrLength := Count;
			end;
			Move(StringIn[1], Buffer[Index], StrLength);
		end;
	end;

	procedure WriteBufferPointAndDirection(
		index:word;
		xy:TPoint;
		var Buffer : TBuffer;
		Dir:byte = 0
	);
	var
		l   :cardinal;
		ByteArray  :array[0..3] of byte;
	begin
		l := (((xy.X and $3ff) shl 14) or ((xy.Y and $3ff) shl 4));
		Move(l, ByteArray[0], 3);
		ByteArray[3] := ByteArray[0];
		ByteArray[0] := ByteArray[2];
		ByteArray[2] := ByteArray[3];
		ByteArray[2] := (ByteArray[2] or (Dir and $f));
		Move(ByteArray[0], Buffer[index], 3);
	End; (* Proc WriteBufferPointAndDirection
*-----------------------------------------------------------------------------*)

	procedure WriteBufferMD5String(Index:word; MD5String:string; var Buffer : TBuffer);
	var
		cnt : integer;
		AByte : byte;
	begin
	{for header:
	A MD5 string has 32 individual characters, each 2 make a byte.  This string
	needs to be dumped as is without conversions to the buffer.  The loop grabs
	every two characters in the string as is (lets say 9C).  It then puts a $ in
	front to denote it is a hex string to delphi, then that number is written to
	the buffer as one byte.}
		for cnt := 0 to 15 do
		begin
			AByte := StrToIntDef( ('$' + MD5String[cnt*2+1] + MD5String[cnt*2+2]), 0);
			WriteBufferByte(Index+cnt,AByte,Buffer);
		end;
	end;

(*------------------------------------------------------------------------------
READING DATA FROM THE BUFFER METHODS
------------------------------------------------------------------------------*)

	//Socket Method BufferReadByte - Reads a Byte from the buffer.
	function BufferReadByte(Index:word; var Buffer : TBuffer) : byte;
	begin
		Assert(Index <= 32766, 'BufferReadByte: Index overflow ' + IntToStr(Index));
		Move(Buffer[Index], Result, 1);
	end;

	//Socket Method BufferReadWord - Reads a Word from the buffer.
	function BufferReadWord(Index:word; var Buffer : TBuffer) : word;
	begin
		Assert(Index <= 32766, 'BufferReadWord: Index overflow ' + IntToStr(Index));
		Move(Buffer[Index], Result, 2);
	end;

	//Socket Method BufferReadCardinal - Reads a Cardinal from the buffer.
	function BufferReadCardinal(Index:word; var Buffer : TBuffer) : cardinal;
	begin
		Assert(Index <= 32766, 'BufferReadCardinal: Index overflow ' + IntToStr(Index));
		Move(Buffer[Index], Result, 4);
	end;

//------------------------------------------------------------------------------
//ReadMD5Password                                                     FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Reads MD5Bytes and makes it into a string.  Its 16 bytes long.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	function BufferReadMD5(Index : word; var Buffer : TBuffer) : string;
	var
		cnt : integer;
	begin
		Result := '';
		//Read every byte, and convert that bite value into a hex string
		//Attach all hexstrings together to make the MD5 hash string.
		for cnt := 0 to 16-1 do
		begin
			Result := Result + IntToHex(BufferReadByte(Index+cnt,Buffer),2);
		end;
	end;//ReadMD5Password
//------------------------------------------------------------------------------

	//Socket Method BufferReadString - Reads a String from the buffer.
	function BufferReadString(Index:word; Count:word; var Buffer : TBuffer):string;
	var
		StringOut : TCBuffer;
	begin
		Assert(Index <= 32767, 'BufferReadString: Index overflow ' + IntToStr(Index));
		Assert(Index + Count <= 32767, 'BufferReadString: Index+Count overflow ' + IntToStr(Index+Count));
		StringOut[Count] := #0;
		Move(Buffer[Index], StringOut, Count);
		Result := StringOut;
	end;

(*------------------------------------------------------------------------------
PREMADE SENDING OF BUFFER TO CLIENT
------------------------------------------------------------------------------*)
	//Padding Packet,
	//used for antibot/antihack upon charaserv and mapserv connections
	procedure SendPadding(AClient : TIdContext);
	var
		ABuf : TBuffer;
	begin
		WriteBufferCardinal(0,$00000000,ABuf);
		SendBuffer(AClient,ABuf,4);
	end;

	//Socket Method SendBuffer - Writes the buffer to the socket.
	procedure SendBuffer(var AClient : TInterClient; var Buffer : TBuffer; Size : Cardinal);
	var
		SendBytes : TIdBytes;
	begin
		SendBytes := RawToBytes(Buffer,Size);
		AClient.IOHandler.Write(SendBytes);
	end;

	//Socket Method SendBuffer - Writes the buffer to the socket.
	procedure SendBuffer(var AClient : TIdContext; var Buffer : TBuffer; Size : Cardinal);
	var
		SendBytes : TIdBytes;
	begin
		SendBytes := RawToBytes(Buffer,Size);
		AClient.Connection.IOHandler.Write(SendBytes);
	end;


	//Socket Method RecvBuffer - Reads the buffer from the socket.
	procedure RecvBuffer(var AClient : TIdContext; var Buffer; Size : Cardinal);
	var
		RecvBytes : TIdBytes;
	begin
		if Size > 0 then
		begin
			AClient.Connection.IOHandler.ReadBytes(RecvBytes,Size);
			BytesToRaw(RecvBytes,Buffer,Size);
		end;
	end;

		//Socket Method RecvBuffer - Reads the buffer from the socket.
	procedure RecvBuffer(var AClient : TInterClient; var Buffer; Size : Cardinal);
	var
		RecvBytes : TIdBytes;
	begin
		AClient.IOHandler.ReadBytes(RecvBytes,Size);
		BytesToRaw(RecvBytes,Buffer,Size);
	end;
end.
