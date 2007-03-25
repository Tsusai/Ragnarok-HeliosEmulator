//------------------------------------------------------------------------------
//BufferIO                                                                  UNIT
//------------------------------------------------------------------------------
//	What it does-
//		Unit contains all incoming and outgoing packet procedures
//	to place data in and out of a databuffer.  Everything here is self
//	explanitory and should not be changed anytime soon.
//
//	Changes:
//		[2007/03/25] CR - Added a thorough commenting and header to
//	WriteBufferTwoPoints.  This legacy routine is cryptic, and Rube
//	Goldbergian in the way it does it's job, thus a good explanation is needed.
//------------------------------------------------------------------------------
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
	procedure WriteBufferLongWord(Index:word; LongWordIn:LongWord; var Buffer : TBuffer);
	procedure WriteBufferString(Index:word; StringIn:string; Count:word; var Buffer : TBuffer);
	procedure WriteBufferPointAndDirection(
		index:word;
		xy:TPoint;
		var Buffer : TBuffer;
		Dir:byte = 0
	);

	Procedure WriteBufferTwoPoints(
		const
			Index  : Word;
		const
			Point1 : TPoint;
		const
			Point2 : TPoint;
		var
			Buffer : TBuffer
		);

	procedure WriteBufferMD5String(
		Index:word;
		MD5String:string;
		var Buffer : TBuffer
	);


	function BufferReadByte(Index:word; var Buffer : TBuffer) : byte;
	function BufferReadWord(Index:word; var Buffer : TBuffer) : word;
	function BufferReadLongWord(Index:word; var Buffer : TBuffer) : LongWord;
	function BufferReadString(Index:word; Count:word; var Buffer : TBuffer) : string;
	function BufferReadMD5(Index : word; var Buffer : TBuffer) : string;
	function BufferReadOnePoint(Index:word; var Buffer : TBuffer) : TPoint;

	procedure SendPadding(AClient : TIdContext);

	procedure SendBuffer(var AClient : TIdContext; const Buffer : TBuffer; Size : LongWord);overload;
	procedure SendBuffer(var AClient : TInterClient; const Buffer : TBuffer; Size : LongWord);overload;
	procedure RecvBuffer(var AClient : TIdContext; var Buffer; Size : LongWord); overload;
	procedure RecvBuffer(var AClient : TInterClient; var Buffer; Size : LongWord);overload;

implementation
uses
	{IDE}
	SysUtils,
	{Third Party}
	IdGlobal;
//------------------------------------------------------------------------------
//PUSHING DATA INTO THE BUFFER METHODS                                PROCEDURES
//------------------------------------------------------------------------------
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

	//Socket Method WriteBuffer - Writes a LongWord to the buffer.
	procedure WriteBufferLongWord(index : word; LongWordIn : LongWord; var Buffer : TBuffer);
	begin
		Assert(Index <= 32766, 'WriteBuffer - LongWord: index overflow ' + IntToStr(Index));
		Move(LongWordIn, Buffer[Index], 4);
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


(*-----------------------------------------------------------------------------*
Proc WriteBufferTwoPoints

--
Overview:
--
	Based on WFIFOM2 used in prior projects (eWeiss, Fusion, Prometheus), only
with a spiffier and much more descriptive naName!
	The code here is VERY cryptic, but what it boils down to:  We take two xy
pairs, sending them in the smallest (40 bit) size possible given the
"architectural" limit for map sizes, and then we perform a byte reversal:

	01234 bytes becomes 43210

	At best, we can only speculate why this reversal was done.  Best guesses are
that this was done within the packets to foil botting clients from reading and
emulating these "encrypted" packets.

	In my own experience... I first saw this routine in Fusion, in early 2004, so
this "encryption" was well know by that time.  Thus the original purpose as best
we can figure has been long defeated, and it's merely an obscure speed bump.
	ChrstphrR

--
Revisions:
--
[2007/03/25] CR - Added Comment Header, and described routine in detail, and in
	summary.  Made remaining three passed parameters constant.
	Renamed local variables for more clarity:
[yyyy/mm/dd] <Author> - <Comment>
*-----------------------------------------------------------------------------*)
Procedure WriteBufferTwoPoints(
	const
		Index  : Word;
	const
		Point1 : TPoint;
	const
		Point2 : TPoint;
	var
		Buffer : TBuffer
	);
Var
	Pack40 : Int64;
	SwapMe : array[0..5] of Byte; {[2007/03/25] CR - Yes we DO need all 6 bytes! }
Begin
	{[2007/03/25] CR - Packing the XY points into 5 bytes - 40 bits.  This is
	possible because of a HARD CODED map dimension limit of 512 x 512 squares.
	512 = 2^10, thus 10 bits is the maximum needed per X or Y position. }

	Pack40 := (
		((Int64(Point2.X) AND $3ff) shl 30) OR
		((Int64(Point2.Y) AND $3ff) shl 20) OR
		((Int64(Point1.X) AND $3ff) shl 10) OR
		(Int64(Point1.Y) AND $3ff)
	);


	{[2007/03/17] CR - Tricky Code: Two parts to this trickiness, one is VERY
	clever:
	A) bb[5] is used as the bubbling byte to swap bytes both times.  Thus the 6th
	bit is needed, BUT the value stored in it doesn't matter, because we overwrite
	it.
	B) Two sets of swaps are performed to reverse the byte level order, to
	"confuse bots".  As mentioned above, this has LONG since been circumvented.
	N.B. we are not reversing the whole thing bit by bit, but rather byte by byte.
	}
	Move(Pack40, SwapMe[0], 5);

	{Swap 01234 ->  41230 }
	SwapMe[5] := SwapMe[0];
	SwapMe[0] := SwapMe[4];
	SwapMe[4] := SwapMe[5];

	{Swap 41230 ->  43210 -- A complete byte level reversal.}
	SwapMe[5] := SwapMe[1];
	SwapMe[1] := SwapMe[3];
	SwapMe[3] := SwapMe[5];

	{[2007/03/25] CR - N.B. If we want to optimize things further, we could make
	these swaps inline ASM, and explicitly use registers instead of an array in
	RAM for all of this.  If anyone is ambitious, I'd suggest checking in at the
	FastCode project online, first. }

	{[2007/03/25] CR - Finally, move the bit-mangled 5 bytes to the Buffer. }
	Move(SwapMe[0], Buffer[Index], 5);
End; (* Proc WriteBufferTwoPoints
*-----------------------------------------------------------------------------*)


	procedure WriteBufferPointAndDirection(
		index:word;
		xy:TPoint;
		var Buffer : TBuffer;
		Dir:byte = 0
	);
	var
		l   :LongWord;
		ByteArray  :array[0..3] of Byte;
	begin
		l := (((xy.X and $3ff) shl 14) or ((xy.Y and $3ff) shl 4));
		Move(l, ByteArray[0], 3);
		ByteArray[3] := ByteArray[0];
		ByteArray[0] := ByteArray[2];
		ByteArray[2] := ByteArray[3];
		ByteArray[2] := (ByteArray[2] or (Dir and $f));
		Move(ByteArray[0], Buffer[index], 3);
	End; (* Proc WriteBufferPointAndDirection *)
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//WriteBufferMD5String                                                  FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//	A MD5 string has 32 individual characters, each 2 make a byte.  This string
//	needs to be dumped as is without conversions to the buffer.  The loop grabs
//	every two characters in the string as is (lets say 9C).  It then puts a $ in
//	front to denote it is a hex string to delphi, then that number is written to
//	the buffer as one byte.
//
//	Changes -
//		March 12th, 2007 - Aeomin - Moved Header
//
//------------------------------------------------------------------------------
	procedure WriteBufferMD5String(Index:word; MD5String:string; var Buffer : TBuffer);
	var
		cnt : integer;
		AByte : byte;
	begin
	{for header:
	}
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

	//Socket Method BufferReadLongWord - Reads a LongWord from the buffer.
	function BufferReadLongWord(Index:word; var Buffer : TBuffer) : LongWord;
	begin
		Assert(Index <= 32766, 'BufferReadLongWord: Index overflow ' + IntToStr(Index));
		Move(Buffer[Index], Result, 4);
	end;

//------------------------------------------------------------------------------
//ReadMD5Password                                                       FUNCTION
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


//------------------------------------------------------------------------------
//BufferReadString                                                      FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Socket Method BufferReadString - Reads a String from the buffer.
//
//	Changes -
//		March 12th, 2007 - Aeomin - Reformat Header.
//
//------------------------------------------------------------------------------
	function BufferReadString(Index:word; Count:word; var Buffer : TBuffer):string;
	begin
		Assert(Index <= 32767, 'BufferReadString: Index overflow ' + IntToStr(Index));
		Assert(Index + Count <= 32767, 'BufferReadString: Index+Count overflow ' + IntToStr(Index+Count));
    SetLength(Result, Count);
    Move(Buffer[Index], Result[1], Count);
    Result := Trim(Result);
	end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//BufferReadOnePoint                                                    FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Socket Method BufferReadLongWord - Reads one point from the buffer.
//
//	Changes -
//		March 12th, 2007 - Aeomin - Reformat Header.
//
//------------------------------------------------------------------------------
	function BufferReadOnePoint(Index:word; var Buffer : TBuffer) : TPoint;
	var
		l   :LongWord;
		bb  :array[0..3] of byte;
	begin
		Move(Buffer[index], bb[0], 3);
		bb[3] := bb[0];
		bb[0] := bb[2];
		bb[2] := bb[3];
		Move(bb[0], l, 3);
		l := l shr 4;
		Result.Y :=  (l and $003ff);
		Result.X := ((l and $ffc00) shr 10);
	end;
//------------------------------------------------------------------------------


(*------------------------------------------------------------------------------
PREMADE SENDING OF BUFFER TO CLIENT
------------------------------------------------------------------------------*)


//------------------------------------------------------------------------------
//SendPadding                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//	    Padding Packet,
//	    used for antibot/antihack upon charaserv and mapserv connections
//
//	Changes -
//		March 12th, 2007 - Aeomin - Reformat Header.
//
//------------------------------------------------------------------------------
	procedure SendPadding(AClient : TIdContext);
	var
		ABuf : TBuffer;
	begin
		WriteBufferLongWord(0,$00000000,ABuf);
		SendBuffer(AClient,ABuf,4);
	end;

	//Socket Method SendBuffer - Writes the buffer to the socket.
	procedure SendBuffer(var AClient : TInterClient; const Buffer : TBuffer; Size : LongWord);
	var
		SendBytes : TIdBytes;
	begin
		SendBytes := RawToBytes(Buffer,Size);
		AClient.IOHandler.Write(SendBytes);
	end;


	//Socket Method SendBuffer - Writes the buffer to the socket.
	procedure SendBuffer(var AClient : TIdContext; const Buffer : TBuffer; Size : LongWord);
	var
		SendBytes : TIdBytes;
	begin
		SendBytes := RawToBytes(Buffer,Size);
		AClient.Connection.IOHandler.Write(SendBytes);
	end;


	//Socket Method RecvBuffer - Reads the buffer from the socket.
	procedure RecvBuffer(var AClient : TIdContext; var Buffer; Size : LongWord);
	var
		RecvBytes : TIdBytes;
	begin
		if Size > 0 then
		begin
			FillChar(Buffer,Size,0);
			AClient.Connection.IOHandler.ReadBytes(RecvBytes,Size);
			BytesToRaw(RecvBytes,Buffer,Size);
		end;
	end;

		//Socket Method RecvBuffer - Reads the buffer from the socket.
	procedure RecvBuffer(var AClient : TInterClient; var Buffer; Size : LongWord);
	var
		RecvBytes : TIdBytes;
	begin
		FillChar(Buffer,Size,0);
		AClient.IOHandler.ReadBytes(RecvBytes,Size);
		BytesToRaw(RecvBytes,Buffer,Size);
	end;


end.
