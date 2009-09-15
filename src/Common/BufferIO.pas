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

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
uses
	//IDE
	Types,
	//Helios
	PacketTypes,
	CommClient,
	//3rd Party
	IdContext;

	procedure WriteBufferByte(const Index:word; const ByteIn:byte; var Buffer : TBuffer);
	procedure WriteBufferWord(const Index:word; const WordIn:word; var Buffer : TBuffer);
	procedure WriteBufferLongWord(const Index:word; const LongWordIn:LongWord; var Buffer : TBuffer);
	procedure WriteBufferString(const Index:word; const StringIn:string; const Count:word; var Buffer : TBuffer);
	procedure WriteBufferPointAndDirection(
		const index:word;
		const xy:TPoint;
		var Buffer : TBuffer;
		const Dir:byte = 0
	);

	Procedure WriteBufferTwoPoints(
		const
			Index  : Word;
		const
			SourcePt : TPoint;
		const
			DestPt : TPoint;
		var
			Buffer : TBuffer
		);

	procedure WriteBufferMD5String(
		const Index:word;
		const MD5String:string;
		var Buffer : TBuffer
	);


	function BufferReadByte(const Index:word; const Buffer : TBuffer) : byte;
	function BufferReadWord(const Index:word; const Buffer : TBuffer) : word;
	function BufferReadLongWord(const Index:word; const Buffer : TBuffer) : LongWord;
	function BufferReadString(const Index:word; const Count:word; const Buffer : TBuffer) : string;
	function BufferReadMD5(const Index : word; const Buffer : TBuffer) : string;
	function BufferReadOnePoint(const Index:word; const Buffer : TBuffer) : TPoint;

	procedure SendPadding(var AClient : TIdContext;const ID:LongWord);

	procedure SendBuffer(var AClient : TIdContext;const Buffer : TBuffer; const Size : LongWord);overload;
	procedure SendBuffer(var AClient : TInterClient; const Buffer : TBuffer; const Size : LongWord);overload;
	procedure RecvBuffer(
		var AClient : TIdContext;
		var Buffer;
		const Size : LongWord
		); overload;
	procedure RecvBuffer(
		var AClient : TInterClient;
		var Buffer;
		const Size : LongWord
		); overload;

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
	procedure WriteBufferByte(const Index:word; const ByteIn:byte; var Buffer : TBuffer);
	begin
		Assert(Index <= 32767, 'WriteBuffer - Byte: index overflow ' + IntToStr(Index));
		Move(ByteIn, Buffer[Index], 1);
	end;

	//Socket Method WriteBuffer - Writes a Word to the buffer.
	procedure WriteBufferWord(const Index : word; const WordIn : word; var Buffer : TBuffer);
	begin
		Assert(Index <= 32766, 'WriteBuffer - Word: index overflow ' + IntToStr(Index));
		Move(WordIn, Buffer[Index], 2);
	end;

	//Socket Method WriteBuffer - Writes a LongWord to the buffer.
	procedure WriteBufferLongWord(const index : word; const LongWordIn : LongWord; var Buffer : TBuffer);
	begin
		Assert(Index <= 32766, 'WriteBuffer - LongWord: index overflow ' + IntToStr(Index));
		Move(LongWordIn, Buffer[Index], 4);
	end;

	//Socket Method WriteBuffer - Writes a String to the buffer.
	procedure WriteBufferString(const Index:word; const StringIn : string; const Count : word; var Buffer : TBuffer);
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
Halloween 2008 - Tsusai - Updated WriteBufferTwoPoints settings & Simplified.
[yyyy/mm/dd] <Author> - <Comment>
*-----------------------------------------------------------------------------*)
Procedure WriteBufferTwoPoints(
	const
		Index  : Word;
	const
		SourcePt : TPoint;
	const
		DestPt : TPoint;
	var
		Buffer : TBuffer
	);

Begin
	WriteBufferByte(Index+0, Byte((SourcePt.X) shr 2),Buffer);
	WriteBufferByte(Index+1, Byte(((SourcePt.X) shl 6) or (((SourcePt.Y) shr 4) and $3f)), Buffer);
	WriteBufferByte(Index+2, Byte(((SourcePt.Y) shl 4) or (((DestPt.X) shr 6) and $0f)), Buffer);
	WriteBufferByte(Index+3, Byte(((DestPt.X) shl 2) or (((DestPt.Y) shr 8) and $03)), Buffer);
	WriteBufferByte(Index+4, Byte(DestPt.Y), Buffer);
	WriteBufferByte(Index+5, Byte(((8) shl 4) or ((8) and $0f)), Buffer);

End; (* Proc WriteBufferTwoPoints
*-----------------------------------------------------------------------------*)

	//Halloween 2008 - Tsusai - Simplified
	procedure WriteBufferPointAndDirection(
		const index:word;
		const xy:TPoint;
		var Buffer : TBuffer;
		const Dir:byte = 0
	);
	{var
		l   :LongWord;
		ByteArray  :array[0..3] of Byte;}
	begin
		WriteBufferByte(Index+0, Byte((xy.X) shr 2), Buffer);
		WriteBufferByte(Index+1, Byte(((xy.x) shl 6) or (((xy.y) shr 4) and $3f)), Buffer);
		WriteBufferByte(Index+2, Byte(((xy.y) shl 4) or ((dir) and $f)), Buffer);
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
	procedure WriteBufferMD5String(const Index:word; const MD5String:string; var Buffer : TBuffer);
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
	function BufferReadByte(const Index:word; const Buffer : TBuffer) : byte;
	begin
		Assert(Index <= 32766, 'BufferReadByte: Index overflow ' + IntToStr(Index));
		Move(Buffer[Index], Result, 1);
	end;

	//Socket Method BufferReadWord - Reads a Word from the buffer.
	function BufferReadWord(const Index:word; const Buffer : TBuffer) : word;
	begin
		Assert(Index <= 32766, 'BufferReadWord: Index overflow ' + IntToStr(Index));
		Move(Buffer[Index], Result, 2);
	end;

	//Socket Method BufferReadLongWord - Reads a LongWord from the buffer.
	function BufferReadLongWord(const Index:word; const Buffer : TBuffer) : LongWord;
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
	function BufferReadMD5(const Index : word; const Buffer : TBuffer) : string;
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


(*- Function ------------------------------------------------------------------*
BufferReadString
--------------------------------------------------------------------------------
Overview:
--
	Socket Method BufferReadString - Reads a String from the buffer.

[2007/06/03] CR - Reverted to original weiss-based code, instead of the latest
	RFIFOS code that was present in Prometheus when Tsusai began filling out his
	buffer read/write routines.

	Contains Tricky Code:
		By using an array of character (StrArray) as an intermediary, it trims the
	Result string up to the first Null (#0) character.

		This sidesteps the need to do an explicit Trim(), for the one case where the
	Client completely foregoes security when re-logging into the Login server when
	dropping from the Zone -- it sends the password, a null, and whatever random
	data you have in that 24 byte block of RAM.  Just hope your credit card number
	wasn't that spot before!

--
Pre:
	(Our buffers are 32k in size, which is also the max packet size for RO packets
	by design. So:)
	Ensure that Index is less than the 32k barrier.
	Ensure that Index + Count together, are less than the 32k barrier.
Post:
	None.

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/03/12] Aeomin - Reformat Header.
[2007/06/03] CR - Bugfix - allows clients to re-login now.  Thoroughly commented
	code, and pre-requisite assertions, so that no one haphazardly goes into this
	code to "improve" it without knowing the perils before them.
*-----------------------------------------------------------------------------*)
Function  BufferReadString(
	const
		Index  : Word; //Starting point on the buffer.
	const
		Count  : Word; //Max number of characters to copy to the string.
	const
		Buffer : TBuffer //Our source Byte Buffer
	) : String;
Var
	StrArray : TCBuffer;
Begin
	//Pre
	Assert(Index <= 32767, 'BufferReadString: Index overflow ' + IntToStr(Index));
	Assert(
		Index + Count <= 32767,
		'BufferReadString: Index+Count overflow ' + IntToStr(Index+Count)
	);
	//--

	StrArray[Count] := #0; //Ensure the string is null terminated, just in case.
	Move(Buffer[Index], StrArray, Count);
	Result := StrArray; //Does more than you think here (read description).
End; (* Func BufferReadString
*-----------------------------------------------------------------------------*)

//------------------------------------------------------------------------------
//BufferReadOnePoint                                                    FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Socket Method BufferReadLongWord - Reads one point from the buffer.
//
//	Changes -
//		March 12th, 2007 - Aeomin - Reformat Header.
//		Dec 2, 2008 - Tsusai - Implemented better bit shifting
//
//------------------------------------------------------------------------------
	function BufferReadOnePoint(const Index:word; const Buffer : TBuffer) : TPoint;
	var
		bb  :array[0..2] of byte;
	begin
		Move(Buffer[index], bb[0], 3);
		Result.X := (bb[0] * 4) + (bb[1] shr 6);
		Result.Y := ((bb[1] and $3f) shl 4) + (bb[2] shr 4);
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
	procedure SendPadding(var AClient : TIdContext;const ID:LongWord);
	var
		ABuf : TBuffer;
	begin
		WriteBufferLongWord(0,ID,ABuf);
		SendBuffer(AClient,ABuf,4);
	end;

	//Socket Method SendBuffer - Writes the buffer to the socket.
	procedure SendBuffer(var AClient : TInterClient; const Buffer : TBuffer; const Size : LongWord);
	var
		SendBytes : TIdBytes;
	begin
		SendBytes := RawToBytes(Buffer,Size);
		AClient.IOHandler.Write(SendBytes);
	end;


	//Socket Method SendBuffer - Writes the buffer to the socket.
	procedure SendBuffer(var AClient : TIdContext;const Buffer : TBuffer; const Size : LongWord);
	var
		SendBytes : TIdBytes;
	begin
		SendBytes := RawToBytes(Buffer,Size);
		{if AClient.Data is TClientLink then
		begin
			if (TClientLink(AClient.Data).EncKey1 > 0)AND(TClientLink(AClient.Data).EncKey2 > 0) then
			begin
				WriteBufferWord(0, TClientLink(AClient.Data).DecryptMessageID(BufferReadWord(0,Buffer)), Buffer);
			end;
		end;}
		AClient.Connection.IOHandler.Write(SendBytes);
	end;


	//Socket Method RecvBuffer - Reads the buffer from the socket.
	procedure RecvBuffer(
		var AClient : TIdContext;
		var Buffer;
		const Size : LongWord
		);
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
	procedure RecvBuffer(var AClient : TInterClient; var Buffer; const Size : LongWord);
	var
		RecvBytes : TIdBytes;
	begin
		FillChar(Buffer,Size,0);
		AClient.IOHandler.ReadBytes(RecvBytes,Size);
		BytesToRaw(RecvBytes,Buffer,Size);
	end;


end.
