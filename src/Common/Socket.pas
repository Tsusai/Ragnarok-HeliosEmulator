unit Socket;

interface
uses
	PacketTypes,
	IdTCPServer;

	procedure WriteBufferByte(Index:word; ByteIn:byte; var Buffer : TBuffer);
	procedure WriteBufferWord(Index:word; WordIn:word; var Buffer : TBuffer);
	procedure WriteBufferCardinal(Index:word; CardinalIn:cardinal; var Buffer : TBuffer);
	procedure WriteBufferString(Index:word; StringIn:string; Count:word; var Buffer : TBuffer);

	function BufferReadWord(Index:word; var Buffer : TBuffer) : word;
	function BufferReadCardinal(Index:word; var Buffer : TBuffer) : cardinal;
	function BufferReadString(Index:word; Count:word; Buffer : TBuffer) : string;

	procedure SendBuffer(AThread : TIdPeerThread; Buffer : TBuffer; Size : Cardinal);

implementation
uses
	SysUtils;

	//Socket Method WriteBuffer - Writes a Byte to the buffer.
	procedure WriteBufferByte(Index:word; ByteIn:byte; var Buffer : TBuffer);
	begin
		Assert(Index <= 32767, 'TSocket.WriteBuffer - Byte: index overflow ' + IntToStr(Index));
		Move(ByteIn, Buffer[Index], 1);
	end;

  //Socket Method WriteBuffer - Writes a Word to the buffer.
	procedure WriteBufferWord(Index : word; WordIn : word; var Buffer : TBuffer);
	begin
		Assert(Index <= 32766, 'TSocket.WriteBuffer - Word: index overflow ' + IntToStr(Index));
		Move(WordIn, Buffer[Index], 2);
	end;

  //Socket Method WriteBuffer - Writes a Cardinal to the buffer.
	procedure WriteBufferCardinal(index : word; CardinalIn : cardinal; var Buffer : TBuffer);
	begin
		Assert(Index <= 32766, 'TSocket.WriteBuffer - Cardinal: index overflow ' + IntToStr(Index));
		Move(CardinalIn, Buffer[Index], 4);
	end;

  //Socket Method WriteBuffer - Writes a String to the buffer.
	procedure WriteBufferString(Index:word; StringIn : string; Count : word; var Buffer : TBuffer);
	var
		StrLength :integer;
	begin
		Assert(Index <= 32767, 'TSocket.WriteBuffer - String: Index overflow ' + IntToStr(Index));
		Assert(Index + Count <= 32767, 'TSocket.WriteBuffer - String: Index+Count overflow ' + IntToStr(Index+Count));

		FillChar(Buffer[Index], Count, 0);
		StrLength := Length(StringIn);
		if StrLength <> 0 then begin
		  if StrLength > Count then begin
				StrLength := Count;
      end;
      Move(StringIn[1], Buffer[Index], StrLength);
    end;
	end;

  //Socket Method BufferReadWord - Reads a Word from the buffer.
	function BufferReadWord(Index:word; var Buffer : TBuffer) : word;
	begin
		Assert(Index <= 32766, 'TSocket.BufferReadWord: Index overflow ' + IntToStr(Index));
		Move(Buffer[Index], Result, 2);
	end;

  //Socket Method BufferReadCardinal - Reads a Cardinal from the buffer.
	function BufferReadCardinal(Index:word; var Buffer : TBuffer) : cardinal;
	begin
		Assert(Index <= 32766, 'TSocket.BufferReadCardinal: Index overflow ' + IntToStr(Index));
		Move(Buffer[Index], Result, 4);
	end;

  //Socket Method BufferReadString - Reads a String from the buffer.
	function BufferReadString(Index:word; Count:word; Buffer : TBuffer):string;
	var
		StringOut : TCBuffer;
	begin
		Assert(Index <= 32767, 'TSocket.BufferReadString: Index overflow ' + IntToStr(Index));
		Assert(Index + Count <= 32767, 'TSocket.BufferReadString: Index+Count overflow ' + IntToStr(Index+Count));
		StringOut[Count] := #0;
		Move(Buffer[Index], StringOut, Count);
		Result := StringOut;
	end;

  //Socket Method SendBuffer - Writes the buffer to the socket.
	procedure SendBuffer(AThread : TIdPeerThread; Buffer : TBuffer; Size : Cardinal);
  begin
    AThread.Connection.WriteBuffer(Buffer,Size);
  end;

end.
