unit Socket;

interface
uses
	PacketTypes,
  AccountTypes,
  IdTCPServer;

type TSocket = class
  public
    procedure ParseLogin(AThread: TIdPeerThread);

  private
	  procedure WriteBuffer(Index:word; ByteIn:byte; var Buffer : TBuffer); overload;
	  procedure WriteBuffer(Index:word; WordIn:word; var Buffer : TBuffer); overload;
	  procedure WriteBuffer(Index:word; CardinalIn:cardinal; var Buffer : TBuffer); overload;
	  procedure WriteBuffer(Index:word; StringIn:string; Count:word; var Buffer : TBuffer); overload;

	  function BufferReadWord(Index:word; var Buffer : TBuffer) : word;
	  function BufferReadCardinal(Index:word; var Buffer : TBuffer) : cardinal;
	  function BufferReadString(Index:word; Count:word; Buffer : TBuffer) : string;

    procedure SendBuffer(AThread : TIdPeerThread; Buffer : TBuffer; Size : Cardinal);

    procedure SendCharacterServers(AnAccount : TAccount; AThread: TIdPeerThread);

    procedure SendLoginError(const AThread: TIdPeerThread; const Error : byte);
end;

const
	    LOGIN_UNREGISTERED    = 0;
	    LOGIN_INVALIDPASSWORD = 1;
	    //LOGIN_SERVERFULL =  ;

implementation
uses
	SysUtils,
  Globals,
  CharaServerTypes,
  Main;

	//Socket Method WriteBuffer - Writes a Byte to the buffer.
	procedure TSocket.WriteBuffer(Index:word; ByteIn:byte; var Buffer : TBuffer);
	begin
		Assert(Index <= 32767, 'TSocket.WriteBuffer - Byte: index overflow ' + IntToStr(Index));
		Move(ByteIn, Buffer[Index], 1);
	end;

  //Socket Method WriteBuffer - Writes a Word to the buffer.
	procedure TSocket.WriteBuffer(Index : word; WordIn : word; var Buffer : TBuffer);
	begin
		Assert(Index <= 32766, 'TSocket.WriteBuffer - Word: index overflow ' + IntToStr(Index));
		Move(WordIn, Buffer[Index], 2);
	end;

  //Socket Method WriteBuffer - Writes a Cardinal to the buffer.
	procedure TSocket.WriteBuffer(index : word; CardinalIn : cardinal; var Buffer : TBuffer);
	begin
		Assert(Index <= 32766, 'TSocket.WriteBuffer - Cardinal: index overflow ' + IntToStr(Index));
		Move(CardinalIn, Buffer[Index], 4);
	end;

  //Socket Method WriteBuffer - Writes a String to the buffer.
	procedure TSocket.WriteBuffer(Index:word; StringIn : string; Count : word; var Buffer : TBuffer);
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
	function TSocket.BufferReadWord(Index:word; var Buffer : TBuffer) : word;
	begin
		Assert(Index <= 32766, 'TSocket.BufferReadWord: Index overflow ' + IntToStr(Index));
		Move(Buffer[Index], Result, 2);
	end;

  //Socket Method BufferReadCardinal - Reads a Cardinal from the buffer.
	function TSocket.BufferReadCardinal(Index:word; var Buffer : TBuffer) : cardinal;
	begin
		Assert(Index <= 32766, 'TSocket.BufferReadCardinal: Index overflow ' + IntToStr(Index));
		Move(Buffer[Index], Result, 4);
	end;

  //Socket Method BufferReadString - Reads a String from the buffer.
	function TSocket.BufferReadString(Index:word; Count:word; Buffer : TBuffer):string;
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
  procedure TSocket.SendBuffer(AThread : TIdPeerThread; Buffer : TBuffer; Size : Cardinal);
  begin
    AThread.Connection.WriteBuffer(Buffer,Size);
  end;

  //Sends the login error back to the client.
  procedure TSocket.SendLoginError(const AThread: TIdPeerThread; const Error : byte);
  var
    Buffer : TBuffer;
  begin
	  WriteBuffer( 0, $006a, Buffer);
	  WriteBuffer( 2, Error, Buffer);
	  SendBuffer(AThread, Buffer ,23);
  end;

  //Socket Method ParseLogin - Checks sent credentials against Account database.
  procedure TSocket.ParseLogin(AThread: TIdPeerThread);
  var
	  PLength : Integer;
	  Buffer : TBuffer;
	  UserName : String;
	  Password : String;
	  AnAccount : TAccount;
	  Index : Integer;
	  Found : Boolean;
	  ID : Word;
  begin
	  Found := FALSE;
	  AnAccount := NIL;
	  if AThread.Connection.Connected then
	  begin
		  PLength := AThread.Connection.ReadFromStack;
		  if PLength >= 2 then
		  begin
			  AThread.Connection.ReadBuffer(Buffer,PLength);
			  ID := BufferReadWord(0,Buffer);
			  Case ID of
			  $0064: //Basic login packet
				  begin
					  UserName := BufferReadString(6,24,Buffer);
					  for Index := 0 to AccountList.Count -1 do begin
						  if AccountList.Objects[Index] is TAccount then begin
							  AnAccount := TAccount(AccountList.Objects[Index]);
							  if AnAccount.Username = UserName then
                begin
								  Found := TRUE;
								  break;
							  end;
						  end else Continue;
					  end;
					  if Found then begin
						  Password := BufferReadString(30,24,Buffer);
						  if AnAccount.Password = Password then
						  begin
							  AnAccount.LoginKey[1] := BufferReadCardinal(54,Buffer);
							  AnAccount.LoginKey[2] := BufferReadCardinal(58,Buffer);
							  AnAccount.LastIP := AThread.Connection.Socket.Binding.PeerIP;
							  SendCharacterServers(AnAccount,AThread);
						  end else begin
							  SendLoginError(AThread,LOGIN_INVALIDPASSWORD);
						  end;
					  end else begin
						  SendLoginError(AThread,LOGIN_UNREGISTERED);
					  end;
				  end;
		 	  end;
		  end;
	  end;
  end;
  
  //Socket Method SendCharacterServers - 
  procedure TSocket.SendCharacterServers(AnAccount : TAccount; AThread: TIdPeerThread);
  var
	  Buffer : TBuffer;
	  Index : byte;
	  Size : cardinal;
  begin
    //Packet Format...
	  //R 0069 <len>.w <login ID1>.l <account ID>.l <login ID2>.l ?.32B <sex>.B
	  //{<IP>.l <port>.w <server name>.20B <login users>.w <maintenance>.w <new>.w}.32B*
	  Size := 2+2+4+4+4+32+1+(CharaServerList.Count * 32);
    WriteBuffer(0,$0069,Buffer);
    WriteBuffer(2,Size,Buffer);
    WriteBuffer(4,AnAccount.LoginKey[1],Buffer);
    WriteBuffer(8,AnAccount.ID,Buffer);
    WriteBuffer(12,AnAccount.LoginKey[2],Buffer);
    WriteBuffer(16, 0, Buffer);
	  WriteBuffer(20, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz'#0, Now), 24, Buffer);
	  WriteBuffer(44, 0, Buffer);
	  WriteBuffer(46,AnAccount.Gender,Buffer);

	  for Index := 0 to CharaServerList.Count - 1 do begin
		  WriteBuffer(47,TCharaServ(CharaServerList.Objects[Index]).IPCardinal,Buffer);
		  WriteBuffer(51,TCharaServ(CharaServerList.Objects[Index]).Port,Buffer);
		  WriteBuffer(53+Index*32,TCharaServ(CharaServerList.Objects[Index]).ServerName,20,Buffer);
		  WriteBuffer(73+Index*32,TCharaServ(CharaServerList.Objects[Index]).OnlineUsers,Buffer);
		  WriteBuffer(75,0,Buffer);
		  WriteBuffer(77,0,Buffer);
	  end;

    SendBuffer(AThread, Buffer ,Size);
  end;
end.
