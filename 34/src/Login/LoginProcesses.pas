unit LoginProcesses;

interface
  uses
    AccountTypes,
    IdTCPServer;

    procedure SendCharacterServers(AnAccount : TAccount; AThread: TIdPeerThread);
    procedure ParseLogin(AThread: TIdPeerThread);
    procedure SendLoginError(const AThread: TIdPeerThread; const Error : byte);

implementation
  uses
    Socket,
    PacketTypes,
    Globals,
    Main,
    SysUtils,
    CharaServerTypes;
  var
    Socket : TSocket;
      //Sends the login error back to the client.
  procedure SendLoginError(const AThread: TIdPeerThread; const Error : byte);
  var
    Buffer : TBuffer;
  begin
	  Socket.WriteBuffer( 0, $006a, Buffer);
	  Socket.WriteBuffer( 2, Error, Buffer);
	  Socket.SendBuffer(AThread, Buffer ,23);
  end;

  //Socket Method ParseLogin - Checks sent credentials against Account database.
  procedure ParseLogin(AThread: TIdPeerThread);
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
			  ID := Socket.BufferReadWord(0,Buffer);
			  Case ID of
			  $0064: //Basic login packet
				  begin
					  UserName := Socket.BufferReadString(6,24,Buffer);
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
						  Password := Socket.BufferReadString(30,24,Buffer);
						  if AnAccount.Password = Password then
						  begin
							  AnAccount.LoginKey[1] := Socket.BufferReadCardinal(54,Buffer);
							  AnAccount.LoginKey[2] := Socket.BufferReadCardinal(58,Buffer);
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
  procedure SendCharacterServers(AnAccount : TAccount; AThread: TIdPeerThread);
  var
	  Buffer : TBuffer;
	  Index : byte;
	  Size : cardinal;
  begin
    //Packet Format...
	  //R 0069 <len>.w <login ID1>.l <account ID>.l <login ID2>.l ?.32B <sex>.B
	  //{<IP>.l <port>.w <server name>.20B <login users>.w <maintenance>.w <new>.w}.32B*
	  Size := 2+2+4+4+4+32+1+(CharaServerList.Count * 32);
    Socket.WriteBuffer(0,$0069,Buffer);
    Socket.WriteBuffer(2,Size,Buffer);
    Socket.WriteBuffer(4,AnAccount.LoginKey[1],Buffer);
    Socket.WriteBuffer(8,AnAccount.ID,Buffer);
    Socket.WriteBuffer(12,AnAccount.LoginKey[2],Buffer);
    Socket.WriteBuffer(16, 0, Buffer);
	  Socket.WriteBuffer(20, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz'#0, Now), 24, Buffer);
	  Socket.WriteBuffer(44, 0, Buffer);
	  Socket.WriteBuffer(46,AnAccount.Gender,Buffer);

	  for Index := 0 to CharaServerList.Count - 1 do begin
		  Socket.WriteBuffer(47,TCharaServ(CharaServerList.Objects[Index]).IPCardinal,Buffer);
		  Socket.WriteBuffer(51,TCharaServ(CharaServerList.Objects[Index]).Port,Buffer);
		  Socket.WriteBuffer(53+Index*32,TCharaServ(CharaServerList.Objects[Index]).ServerName,20,Buffer);
		  Socket.WriteBuffer(73+Index*32,TCharaServ(CharaServerList.Objects[Index]).OnlineUsers,Buffer);
		  Socket.WriteBuffer(75,0,Buffer);
		  Socket.WriteBuffer(77,0,Buffer);
	  end;

    Socket.SendBuffer(AThread, Buffer ,Size);
  end;
end.
 