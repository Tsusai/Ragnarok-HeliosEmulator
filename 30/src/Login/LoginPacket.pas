unit LoginPacket;

interface
uses
	IdTCPServer;

procedure ParseLogin(AThread: TIdPeerThread);

implementation
uses
	SysUtils,

	Globals,
	PacketFunc,
	PacketTypes,
	Main,
	CharaServerTypes,
	AccountTypes;

const
	LOGIN_UNREGISTERED    = 0;
	LOGIN_INVALIDPASSWORD = 1;
	//LOGIN_SERVERFULL =


procedure SendLoginError(const AThread: TIdPeerThread; const Error : byte);
var
	Buffer : TBuffer;
begin
	Socket.WriteBuffer( 0, $006a, Buffer);
	Socket.WriteBuffer( 2, Error, Buffer);
	Socket.SendBuffer(AThread, Buffer ,23);
end;

procedure SendCharacterServers(AnAccount : TAccount; AThread: TIdPeerThread);
var
	Buffer : TBuffer;
	idx : byte;
	Size : cardinal;
begin
	//R 0069 <len>.w <login ID1>.l <account ID>.l <login ID2>.l ?.32B <sex>.B
	//{<IP>.l <port>.w <server name>.20B <login users>.w <maintenance>.w <new>.w}.32B*
	Size := 2+2+4+4+4+32+1+(CharaServerList.Count * 32);
  with Socket do begin
	  WriteBuffer(0,$0069,Buffer);
	  WriteBuffer(2,Size,Buffer);
	  WriteBuffer(4,AnAccount.LoginKey[1],Buffer);
	  WriteBuffer(8,AnAccount.ID,Buffer);
	  WriteBuffer(12,AnAccount.LoginKey[2],Buffer);
	  WriteBuffer(16, 0, Buffer);
	  WriteBuffer(20, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz'#0, Now), 24, Buffer);
	  WriteBuffer(44, 0, Buffer);
	  WriteBuffer(46,AnAccount.Gender,Buffer);

	  for idx := 0 to CharaServerList.Count - 1 do
	  begin
		  WriteBuffer(47,TCharaServ(CharaServerList.Objects[idx]).IPCardinal,Buffer);
		  WriteBuffer(51,TCharaServ(CharaServerList.Objects[idx]).Port,Buffer);
		  WriteBuffer(53+idx*32,TCharaServ(CharaServerList.Objects[idx]).ServerName,20,Buffer);
		  WriteBuffer(73+idx*32,TCharaServ(CharaServerList.Objects[idx]).OnlineUsers,Buffer);
		  WriteBuffer(75,0,Buffer);
		  WriteBuffer(77,0,Buffer);
	  end;
  end;
	Socket.SendBuffer(AThread, Buffer ,Size);

end;

procedure ParseLogin(AThread: TIdPeerThread);
var
	PLength : integer;
	Buffer : TBuffer;
	UserName : string;
	Password : string;
	AnAccount : TAccount;
	idx : integer;
	Found : boolean;
	ID : word;
begin
	Found := false;
	AnAccount := nil;
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
					for idx := 0 to AccountList.Count -1 do
					begin
						if AccountList.Objects[idx] is TAccount then begin
							AnAccount := TAccount(AccountList.Objects[idx]);
							if AnAccount.Username = UserName then
							begin
								Found := true;
								break;
							end;
						end else continue;
					end;
					if Found then begin
						Password := Socket.BufferReadString(30,24,Buffer);
						if AnAccount.Password = Password then
						begin
							AnAccount.LoginKey[1] := Socket.BufferReadCardinal(54,Buffer);
							AnAccount.LoginKey[2] := Socket.BufferReadCardinal(58,Buffer);
							AnAccount.LastIP := AThread.Connection.Socket.Binding.PeerIP;
							SendCharacterServers(AnAccount,AThread);
						end else
						begin
							SendLoginError(AThread,LOGIN_INVALIDPASSWORD);
						end;
					end else
					begin
						SendLoginError(AThread,LOGIN_UNREGISTERED);
					end;
				end;
			end;
		end;
	end;
end;

end.

 