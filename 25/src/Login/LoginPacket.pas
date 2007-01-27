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
	WFIFOW( 0, $006a, Buffer);
	WFIFOB( 2, Error, Buffer);
	AThread.Connection.WriteBuffer(Buffer,23);
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

	WFIFOW(0,$0069,Buffer);
	WFIFOW(2,Size,Buffer);
	WFIFOL(4,AnAccount.LoginKey[1],Buffer);
	WFIFOL(8,AnAccount.ID,Buffer);
	WFIFOL(12,AnAccount.LoginKey[2],Buffer);
	WFIFOL(16, 0, Buffer);
	WFIFOS(20, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz'#0, Now), 24, Buffer);
	WFIFOW(44, 0, Buffer);
	WFIFOB(46,AnAccount.Gender,Buffer);

	for idx := 0 to CharaServerList.Count - 1 do
	begin
		WFIFOL(47,TCharaServ(CharaServerList.Objects[idx]).IPCardinal,Buffer);
		WFIFOW(51,TCharaServ(CharaServerList.Objects[idx]).Port,Buffer);
		WFIFOS(53+idx*32,TCharaServ(CharaServerList.Objects[idx]).ServerName,20,Buffer);
		WFIFOW(73+idx*32,TCharaServ(CharaServerList.Objects[idx]).OnlineUsers,Buffer);
		WFIFOW(75,0,Buffer);
		WFIFOW(77,0,Buffer);
	end;

	AThread.Connection.WriteBuffer(Buffer,Size);

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
			ID := RFIFOW(0,Buffer);
			Case ID of
			$0064: //Basic login packet
				begin
					UserName := RFIFOS(6,24,Buffer);
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
						Password := RFIFOS(30,24,Buffer);
						if AnAccount.Password = Password then
						begin
							AnAccount.LoginKey[1] := RFIFOL(54,Buffer);
							AnAccount.LoginKey[2] := RFIFOL(58,Buffer);
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

 