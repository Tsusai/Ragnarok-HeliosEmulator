unit Console;

interface
uses
	IdTCPServer,
	SysUtils,
	Classes;

type TMainProc = class(TComponent)
	public
		LoginServer : TIdTCPServer;
		CharaServer: TIdTCPServer;
		ZoneServer: TIdTCPServer;

		//Active : boolean;
		SQLConnected : boolean;

		procedure Console(Line : string);

		procedure Startup;
		procedure Shutdown;

		procedure LoginServerConnect(AThread: TIdPeerThread);
		procedure LoginServerExecute(AThread: TIdPeerThread);
		procedure CharaServerConnect(AThread: TIdPeerThread);
		procedure CharaServerExecute(AThread: TIdPeerThread);
		procedure ServerException(AThread: TIdPeerThread;
			AException: Exception);

		procedure ThirdPartyCredits;

		constructor Create(AOwner : TComponent); override;
		destructor  Destroy; override;
end;

var
	MainProc : TMainProc;

implementation
uses
	StrUtils,
	LoginProcesses,
	CharaServerTypes,
	CharaServerProcess,
	PacketTypes,
	WinLinux,
	TCPServerRoutines,
	Globals;

procedure TMainProc.Console(Line : string);
begin
	Writeln(Line);
end;

procedure TMainProc.LoginServerExecute(AThread: TIdPeerThread);
begin
	ParseLogin(AThread);
end;

procedure TMainProc.Startup;
var
	LocalCharaServ : TCharaServ;
begin
	AppPath  := ExtractFilePath(ParamStr(0));
	InitGlobals;

	SetupTerminationCapturing;

	LoginServer.DefaultPort := ServerConfig.LoginPort;
	CharaServer.DefaultPort := ServerConfig.CharaPort;;
	ZoneServer.DefaultPort  := ServerConfig.ZonePort;
	ActivateServer('Login',LoginServer);
	ActivateServer('Character',CharaServer);
	//ActivateServer('Zone',ZoneServer);

	MainProc.Console('');

	if CharaServer.Active then
	begin
		//Add local character server to the list
		LocalCharaServ := TCharaServ.Create;
		LocalCharaServ.IP := '127.0.0.1';
		LocalCharaServ.InternalServer := TRUE;
		LocalCharaServ.ServerName := ServerConfig.ServerName;
		LocalCharaServ.Port := CharaServer.DefaultPort;
		CharaServerList.AddObject(LocalCharaServ.ServerName,LocalCharaServ);
	end;
	ThirdPartyCredits; //Load external credits file.
	if ConnectToMySQL then
	begin
		Console('- Startup Success');
	end else
	begin
		Console('- Startup Failed');
	end;
	MainProc.Console('  For a list of console commands, input "/help".');

end;

procedure TMainProc.Shutdown;
begin
	Console('- Helios is shutting down...');
	DeActivateServer(LoginServer);
	DeActivateServer(CharaServer);
	DeActivateServer(ZoneServer);
	DestroyGlobals;//Make sure globals are Free'd on Application exit.
end;

procedure TMainProc.ServerException(AThread: TIdPeerThread;
	AException: Exception);
begin
	if AnsiContainsStr(AException.Message, IntToStr(10053)) or
		AnsiContainsStr(AException.Message, IntToStr(10054))
	then begin
		AThread.Connection.Disconnect;
	end;
end;

procedure TMainProc.LoginServerConnect(AThread: TIdPeerThread);
begin
	Console('Connection from ' + AThread.Connection.Socket.Binding.PeerIP);
end;

procedure TMainProc.CharaServerConnect(AThread: TIdPeerThread);
var
	Link : TThreadLink;
begin
	Link := TThreadLink.Create;
	AThread.Data := Link;
end;

procedure TMainProc.CharaServerExecute(AThread: TIdPeerThread);
begin
	ParseCharaServ(AThread);
end;

procedure TMainProc.ThirdPartyCredits;
var
	ThirdParty : TStringList;
	idx : integer;
begin
	if FileExists(AppPath + '3rdParty.txt') then
	begin
		ThirdParty := TStringList.Create;
		ThirdParty.LoadFromFile(AppPath + '3rdParty.txt');
		for idx := 0 to ThirdParty.Count - 1 do
		begin
			Console('  '+ThirdParty.Strings[idx]);
		end;
		ThirdParty.Free;
	end
end;

constructor TMainProc.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
	LoginServer := TIdTCPServer.Create(nil);
	CharaServer := TIdTCPServer.Create(nil);
	ZoneServer  := TIdTCPServer.Create(nil);

	LoginServer.OnExecute := LoginServerExecute;
	LoginServer.OnConnect := LoginServerConnect;
	LoginServer.OnException := ServerException;

	CharaServer.OnConnect := CharaServerConnect;
	CharaServer.OnExecute := CharaServerExecute;
	CharaServer.OnException := ServerException;

	ZoneServer.OnException := ServerException;

	SQLConnected := False;
end;

destructor  TMainProc.Destroy;
begin
	//MUST KILL COMPONENTS
	if Assigned(LoginServer) then FreeAndNil(LoginServer);
	if Assigned(CharaServer) then FreeAndNil(CharaServer);
	if Assigned(ZoneServer) then FreeAndNil(ZoneServer);

	inherited Destroy;
end;





end.
