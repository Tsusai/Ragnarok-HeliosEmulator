unit Console;

interface
uses
	IdTCPServer,
	SysUtils,
	Classes;

type TMainProc = class(TComponent)
		LoginServer : TIdTCPServer;
		CharaServer: TIdTCPServer;
		ZoneServer: TIdTCPServer;

		procedure Console(Line : string);

		procedure Startup;
		procedure Shutdown;

		procedure LoginServerConnect(AThread: TIdPeerThread);
    procedure LoginServerExecute(AThread: TIdPeerThread);
		procedure CharaServerExecute(AThread: TIdPeerThread);
		procedure ServerException(AThread: TIdPeerThread;
			AException: Exception);

		constructor Create(AOwner : TComponent); override;
		destructor  Destroy; override;
end;

var
  MainProc : TMainProc;

implementation
uses
	LoginProcesses,
	CharaServerTypes,
	CharaServerPacket,
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
	Success : Boolean;
begin
  Console('');
	Console('- Helios is starting...');

	AppPath  := ExtractFilePath(ParamStr(0));
	InitGlobals;

	LoginServer.DefaultPort := ServerConfig.LoginPort;
	CharaServer.DefaultPort := ServerConfig.CharaPort;;
	ZoneServer.DefaultPort  := ServerConfig.ZonePort;
	LoginServer.Active := true;
	//CharaServer.Active := true;
	//ZoneServer.Active := true;

	Success := TRUE;

	MainProc.Console('');

	if CharaServer.Active then
	begin
		//Add local character server to the list
		LocalCharaServ := TCharaServ.Create;
		LocalCharaServ.IP := '127.0.0.1';
		LocalCharaServ.IPCardinal := 0;
		LocalCharaServ.InternalServer := TRUE;
		LocalCharaServ.ServerName := ServerConfig.ServerName;
		LocalCharaServ.Port := CharaServer.DefaultPort;
	end;

	if LoginServer.Active then
	begin
		if not ADatabase.LoadAccounts then begin  //changed to database object from functions - RaX
			Success := FALSE;
		end;
	end;

	if Success then
	begin
		Console('- Startup Success');
	end else
	begin
		Console('- Startup Failed');
	end;

	Console('------------------------------------');
	Console('For a list of console commands, input "/help".');
end;

procedure TMainProc.Shutdown;
begin
	Console('- Helios is shutting down...');
	DestroyGlobals;//Make sure globals are Free'd on Application exit.
end;

procedure TMainProc.ServerException(AThread: TIdPeerThread;
	AException: Exception);
begin
	if AThread.Connection.Connected then begin
		AThread.Connection.Disconnect; //On server exception, make sure client disconnects.
  end;
end;

procedure TMainProc.LoginServerConnect(AThread: TIdPeerThread);
begin
	Console('Connection from ' + AThread.Connection.Socket.Binding.PeerIP);
end;

procedure TMainProc.CharaServerExecute(AThread: TIdPeerThread);
begin
	ParseCharaServ(AThread);
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

	CharaServer.OnConnect := CharaServerExecute;
	CharaServer.OnException := ServerException;

	ZoneServer.OnException := ServerException;
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
