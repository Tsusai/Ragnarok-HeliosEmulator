//------------------------------------------------------------------------------
//Console()				                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is the main process, it handles each server and the starting/
//    stopping of each.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit Console;

interface

uses
	IdTCPServer,
	SysUtils,
	Classes,
	SaveLoop;

type
//------------------------------------------------------------------------------
//TMainProc                                                               CLASS
//------------------------------------------------------------------------------
  TMainProc = class(TComponent)
	public
		LoginServer : TIdTCPServer;
		CharaServer: TIdTCPServer;
		ZoneServer: TIdTCPServer;

		SaveLoop : TSaveLoop;

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
  end;{TMainProc}
//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------
//TMainProc.Console()                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Alias of WriteLn. Makes a bit more sense here =)
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TMainProc.Console(Line : string);
begin
	WriteLn(Line);
end;{TMainProc.Console}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMainProc.LoginServerExecute()                                          EVENT
//------------------------------------------------------------------------------
//	What it does-
//		 Starts the Internal Login server to process incoming connection attempts.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TMainProc.LoginServerExecute(AThread: TIdPeerThread);
begin
	ParseLogin(AThread);
end;{TMainProc.LoginServerExecute}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMainProc.StartUp()                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Initializes each server to it's default values, then it activates them.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TMainProc.Startup;
var
	LocalCharaServ : TCharaServ;
begin
	AppPath  := ExtractFilePath(ParamStr(0));
  SaveLoop := TSaveLoop.Create;
  SaveLoop.Interval := 5000;

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
		LocalCharaServ.IP := ServerConfig.WAN_IP;
		LocalCharaServ.InternalServer := TRUE;
		LocalCharaServ.ServerName := ServerConfig.ServerName;
		LocalCharaServ.Port := CharaServer.DefaultPort;
		CharaServerList.AddObject(LocalCharaServ.ServerName,LocalCharaServ);
	end;
	ThirdPartyCredits; //Load external credits file.

  Console('- Startup Success');
	MainProc.Console('  For a list of console commands, input "/help".');

end;{TMainProc.Startup}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMainProc.Shutdown()                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Gracefully disconnects clients, then calls Destroy Globals.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TMainProc.Shutdown;
begin
	Console('- Helios is shutting down...');
	DeActivateServer(LoginServer);
	DeActivateServer(CharaServer);
	DeActivateServer(ZoneServer);

	DestroyGlobals;//Make sure globals are Free'd on Application exit.
end;{TMainProc.Shutdown}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMainProc.ServerException()                                             EVENT
//------------------------------------------------------------------------------
//	What it does-
//			Handles Socket exceptions gracefully by outputting the exception message
//    and then disconnecting the client.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TMainProc.ServerException(AThread: TIdPeerThread;
	AException: Exception);
begin
	if AnsiContainsStr(AException.Message, IntToStr(10053)) or
		AnsiContainsStr(AException.Message, IntToStr(10054))
	then begin
		AThread.Connection.Disconnect;
	end;
end;{TMainProc.ServerException}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMainProc.LoginServerConnect()                                          EVENT
//------------------------------------------------------------------------------
//	What it does-
//			Event which is fired when a user attempts to connect to the login
//    server. It writes information about the connection to the console.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TMainProc.LoginServerConnect(AThread: TIdPeerThread);
begin
	Console('Connection from ' + AThread.Connection.Socket.Binding.PeerIP);
end;{TMainProc.LoginServerConnect}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMainProc.CharaServerConnect()                                          EVENT
//------------------------------------------------------------------------------
//	What it does-
//			An event which fires when a user connects to the chara server.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TMainProc.CharaServerConnect(AThread: TIdPeerThread);
var
	Link : TThreadLink;
begin
	Link := TThreadLink.Create;
	AThread.Data := Link;
end;{TMainProc.CharaServerConnect}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMainProc.CharaServerExecute()                                          EVENT
//------------------------------------------------------------------------------
//	What it does-
//			An event which fires when the server is started. It allows the server
//    to accept incoming client connections.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TMainProc.CharaServerExecute(AThread: TIdPeerThread);
begin
	ParseCharaServ(AThread);
end;{TMainProc.ChaServerExecute}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMainProc.ThirdPartyCredits()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Writes the credits defined in thirdparty.txt to the console.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
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
end;{TMainProc.ThirdPartyCredits}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMainProc.Create()                                                CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes the servers and starts them.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
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

end;{TMainProc.Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMainProc.Destroy()                                               DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Shutdown all servers and free em' up!
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
destructor  TMainProc.Destroy;
begin
	//MUST KILL COMPONENTS
	if Assigned(LoginServer) then FreeAndNil(LoginServer);
	if Assigned(CharaServer) then FreeAndNil(CharaServer);
	if Assigned(ZoneServer) then FreeAndNil(ZoneServer);

  SaveLoop.Terminate;

	inherited Destroy;
end;{TMainProc.Destroy}
//------------------------------------------------------------------------------


end{Console}.
