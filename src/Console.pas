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
	IdTCPClient,
	IdContext,
	SysUtils,
	Classes,
	XTimer;

type
//------------------------------------------------------------------------------
//TMainProc                                                               CLASS
//------------------------------------------------------------------------------
	TMainProc = class(TComponent)
	public
		Run         : Boolean;

		LoginServer : TIdTCPServer;
		CharaServer : TIdTCPServer;
		ZoneServer  : TIdTCPServer;
		InterServer : TIdTCPServer;

		CharaToLoginClient : TIdTCPClient;
		ZoneToCharaClient  : TIdTCPClient;
		ZoneToInterClient  : TIdTCPClient;

		SaveTimer   : TXTimer;

		procedure ForceSave(Sender : TObject);

		procedure Console(Line : string);

		procedure Startup;
		procedure Shutdown;

		procedure LoginServerConnect(AConnection: TIdContext);
		procedure LoginServerExecute(AConnection: TIdContext);
		procedure CharaServerConnect(AConnection: TIdContext);
		procedure CharaServerExecute(AConnection: TIdContext);
		procedure ZoneServerConnect (AConnection: TIdContext);
		procedure ZoneServerExecute (AConnection: TIdContext);
		procedure InterServerConnect(AConnection: TIdContext);
		procedure InterServerExecute(AConnection: TIdContext);
		procedure ServerException(AConnection: TIdContext;
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
	{Internal}
	StrUtils,
	{Helios}
	CharaServerProcess,
	CharaServerTypes,
	Globals,
	LoginProcesses,
	PacketTypes,
	SaveThread,
	TCPServerRoutines,
	WinLinux,
	ZoneCore
	{Third Party}
	;

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
//TMainProc.ForceSave()                                             Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Executes the save thread
//
//	Changes -
//		October 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TMainProc.ForceSave(Sender : Tobject);
begin
	TSaveThread.Create;
end;{TMainProc.CharaServerConnect}

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
procedure TMainProc.LoginServerExecute(AConnection: TIdContext);
begin
	ParseLogin(AConnection);
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
	ThirdPartyCredits; //Load external credits file.
	AppPath  := ExtractFilePath(ParamStr(0));

	InitGlobals;

	SetupTerminationCapturing;

	LoginServer.DefaultPort := ServerConfig.LoginPort;
	CharaServer.DefaultPort := ServerConfig.CharaPort;
	ZoneServer.DefaultPort  := ServerConfig.ZonePort;
	InterServer.DefaultPort := ServerConfig.InterPort;

	if ServerConfig.LoginEnabled then
	begin
		ActivateServer('Login',LoginServer);
	end;
  //NOTE: Prior 
	if ServerConfig.CharaEnabled then
	begin
		ActivateServer('Character',CharaServer);
		ActivateClient(CharaToLoginClient);
	end;

	if ServerConfig.InterEnabled then
	begin
		ActivateServer('Inter', InterServer);
	end;

	if ServerConfig.ZoneEnabled then
	begin
		ActivateServer('Zone',ZoneServer);
		ActivateClient(ZoneToCharaClient);
		ActivateClient(ZoneToInterClient);
	end;

	MainProc.Console('');

	if CharaServer.Active then
	begin
		//REMOVE SOON!!!
		//Add local character server to the list
		LocalCharaServ := TCharaServ.Create;
		LocalCharaServ.IP := ServerConfig.WAN_IP;
		LocalCharaServ.InternalServer := TRUE;
		LocalCharaServ.ServerName := ServerConfig.ServerName;
		LocalCharaServ.Port := CharaServer.DefaultPort;
		CharaServerList.AddObject(LocalCharaServ.ServerName,LocalCharaServ);
	end;

	Run := TRUE;

	Console('- Startup Success');
	Console('  For a list of console commands, input "/help".');

end;{TMainProc.Startup}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMainProc.Shutdown()                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Gracefully disconnects clients, Saves online characters,  then calls
//    Destroy Globals.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TMainProc.Shutdown;
begin
	Console('- Helios is shutting down...');

	//Disconnect clients.
	DeActivateServer(LoginServer);
	DeActivateServer(CharaServer);
	DeActivateServer(ZoneServer);
	DeActivateServer(InterServer);

	//Deactivate interserver relations
	CharaToLoginClient.Disconnect;
	ZoneToCharaClient.Disconnect;
	ZoneToInterClient.Disconnect;

	//Make sure globals are Free'd on Application exit.
	DestroyGlobals;

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
procedure TMainProc.ServerException(AConnection: TIdContext;
	AException: Exception);
begin
	if AnsiContainsStr(AException.Message, IntToStr(10053)) or
		AnsiContainsStr(AException.Message, IntToStr(10054))
	then begin
		AConnection.Connection.Disconnect;
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
procedure TMainProc.LoginServerConnect(AConnection: TIdContext);
begin
	Console('Connection from ' + AConnection.Connection.Socket.Binding.PeerIP);
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
//		November  11th, 2006 - Tsusai - Removed Link variable, not needed.
//
//------------------------------------------------------------------------------
procedure TMainProc.CharaServerConnect(AConnection: TIdContext);
begin
	AConnection.Data := TThreadLink.Create;
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
procedure TMainProc.CharaServerExecute(AConnection: TIdContext);
begin
	ParseCharaServ(AConnection);
end;{TMainProc.ChaServerExecute}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMainProc.ZoneServerConnect()                                           EVENT
//------------------------------------------------------------------------------
//	What it does-
//			An event which fires when a user connects to the Zone Server.
//
//	Changes -
//		November 11th, 2006 - Tsusai - Created.
//
//------------------------------------------------------------------------------
procedure TMainProc.ZoneServerConnect(AConnection: TIdContext);
begin
	AConnection.Data := TThreadLink.Create;
end;{TMainProc.ZoneServerConnect}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMainProc.ZoneServerExecute()                                          EVENT
//------------------------------------------------------------------------------
//	What it does-
//			An event which fires when the server is started. It allows the server
//    to accept incoming client connections.
//
//	Changes -
//		November 11th, 2006 - Tsusai - Created.
//
//------------------------------------------------------------------------------
procedure TMainProc.ZoneServerExecute(AConnection: TIdContext);
begin
	ProcessZonePacket(AConnection);
end;{TMainProc.ZoneServerExecute}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMainProc.InterServerConnect()                                           EVENT
//------------------------------------------------------------------------------
//	What it does-
//			An event which fires when a user connects to the Inter Server.
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TMainProc.InterServerConnect(AConnection: TIdContext);
begin
	//inter server connect code here
end;{TMainProc.ZoneServerConnect}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMainProc.InterServerExecute()                                          EVENT
//------------------------------------------------------------------------------
//	What it does-
//			An event which fires when the server is started. It allows the server
//    to accept incoming client connections.
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TMainProc.InterServerExecute(AConnection: TIdContext);
begin
	//add packet parser here
end;{TMainProc.InterServerExecute}
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
	ThirdParty  : TStringList;
	LineIndex   : Integer;
begin
	if FileExists(AppPath + '3rdParty.txt') then
	begin
		ThirdParty := TStringList.Create;
		ThirdParty.LoadFromFile(AppPath + '3rdParty.txt');
		for LineIndex := 0 to ThirdParty.Count - 1 do
		begin
			Console('  '+ThirdParty.Strings[LineIndex]);
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
	LoginServer := TIdTCPServer.Create;
	CharaServer := TIdTCPServer.Create;
	ZoneServer  := TIdTCPServer.Create;
	InterServer := TIdTCPServer.Create;

	CharaToLoginClient := TIdTCPClient.Create;
	ZoneToCharaClient  := TIdTCPClient.Create;
	ZoneToInterClient  := TIdTCPClient.Create;

	LoginServer.OnExecute := LoginServerExecute;
	LoginServer.OnConnect := LoginServerConnect;
	LoginServer.OnException := ServerException;

	CharaServer.OnConnect := CharaServerConnect;
	CharaServer.OnExecute := CharaServerExecute;
	CharaServer.OnException := ServerException;

	ZoneServer.OnConnect  := ZoneServerConnect;
	ZoneServer.OnExecute  := ZoneServerExecute;
	ZoneServer.OnException := ServerException;

	InterServer.OnConnect  := InterServerConnect;
	InterServer.OnExecute  := InterServerExecute;
	InterServer.OnException := ServerException;

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
	if Assigned(LoginServer)  then FreeAndNil(LoginServer);
	if Assigned(CharaServer)  then FreeAndNil(CharaServer);
	if Assigned(ZoneServer)   then FreeAndNil(ZoneServer);
	if Assigned(InterServer)  then FreeAndNil(InterServer);

	if Assigned(CharaToLoginClient)  then FreeAndNil(CharaToLoginClient);
	if Assigned(ZoneToCharaClient)   then FreeAndNil(ZoneToCharaClient);
	if Assigned(ZoneToInterClient)   then FreeAndNil(ZoneToInterClient);

	inherited Destroy;
end;{TMainProc.Destroy}
//------------------------------------------------------------------------------


end{Console}.
