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
  Database,
  LoginServer,
  CharacterServer,
  InterServer,
  ZoneServer;

type
//------------------------------------------------------------------------------
//TMainProc                                                               CLASS
//------------------------------------------------------------------------------
	TMainProc = class(TComponent)
	public
		Run         : Boolean;

		LoginServer : TLoginServer;
		CharacterServer : TCharacterServer;
		ZoneServer  : TZoneServer;
		InterServer : TInterServer;

    AGameDatabase      : TDatabase;
    ACommonDatabase    : TDatabase;

		procedure Console(Line : string);

		procedure Startup;
		procedure Shutdown;

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
	Globals,
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
begin
	ThirdPartyCredits; //Load external credits file.
	AppPath  := ExtractFilePath(ParamStr(0));

	InitGlobals;

	SetupTerminationCapturing;

  //Create Database Objects
  ACommonDatabase := TDatabase.Create(FALSE);
  AGameDatabase   := TDatabase.Create(TRUE);

//Start and create Enabled Servers
	if ServerConfig.LoginEnabled then
	begin
    LoginServer      := TLoginServer.Create;
		LoginServer.Port := ServerConfig.LoginPort;
    LoginServer.Start;
	end;
	//NOTE: Prior
	if ServerConfig.CharaEnabled then
	begin
    CharacterServer             := TCharacterServer.Create;
    CharacterServer.Port        := ServerConfig.CharaPort;
    CharacterServer.IP          := ServerConfig.CharaWANIP;
    CharacterServer.ServerName  := ServerConfig.ServerName;
		CharacterServer.Start;
    CharaServerList.AddObject(CharacterServer.ServerName,CharacterServer);
	end;

	if ServerConfig.InterEnabled then
	begin
    InterServer       := TInterServer.Create;
		InterServer.Port  := ServerConfig.InterPort;
    InterServer.IP    := ServerConfig.InterWANIP;
		InterServer.Start;
	end;

	if ServerConfig.ZoneEnabled then
	begin
    ZoneServer       := TZoneServer.Create;
		ZoneServer.Port  := ServerConfig.ZonePort;
    ZoneServer.IP    := ServerConfig.ZoneWANIP;
    ZoneServer.Start;
	end;

	MainProc.Console('');

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
	if Assigned(LoginServer) then
	begin
    LoginServer.Stop;
    LoginServer.Free;
	end;
	//NOTE: Prior
	if Assigned(CharacterServer) then
	begin
    CharacterServer.Stop;
    CharacterServer.Free;
	end;

	if Assigned(InterServer) then
	begin
    InterServer.Stop;
    InterServer.Free;
	end;

	if Assigned(ZoneServer) then
	begin
    ZoneServer.Stop;
    ZoneServer.Free;
	end;

  //Free Databases
  ACommonDatabase.Free;
  AGameDatabase.Free;

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
	inherited Destroy;
end;{TMainProc.Destroy}
//------------------------------------------------------------------------------


end{Console}.
