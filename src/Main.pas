//------------------------------------------------------------------------------
//Main()				                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is the main process, it handles each server and the starting/
//    stopping of each.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit Main;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
	Classes,
	LoginServer,
	CharacterServer,
	InterServer,
	ZoneServer,
	HeliosOptions;

type

//------------------------------------------------------------------------------
//TMainProc                                                               CLASS
//------------------------------------------------------------------------------
	TMainProc = class(TComponent)
	private
		Procedure EnsureFileStructure;

	public
		Run							: Boolean;
		Loaded					: Boolean;

		ContinueLoading : Boolean;

		LoginServer 		: TLoginServer;
		CharacterServer : TCharacterServer;
		ZoneServer  		: TZoneServer;
		InterServer 		: TInterServer;

		Options         : THeliosOptions;

		procedure LoadOptions;
		
		procedure Startup;
		procedure Shutdown;

		procedure DisplayHeader;

		constructor Create(AOwner : TComponent); override;
		destructor  Destroy; override;
	end;{TMainProc}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//                            Published Variables
//------------------------------------------------------------------------------
var
	MainProc : TMainProc;
//------------------------------------------------------------------------------


implementation
uses
	SysUtils,
	{Helios}
	Globals,
	Version
	{Third Party}
	;


//------------------------------------------------------------------------------
//TMainProc.Create()                                                CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes the servers and starts them.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//		[2007/06/21] Tsusai - Removed server creation.
//
//------------------------------------------------------------------------------
constructor TMainProc.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
	Loaded := FALSE;
	ContinueLoading := true;
	LoadOptions;
	EnsureFileStructure;

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
//		[2007/06/21] Tsusai - Servers removed only if they are created.
//
//------------------------------------------------------------------------------
destructor  TMainProc.Destroy;
begin
	Options.Save;
	Options.Free;

	inherited Destroy;
end;{TMainProc.Destroy}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMainProc.StartUp()                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Initializes each server to it's default values, then it activates them.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//		January 20th, 2007 - Tsusai - If server passes PreLoad check (packet_db
//			and database connect), then start the servers.  Readded Failed to
//			Start message.
//		[2007/06/21] Tsusai - Creates the servers only if they are needed.
//
//------------------------------------------------------------------------------
procedure TMainProc.Startup;
begin
	Run := TRUE;
	ContinueLoading := InitGlobals;

	//Start and create Enabled Servers
	if Options.LoginEnabled then
	begin
		LoginServer      := TLoginServer.Create;
		if ContinueLoading then
		begin
			LoginServer.Start;
		end;
	end;
	//NOTE: Prior
	if Options.CharaEnabled then
	begin
		CharacterServer  := TCharacterServer.Create;
		if ContinueLoading then
		begin
			CharacterServer.Start;
		end;
	end;

	if Options.InterEnabled then
	begin
		InterServer      := TInterServer.Create;
		if ContinueLoading then
		begin
			InterServer.Start;
		end;
	end;

	if Options.ZoneEnabled then
	begin
		ZoneServer       := TZoneServer.Create;
		if ContinueLoading then
		begin
			ZoneServer.Start;
		end;
	end;

	if ContinueLoading then
	begin
		Console.Message('Startup success', 'SYSTEM', MS_INFO);
		Console.Message('For a list of commands, please type "/help".', 'SYSTEM', MS_INFO);

		//Link Enabled Servers
		if Options.CharaEnabled then
		begin
			CharacterServer.ConnectToLogin;
		end;
		if Options.ZoneEnabled then
		begin
			ZoneServer.ConnectToCharacter;
			ZoneServer.ConnectToInter;
		end;
	end else
  begin
		Console.Message('Startup failed', 'SYSTEM', MS_ERROR);
		Console.Message('Please shutdown helios and correct the above errors.', 'SYSTEM', MS_INFO);
  end;
	Loaded := TRUE;
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
//		January 20th, 2007 - Tsusai - Reversed shutdown order so server clients
//			aren't disconnected and attempt to reconnect (cleaner shutdown
//			messages)
//		[2007/06/21] Tsusai - Stops created servers.
//
//------------------------------------------------------------------------------
procedure TMainProc.Shutdown;
begin
	Loaded := FALSE;
	Console.WriteLn('- Helios is shutting down...');
	
	//Go backwards (so zone doesn't try and connect to character while shutting down)
	if Assigned(ZoneServer) then ZoneServer.Stop;
	if Assigned(InterServer) then InterServer.Stop;
	if Assigned(CharacterServer) then CharacterServer.Stop;
	if Assigned(LoginServer) then LoginServer.Stop;

	if Assigned(ZoneServer) then ZoneServer.Free;
	if Assigned(InterServer) then InterServer.Free;
	if Assigned(CharacterServer) then CharacterServer.Free;
	if Assigned(LoginServer) then LoginServer.Free;
	//Make sure globals are Free'd on Application exit.
	DestroyGlobals;
end;{TMainProc.Shutdown}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//DisplayHeader                                                      PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Writes the Helios header to the console.
//
//	Changes -
//		February 24th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TMainProc.DisplayHeader;
begin
	Console.WriteLn('  _    _          _   _                ');
	Console.WriteLn(' | |  | |        | | (_)               ');
	Console.WriteLn(' | |__| |   ___  | |  _    ___    ___  ');
	Console.WriteLn(' |  __  |  / _ \ | | | |  / _ \  / __| ');
	Console.WriteLn(' | |  | | |  __/ | | | | | (_) | \__ \ ');
	Console.WriteLn(' |_|  |_|  \___| |_| |_|  \___/  |___/ ');
	Console.WriteLn('');
	Console.WriteLn(Format('- %s is starting...',[HeliosVersion]));
	Console.WriteLn('');
end;//DisplayHeader
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadOptions                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Creates and Loads the inifile.
//
//	Changes -
//		January 4th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TMainProc.LoadOptions;
begin
	Options    := THeliosOptions.Create('./'+ChangeFileExt(ExeName,'')+'.ini');

	Options.Load;
	Options.Save;
end;{LoadOptions}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//EnsureFileStructure                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Makes sure the files/folders required to run helios exist before startup.
//
//	Changes -
//		February 25th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TMainProc.EnsureFileStructure;
begin

	if NOT DirectoryExists(Options.ConfigDirectory) then
	begin
		CreateDir(Options.ConfigDirectory);
	end;

	if NOT DirectoryExists(Options.DatabaseDirectory) then
	begin
		CreateDir(Options.DatabaseDirectory);
	end;

	if NOT DirectoryExists(Options.MapDirectory) then
	begin
		CreateDir(Options.MapDirectory);
	end;

	if NOT DirectoryExists(Options.ScriptDirectory) then
	begin
		CreateDir(Options.ScriptDirectory);
	end;
end;{LoadOptions}
//------------------------------------------------------------------------------

end{Console}.
