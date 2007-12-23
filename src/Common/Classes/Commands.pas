//------------------------------------------------------------------------------
//Commands                                                                  UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This Unit was built to house the routines dealing with processing
//    console commands.
//
//  Notes -
//      RaX-The EXIT command simply sets Run FALSE in the parser. Look for it in
//    Parse()
//
//	Changes -
//		September 20th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit Commands;

interface
uses
	Classes;

type
//------------------------------------------------------------------------------
//TCommands                                                                CLASS
//------------------------------------------------------------------------------
	TCommands = class
	public
		Procedure Parse(InputText : String);

	private
		function Credits : String;
		function Help : String;
		//function Reload() : String;
		function Restart() : String;
		function Start(Values : TStringList) : String;
		function Stop(Values : TStringList) : String;
	end;{TCommands}
//------------------------------------------------------------------------------

implementation

	uses
		SysUtils,
		Main,
		LoginServer,
		CharacterServer,
		InterServer,
		ZoneServer,
		Globals;

//------------------------------------------------------------------------------
//Parse()                                                              PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Parses InputText for commands. Sets MainProc.Run which determines
//    whether or not the entire program should continue to run.
//    (See Exit Command)
//
//	Changes -
//		September 20th, 2006 - RaX - Added Trim function before using InputText to
//                                force commands even when whitespace is present
//                                before it.
//		September 20th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TCommands.Parse(InputText : String);
var
	StringIn  : TStringList;
	Index     : Integer;
	Command   : String;
	Values    : TStringList;
	Error     : String;
begin
	if InputText > '' then begin
		StringIn  := TStringList.Create;
		try
			StringIn.DelimitedText := Trim(InputText);
			if StringIn.DelimitedText[1] = '/' then begin
				Command := LowerCase(StringIn.Strings[0]);  //Gets the command text
				Values  := TStringList.Create;
				try
					for Index := 1 to StringIn.Count-1 do begin  //retrieves values after the command
						Values.Add(StringIn.Strings[Index]);
					end;
					{Start Command Parser}
					if Command = '/exit' then
					begin
						MainProc.Run  := FALSE;
						Error   := '';
					end else
					if Command = '/reload' then
					begin
						Error   := 'Reload not setup till all DB is done';//ADataBase.Reload;
					end else
					if Command = '/credits' then
					begin
						Error   := Credits;
					end else
					if Command = '/help' then
					begin
						Error   := Help;
					end else
					if Command = '/restart' then
					begin
						Error   := Restart;
					end else
					if Command = '/start' then
					begin
						Error   := Start(Values);
					end else
					if Command = '/stop' then
					begin
						Error   := Stop(Values);
					end else
					begin
						Error   := Command + ' does not exist!';
					end;
					{End Command Parser}
				finally  //Begin Cleanup
					Values.Free;
				end;
				if Error <> '' then begin  //Display Errors
					Console.Message('Command ' + Command + ' failed - ' + Error, 'Command Parser', MS_ALERT)
				end else begin
					Console.Message('Command ' + Command + ' success!', 'Command Parser', MS_ALERT);
				end;
			end;
		finally
			StringIn.Free;
		end;
	end;
end;{TCommands.Parse}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Credits()                                                             FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Displays third party credits for Helios.
//
//	Changes -
//		February 25th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TCommands.Credits;
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
			Console.WriteLn('  '+ThirdParty.Strings[LineIndex]);
		end;
		ThirdParty.Free;
	end;
end;//Credits
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Help()                                                                FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Writes a list of commands to the console.
//
//	Changes -
//		September 20th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TCommands.Help : String;
begin
	Console.WriteLn('The available console commands are as follows...');
	Console.WriteLn('--------------------------------------');
	Console.WriteLn('/start - starts a server');
	Console.WriteLn('/stop - stops a server');
	Console.WriteLn('/restart - restarts all enabled servers');
	Console.WriteLn('/exit - exits the program');
	Console.WriteLn('/credits - displays our credits.');
	Console.WriteLn('/help - lists all console commands');
	Console.WriteLn('--------------------------------------');
	Result := '';
end;{Help}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Reload()                                                              FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Will(in the future) free up and reload the Databases.
//
//	Changes -
//		September 20th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
{function TCommands.Reload() : String;
begin
	//To be done when all DB is done.  One swoop kill
end;{Reload}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Restart()                                                             FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Restarts all enabled servers.
//
//	Changes -
//		September 20th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TCommands.Restart() : String;
begin
	MainProc.Shutdown;
	MainProc.Startup;
	Result := '';
end;{Restart}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Start()                                                               FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Starts a server.
//
//	Changes -
//		September 20th, 2006 - RaX - Created Header.
//		[2007/06/21] Tsusai - Creates the servers now.
//
//------------------------------------------------------------------------------
function TCommands.Start(Values :  TStringList) : String;
begin
	if Values.Count > 0 then
	begin
		Values[0] := LowerCase(Values[0]);
		if Values[0] = 'login' then
		begin
			if not Assigned(MainProc.LoginServer) then
			begin
				MainProc.LoginServer := TLoginServer.Create;
				MainProc.LoginServer.Start;
			end;
		end else
		if Values[0] = 'character' then
		begin
			if not Assigned(MainProc.CharacterServer) then
			begin
				MainProc.CharacterServer := TCharacterServer.Create;
				MainProc.CharacterServer.Start;
				MainProc.CharacterServer.ConnectToLogin;
			end;
		end else
		if Values[0] = 'inter' then
		begin
			if not Assigned(MainProc.InterServer) then
			begin
				MainProc.InterServer := TInterServer.Create;
				MainProc.InterServer.Start;
			end;
		end else
		if Values[0] = 'zone' then
		begin
			if not Assigned(MainProc.ZoneServer) then
			begin
				MainProc.ZoneServer := TZoneServer.Create;
				MainProc.ZoneServer.Start;
				MainProc.ZoneServer.ConnectToCharacter;
				MainProc.ZoneServer.ConnectToInter;
			end;
		end else
		begin
			Result := Values[0] + ' is not a valid server';
		end;
	end else
	begin
		//display help for Start()
		Console.WriteLn('Using /Start...');
		Console.WriteLn('"/Start <ServerName>" - starts <ServerName>');
		Console.WriteLn('ServerName can be "Login", "Character", "Zone", or "Inter"');
	end;

end;{Start}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Stop()                                                                FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Stops a server.
//
//	Changes -
//		September 20th, 2006 - RaX - Created Header.
//		[2007/06/21] Tsusai - Frees the servers now.
//
//------------------------------------------------------------------------------
function TCommands.Stop(Values :  TStringList) : String;
begin
	if Values.Count > 0 then
	begin
		Values[0] := LowerCase(Values[0]);
		if Values[0] = 'login' then
		begin
			if Assigned(MainProc.LoginServer) then
			begin
				MainProc.LoginServer.Stop;
				system.Writeln('#1');
				FreeAndNil(MainProc.LoginServer);
				system.Writeln('#2');
			end;
		end else
		if Values[0] = 'character' then
		begin
			if Assigned(MainProc.CharacterServer) then
			begin
				MainProc.CharacterServer.Stop;
				FreeAndNil(MainProc.CharacterServer);
			end;
		end else
		if Values[0] = 'inter' then
		begin
			if Assigned(MainProc.InterServer) then
			begin
				MainProc.InterServer.Stop;
				FreeAndNil(MainProc.InterServer);
			end;
		end else
		if Values[0] = 'zone' then
		begin
			if Assigned(MainProc.ZoneServer) then
			begin
				MainProc.ZoneServer.Stop;
				FreeAndNil(MainProc.ZoneServer);
			end;
		end else
		begin
			Result := Values[0] + ' is not a valid server';
		end;
	end else
	begin
		//display help for Stop()
		Console.WriteLn('Using /Stop...');
		Console.WriteLn('"/Stop <ServerName>" - stops <ServerName>');
		Console.WriteLn('ServerName can be "Login", "Character", "Zone", or "Inter"');
	end;

end;{Stop}
//------------------------------------------------------------------------------
end{Commands}.
