//------------------------------------------------------------------------------
//Commands				                                                         UNIT
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
//TCommands                                                               CLASS
//------------------------------------------------------------------------------
  TCommands = class
  public
    Procedure Parse(InputText : String);

  private
    function Help : String;
    function Reload() : String;
    function Restart() : String;
    function Start(Values : TStringList) : String;
    function Stop(Values : TStringList) : String;
  end;{TCommands}
//------------------------------------------------------------------------------

implementation

	uses
		SysUtils,
		Console,
    LoginServer,
    CharacterServer,
    InterServer,
    ZoneServer,
    Globals;

//------------------------------------------------------------------------------
//TCommands.Parse()				                                           PROCEDURE
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
					MainProc.Console('Command ' + Command + ' failed - ' + Error)
				end else begin
					MainProc.Console('Command ' + Command + ' success!');
				end;
			end;
		finally
			StringIn.Free;
		end;
	end;
end;{TCommands.Parse}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TCommands.Help()				                                             FUNCTION
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
	MainProc.Console('The available console commands are as follows...');
	MainProc.Console('--------------------------------------');
  MainProc.Console('/start - starts a server');
  MainProc.Console('/stop - stops a server');
	MainProc.Console('/restart - restarts all enabled servers');
	MainProc.Console('/exit - exits the program');
	MainProc.Console('/help - lists all console commands');
	MainProc.Console('--------------------------------------');
	Result := '';
end;{TCommands.Help}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TCommands.Reload()				                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Will(in the future) free up and reload the Databases.
//
//	Changes -
//		September 20th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TCommands.Reload() : String;
begin
	//To be done when all DB is done.  One swoop kill
end;{TCommands.Reload}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TCommands.Restart() 		                                             FUNCTION
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
end;{TCommands.Restart}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TCommands.Start() 		                                             FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Starts a server.
//
//	Changes -
//		September 20th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TCommands.Start(Values :  TStringList) : String;
begin
  if Values.Count > 0 then
  begin
    Values[0] := LowerCase(Values[0]);
    if Values[0] = 'login' then
    begin
      if NOT MainProc.LoginServer.Started then
      begin
        MainProc.LoginServer.Start;
      end else
      begin
        Result := 'Login Server already started';
      end;
    end else
    if Values[0] = 'character' then
    begin
      if NOT MainProc.CharacterServer.Started then
      begin
        MainProc.CharacterServer.Start;
      end else
      begin
        Result := 'Character Server already started';
      end;
    end else
    if Values[0] = 'inter' then
    begin
      if NOT MainProc.InterServer.Started then
      begin
        MainProc.InterServer.Start;
      end else
      begin
        Result := 'Inter Server already started';
      end;
    end else
    if Values[0] = 'zone' then
    begin
      if NOT MainProc.ZoneServer.Started then
      begin
        MainProc.ZoneServer.Start;
      end else
      begin
        Result := 'Zone Server already started';
      end;
    end else
    begin
      Result := Values[0] + ' is not a valid server';
    end;
  end else
  begin
    //display help for Start()
    MainProc.Console('Using /Start...');
    MainProc.Console('"/Start <ServerName>" - starts <ServerName>');
    MainProc.Console('ServerName can be "Login", "Character", "Zone", or "Inter"');
  end;

end;{Start}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TCommands.Stop() 		                                                 FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Stops a server.
//
//	Changes -
//		September 20th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TCommands.Stop(Values :  TStringList) : String;
begin
  if Values.Count > 0 then
  begin
    Values[0] := LowerCase(Values[0]);
    if Values[0] = 'login' then
    begin
      if MainProc.LoginServer.Started then
      begin
        MainProc.LoginServer.Stop;;
      end else
      begin
        Result := 'Login Server is already stopped.';
      end;
    end else
    if Values[0] = 'character' then
    begin
      if MainProc.CharacterServer.Started then
      begin
        MainProc.CharacterServer.Stop;
      end else
      begin
        Result := 'Character Server is already stopped.';
      end;
    end else
    if Values[0] = 'inter' then
    begin
      if MainProc.InterServer.Started then
      begin
        MainProc.InterServer.Stop;
      end else
      begin
        Result := 'Inter Server is already stopped.';
      end;
    end else
    if Values[0] = 'zone' then
    begin
      if MainProc.ZoneServer.Started then
      begin
        MainProc.ZoneServer.Stop;
      end else
      begin
        Result := 'Zone Server is already stopped.';
      end;
    end else
    begin
      Result := Values[0] + ' is not a valid server name';
    end;
  end else
  begin
    //display help for Stop()
    MainProc.Console('Using /Stop...');
    MainProc.Console('"/Stop <ServerName>" - stops <ServerName>');
    MainProc.Console('ServerName can be "Login", "Character", "Zone", or "Inter"');
  end;

end;{Stop}
//------------------------------------------------------------------------------
end.
