//------------------------------------------------------------------------------
//Commands				                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This Unit was built to house the routines dealing with processing
//    console commands.
//
//  Notes -
//      RaX-The EXIT command simply returns FALSE in the parser. Look for it in
//    Parse()
//
//	Changes -
//		September 20th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit Commands;

interface
type
//------------------------------------------------------------------------------
//TCommands                                                               CLASS
//------------------------------------------------------------------------------
  TCommands = class
  public
    function Parse(InputText : String) : Boolean;

    function Help : String;
    function Reload() : String;
    function Restart() : String;
  end;{TCommands}
//------------------------------------------------------------------------------
implementation
	uses
		Classes,
		SysUtils,
		Console;

//------------------------------------------------------------------------------
//TCommands.Parse()				                                             FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Parses InputText for commands.
//
//	Changes -
//		September 20th, 2006 - RaX - Added Trim function before using InputText to
//                                force commands even when whitespace is present
//                                before it.
//		September 20th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TCommands.Parse(InputText : String) : Boolean;
var
	StringIn  : TStringList;
	Index     : Integer;
	Command   : String;
	Values    : TStringList;
	Error     : String;
begin
	Result := TRUE; //Assume the program will continue to run.
	if InputText > '' then begin
		Error := ' ';
		StringIn := TStringList.Create;
		try
			StringIn.DelimitedText := Trim(InputText);
			if StringIn.DelimitedText[1] = '/' then begin
				Command := LowerCase(StringIn.Strings[0]);  //Gets the command text
				Values := TStringList.Create;
				try
					for Index := 1 to StringIn.Count-1 do begin  //retrieves values after the command
						Values.Add(StringIn.Strings[Index]);
					end;
					{Start Command Parser}
					if Command = '/exit' then begin
						Result := FALSE;
						Error := '';
					end else if Command = '/reload' then begin
						{if Assigned(ADatabase) then} Error := 'Reload not setup till all DB is done';//ADataBase.Reload;
					end else if Command = '/help' then begin
						Error := Help;
					end else if Command = '/restart' then begin
						Error := Restart;
					end else begin
						Error := Command + ' does not exist!';
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
	MainProc.Console('/reload - reloads account database');
	MainProc.Console('/restart - restarts the server');
	MainProc.Console('/exit - exits the program');
	MainProc.Console('/help - list all console commands');
	MainProc.Console('--------------------------------------');
	Result := '';
end;{TCommands.Help}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TCommands.Help()				                                             FUNCTION
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
//TCommands.Help()				                                             FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Writes a list of commands to the console.
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


end{Commands}.
