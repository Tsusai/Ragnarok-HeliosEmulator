unit Commands;

interface
	type
		TCommands = class
			public
				function Parse(InputText : String) : Boolean;

				function Help : String;
				function Reload() : String;
				function Restart() : String;
    end;
implementation
	uses
		Classes,
		SysUtils,
		Console,
		Globals;


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
			StringIn.DelimitedText := InputText;
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
						if Assigned(ADatabase) then Error := ADataBase.Reload;
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
end;

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
end;

function TCommands.Reload() : String;
begin
	ADatabase.Reload;
end;

function TCommands.Restart() : String;
begin
	MainProc.Shutdown;
	MainProc.Startup;
	Result := '';
end;
end.
