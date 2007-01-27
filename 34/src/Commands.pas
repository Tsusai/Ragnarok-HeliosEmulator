unit Commands;
//Created as a portable command system utilizing a single TEdit and a single TMemo
//as input and output. FreeBSD Licensing applies. - RaX
interface

  uses
	  Forms,
	  Classes;

  type
    TCommands = class
		  public
			procedure Parse(InputText : String);
			function Clear() : String;
      function Reload() : String;
      function Commands() : String;
			procedure Exit(Application : TApplication);
		end;

implementation
  uses
	  Main,
	  Database,
	  Globals,
	  SysUtils;

  var
	  Database : TDatabase;

  procedure TCommands.Parse(InputText : String);
  var
	  StringIn  : TStringList;
	  Index     : Integer;
	  Command   : String;
	  Values    : TStringList;
	  Error     : String;
  begin
	  Error := ' ';
	  StringIn := TStringList.Create;
	  try
		  StringIn.DelimitedText := MainForm.ConsoleIn.Text;
		  if StringIn.DelimitedText[1] = '/' then begin
			  Command := LowerCase(StringIn.Strings[0]);  //Gets the command text
			  Values := TStringList.Create;
			  try
				  for Index := 1 to StringIn.Count-1 do begin  //retrieves values after the command
					  Values.Add(StringIn.Strings[Index]);
			  	end;
				  {Start Command Parser}

				  if Command = '/exit' then begin
					  Exit(Application);
				  end else if Command = '/reload' then begin
					  Error := Reload;
				  end else if Command = '/clear' then begin
					  Error := Clear;
          end else if Command = '/commands' then begin
            Error := Commands;
				  end else begin
					  Error := Command + ' does not exist!';
				  end;

				{End Command Parser}
			  finally  //Begin Cleanup
				  Values.Free;
			  end;
			  if Error > '' then begin  //Display Errors
				  Output('Command ' + Command + ' failed - ' + Error)
			  end else begin
				  Output('Command ' + Command + ' success!');
			  end;
		  end;
	  finally
		  StringIn.Free;
		  MainForm.ConsoleIn.Clear;
	  end;
  end;

  function TCommands.Clear() : String;
  begin
	  MainForm.Console.Clear;
	  Result := '';
  end;

  procedure TCommands.Exit(Application : TApplication);
  begin
    DestroyGlobals;
	  Application.Terminate;
  end;

  function TCommands.Reload() : String;
  begin
    Result := '';
    AccountList.Free;//Changed from AccountList.Clear() to Free to fix apparent Memory Leak - RaX
    AccountList := TStringList.Create;
    if not Database.LoadAccounts then begin
      Result := 'Failed loading accounts, check your gamedata';
    end;
  end;

  function TCommands.Commands() : String;
  begin
    Output('The available console commands are as follows...');
    Output('--------------------------------------');
    Output('/clear - clears the console window');
    Output('/reload - reloads account database');
    Output('/exit - exits the program');
    Output('/commands - list all console commands');
    Output('--------------------------------------');
    Result := '';
  end;
end.
