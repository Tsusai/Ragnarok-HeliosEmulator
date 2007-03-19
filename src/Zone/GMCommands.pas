//------------------------------------------------------------------------------
//GMCommands																															 UNIT
//------------------------------------------------------------------------------
//	What it does-
//      	Holds all of our GMCommand related routines.
//
//	Changes -
//		March 19th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
unit GMCommands;

interface

var
	Commands : array of function(Arguments : array of String; Error : String) : Boolean;

	function IsGMCommand(const Chat : String)	: Boolean;
	function GetCommandID(ACommand : String)	: Word;
	procedure SetupCommands;

implementation

uses
	SysUtils,
	Globals;

function TestCommand(Arguments : array of String; Error : String) : Boolean;
begin
	Error := 'Command failed, but you succeed!';
	Result := FALSE;
end;

//------------------------------------------------------------------------------
//SetupCommands																											PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      	Sets up our commands array.
//
//	Changes -
//		March 19th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure SetupCommands;
begin
	SetLength(Commands, 10);
	Commands[1] := TestCommand;
end;//SetupCommands
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//IsGMCommand																													PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      	Checks to see if a supplied chat string is a gm command.
//
//	Changes -
//		March 19th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
function IsGMCommand(const Chat : String) : Boolean;
begin
	//This is just a place holder for now, will eventually check through our
	//database of gm commands to see if the command exists. For now, it just
	//checks to see if the supplied chat argument begins with a #
	if Trim(Chat)[1] = '#' then
	begin
		Result := TRUE;
	end else
	begin
		Result := FALSE;
	end;
end;{IsGMCommand}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetCommandID																												PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      	Gets the command ID for a gm command.
//
//	Changes -
//		March 19th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
function GetCommandID(ACommand : String) : Word;
begin
	Result := 1;
end;{GetCommandID}
//------------------------------------------------------------------------------
end.
