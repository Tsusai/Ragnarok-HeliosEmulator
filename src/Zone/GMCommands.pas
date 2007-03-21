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
uses
	Classes,
	List32;

type
	TGMCommand = function(Arguments : array of String; var Error : String) : Boolean;

	TGMCommands = class
	Constructor Create;
	Destructor Destroy;override;
	private
		fNames : TStringList;
		fLevels: TIntList32;
	public
		Commands  : array of TGMCommand;
		function AddCommand(Name : String; Command : TGMCommand; Level : Byte) : Word;
		function IsCommand(const Chat : String)	: Boolean;
		function GetCommandID(Name : String)	: Integer;
		function GetCommandGMLevel(CommandID : Integer): Byte;

		function GetCommandName(const Chat : String) : String;
	end;


implementation

uses
	SysUtils,
	Globals;
function test(Arguments : array of String; var Error : String) : Boolean;
begin
	Result := FALSE;
	Error := 'HARBL?';
end;
//------------------------------------------------------------------------------
//Create																														CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//      	Creates our gm command component.
//
//	Changes -
//		March 19th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Constructor TGMCommands.Create;
begin
	inherited;
	fNames := TStringList.Create;
	fLevels:= TIntList32.Create;
	AddCommand('test',test, 0);
end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy																														DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//      	destroys our component.
//
//	Changes -
//		March 19th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Destructor TGMCommands.Destroy;
begin
	fNames.Free;
	fLevels.Free;
	inherited;
end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//AddCommand																												PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      	Adds a gm command to the list.
//
//	Changes -
//		March 19th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
function TGMCommands.AddCommand(Name : String; Command : TGMCommand; Level : Byte) : Word;
begin
	SetLength(Commands, Length(Commands)+1);
	Commands[Length(Commands)-1] := Command;
	fLevels.Add(Level);
	Result := fNames.Add(Lowercase(Name));
end;{AddCommand}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetCommandName        																						PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      	Parses a gm command and returns the command name
//
//	Changes -
//		March 21st, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
function TGMCommands.GetCommandName(const Chat : String) : String;
var
	TempList : TStringList;
	TempChat : String;

begin
	TempChat := Trim(Chat);
	TempList := TStringList.Create;
	TempList.DelimitedText := TempChat;
	TempList[0] := copy(TempList[0], 2, Length(TempList[0]));
	Result := TempList[0];
	TempList.Free;
end;{AddCommand}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetCommandName        																						PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      	Parses a gm command and returns the command name
//
//	Changes -
//		March 21st, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
function TGMCommands.GetCommandGMLevel(CommandID: Integer) : Byte;
begin
	Result := fLevels[CommandID];
end;
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
function TGMCommands.IsCommand(const Chat : String) : Boolean;
var
	TempList : TStringList;
	TempChat : String;
begin
	Result := FALSE;
	TempChat := Trim(Chat);
	if TempChat[1] = '#' then
	begin
		TempList := TStringList.Create;
		TempList.DelimitedText := TempChat;
		TempList[0] := copy(TempList[0], 2, Length(TempList[0]));
		if (GetCommandID(TempList[0]) <> -1) then
		begin
			Result := TRUE;
		end;
		TempList.Free;
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
function TGMCommands.GetCommandID(Name : String) : Integer;
begin
	Result := fNames.IndexOf(lowercase(Name));
end;{GetCommandID}
//------------------------------------------------------------------------------
end.
