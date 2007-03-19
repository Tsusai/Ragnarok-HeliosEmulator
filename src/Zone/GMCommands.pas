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
	Classes;

type
	TGMCommand = function(Arguments : array of String; var Error : String) : Boolean;

	TGMCommands = class
	Constructor Create;
	Destructor Destroy;override;
	private
		fNames : TStringList;
	public
		Commands  : array of TGMCommand;
		function AddCommand(Name : String; Command : TGMCommand) : Word;
		function IsCommand(const Chat : String)	: Boolean;
		function GetCommandID(Name : String)	: Integer;
	end;


implementation

uses
	SysUtils,
	Globals;

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
function TGMCommands.AddCommand(Name : String; Command : TGMCommand) : Word;
begin
	SetLength(Commands, Length(Commands)+1);
	Commands[Length(Commands)-1] := Command;
	Result := fNames.Add(Name);
end;{AddCommand}
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
	Result := fNames.IndexOf(Name);
end;{GetCommandID}
//------------------------------------------------------------------------------
end.
