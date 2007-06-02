//------------------------------------------------------------------------------
//GMCommands																															 UNIT
//------------------------------------------------------------------------------
//	What it does-
//      	Holds all of our GMCommand related routines.
//
//	Changes -
//		March 19th, 2007 - RaX - Created.
//		[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
//
//------------------------------------------------------------------------------
unit GMCommands;


interface


uses
	{RTL/VCL}
	Classes,
	{Project}
	//none
	{3rd Party}
	List32
	;


type
	TGMCommand = function(
			Arguments : array of String;
		var
			Error : String
		) : Boolean;

	TGMCommands = class(TObject)
	protected
		fNames  : TStringList;
		fLevels : TIntList32;
	public
		Commands : array of TGMCommand;

		Constructor Create;
		Destructor  Destroy; override;

		function AddCommand(
			const
				Name    : String;
			const
				Command : TGMCommand;
			const
				Level   : Byte
			) : Word;

		function IsCommand(
			const
				Chat : String
			) : Boolean;

		function GetCommandID(
			const
				Name : String
			) : Integer;

		function GetCommandGMLevel(
				CommandID : Integer
			): Byte;

		Function  GetCommandName(
			const
				Chat : String
			) : String;
	end;


implementation

uses
	{RTL/VCL}
	SysUtils,
	{Project}
	Main
	{3rd Party}
	//none
	;


function ZoneStatus(Arguments : array of String; var Error : String) : Boolean;
begin
	Result := TRUE;
	Error := 'Zone '+ IntToStr(MainProc.ZoneServer.Options.ID) + ' : ' + IntToStr(MainProc.ZoneServer.CharacterList.Count) + ' Online!';
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
	AddCommand('ZoneStatus',ZoneStatus, 99);
	AddCommand('Warp',ZoneStatus, 0);
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


(*- Function ------------------------------------------------------------------*
TGMCommands.AddCommand
--------------------------------------------------------------------------------
Overview:
--
	Adds a GM command to the list.

	Returns the index in the Commands array where this newly added command was
	inserted.

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/03/19] RaX - Created
[2007/06/01] CR - Added to explanation of routine (return value).  Made all
	parameters constant.
*-----------------------------------------------------------------------------*)
function TGMCommands.AddCommand(
	const
		Name    : String;
	const
		Command : TGMCommand;
	const
		Level   : Byte
	) : Word;
Begin
	SetLength(Commands, Length(Commands)+1);
	Commands[Length(Commands)-1] := Command;
	fLevels.Add(Level);
	Result := fNames.Add(Lowercase(Name));
End; (* Func TGMCommands.AddCommand
*-----------------------------------------------------------------------------*)


(*- Function ------------------------------------------------------------------*
TGMCommands.GetCommandName
--------------------------------------------------------------------------------
Overview:
--
	Parses a GM command and returns the command name

--
Pre:
	TODO
Post:
	TODO

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/03/21] RaX - Created.
[2007/06/01] CR - Altered comment header, used try-finally as a resource
	protection for our local Stringlist until freed.
*-----------------------------------------------------------------------------*)
Function  TGMCommands.GetCommandName(
	const
		Chat : String
	) : String;
Var
	TempList : TStringList;
	TempChat : String;
Begin
	TempChat := Trim(Chat);
	TempList := TStringList.Create;
	try
		TempList.DelimitedText := TempChat;
		TempList[0] := Copy(TempList[0], 2, Length(TempList[0]));
		Result := TempList[0];
	finally
		TempList.Free;
	end;//t-f
End; (* Func TGMCommands.GetCommandName
*-----------------------------------------------------------------------------*)

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


(*- Function ------------------------------------------------------------------*
TGMCommands.IsCommand
--------------------------------------------------------------------------------
Overview:
--
	Checks to see if a supplied chat string is a gm command.

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/03/19] RaX - Created.
[2007/06/01] CR - Altered Comment Header.  Boolean simplification for Result
	assignment (no if statement needed).  Used try-finally resource protection for
	local StringList variable.
*-----------------------------------------------------------------------------*)
Function  TGMCommands.IsCommand(
	const
		Chat : String
	) : Boolean;
Var
	TempList : TStringList;
	TempChat : String;
Begin
	Result := FALSE;
	TempChat := Trim(Chat);
	if (TempChat[1] = '#') then
	begin
		TempList := TStringList.Create;
		try
			TempList.DelimitedText := TempChat;
			TempList[0] := Copy(TempList[0], 2, Length(TempList[0]));
			Result := (GetCommandID(TempList[0]) <> -1);
		finally
			TempList.Free;
		end;//t-f
	end;
End; (* Func TGMCommands.IsCommand
*-----------------------------------------------------------------------------*)


(*- Function ------------------------------------------------------------------*
TGMCommands.GetCommandID
--------------------------------------------------------------------------------
Overview:
--
	Gets the command ID for a gm command.


--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/03/19] RaX - Created
[2007/06/01] CR - Made string parameter constant (speedup/efficiency)
*-----------------------------------------------------------------------------*)
Function  TGMCommands.GetCommandID(
	const
		Name : String
	) : Integer;
Begin
	Result := fNames.IndexOf(LowerCase(Name));
End; (* Func TGMCommands.GetCommandID
*-----------------------------------------------------------------------------*)


end.
