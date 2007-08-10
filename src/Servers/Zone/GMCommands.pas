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
	Character,
	GMCommandsOptions,
	{3rd Party}
	List32
	;

const
	TYPE_BROADCAST  = 0;  //All zone receive, no player involved
	TYPE_RETURNBACK = 1;  //Orignal zone receiv, no player involved
	TYPE_ALLPLAYERS = 2;  //All players in all zone involved
	TYPE_TARGETCHAR = 3;  //Specific player(character) involved
	TYPE_TARGETMAP  = 4;  //All players in specific map involved

	GMFLAG_NORMAL   = 0;
	GMFLAG_NOSPLIT  = 1;
type
	TGMCommand = function(
		const
			Arguments  : array of String;
			FromChar   : String;
			TargetChar : TCharacter;
		var
			Error : TStringList
		) : Boolean;

	TGMCommands = class(TObject)
	protected
		fNames  : TStringList;
		fLevels : TIntList32;
		fTypes  : TIntList32;  //Should the command send to single zone/broadcast? or something else
		fFlag   : TIntList32;  //To tell inter server don't break parameter!

		Options : TGMCommandsOptions;
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
				Level   : Byte;
			const
				AType   : Byte;
			const
				AFlag   : Byte
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

		function GetCommandType(
			const
				CommandID : Word
			): Byte;

		function GetCommandFlag(
			const
				CommandID : Word
			): Byte;
	end;


implementation

uses
	{RTL/VCL}
	SysUtils,
	{Project}
	Main,
	GMCommandExe
	{3rd Party}
	//none
	;

//------------------------------------------------------------------------------
//Create                                                             CONSTRUCTOR
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
	fTypes := TIntList32.Create;
	fFlag  := TIntList32.Create;

	Options := TGMCommandsOptions.Create(MainProc.Options.ConfigDirectory+'/GMCommands.ini');
	
	//AddCommand( Command Name , Calling Function, Default GM Lvl required, command type)
	AddCommand('ZoneStatus', GMZoneStatus, 99, TYPE_BROADCAST, GMFLAG_NORMAL);
	AddCommand('Warp', GMWarp, 0, TYPE_RETURNBACK, GMFLAG_NORMAL);
	AddCommand('GiveBaseExp', GMGiveBaseExperience, 99, TYPE_TARGETCHAR, GMFLAG_NORMAL);
	AddCommand('GiveJobExp', GMGiveJobExperience, 99, TYPE_TARGETCHAR, GMFLAG_NORMAL);
	AddCommand('BroadCast', GMBroadCast, 99, TYPE_ALLPLAYERS, GMFLAG_NOSPLIT);
	AddCommand('BroadCastN', GMBroadCastNoName, 99, TYPE_ALLPLAYERS, GMFLAG_NOSPLIT);

	Options.Load(fNames, fLevels);
end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy                                                             DESTRUCTOR
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
	Options.Save(fNames, fLevels);

	fNames.Free;
	fLevels.Free;
	fTypes.Free;
	fFlag.Free;

	Options.Free;
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
		Level   : Byte;
	const
		AType   : Byte;
	const
		AFlag   : Byte
	) : Word;
Begin
	SetLength(Commands, Length(Commands)+1);
	Commands[Length(Commands)-1] := Command;
	fLevels.Add(Level);
	fTypes.Add(AType);
	fFlag.Add(AFlag);
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


(*- Function ------------------------------------------------------------------*
TGMCommands.GetCommandType
--------------------------------------------------------------------------------
Overview:
--
	Gets the command type (used in inter server)

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/08/08] Aeomin - Created
*-----------------------------------------------------------------------------*)
function TGMCommands.GetCommandType(
	const
		CommandID : Word
	): Byte;
begin
	Result := fTypes[CommandID];
end; (* Func TGMCommands.GetCommandType
*-----------------------------------------------------------------------------*)


(*- Function ------------------------------------------------------------------*
TGMCommands.GetCommandFlag
--------------------------------------------------------------------------------
Overview:
--
	Gets the command Flag (used in inter server)

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/08/09] Aeomin - Created
*-----------------------------------------------------------------------------*)
function TGMCommands.GetCommandFlag(
	const
		CommandID : Word
	): Byte;
begin
	Result := fFlag[CommandID];
end; (* Func TGMCommands.GetCommandFlag
*-----------------------------------------------------------------------------*)
end{GMCommands}.
