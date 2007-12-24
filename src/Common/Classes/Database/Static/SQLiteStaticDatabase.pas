//------------------------------------------------------------------------------
//SQLiteStaticDatabase		                                                  UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is one of our database objects which enabled Helios to use a SQLite
//    Database.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
unit SQLiteStaticDatabase;


interface


uses
	{RTL/VCL}
	Classes,
	{Project}
	Being,
	Character,
	Database,
	MapTypes,
	StaticDatabaseTemplate,
	{Third Party}
	SQLiteTable3
	;


Type
(*= CLASS =====================================================================*
TSQLiteStaticDatabase

[2007/06/18] Tsusai

*------------------------------------------------------------------------------*
Overview:
*------------------------------------------------------------------------------*

	This child class descends from an abstract class defining all the public
methods.  TSQLiteStaticDatabase manipulates a SQLite database which will house
Static (Base and Mob stats) databases.

*------------------------------------------------------------------------------*
Revisions:
*------------------------------------------------------------------------------*
(Format: [yyyy/mm/dd] <Author> - <Description of Change>)
[2007/06/18] Tsusai - Created.
*=============================================================================*)
TSQLiteStaticDatabase = class(TStaticDatabaseTemplate)
protected
	fDatabase : TSQLiteDatabase;
	fParent   : TDatabase;

	function  SendQuery(
		const
			QString : String
		) : TSQLiteTable;

public

	Constructor Create(
		const
			AParent : TDatabase
		);

	Destructor Destroy; override;

	Function  GetBaseMaxHP(
		const
			ACharacter : TCharacter
		) : Word; override;

	Function  GetBaseMaxSP(
		const
			ACharacter : TCharacter
		) : Word; override;

	Function  GetBaseMaxWeight(
		const
			ACharacter : TCharacter
		) : LongWord; override;

	Function  GetMapCannotSave(
		const
			MapName : String
		) : Boolean; override;

	Function  GetMapZoneID(
		const
			MapName : String
		): Integer; override;

	Function  GetMapFlags(
		const
			MapName : String
		) : TFlags; override;

	Function  GetMapsForZone(
		const
			ID : LongWord
		) : TStringList; override;

	Function  GetBaseEXPToNextLevel(
		const
			JobName : String;
			Level : Word
		) : LongWord; override;

	Function  GetJobEXPToNextLevel(
		const
			JobName : String;
			Level : Word
		) : LongWord; override;

	Function GetStatPoints(
		const
			Level : Word
		) : LongWord;override;

	Function GetSkillPoints(
		const
			JobName : String;
			Level : Word
		) : LongWord; override;

	Function GetJobBonus(
		const
			JobName : String;
			Level : Word
		) : StatArray;override;

	function  Connect : Boolean; override;

	procedure Disconnect; override;

	property Database : TSQLiteDatabase
		read  fDatabase;
	property Parent : TDatabase
		read  fParent;

End;(* TSQLiteStaticDatabase
*== CLASS ====================================================================*)


implementation
	uses
		Globals,
		SysUtils,
		Main;


//------------------------------------------------------------------------------
//TSQLiteStaticDatabase.Create()                                     CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our connection object.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
Constructor TSQLiteStaticDatabase.Create(
	const
		AParent : TDatabase
	);
begin
	inherited Create;
	fParent := AParent;
	fDatabase := TSQLiteDatabase.Create;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteStaticDatabase.Destroy()                                     DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys our connection object.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
Destructor TSQLiteStaticDatabase.Destroy();
begin
	Disconnect;
	Database.Free;
	inherited;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteStaticDatabase.Disconnect()                                   Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Destroys the TEXT Connection.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
procedure TSQLiteStaticDatabase.Disconnect;
begin

end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteStaticDatabase.Connect()                                      Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Initializes the TEXT Connection.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteStaticDatabase.Connect() : boolean;
begin
	if DirectoryExists(MainProc.DatabaseOptions.StaticHost) then
	begin
		Result :=
			Database.Connect(
				ExpandFileName(MainProc.DatabaseOptions.StaticHost)
				 + '/'+MainProc.DatabaseOptions.StaticDB+'.db');
	end else
	begin
		Console.WriteLn('');
		Console.WriteLn('The database at '+MainProc.DatabaseOptions.StaticHost+' does not exist!');
		Console.WriteLn('Please ensure that you have correctly configured your ini file');
		Result := false;
	end;

	if not Result then
	begin
		Console.WriteLn('*****Could not open text database.');
		Console.WriteLn(MainProc.DatabaseOptions.StaticHost);
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteStaticDatabase.SendQuery()                                     Function
//------------------------------------------------------------------------------
//	What it does-
//			Sends a query to the SQLite object.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteStaticDatabase.SendQuery(
	const QString : string
) : TSQLiteTable;
begin
	Result := Database.GetTable(QString);
	{if (Result = 0) AND (Database.Error <> 'SELECT FROM: no records') then
	begin
		Console.Message('Text Query Error: ' + QString + '- ' + Database.Error, 'Static Database', MS_ERROR);
		Console.WriteLn(Database.Error);
	end;}
end;//SendQuery
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteStaticDatabase.GetBaseMaxHP()                                  FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters Maximum HP before modifers.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
Function TSQLiteStaticDatabase.GetBaseMaxHP(
	const
		ACharacter : TCharacter
	) : Word;
var
	QueryResult : TSQLiteTable;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT %s FROM hp WHERE level = %d',
			[ACharacter.JobName, ACharacter.BaseLV]));
	if (QueryResult.Count = 1) then
	begin
			Result := QueryResult.FieldAsInteger(0);
	end else Result := 0;
	if Assigned(QueryResult) then QueryResult.Free;
end;//GetBaseHP
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteStaticDatabase.GetBaseMaxSP()                                  FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters Maximum SP before modifiers.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
Function TSQLiteStaticDatabase.GetBaseMaxSP(
	const
		ACharacter : TCharacter
	) : Word;
var
	QueryResult : TSQLiteTable;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT %s FROM sp WHERE level = %d',
			[ACharacter.JobName, ACharacter.BaseLV]));
	if (QueryResult.Count = 1) then
	begin
			Result  := QueryResult.FieldAsInteger(0);
	end else Result := 0;
	if Assigned(QueryResult) then QueryResult.Free;
end;//GetBaseSP
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteStaticDatabase.GetBaseMaxWeight()                                    FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters Maximum weight before modifiers.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
Function TSQLiteStaticDatabase.GetBaseMaxWeight(
	const
		ACharacter : TCharacter
	) : LongWord;
var
	QueryResult : TSQLiteTable;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT %s FROM weight',
			[ACharacter.JobName]));
	if (QueryResult.Count = 1) then
	begin
			Result  := QueryResult.FieldAsInteger(0);
	end else Result := 0;
	if Assigned(QueryResult) then QueryResult.Free;
end;//GetBaseSP
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetMapCannotSave					                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Checks to see if a map can save or not.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
Function TSQLiteStaticDatabase.GetMapCannotSave(
	const
		MapName : String
	) : Boolean;
var
	QueryResult : TSQLiteTable;
begin
	Result := false;
	QueryResult :=
		SendQuery(
		Format('SELECT noreturnondc FROM maps WHERE mapname = ''%s''',
			[MapName]));

	if (QueryResult.Count = 1) then
	begin
			Result  := Boolean(QueryResult.FieldAsInteger(0));
	end;
	if Assigned(QueryResult) then QueryResult.Free;
end;//GetMapCanSave
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetMapZoneID							                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Returns the ID of a zone that is handling a certain map.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
Function TSQLiteStaticDatabase.GetMapZoneID(
	const
		MapName : String
	) : Integer;
var
	QueryResult : TSQLiteTable;
begin
	Result := -1;
	QueryResult :=
		SendQuery(
		Format('SELECT zoneid FROM maps WHERE mapname = ''%s''',
			[MapName]));
	if (QueryResult.Count = 1) then
	begin
		Result  := QueryResult.FieldAsInteger(0);
	end;
	if Assigned(QueryResult) then QueryResult.Free;
end;//GetMapZoneID
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//GetMapFlags 							                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Queries and returns a map's flags.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
Function TSQLiteStaticDatabase.GetMapFlags(
	const
		MapName : String
	) : TFlags;
var
	QueryResult : TSQLiteTable;
	Weather : Integer;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT memo, noreturnondc, teleport, itemdrop, exploss, pvp, pvpnightmare, guildpvp, items, skill, deadbranches, flywings, butterflywings, turbotrack, noparty, noguild, weather FROM maps WHERE mapname = ''%s''',
			[MapName]));
	if (QueryResult.Count = 1) then
	begin
		Result.Memo           := Boolean(QueryResult.FieldAsInteger(0));
		Result.NoReturnOnDC   := Boolean(QueryResult.FieldAsInteger(1));
		Result.Teleport       := Boolean(QueryResult.FieldAsInteger(2));
		Result.ItemDrop       := Boolean(QueryResult.FieldAsInteger(3));
		Result.ExpLoss        := Boolean(QueryResult.FieldAsInteger(4));
		Result.PvP            := Boolean(QueryResult.FieldAsInteger(5));
		Result.PvPNightmare   := Boolean(QueryResult.FieldAsInteger(6));
		Result.GuildPvP       := Boolean(QueryResult.FieldAsInteger(7));
		Result.Items          := Boolean(QueryResult.FieldAsInteger(8));
		Result.Skill          := Boolean(QueryResult.FieldAsInteger(9));
		Result.DeadBranches   := Boolean(QueryResult.FieldAsInteger(10));
		Result.FlyWings       := Boolean(QueryResult.FieldAsInteger(11));
		Result.ButterflyWings := Boolean(QueryResult.FieldAsInteger(12));
		Result.TurboTrack     := Boolean(QueryResult.FieldAsInteger(13));
		Result.NoParty        := Boolean(QueryResult.FieldAsInteger(14));
		Result.NoGuild        := Boolean(QueryResult.FieldAsInteger(15));

		//initialize weather
		Result.Rain   := FALSE;
		Result.Snow   := FALSE;
		Result.Sakura := FALSE;
		Result.Fog    := FALSE;
		Result.Leaves := FALSE;
		Result.Smog   := FALSE;

		//Figure out weather.
		Weather := QueryResult.FieldAsInteger(16);
		case Weather of
			1 : Result.Rain     := TRUE;
			2 : Result.Snow     := TRUE;
			3 : Result.Sakura   := TRUE;
			4 : Result.Fog      := TRUE;
			5 : Result.Leaves   := TRUE;
			6 : Result.Smog     := TRUE;
		end;
	end;
	if Assigned(QueryResult) then QueryResult.Free;
end;//GetMapFlags
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetMapsForZone						                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Queries and returns a zone's maps
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//		[2007/07/22] Tsusai - Index out of bounds fix w/ the loop
//
//------------------------------------------------------------------------------
Function TSQLiteStaticDatabase.GetMapsForZone(
	const
		ID : LongWord
	) : TStringList;
var
	QueryResult      : TSQLiteTable;
	Index            : integer;
begin
	Result := TStringList.Create;
	QueryResult :=
		SendQuery(
		Format('SELECT mapname FROM maps WHERE zoneid = %d',
			[ID]));
	if (QueryResult.Count > 0) then
	begin
		for Index := 0 to (QueryResult.Count - 1) do
		begin
			Result.Add(QueryResult.Fields[0]);
			QueryResult.Next;
		end;
	end;
	if Assigned(QueryResult) then QueryResult.Free;
end;//GetMapsForZone
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetBaseEXPToNextLevel 			                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Queries and returns a map's flags.
//
//	Changes -
//		July 25th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Function TSQLiteStaticDatabase.GetBaseEXPToNextLevel(
		const
			JobName : String;
			Level : Word
		) : LongWord;
var
	QueryResult : TSQLiteTable;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT %s FROM baseexp WHERE level = %d',
			[JobName, Level]));
	if (QueryResult.RowCount = 1) then
	begin
			Result              := StrToIntDef(QueryResult.Fields[0],0);
	end else Result := 0;
	if Assigned(QueryResult) then QueryResult.Free;
end;//GetBaseEXPToNextLevel
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetJobEXPToNextLevel 			                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Queries and returns a map's flags.
//
//	Changes -
//		July 25th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Function TSQLiteStaticDatabase.GetJobEXPToNextLevel(
		const
			JobName : String;
			Level : Word
		) : LongWord;
var
	QueryResult : TSQLiteTable;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT %s FROM jobexp WHERE level = %d',
			[JobName, Level]));
	if (QueryResult.RowCount = 1) then
	begin
			Result              := StrToIntDef(QueryResult.Fields[0],0);
	end else Result := 0;
	if Assigned(QueryResult) then QueryResult.Free;
end;//GetJobEXPToNextLevel
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetStatPoints 			                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Queries and returns a character's stat points for a level.
//
//	Changes -
//		August 11th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Function TSQLiteStaticDatabase.GetStatPoints(
		const
			Level : Word
		) : LongWord;
var
	QueryResult : TSQLiteTable;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT points FROM statpoints WHERE level = %d',
			[Level]));
	if (QueryResult.RowCount = 1) then
	begin
			Result              := StrToIntDef(QueryResult.Fields[0],0);
	end else Result := 0;
	if Assigned(QueryResult) then QueryResult.Free;
end;//GetStatPoints
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetSkillPoints         			                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Queries and returns a character's skill points for a level.
//
//	Changes -
//		August 14th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Function TSQLiteStaticDatabase.GetSkillPoints(
		const
			JobName : String;
			Level : Word
		) : LongWord;
var
	QueryResult : TSQLiteTable;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT %s FROM skillpoints WHERE level = %d',
			[JobName, Level]));
	if (QueryResult.RowCount = 1) then
	begin
			Result              := StrToIntDef(QueryResult.Fields[0], 0);
	end else Result := 0;
	if Assigned(QueryResult) then QueryResult.Free;
end;//GetSkillPoints
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetJobBonus         			                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Queries and returns a character's job bonus for a level.
//
//	Changes -
//		August 19th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Function TSQLiteStaticDatabase.GetJobBonus(
		const
			JobName : String;
			Level : Word
		) : StatArray;
var
	QueryResult : TSQLiteTable;
	AStringList : TStringList;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT %s FROM jobbonus WHERE level = %d',
			[JobName, Level]));
	if (QueryResult.RowCount = 1) then
	begin
		AStringList := TStringList.Create;
		try
			AStringList.Delimiter := ',';
			AStringList.QuoteChar := '"';
			AStringList.DelimitedText := QueryResult.FieldAsString(0);
			if AStringList.Count = 6 then
			begin
				Result[0] := StrToIntDef(AStringList.Strings[0], 0);
				Result[1] := StrToIntDef(AStringList.Strings[1], 0);
				Result[2] := StrToIntDef(AStringList.Strings[2], 0);
				Result[3] := StrToIntDef(AStringList.Strings[3], 0);
				Result[4] := StrToIntDef(AStringList.Strings[4], 0);
				Result[5] := StrToIntDef(AStringList.Strings[5], 0);
			end else
			begin
				Console.WriteLn('jobbonus Database Error. Count of parameters should be 6, actual value : '
					+IntToStr(AStringList.Count)+ ' for job '+JobName
					+' at level '+IntToStr(level)+'. Please fix this and try again.');
				Result[0] := 0;
				Result[1] := 0;
				Result[2] := 0;
				Result[3] := 0;
				Result[4] := 0;
				Result[5] := 0;
			end;
		finally
			AStringList.Free;
		end;
	end else
	begin
		Result[0] := 0;
		Result[1] := 0;
		Result[2] := 0;
		Result[3] := 0;
		Result[4] := 0;
		Result[5] := 0;
	end;
	if Assigned(QueryResult) then QueryResult.Free;
end;//GetJobBonus
//------------------------------------------------------------------------------
{END SQLiteStaticDatabase}
end.
