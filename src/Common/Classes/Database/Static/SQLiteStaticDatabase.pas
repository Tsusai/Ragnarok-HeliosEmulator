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
				ExpandFileName(MainProc.DatabaseOptions.GameHost)
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
//			Sends a query to the jansql object.
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
		Console.Message('Text Query Error: ' + QString + '- ' + Database.Error, 'Game Database', MS_ERROR);
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
	Result := 1; //Assume 1
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
			Result.Add(QueryResult.Fields[Index]);
			QueryResult.Next;
		end;
	end;
	if Assigned(QueryResult) then QueryResult.Free;
end;//GetMapsForZone
//------------------------------------------------------------------------------
{END SQLiteStaticDatabase}
end.
