//------------------------------------------------------------------------------
//JanSQLStaticDatabase		                                                        UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is one of our database objects which enabled Helios to use a TEXT
//    Database.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit JanSQLStaticDatabase;

interface
uses
	StaticDatabaseTemplate,
	Character,
	janSQL,
  Database,
  MapTypes,
  Classes;

//------------------------------------------------------------------------------
//TJanSQLStaticDatabase			                                                           CLASS
//------------------------------------------------------------------------------
//	What it does-
//			This is a child class for our database object system. It allows Helios
//    to communicate with a TEXT database and houses all routines for doing so.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//		January 20th, 2007 - Tsusai - Connect is now a bool function
//			Create holds connection result
//
//------------------------------------------------------------------------------
type
	TJanSQLStaticDatabase = class(TStaticDatabaseTemplate)
	private
		Database : TjanSQL;
    Parent  : TDatabase;
	public

		Constructor Create(
			EnableStaticDatabase : boolean;
			AParent : TDatabase
		); reintroduce; overload;
		Destructor Destroy();override;

		Function GetBaseMaxHP(ACharacter : TCharacter) : Word;override;
		Function GetBaseMaxSP(ACharacter : TCharacter) : Word;override;
    Function GetBaseMaxWeight(ACharacter : TCharacter) : LongWord;override;

		Function GetMapCannotSave(MapName : String) : Boolean;override;
		Function GetMapZoneID(MapName : String): Integer; override;
    Function GetMapFlags(MapName : String) : TFlags; override;
    Function GetMapsForZone(ID : LongWord) : TStringList; override;
	protected
		function Connect() : boolean; override;
		procedure Disconnect; override;
		function SendQuery(
			const QString : string
		) : Integer;
	end;
//------------------------------------------------------------------------------

implementation
	uses
		Globals,
		SysUtils,
		Main;


//------------------------------------------------------------------------------
//TJanSQLStaticDatabase.Create()                                          CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our connection object.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//		November 13th, 2006 - Tsusai - create inherit comes first.
//		January 20th, 2007 - Tsusai - Create holds connection result
//
//------------------------------------------------------------------------------
Constructor TJanSQLStaticDatabase.Create(
	EnableStaticDatabase : boolean;
	AParent : TDatabase
);
begin
	inherited Create(EnableStaticDatabase);
	Parent := AParent;
	Database := TJanSQL.Create;
	if EnableStaticDatabase then
	begin
		Connect();
	end;

end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLStaticDatabase.Destroy()                                          DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys our connection object.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Destructor TJanSQLStaticDatabase.Destroy();
begin
	Disconnect;
  Database.Free;
	inherited;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLStaticDatabase.Disconnect()                                        Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Destroys the TEXT Connection.
//
//	Changes -
//		December 21st, 2006 - RaX - Created
//
//------------------------------------------------------------------------------
procedure TJanSQLStaticDatabase.Disconnect;
begin

end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLStaticDatabase.Connect()                                            Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Initializes the TEXT Connection.
//
//	Changes -
//		October 5th, 2006 - RaX - Moved here from globals.
//		December 18th, 2006 - Tsusai - Modified the connect to actually...connect
//			Also FileExists doesn't work for directories, its DirectoryExists
//		January 20th, 2007 - Tsusai - Connect is now a bool function
//
//------------------------------------------------------------------------------
function TJanSQLStaticDatabase.Connect() : boolean;
var
	ResultIdentifier : Integer;
const ConnectQuery = 'Connect to ''%s''';
begin
	Result := true;
	ResultIdentifier := 0;

	if DirectoryExists(MainProc.DatabaseOptions.StaticHost) then
	begin
		ResultIdentifier := Database.SQLDirect(Format(ConnectQuery,[MainProc.DatabaseOptions.StaticHost]));
	end else
	begin
		Console.WriteLn('');
		Console.WriteLn('The database at '+MainProc.DatabaseOptions.StaticHost+' does not exist!');
		Console.WriteLn('Please ensure that you have correctly configured your ini file');
		Result := false;
	end;

	if ResultIdentifier = 0 then
	begin
		Console.WriteLn('*****Could not open text database. Error : ' + Database.Error);
		Console.WriteLn(MainProc.DatabaseOptions.StaticHost);
		Result := false;
	end else
	begin
		Database.ReleaseRecordset(ResultIdentifier);
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLStaticDatabase.SendQuery()                                          Function
//------------------------------------------------------------------------------
//	What it does-
//			Sends a query to the jansql object.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//		December 18th, 2006 - Tsusai - Would not return a blank set if query failed
//			to return anything, only a nil pointer that would cause issues when read.
//
//------------------------------------------------------------------------------
function TJanSQLStaticDatabase.SendQuery(
	const QString : string
) : Integer;
begin
	Result := Database.SQLDirect(QString);
	if (Result = 0) AND (Database.Error <> 'SELECT FROM: no records') then
	begin
		Console.Message('Text Query error: ' + QString, 'Static Database', MS_ERROR);
    Console.WriteLn(Database.Error);
	end;
end;//SendQuery
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLStaticDatabase.GetBaseMaxHP()                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters Maximum HP before modifers.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TJanSQLStaticDatabase.GetBaseMaxHP(ACharacter : TCharacter) : Word;
var
	QueryResult : TJanRecordSet;
  ResultIdentifier : Integer;
begin
	ResultIdentifier :=
		SendQuery(
		Format('SELECT %s FROM hp WHERE level = %d',
			[ACharacter.JobName, ACharacter.BaseLV]));
	QueryResult := Database.RecordSets[ResultIdentifier];
	if (QueryResult.RecordCount = 1) then
	begin
			Result              := StrToInt(QueryResult.Records[0].Fields[0].value);
	end else Result := 0;
	SendQuery('RELEASE TABLE hp');
	if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
end;//GetBaseHP
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLStaticDatabase.GetBaseMaxSP()                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters Maximum Weight before modifiers.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TJanSQLStaticDatabase.GetBaseMaxSP(ACharacter : TCharacter) : Word;
var
	QueryResult : TJanRecordSet;
	ResultIdentifier : Integer;
begin
	ResultIdentifier :=
		SendQuery(
		Format('SELECT %s FROM sp WHERE level = %d',
			[ACharacter.JobName, ACharacter.BaseLV]));
	QueryResult := Database.RecordSets[ResultIdentifier];
	if (QueryResult.RecordCount = 1) then
	begin
			Result  := StrToInt(QueryResult.Records[0].Fields[0].Value);
	end else Result := 0;
	SendQuery('RELEASE TABLE sp');
	if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
end;//GetBaseSP
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLStaticDatabase.GetBaseMaxWeight()                                    FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters Maximum weight before modifiers.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TJanSQLStaticDatabase.GetBaseMaxWeight(ACharacter : TCharacter) : LongWord;
var
	QueryResult : TJanRecordSet;
	ResultIdentifier : Integer;
begin
	ResultIdentifier :=
		SendQuery(
		Format('SELECT %s FROM weight',
			[ACharacter.JobName]));
	QueryResult := Database.RecordSets[ResultIdentifier];
	Result  := StrToIntDef(QueryResult.Records[0].Fields[0].Value, 0);
	SendQuery('RELEASE TABLE weight');
	if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
end;//GetBaseSP
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetMapCannotSave					                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Checks to see if a map can save or not.
//
//	Changes -
//		January 10th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TJanSQLStaticDatabase.GetMapCannotSave(MapName : String) : Boolean;
var
	QueryResult : TJanRecordSet;
	ResultIdentifier : Integer;
begin
	Result := false;
	ResultIdentifier :=
		SendQuery(
		Format('SELECT noreturnondc FROM maps WHERE mapname = ''%s''',
			[MapName]));

	if ResultIdentifier > 0 then
	begin
		QueryResult := Database.RecordSets[ResultIdentifier];
		if (QueryResult.RecordCount = 1) then
		begin
				Result := StrToBool(QueryResult.Records[0].Fields[0].Value);
		end;
		SendQuery('RELEASE TABLE maps');
		Database.ReleaseRecordset(ResultIdentifier);
	end;
end;//GetMapCanSave
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetMapZoneID							                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Checks to see if a map can save or not.
//
//	Changes -
//		January 10th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TJanSQLStaticDatabase.GetMapZoneID(MapName : String) : Integer;
var
	QueryResult : TJanRecordSet;
	ResultIdentifier : Integer;
begin
	Result := 1; //Assume 1
	ResultIdentifier :=
		SendQuery(
		Format('SELECT zoneid FROM maps WHERE mapname = ''%s''',
			[MapName]));
	if ResultIdentifier > 0 then
	begin
		QueryResult := Database.RecordSets[ResultIdentifier];
		if (QueryResult.RecordCount = 1) then
		begin
			Result := StrToIntDef(QueryResult.Records[0].Fields[0].Value,1);
		end;
		SendQuery('RELEASE TABLE maps');
		if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
	end;
end;//GetMapZoneID
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//GetMapFlags 							                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Queries and returns a map's flags.
//
//	Changes -
//		January 22nd, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Function TJanSQLStaticDatabase.GetMapFlags(MapName : String) : TFlags;
var
	QueryResult : TJanRecordSet;
	ResultIdentifier : Integer;
  Weather : Integer;
begin
	ResultIdentifier :=
		SendQuery(
		Format('SELECT memo, noreturnondc, teleport, itemdrop, exploss, pvp, pvpnightmare, guildpvp, items, skill, deadbranches, flywings, butterflywings, turbotrack, noparty, noguild, weather FROM maps WHERE mapname = ''%s''',
			[MapName]));
	if ResultIdentifier > 0 then
	begin
		QueryResult := Database.RecordSets[ResultIdentifier];
		if (QueryResult.RecordCount = 1) then
		begin
			Result.Memo           := StrtoBoolDef(QueryResult.Records[0].fields[0].Value, FALSE);
      Result.NoReturnOnDC   := StrtoBoolDef(QueryResult.Records[0].fields[1].Value, FALSE);
      Result.Teleport       := StrtoBoolDef(QueryResult.Records[0].fields[2].Value, FALSE);
      Result.ItemDrop       := StrtoBoolDef(QueryResult.Records[0].fields[3].Value, FALSE);
      Result.ExpLoss        := StrtoBoolDef(QueryResult.Records[0].fields[4].Value, FALSE);
      Result.PvP            := StrtoBoolDef(QueryResult.Records[0].fields[5].Value, FALSE);
      Result.PvPNightmare   := StrtoBoolDef(QueryResult.Records[0].fields[6].Value, FALSE);
      Result.GuildPvP       := StrtoBoolDef(QueryResult.Records[0].fields[7].Value, FALSE);
      Result.Items          := StrtoBoolDef(QueryResult.Records[0].fields[8].Value, FALSE);
      Result.Skill          := StrtoBoolDef(QueryResult.Records[0].fields[9].Value, FALSE);
      Result.DeadBranches   := StrtoBoolDef(QueryResult.Records[0].fields[10].Value, FALSE);
      Result.FlyWings       := StrtoBoolDef(QueryResult.Records[0].fields[11].Value, FALSE);
      Result.ButterflyWings := StrtoBoolDef(QueryResult.Records[0].fields[12].Value, FALSE);
      Result.TurboTrack     := StrtoBoolDef(QueryResult.Records[0].fields[13].Value, FALSE);
      Result.NoParty        := StrtoBoolDef(QueryResult.Records[0].fields[14].Value, FALSE);
      Result.NoGuild        := StrtoBoolDef(QueryResult.Records[0].fields[15].Value, FALSE);

      //initialize weather
      Result.Rain   := FALSE;
      Result.Snow   := FALSE;
      Result.Sakura := FALSE;
      Result.Fog    := FALSE;
      Result.Leaves := FALSE;
      Result.Smog   := FALSE;

      //Figure out weather.
      Weather := StrToIntDef(QueryResult.Records[0].fields[16].Value, 0);
      case Weather of
        1 : Result.Rain     := TRUE;
        2 : Result.Snow     := TRUE;
        3 : Result.Sakura   := TRUE;
        4 : Result.Fog      := TRUE;
        5 : Result.Leaves   := TRUE;
        6 : Result.Smog     := TRUE;
      end;
		end;
		SendQuery('RELEASE TABLE maps');
		if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
	end;
end;//GetMapFlags
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetMapsForZone						                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Queries and returns a zone's maps
//
//	Changes -
//		January 22nd, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Function TJanSQLStaticDatabase.GetMapsForZone(ID : LongWord) : TStringList;
var
	QueryResult      : TJanRecordSet;
	ResultIdentifier : Integer;
  Index            : Integer;
begin
  Result := TStringList.Create;
	ResultIdentifier :=
		SendQuery(
		Format('SELECT mapname FROM maps WHERE zoneid = %d',
			[ID]));
	if ResultIdentifier > 0 then
	begin
		QueryResult := Database.RecordSets[ResultIdentifier];
		if (QueryResult.RecordCount > 0) then
		begin
      for Index := 0 to QueryResult.RecordCount-1 do
      begin
        Result.Add(QueryResult.Records[Index].fields[0].Value);
      end;
		end;
		SendQuery('RELEASE TABLE maps');
		if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
	end;
end;//GetMapsForZone
//------------------------------------------------------------------------------
{END JanSQLStaticDatabase}
end.
