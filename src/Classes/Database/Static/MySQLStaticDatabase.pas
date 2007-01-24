//------------------------------------------------------------------------------
//MySQLStaticDatabase		                                                        UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is one of our database objects which enabled Helios to use a MySQL
//    Database.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit MySQLStaticDatabase;

interface
uses
	StaticDatabaseTemplate,
	Character,
	uMysqlClient,
  Database,
  MapTypes;
type
//------------------------------------------------------------------------------
//TMySQLStaticDatabase			                                                           CLASS
//------------------------------------------------------------------------------
//	What it does-
//			This is a child class for our database object system. It allows Helios
//    to communicate with a MySQL database and houses all routines for doing so.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//		January 20th, 2007 - Tsusai - Connect is now a bool function
//			Create holds connection result
//
//------------------------------------------------------------------------------
	TMySQLStaticDatabase = class(TStaticDatabaseTemplate)
	private
		Connection   : TMySQLClient;
    Parent : TDatabase;
	public


		Constructor Create(
			EnableStaticDatabase : boolean;
			var LoadedOK : boolean;
			AParent : TDatabase
		); reintroduce; overload;
		Destructor Destroy();override;

		Function GetBaseMaxHP(ACharacter : TCharacter) : Word;override;
		Function GetBaseMaxSP(ACharacter : TCharacter) : Word;override;
    Function GetBaseMaxWeight(ACharacter : TCharacter) : Cardinal;override;

		Function GetMapCannotSave(MapName : String) : Boolean;override;
		Function GetMapZoneID(MapName : String) : Integer;override;
    Function GetMapFlags(MapName : String) : TFlags; override;

	protected
		function Connect() : boolean; override;
		function SendQuery(
			const QString : string;
			StoreResult : boolean;
			var ExecutedOK : boolean
		) : TMySQLResult;
		procedure Disconnect();override;
	end;
//------------------------------------------------------------------------------

implementation
	uses
		Types,
		GameConstants,
		Globals,
		Console,
		SysUtils,
		Classes;
//------------------------------------------------------------------------------
//TMySQLStaticDatabase.Create()                                          CONSTRUCTOR
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
Constructor TMySQLStaticDatabase.Create(
	EnableStaticDatabase : boolean;
	var LoadedOK : boolean;
	AParent : TDatabase
);
begin
	inherited Create(EnableStaticDatabase);
	Parent := AParent;
	Connection := TMySQLClient.Create;
	if EnableStaticDatabase then
	begin
	  LoadedOK := Connect();
  end;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMySQLStaticDatabase.Destroy()                                          DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys our connection object.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Destructor TMySQLStaticDatabase.Destroy();
begin
	Disconnect;
  Connection.Free;
	inherited;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMySQLStaticDatabase.Connect()                                            Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Initializes the MySQL Connection.
//
//	Changes -
//		October 5th, 2006 - RaX - Moved here from globals.
//		January 20th, 2007 - Tsusai - Connect is now a bool function
//
//------------------------------------------------------------------------------
function TMySQLStaticDatabase.Connect() : boolean;
begin
	Result := true;
	if NOT Connection.Connected then
	begin
		Connection.Host            := Parent.Options.StaticHost;
		Connection.Port            := Parent.Options.StaticPort;
		Connection.Db              := Parent.Options.StaticDB;
		Connection.User            := Parent.Options.StaticUser;
		Connection.Password        := Parent.Options.StaticPass;
	end;

	Connection.ConnectTimeout  := 10;

	if NOT Connection.Connect then
	begin
		MainProc.Console('*****Could not connect to mySQL database server.');
		Result := false;
	end;

end;
//------------------------------------------------------------------------------

function TMySQLStaticDatabase.SendQuery(
	const QString : string;
	StoreResult : boolean;
	var ExecutedOK : boolean
) : TMySQLResult;
begin
	Result := Connection.query(QString,StoreResult,ExecutedOK);
	if not ExecutedOK then
	begin
		MainProc.Console('MySQL Query error: ' + QString);
	end;
end;

//------------------------------------------------------------------------------
//TMySQLStaticDatabase.Disconnect()                                         Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Destroys the MySQL Connection.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TMySQLStaticDatabase.Disconnect();
begin
	if Connection.Connected then
	begin
		Connection.close;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLStaticDatabase.GetBaseMaxHP()                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters Maximum hp before modifiers.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TMySQLStaticDatabase.GetBaseMaxHP(ACharacter : TCharacter) : Word;
var
	Success     : Boolean;
	QueryResult : TMySQLResult;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT %s FROM hp WHERE level = %d',
			[ACharacter.JobName, ACharacter.BaseLV])
		,TRUE,Success);
	if (QueryResult.RowsCount = 1) then
	begin
			Result              := StrToInt(QueryResult.FieldValue(0));
	end else Result := 0;
	if Assigned(QueryResult) then QueryResult.Free;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLStaticDatabase.GetBaseMaxSP()                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters maximum sp before modifiers.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TMySQLStaticDatabase.GetBaseMaxSP(ACharacter : TCharacter) : Word;
var
	Success     : Boolean;
	QueryResult : TMySQLResult;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT %s FROM sp WHERE level = %d',
			[ACharacter.JobName, ACharacter.BaseLV])
		,TRUE,Success);
	if (QueryResult.RowsCount = 1) then
	begin
			Result              := StrToInt(QueryResult.FieldValue(0));
	end else Result := 0;
	if Assigned(QueryResult) then QueryResult.Free;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLStaticDatabase.GetBaseSP()                                     FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters basesp.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TMySQLStaticDatabase.GetBaseMaxWeight(ACharacter : TCharacter) : Cardinal;
var
	Success     : Boolean;
	QueryResult : TMySQLResult;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT %s FROM weight WHERE',
			[ACharacter.JobName])
		,TRUE,Success);
	if (QueryResult.RowsCount = 1) then
	begin
			Result              := StrToInt(QueryResult.FieldValue(0));
	end else Result := 0;
	if Assigned(QueryResult) then QueryResult.Free;
end;
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
Function TMySQLStaticDatabase.GetMapCannotSave(MapName : String) : Boolean;
var
	QueryResult : TMySQLResult;
	Success			: Boolean;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT noreturnondc FROM maps WHERE mapname = ''%s''',
			[MapName]),TRUE,Success);

	if (QueryResult.RowsCount = 1) then
	begin
			Result := StrToBool(QueryResult.FieldValue(0));
	end else Result := FALSE;
	QueryResult.Free;
end;//GetMapCanSave
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetMapZoneID							                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Returns the ID of a zone that is handling a certain map.
//
//	Changes -
//		January 16th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
function TMySQLStaticDatabase.GetMapZoneID(MapName : String) : Integer;
var
	QueryResult : TMySQLResult;
	Success			: Boolean;
begin
	Result := 1; //Assume 1
	QueryResult :=
		SendQuery(
		Format('SELECT zoneid FROM maps WHERE mapname = ''%s''',
			[MapName]),TRUE,Success);

	if (QueryResult.RowsCount = 1) then
	begin
		Result := StrToInt(QueryResult.FieldValue(0));
	end;
	QueryResult.Free;
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
Function TMySQLStaticDatabase.GetMapFlags(MapName : String) : TFlags;
var
	QueryResult : TMySQLResult;
  Weather : Integer;
  Success			: Boolean;
begin
	QueryResult := SendQuery(
		Format('SELECT memo, noreturnondc, teleport, itemdrop, exploss, pvp, pvpnightmare, guildpvp, items, skill, deadbranches, flywings, butterflywings, turbotrack, noparty, noguild, weather FROM maps WHERE mapname = ''%s''',
			[MapName]),TRUE,Success);
  if (QueryResult.RowsCount = 1) then
  begin
    Result.Memo           := StrToBool(QueryResult.FieldValue(0));
    Result.NoReturnOnDC   := StrToBool(QueryResult.FieldValue(1));
    Result.Teleport       := StrToBool(QueryResult.FieldValue(2));
    Result.ItemDrop       := StrToBool(QueryResult.FieldValue(3));
    Result.ExpLoss        := StrToBool(QueryResult.FieldValue(4));
    Result.PvP            := StrToBool(QueryResult.FieldValue(5));
    Result.PvPNightmare   := StrToBool(QueryResult.FieldValue(6));
    Result.GuildPvP       := StrToBool(QueryResult.FieldValue(7));
    Result.Items          := StrToBool(QueryResult.FieldValue(8));
    Result.Skill          := StrToBool(QueryResult.FieldValue(9));
    Result.DeadBranches   := StrToBool(QueryResult.FieldValue(10));
    Result.FlyWings       := StrToBool(QueryResult.FieldValue(11));
    Result.ButterflyWings := StrToBool(QueryResult.FieldValue(12));
    Result.TurboTrack     := StrToBool(QueryResult.FieldValue(13));
    Result.NoParty        := StrToBool(QueryResult.FieldValue(14));
    Result.NoGuild        := StrToBool(QueryResult.FieldValue(15));

    //initialize weather
    Result.Rain   := FALSE;
    Result.Snow   := FALSE;
    Result.Sakura := FALSE;
    Result.Fog    := FALSE;
    Result.Leaves := FALSE;
    Result.Smog   := FALSE;

    //Figure out weather.
    Weather := StrToInt(QueryResult.FieldValue(16));
    case Weather of
      1 : Result.Rain     := TRUE;
      2 : Result.Snow     := TRUE;
      3 : Result.Sakura   := TRUE;
      4 : Result.Fog      := TRUE;
      5 : Result.Leaves   := TRUE;
      6 : Result.Smog     := TRUE;
    end;
  end;
  QueryResult.Free;
end;//GetMapZoneID
//------------------------------------------------------------------------------
{END MySQLStaticDatabase}
end.
