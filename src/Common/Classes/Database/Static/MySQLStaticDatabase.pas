(*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

Unit
MySQLStaticDatabase

*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

[2006/09/29] Helios - RaX

================================================================================
License:  (FreeBSD, plus commercial with written permission clause.)
================================================================================

Project Helios - Copyright (c) 2005-2007

All rights reserved.

Please refer to Helios.dpr for full license terms.

================================================================================
Overview:
================================================================================

	Helios uses this unit to manipulate MySQL for access to the static (Base and
	Mob stats) Database.

================================================================================
Revisions:
================================================================================
(Format: [yyyy/mm/dd] <Author> - <Desc of Changes>)
[2006/09/29] RaX - Created Unit
[2007/04/07] CR - Altered header, improved description.  Changes to
	TMySQLStaticDatabase to follow its template's changes.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)


unit MySQLStaticDatabase;


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
	uMysqlClient
	;


type
(*= CLASS =====================================================================*
TMySQLStaticDatabase

[2006/09/29] ChrstphrR

*------------------------------------------------------------------------------*
Overview:
*------------------------------------------------------------------------------*

	This child class descends from an abstract class defining all the public
methods.  TMySQLStaticDatabase manipulates a MySQL database which will house
Static (Base and Mob stats) databases.

*------------------------------------------------------------------------------*
Revisions:
*------------------------------------------------------------------------------*
(Format: [yyyy/mm/dd] <Author> - <Description of Change>)
[2006/09/29] RaX - Created.
[2007/01/20] Tsusai - Connect is now a bool function
	Create holds connection result
[2007/04/07] CR - Altered header, improved description.  Private fields made
	protected, and into public read-only properties.  Parameter changes made to
	follow the changes made in the ancestor class.
*=============================================================================*)
TMySQLStaticDatabase = class(TStaticDatabaseTemplate)
protected
	fConnection : TMySQLClient;
	fParent     : TDatabase;

	function SendQuery(
		const
			QString     : String;
		const
			StoreResult : Boolean;
		var
			ExecutedOK  : Boolean
	) : TMySQLResult;

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
		) : Word;override;

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
		) : Integer; override;

	Function  GetMapFlags(
		const
			MapName : String
		) : TFlags; override;

	Function  GetMapsForZone(
		const
			ID : LongWord
		) : TStringList; override;

	function  Connect : Boolean; override;

	procedure Disconnect;override;

	property Connection : TMySQLClient
		read  fConnection;
	property Parent : TDatabase
		read  fParent;

End;(* TMySQLStaticDatabase
*== CLASS ====================================================================*)


implementation


uses
	{RTL/VCL}
	SysUtils,
	{Project}
	Globals,
	Main
	{ThirdParty}
	//none
	;


//------------------------------------------------------------------------------
//TMySQLStaticDatabase.Create()                                      CONSTRUCTOR
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
	const
		AParent : TDatabase
	);
begin
	inherited Create();
	fParent := AParent;
	fConnection := TMySQLClient.Create;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMySQLStaticDatabase.Destroy()                                      DESTRUCTOR
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
//TMySQLStaticDatabase.Connect()                                       Procedure
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
		Connection.Host            := MainProc.DatabaseOptions.StaticHost;
		Connection.Port            := MainProc.DatabaseOptions.StaticPort;
		Connection.Db              := MainProc.DatabaseOptions.StaticDB;
		Connection.User            := MainProc.DatabaseOptions.StaticUser;
		Connection.Password        := MainProc.DatabaseOptions.StaticPass;
	end;

	Connection.ConnectTimeout  := 10;

	if NOT Connection.Connect then
	begin
		Console.WriteLn('*****Could not connect to mySQL database server.');
		Result := false;
	end;

end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLStaticDatabase.SendQuery()                                     Function
//------------------------------------------------------------------------------
//	What it does-
//			Sends a query to the mysql object.
//
//	Changes -
//		December 17th, 2006 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
function TMySQLStaticDatabase.SendQuery(
	const
		QString     : String;
	const
		StoreResult : Boolean;
	var
		ExecutedOK  : Boolean
) : TMySQLResult;
begin
	Result := Connection.query(QString,StoreResult,ExecutedOK);
	if not ExecutedOK then
	begin
		Console.Message('MySQL Query error: ' + QString, 'Static Database', MS_ERROR);
	end;
end;

//------------------------------------------------------------------------------
//TMySQLStaticDatabase.Disconnect                                      Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Destroys the MySQL Connection.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TMySQLStaticDatabase.Disconnect;
begin
	if Connection.Connected then
	begin
		Connection.close;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLStaticDatabase.GetBaseMaxHP()                                   FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters Maximum hp before modifiers.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TMySQLStaticDatabase.GetBaseMaxHP(
	const
		ACharacter : TCharacter
	) : Word;
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
//TMySQLStaticDatabase.GetBaseMaxSP()                                   FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters maximum sp before modifiers.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TMySQLStaticDatabase.GetBaseMaxSP(
	const
		ACharacter : TCharacter
	) : Word;
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
Function TMySQLStaticDatabase.GetBaseMaxWeight(
	const
		ACharacter : TCharacter
	) : LongWord;
var
	Success     : Boolean;
	QueryResult : TMySQLResult;
begin
	Result := 0;
	QueryResult :=
		SendQuery(
		Format('SELECT %s FROM weight',
			[ACharacter.JobName])
		,TRUE,Success);
	if QueryResult.RowsCount = 1 then
	begin
		Result := StrToIntDef(QueryResult.FieldValue(0), 0);
	end;
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
Function TMySQLStaticDatabase.GetMapCannotSave(
	const
		MapName : String
	) : Boolean;
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
//		January 16th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
function TMySQLStaticDatabase.GetMapZoneID(
	const
		MapName : String
	) : Integer;
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
Function TMySQLStaticDatabase.GetMapFlags(
	const
		MapName : String
	) : TFlags;
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


//------------------------------------------------------------------------------
//GetMapsForZone 							                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Queries and returns a map's flags.
//
//	Changes -
//		January 22nd, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Function TMySQLStaticDatabase.GetMapsForZone(
	const
		ID : LongWord
	) : TStringList;
var
	QueryResult : TMySQLResult;
  Index       : Integer;
  Success			: Boolean;
begin
  Result := TStringList.Create;
	QueryResult := SendQuery(
		Format('SELECT mapname FROM maps WHERE zoneid = %d',
			[ID]),TRUE,Success);
  if (QueryResult.RowsCount > 0) then
  begin
    for Index := 0 to QueryResult.RowsCount - 1 do
    begin
      Result.Add(QueryResult.FieldValue(0));
      QueryResult.Next;
    end;  
  end;
  QueryResult.Free;
end;//GetMapsForZone
//------------------------------------------------------------------------------
{END MySQLStaticDatabase}
end.
