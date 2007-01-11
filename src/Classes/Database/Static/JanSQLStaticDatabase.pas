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
	CharaList,
	Account,
	janSQL,
  Database;

//------------------------------------------------------------------------------
//TJanSQLStaticDatabase			                                                           CLASS
//------------------------------------------------------------------------------
//	What it does-
//			This is a child class for our database object system. It allows Helios
//    to communicate with a TEXT database and houses all routines for doing so.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
type
	TJanSQLStaticDatabase = class(TStaticDatabaseTemplate)
	private
		Database : TjanSQL;
    Parent  : TDatabase;
	public

		Constructor Create(EnableStaticDatabase : boolean; AParent : TDatabase); reintroduce; overload;
		Destructor Destroy();override;

		Function GetBaseHP(ACharacter : TCharacter) : Cardinal;override;
		Function GetBaseSP(ACharacter : TCharacter) : Cardinal;override;

		Function GetMapCanSave(MapName : String) : Boolean;override;

	protected
		procedure Connect(); override;
		procedure Disconnect; override;
		function SendQuery(
			const QString : string
		) : Integer;
	end;
//------------------------------------------------------------------------------

implementation
	uses
		Types,
		GameConstants,
		Globals,
		Console,
		SysUtils,
		Classes,
    Math;


//------------------------------------------------------------------------------
//TJanSQLStaticDatabase.Create()                                          CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our connection object.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//		November 13th, 2006 - Tsusai - create inherit comes first.
//
//------------------------------------------------------------------------------
Constructor TJanSQLStaticDatabase.Create(EnableStaticDatabase : boolean; AParent : TDatabase);
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
//
//------------------------------------------------------------------------------
Procedure TJanSQLStaticDatabase.Connect();
var
	ResultIdentifier : Integer;
const ConnectQuery = 'Connect to ''%s''';
begin

	ResultIdentifier := 0;

  if DirectoryExists(Parent.Options.StaticHost) then
  begin
    ResultIdentifier := Database.SQLDirect(Format(ConnectQuery,[Parent.Options.GameHost]));
  end else
  begin
    MainProc.Console('');
    MainProc.Console('The database at '+Parent.Options.StaticHost+' does not exist!');
    MainProc.Console('Please ensure that you have correctly configured your ini file');
  end;

	if ResultIdentifier = 0 then
	begin
		MainProc.Console('*****Could not open text database. Error : ' + Database.Error);
		MainProc.Console(Parent.Options.GameHost);
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
		MainProc.Console('Text Query error: ' + QString);
    MainProc.Console(Database.Error);
	end;
end;//SendQuery
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLStaticDatabase.SetAccount()                                        Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Builds a taccount object from a query result.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//		December 27th, 2006 - Tsusai - Fixed login key and gender char reading
//
//------------------------------------------------------------------------------
procedure SetAccount(
	var AnAccount : TAccount;
	var QueryResult : TJanRecordSet
);
begin
	AnAccount := TAccount.Create;
	AnAccount.ID          := StrToInt(QueryResult.records[0].fields[0].value);
	AnAccount.Username     := QueryResult.records[0].fields[1].value;
	AnAccount.Password     := QueryResult.records[0].fields[2].value;
	//Tsusai - For Gender, we need to return the first char, thats
	//why there is a [1]
	AnAccount.Gender       := String(QueryResult.records[0].fields[4].value)[1];
	AnAccount.LoginCount   := StrToIntDef(QueryResult.records[0].fields[5].value,0);
	AnAccount.EMail        := QueryResult.records[0].fields[6].value;
	AnAccount.LoginKey[1]  := StrToIntDef(QueryResult.records[0].fields[7].value,0);
	AnAccount.LoginKey[2]  := StrToIntDef(QueryResult.records[0].fields[8].value,0);
	AnAccount.Level        := StrToIntDef(QueryResult.records[0].fields[9].value,0);
	AnAccount.ConnectUntil := ConvertMySQLTime(QueryResult.records[0].fields[11].value);
	AnAccount.LastIP       := QueryResult.records[0].fields[12].value;
	AnAccount.Bantime      := ConvertMySQLTime(QueryResult.records[0].fields[14].value);
end;//SetAccount
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLStaticDatabase.GetBaseHP()                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters basehp.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TJanSQLStaticDatabase.GetBaseHP(ACharacter : TCharacter) : Cardinal;
var
	QueryResult : TJanRecordSet;
  ResultIdentifier : Integer;
begin
	ResultIdentifier :=
		SendQuery(
		Format('SELECT * FROM hp WHERE level = %d, job = ''%s''',
			[ACharacter.BaseLV,ACharacter.JID]));
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
//TJanSQLStaticDatabase.GetBaseSP()                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters basesp.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TJanSQLStaticDatabase.GetBaseSP(ACharacter : TCharacter) : Cardinal;
var
	QueryResult : TJanRecordSet;
	ResultIdentifier : Integer;
begin
	ResultIdentifier :=
		SendQuery(
		Format('SELECT * FROM sp WHERE level = %d, job = ''%s''',
			[ACharacter.BaseLV,ACharacter.JID]));
	QueryResult := Database.RecordSets[ResultIdentifier];
	if (QueryResult.RecordCount = 1) then
	begin
			Result              := StrToInt(QueryResult.Records[0].Fields[0].Value);
	end else Result := 0;
	SendQuery('RELEASE TABLE sp');
	if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
end;//GetBaseSP
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetMapCanSave							                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Checks to see if a map can save or not.
//
//	Changes -
//		January 10th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TJanSQLStaticDatabase.GetMapCanSave(MapName : String) : Boolean;
var
	QueryResult : TJanRecordSet;
	ResultIdentifier : Integer;
begin
	ResultIdentifier :=
		SendQuery(
		Format('SELECT save FROM maps WHERE mapname = ''%s''',
			[MapName]));
	QueryResult := Database.RecordSets[ResultIdentifier];
	if (QueryResult.RecordCount = 1) then
	begin
			Result := StrToBool(QueryResult.Records[0].Fields[0].Value);
	end else Result := FALSE;
	SendQuery('RELEASE TABLE maps');
	if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
end;//GetMapCanSave
//------------------------------------------------------------------------------
{END JanSQLStaticDatabase}
end.
