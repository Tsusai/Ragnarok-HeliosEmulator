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
  Database;
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
//
//------------------------------------------------------------------------------
	TMySQLStaticDatabase = class(TStaticDatabaseTemplate)
	private
		Connection   : TMySQLClient;
    Parent : TDatabase;
	public


		Constructor Create(EnableStaticDatabase : boolean; AParent : TDatabase); reintroduce; overload;
		Destructor Destroy();override;

		Function GetBaseHP(ACharacter : TCharacter) : Cardinal;override;
		Function GetBaseSP(ACharacter : TCharacter) : Cardinal;override;

	protected
		procedure Connect(); override;
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
//
//------------------------------------------------------------------------------
Constructor TMySQLStaticDatabase.Create(EnableStaticDatabase : boolean; AParent : TDatabase);
begin
	inherited Create(EnableStaticDatabase);
  Parent := AParent;
  Connection := TMySQLClient.Create;
  if EnableStaticDatabase then
  begin
	  Connect();
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
//
//------------------------------------------------------------------------------
Procedure TMySQLStaticDatabase.Connect();
begin
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
//TMySQLStaticDatabase.GetBaseHP()                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters basehp.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TMySQLStaticDatabase.GetBaseHP(ACharacter : TCharacter) : Cardinal;
var
	Success     : Boolean;
	QueryResult : TMySQLResult;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT * FROM hp WHERE level = %d, job = ''%s''',
			[ACharacter.BaseLV,ACharacter.JID])
		,TRUE,Success);
	if (QueryResult.RowsCount = 1) and (QueryResult.FieldsCount = 48) then
	begin
			Result              := StrToInt(QueryResult.FieldValue(0));
	end else Result := 0;
	if Assigned(QueryResult) then QueryResult.Free;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLStaticDatabase.GetBaseSP()                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters basesp.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TMySQLStaticDatabase.GetBaseSP(ACharacter : TCharacter) : Cardinal;
var
	Success     : Boolean;
	QueryResult : TMySQLResult;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT * FROM sp WHERE level = %d, job = ''%s''',
			[ACharacter.BaseLV,ACharacter.JID])
		,TRUE,Success);
	if (QueryResult.RowsCount = 1) and (QueryResult.FieldsCount = 48) then
	begin
			Result              := StrToInt(QueryResult.FieldValue(0));
	end else Result := 0;
	if Assigned(QueryResult) then QueryResult.Free;
end;
//------------------------------------------------------------------------------

{END MySQLStaticDatabase}
end.
