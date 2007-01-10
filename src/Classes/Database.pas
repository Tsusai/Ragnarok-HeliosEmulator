//------------------------------------------------------------------------------
//Database			                                                           UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This class decides what database interface we use in Create. AnInterface
//    refers to whatever database interface we use, whether it be MySQL, TEXT,
//    MSSQL, whatever. Also, may, in the future, contain generic database
//    routines as well. Such as simple string search routines, etc.
//
//	Changes -
//		September 30th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit Database;

interface
uses
	CommonDatabaseTemplate,
  GameDatabaseTemplate,
  StaticDatabaseTemplate,
  DatabaseOptions;

type

//------------------------------------------------------------------------------
//TDatabase			                                                          CLASS
//------------------------------------------------------------------------------
	TDatabase = class(TObject)
  private
    Procedure LoadOptions;

	public
    CommonData    : TCommonDatabaseTemplate;
    GameData      : TGameDatabaseTemplate;
    StaticData    : TStaticDatabaseTemplate;

    Options       : TDatabaseOptions;

		Constructor Create(
									EnableCommonDatabase  : Boolean;
                  EnableGameDatabase    : Boolean;
                  EnableStaticDatabase  : Boolean;
									DatabaseType : Integer = -1
		);
		Destructor  Destroy();override;


  end;
//------------------------------------------------------------------------------


implementation

uses
	Console,
	Globals,
	DatabaseConstants,
	MySQLCommonDatabase,
	JanSQLCommonDatabase,
	MySQLGameDatabase,
	JanSQLGameDatabase,
	MySQLStaticDatabase,
	JanSQLStaticDatabase,
  SysUtils;

//------------------------------------------------------------------------------
//TDatabase.Create()			                                          CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Figures out which database interface we're using.
//
//	Changes -
//		September 30th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Constructor TDatabase.Create(
  EnableCommonDatabase  : Boolean;
  EnableGameDatabase    : Boolean;
  EnableStaticDatabase  : Boolean;
  DatabaseType          : Integer = -1
);
begin
	Inherited Create();
  LoadOptions;
	//Checks to see if the DatabaseType variable has been specified, if not we...
	if DatabaseType = -1 then
	begin
		DatabaseType := Options.DatabaseType;//set it to the value of our config.
	end;

	//Here's where we figure out which database interfaces we want to use.
	case DatabaseType of
		TEXT ://1    Helios Text Database
			begin
        CommonData  := TJanSQLCommonDatabase.Create(EnableCommonDatabase, self);
        GameData    := TJanSQLGameDatabase.Create(EnableGameDatabase, self);
        StaticData  := TJanSQLStaticDatabase.Create(EnableStaticDatabase, self);
			end;

		MYSQL://2
			begin
        CommonData  := TMySQLCommonDatabase.Create(EnableCommonDatabase, self);
        GameData    := TMySQLGameDatabase.Create(EnableGameDatabase, self);
        StaticData  := TMySQLStaticDatabase.Create(EnableStaticDatabase, self);
			end;

		else begin //anything else
			MainProc.Console('DATABASE NOT CORRECTLY CONFIGURED, HELIOS WILL NOT FUNCTION!!!');
			MainProc.Console('     See ServerOptions.ini for configuration options.');
		end;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TDatabase.Destroy()			                                               DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Frees up our custom properties.
//
//	Changes -
//		September 30th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Destructor TDatabase.Destroy();
begin
	CommonData.Free;
  GameData.Free;
  StaticData.Free;
  Options.Save;
  Options.Free;
	Inherited;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadOptions                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Creates and Loads the inifile.
//
//	Changes -
//		January 4th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TDatabase.LoadOptions;
begin
  if Assigned(Options) then
  begin
    FreeAndNIL(Options);
  end;

  Options    := TDatabaseOptions.Create('./Database.ini');

	Options.Load;
end;{LoadOptions}
//------------------------------------------------------------------------------
end.
