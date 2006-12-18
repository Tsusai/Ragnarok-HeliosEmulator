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
	DatabaseTemplate;

type

//------------------------------------------------------------------------------
//TDatabase			                                                          CLASS
//------------------------------------------------------------------------------
	TDatabase = class(TObject)
	public
		AnInterface : TDatabaseTemplate;
		Constructor Create(
									UseGameDatabase : boolean = false;
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
	MySQLDatabase,
	JanSQLDatabase;

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
	UseGameDatabase : boolean = false;
	DatabaseType : Integer = -1
);
begin
	Inherited Create();

	//Checks to see if the DatabaseType variable has been specified, if not we...
	if DatabaseType = -1 then
	begin
		DatabaseType := ServerConfig.DatabaseType;//set it to the value of our config.
	end;
	//Here's where we figure out which database interface we want to use.
	case DatabaseType of
		TEXT ://1    Helios Text Database
			begin
				AnInterface := TJanSQLDatabase.Create(UseGameDatabase);
			end;

		MYSQL://2
			begin
				AnInterface := TMySQLDatabase.Create(UseGameDatabase);
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
	AnInterface.Free;
	Inherited;
end;
//------------------------------------------------------------------------------
end.
