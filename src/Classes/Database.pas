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
									var LoadedOK					: Boolean;
									CommonDatabaseType    : Integer = -1;
									GameDatabaseType      : Integer = -1;
									StaticDatabaseType    : Integer = -1
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
//		January 20th, 2007 - Tsusai - Added feedback boolean variable, in order
//			to know if any of the databases failed to connect.
//
//------------------------------------------------------------------------------
Constructor TDatabase.Create(
  EnableCommonDatabase  : Boolean;
  EnableGameDatabase    : Boolean;
	EnableStaticDatabase  : Boolean;
	var LoadedOK					: Boolean;
	CommonDatabaseType    : Integer = -1;
	GameDatabaseType      : Integer = -1;
	StaticDatabaseType    : Integer = -1
);
Var
	CommonOK : boolean;
	GameOK   : boolean;
	StaticOK : boolean;

begin
	Inherited Create();
	LoadOptions;
	//Checks to see if the DatabaseType variable has been specified, if not we...
	//Common
	if CommonDatabaseType = -1 then
	begin
		CommonDatabaseType := Options.CommonType;//set it to the value of our config.
	end;
  //Game
  if GameDatabaseType = -1 then
	begin
		GameDatabaseType := Options.GameType;//set it to the value of our config.
	end;
	//Static
  if StaticDatabaseType = -1 then
	begin
		StaticDatabaseType := Options.StaticType;//set it to the value of our config.
	end;

	//Here's where we figure out which database interfaces we want to use.
  //Common
	case CommonDatabaseType of
		TEXT ://1    Helios Text Database
			begin
				CommonData  := TJanSQLCommonDatabase.Create(EnableCommonDatabase,CommonOK,self);
			end;

		MYSQL://2
			begin
				CommonData  := TMySQLCommonDatabase.Create(EnableCommonDatabase,CommonOK,self);
			end;

		else begin //anything else
			MainProc.Console('COMMON DATABASE NOT CORRECTLY CONFIGURED, HELIOS WILL NOT FUNCTION!!!');
			MainProc.Console('     See ServerOptions.ini for configuration options.');
		end;
  end;
	//Game
	case GameDatabaseType of
		TEXT ://1    Helios Text Database
			begin
				GameData    := TJanSQLGameDatabase.Create(EnableGameDatabase,GameOK,self);
			end;

		MYSQL://2
			begin
				GameData    := TMySQLGameDatabase.Create(EnableGameDatabase,GameOK,self);
			end;

    else begin //anything else
			MainProc.Console('GAME DATABASE NOT CORRECTLY CONFIGURED, HELIOS WILL NOT FUNCTION!!!');
			MainProc.Console('     See ServerOptions.ini for configuration options.');
		end;
	end;
  //Static
	case StaticDatabaseType of
		TEXT ://1    Helios Text Database
			begin
				StaticData    := TJanSQLStaticDatabase.Create(EnableStaticDatabase,StaticOK,self);
			end;

		MYSQL://2
			begin
        StaticData    := TMySQLStaticDatabase.Create(EnableStaticDatabase,StaticOK,self);
			end;

		else begin //anything else
			MainProc.Console('STATIC DATABASE NOT CORRECTLY CONFIGURED, HELIOS WILL NOT FUNCTION!!!');
			MainProc.Console('     See ServerOptions.ini for configuration options.');
		end;
	end;
	LoadedOK := (CommonOK and GameOK and StaticOK);
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
