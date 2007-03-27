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
	DatabaseOptions,
	IdContext;

type

//------------------------------------------------------------------------------
//TDatabase			                                                          CLASS
//------------------------------------------------------------------------------
	TDatabase = class(TObject)
	public
    CommonData    : TCommonDatabaseTemplate;
		GameData      : TGameDatabaseTemplate;
		StaticData    : TStaticDatabaseTemplate;

		ClientInfo		: TIdContext;

		Constructor Create(
									AClient								: TIdContext;
									EnableCommonDatabase  : Boolean;
									EnableGameDatabase    : Boolean;
									EnableStaticDatabase  : Boolean;
									CommonDatabaseType    : Integer = -1;
									GameDatabaseType      : Integer = -1;
									StaticDatabaseType    : Integer = -1
		);
		Destructor  Destroy();override;


  end;
//------------------------------------------------------------------------------


implementation

uses
	Main,
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
	AClient								: TIdContext;
	EnableCommonDatabase  : Boolean;
  EnableGameDatabase    : Boolean;
	EnableStaticDatabase  : Boolean;
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
	ClientInfo := AClient;
	//Checks to see if the DatabaseType variable has been specified, if not we...
	//Common
	if CommonDatabaseType = -1 then
	begin
		CommonDatabaseType := MainProc.DatabaseOptions.CommonType;//set it to the value of our config.
	end;
	//Game
  if GameDatabaseType = -1 then
	begin
		GameDatabaseType := MainProc.DatabaseOptions.GameType;//set it to the value of our config.
	end;
	//Static
  if StaticDatabaseType = -1 then
	begin
		StaticDatabaseType := MainProc.DatabaseOptions.StaticType;//set it to the value of our config.
	end;

	//Here's where we figure out which database interfaces we want to use.
  //Common
	case CommonDatabaseType of
		TEXT ://1    Helios Text Database
			begin
				CommonData  := TJanSQLCommonDatabase.Create(EnableCommonDatabase,self);
			end;

		MYSQL://2
			begin
				CommonData  := TMySQLCommonDatabase.Create(EnableCommonDatabase,self);
			end;

		else begin //anything else
			Console.WriteLn('COMMON DATABASE NOT CORRECTLY CONFIGURED, HELIOS WILL NOT FUNCTION!!!');
			Console.WriteLn('     See ServerOptions.ini for configuration options.');
		end;
  end;
	//Game
	case GameDatabaseType of
		TEXT ://1    Helios Text Database
			begin
				GameData    := TJanSQLGameDatabase.Create(EnableGameDatabase,self);
			end;

		MYSQL://2
			begin
				GameData    := TMySQLGameDatabase.Create(EnableGameDatabase,self);
			end;

    else begin //anything else
			Console.WriteLn('GAME DATABASE NOT CORRECTLY CONFIGURED, HELIOS WILL NOT FUNCTION!!!');
			Console.WriteLn('     See ServerOptions.ini for configuration options.');
		end;
	end;
  //Static
	case StaticDatabaseType of
		TEXT ://1    Helios Text Database
			begin
				StaticData    := TJanSQLStaticDatabase.Create(EnableStaticDatabase,self);
			end;

		MYSQL://2
			begin
				StaticData    := TMySQLStaticDatabase.Create(EnableStaticDatabase,self);
			end;

		else begin //anything else
			Console.WriteLn('STATIC DATABASE NOT CORRECTLY CONFIGURED, HELIOS WILL NOT FUNCTION!!!');
			Console.WriteLn('     See ServerOptions.ini for configuration options.');
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
	Inherited;
end;
//------------------------------------------------------------------------------

end.
