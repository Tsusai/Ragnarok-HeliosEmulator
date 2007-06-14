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
//		[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
//
//------------------------------------------------------------------------------
unit Database;


interface


uses
	{RTL/VCL}
	//none
	{Project}
	CommonDatabaseTemplate,
	GameDatabaseTemplate,
	StaticDatabaseTemplate,
	{3rd Party}
	IdContext
	;


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
									CommonDatabaseType    : Integer = -1;
									GameDatabaseType      : Integer = -1;
									StaticDatabaseType    : Integer = -1
		);
		Destructor  Destroy();override;


	end;
//------------------------------------------------------------------------------


implementation


uses
	{RTL/VCL}
	//none
	{Project}
	DatabaseOptions,
	DatabaseConstants,
	Globals,
	JanSQLCommonDatabase,
	JanSQLGameDatabase,
	JanSQLStaticDatabase,
	Main,
	MySQLCommonDatabase,
	MySQLGameDatabase,
	MySQLStaticDatabase,
	SQLiteCommonDatabase
	{3rd Party}
	//none
	;


(*- Constructor ---------------------------------------------------------------*
TDatabase.Create
--------------------------------------------------------------------------------
Overview:
--
	Figures out which database interface we're using.

[2007/03/28] CR - TODO: Better description needed here!

--
Pre:
	TODO
Post:
	TODO

--
Revisions:
--------------------------------------------------------------------------------
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2006/09/30] RaX - Created.
[2007/01/20] Tsusai - Added feedback boolean variable, in order to know if any 
	of the databases failed to connect.
[2007/03/28] CR - Removed hints about unused local variables.  Reindented code,
	wrapping comments and long lines.
*-----------------------------------------------------------------------------*)
Constructor TDatabase.Create(
		AClient               : TIdContext;
		CommonDatabaseType    : Integer = -1;
		GameDatabaseType      : Integer = -1;
		StaticDatabaseType    : Integer = -1
	);
{[2007/03/28] CR - Unused variable declarations - commented out rev 206
Var
	CommonOK : Boolean;
	GameOK   : Boolean;
	StaticOK : Boolean;
}
Begin
	Inherited Create;

	ClientInfo := AClient;

	//Checks to see if the DatabaseType variable has been specified, if not we...
	//Common
	if (CommonDatabaseType = -1) then
	begin//set it to the value of our config.
		CommonDatabaseType := MainProc.DatabaseOptions.CommonType;
	end;
	//Game
	if (GameDatabaseType = -1) then
	begin//set it to the value of our config.
		GameDatabaseType := MainProc.DatabaseOptions.GameType;
	end;
	//Static
	if (StaticDatabaseType = -1) then
	begin//set it to the value of our config.
		StaticDatabaseType := MainProc.DatabaseOptions.StaticType;
	end;

	//Here's where we figure out which database interfaces we want to use.

	//Common
	case CommonDatabaseType of

	TEXT ://1    Helios Text Database
		begin
			CommonData  := TJanSQLCommonDatabase.Create(Self);
		end;

	MYSQL://2
		begin
			CommonData  := TMySQLCommonDatabase.Create(self);
		end;

	SQLITE://3
		begin
			CommonData := TSQLiteCommonDatabase.Create(self);
		end;

	else
		begin //anything else
			Console.WriteLn(
				'COMMON DATABASE NOT CORRECTLY CONFIGURED, HELIOS WILL NOT FUNCTION!!!'
			);
			Console.WriteLn('     See ServerOptions.ini for configuration options.');
		end;
	end;

	//Game
	case GameDatabaseType of

	TEXT ://1    Helios Text Database
		begin
			GameData := TJanSQLGameDatabase.Create(self);
		end;

	MYSQL://2
		begin
			GameData := TMySQLGameDatabase.Create(Self);
		end;

	else
		begin //anything else
			Console.WriteLn(
				'GAME DATABASE NOT CORRECTLY CONFIGURED, HELIOS WILL NOT FUNCTION!!!'
			);
			Console.WriteLn('     See ServerOptions.ini for configuration options.');
		end;
	end;

	//Static
	case StaticDatabaseType of

	TEXT ://1    Helios Text Database
		begin
			StaticData := TJanSQLStaticDatabase.Create(Self);
		end;

	MYSQL://2
		begin
			StaticData := TMySQLStaticDatabase.Create(Self);
		end;

	else
		begin //anything else
			Console.WriteLn(
				'STATIC DATABASE NOT CORRECTLY CONFIGURED, HELIOS WILL NOT FUNCTION!!!'
			);
			Console.WriteLn('     See ServerOptions.ini for configuration options.');
		end;
	end;//case StaticDatabaseType

End; (* Cons TDatabase.Create
*-----------------------------------------------------------------------------*)

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
