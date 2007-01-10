//------------------------------------------------------------------------------
//DatabaseOptions	                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This unit is used to gain access to configuration variables loaded from
//    Database.ini.
//
//	Changes -
//		January 7th, 2007 - RaX - Broken out from ServerOptions.
//
//------------------------------------------------------------------------------
unit DatabaseOptions;

interface
	uses
		IniFiles;

	type

//------------------------------------------------------------------------------
//TDatabaseOptions	                                                        CLASS
//------------------------------------------------------------------------------
		TDatabaseOptions = class(TMemIniFile)
		private
//private variables
			fType         : Integer;

			fCommonHost 	: string;
			fCommonPort 	: integer;
			fCommonDB   	: string;
			fCommonUser 	: string;
			fCommonPass 	: string;

			fGameHost 		: string;
			fGamePort 		: integer;
			fGameDB   		: string;
			fGameUser 		: string;
			fGamePass 		: string;

      fStaticHost 	: string;
			fStaticPort 	: integer;
			fStaticDB   	: string;
			fStaticUser 	: string;
			fStaticPass 	: string;

			procedure SetType(Value : Integer);
		public
			property DatabaseType : Integer read fType write SetType;

      //CommonDB
			property CommonHost : string Read fCommonHost;
			property CommonPort : integer read fCommonPort;
			property CommonDB   : string Read fCommonDB;
			property CommonUser : string Read fCommonUser;
			property CommonPass : string Read fCommonPass;

			//GameDB
			property GameHost : string Read fGameHost;
			property GamePort : integer read fGamePort;
			property GameDB   : string Read fGameDB;
			property GameUser : string Read fGameUser;
			property GamePass : string Read fGamePass;

			//StaticDB
			property StaticHost : string Read fStaticHost;
			property StaticPort : integer read fStaticPort;
			property StaticDB   : string Read fStaticDB;
			property StaticUser : string Read fStaticUser;
			property StaticPass : string Read fStaticPass;

      Procedure Load;
      Procedure Save;
		end;
//------------------------------------------------------------------------------

implementation
	uses
		Classes,
		SysUtils;

//------------------------------------------------------------------------------
//Load()                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			This routine is called to load the ini file values from file itself.
//    This routine contains multiple subroutines. Each one loads a different
//    portion of the ini. All changes to said routines should be documented in
//    THIS changes block.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TDatabaseOptions.Load;
	var
		Section    : TStringList;

    //--------------------------------------------------------------------------
    //LoadDatabase                                                SUB PROCEDURE
    //--------------------------------------------------------------------------
    Procedure LoadDatabase;
    begin
      ReadSectionValues('Database', Section);
      fType := StrToIntDef(Section.Values['Type'],1);
    end;{LoadDatabase}
    //--------------------------------------------------------------------------


    //--------------------------------------------------------------------------
    //LoadCommon                                                  SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadCommon;
		begin
			ReadSectionValues('Common', Section);
			if Section.Values['Host'] = '' then begin
				Section.Values['Host'] := './save';
			end;
			fCommonHost := Section.Values['Host'];
			fCommonPort := StrToIntDef(Section.Values['Port'], 3306);
			if Section.Values['DBName'] = '' then begin
				Section.Values['DBName'] := 'helioscommon';
			end;
			fCommonDB := Section.Values['DBName'];
			if Section.Values['Username'] = '' then begin
				Section.Values['Username'] := 'root';
			end;
			fCommonUser := Section.Values['Username'];
			fCommonPass := Section.Values['Password'];
		end;{Subroutine LoadCommon}
    //--------------------------------------------------------------------------


    //--------------------------------------------------------------------------
    //LoadGame                                                    SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadGame;
		begin
			ReadSectionValues('Game', Section);
			if Section.Values['Host'] = '' then begin
				Section.Values['Host'] := './save';
			end;
			fGameHost := Section.Values['Host'];
			fGamePort := StrToIntDef(Section.Values['Port'], 3306);
			if Section.Values['DBName'] = '' then begin
				Section.Values['DBName'] := 'heliosGame';
			end;
			fGameDB := Section.Values['DBName'];
			if Section.Values['Username'] = '' then begin
				Section.Values['Username'] := 'root';
			end;
			fGameUser := Section.Values['Username'];
			fGamePass := Section.Values['Password'];
		end;{Subroutine LoadGame}
    //--------------------------------------------------------------------------

    //--------------------------------------------------------------------------
    //LoadStatic                                                  SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadStatic;
		begin
			ReadSectionValues('Static', Section);
			if Section.Values['Host'] = '' then begin
				Section.Values['Host'] := './save';
			end;
			fStaticHost := Section.Values['Host'];
			fStaticPort := StrToIntDef(Section.Values['Port'], 3306);
			if Section.Values['DBName'] = '' then begin
				Section.Values['DBName'] := 'HeliosStatic';
			end;
			fStaticDB := Section.Values['DBName'];
			if Section.Values['Username'] = '' then begin
				Section.Values['Username'] := 'root';
			end;
			fStaticUser := Section.Values['Username'];
			fStaticPass := Section.Values['Password'];
		end;{Subroutine LoadStatic}
    //--------------------------------------------------------------------------
	begin
		Section    := TStringList.Create;

		Section.QuoteChar := '"';
		Section.Delimiter := ',';

    LoadCommon;
    LoadGame;
    LoadStatic;
    LoadDatabase;

		Section.Free;

	end;{Load}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Save()                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			This routine saves all configuration values defined here to the .ini
//    file.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TDatabaseOptions.Save;
	begin
    //Database
		WriteString('Database', 'Type',IntToStr(fType));

    //Common
		WriteString('Common','Host', CommonHost);
		WriteString('Common','Port', IntToStr(CommonPort));
		WriteString('Common','DBName', CommonDB);
		WriteString('Common','Username', CommonUser);
		WriteString('Common','Password', CommonPass);

    //Game
		WriteString('Game','Host', GameHost);
		WriteString('Game','Port', IntToStr(GamePort));
		WriteString('Game','DBName', GameDB);
		WriteString('Game','Username', GameUser);
		WriteString('Game','Password', GamePass);

    //Static
		WriteString('Static','Host', StaticHost);
		WriteString('Static','Port', IntToStr(StaticPort));
		WriteString('Static','DBName', StaticDB);
		WriteString('Static','Username', StaticUser);
		WriteString('Static','Password', StaticPass);
    
		UpdateFile;
	end;{Save}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TServerOptions.SetDatabaseType()                                    PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//		September 29th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TDatabaseOptions.SetType(Value : Integer);
	begin
		if fType <> Value then
		begin
			fType := Value;
			WriteString('Database', 'Type', IntToStr(fType));
		end;
	end;{SetType}
//------------------------------------------------------------------------------
end{ServerOptions}.
