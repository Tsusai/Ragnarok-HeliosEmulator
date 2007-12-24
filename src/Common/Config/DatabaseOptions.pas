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
			fCommonType   : Integer;
			fCommonHost 	: string;
			fCommonPort 	: integer;
			fCommonDB   	: string;
			fCommonUser 	: string;
			fCommonPass 	: string;

			fGameType     : Integer;
			fGameHost 		: string;
			fGamePort 		: integer;
			fGameDB   		: string;
			fGameUser 		: string;
			fGamePass 		: string;

			fStaticType   : Integer;
			fStaticHost 	: string;
			fStaticPort 	: integer;
			fStaticDB   	: string;
			fStaticUser 	: string;
			fStaticPass 	: string;

		public
			//CommonDB
			property CommonType : Integer read fCommonType write fCommonType;
			property CommonHost : string Read fCommonHost;
			property CommonPort : integer read fCommonPort;
			property CommonDB   : string Read fCommonDB;
			property CommonUser : string Read fCommonUser;
			property CommonPass : string Read fCommonPass;

			//GameDB
			property GameType : Integer read fGameType write fGameType;
			property GameHost : string Read fGameHost;
			property GamePort : integer read fGamePort;
			property GameDB   : string Read fGameDB;
			property GameUser : string Read fGameUser;
			property GamePass : string Read fGamePass;

			//StaticDB
			property StaticType : Integer read fStaticType write fStaticType;
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
		SysUtils,
		DatabaseConstants,
		NetworkConstants,
		Math;

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
		//LoadCommon                                                  SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadCommon;
		begin
			ReadSectionValues('Common', Section);
			fCommonType := EnsureRange(StrToIntDef(Section.Values['Type'], 1), 1, MAX_DBTYPE);
			if Section.Values['Host'] = '' then
			begin
				Section.Values['Host'] := './save';
			end;
			fCommonHost := Section.Values['Host'];
			fCommonPort := EnsureRange(StrToIntDef(Section.Values['Port'], 3306), 1, MAX_PORT);
			if Section.Values['DBName'] = '' then
			begin
				Section.Values['DBName'] := 'helioscommon';
			end;
			fCommonDB := Section.Values['DBName'];
			if Section.Values['Username'] = '' then
			begin
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
			fGameType := EnsureRange(StrToIntDef(Section.Values['Type'], 1), 1, MAX_DBTYPE);

			if Section.Values['Host'] = '' then
			begin
				Section.Values['Host'] := './save';
			end;
			fGameHost := Section.Values['Host'];
			fGamePort := EnsureRange(StrToIntDef(Section.Values['Port'], 3306), 1, MAX_PORT);

			if Section.Values['DBName'] = '' then
			begin
				Section.Values['DBName'] := 'heliosGame';
			end;
			fGameDB := Section.Values['DBName'];

			if Section.Values['Username'] = '' then
			begin
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
			fStaticType := EnsureRange(StrToIntDef(Section.Values['Type'], 1), 1, MAX_DBTYPE);

			if Section.Values['Host'] = '' then begin
				Section.Values['Host'] := './save';
			end;
			fStaticHost := Section.Values['Host'];
			fStaticPort := EnsureRange(StrToIntDef(Section.Values['Port'], 3306), 1, MAX_PORT);
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
		//Common
		WriteString('Common','Type', IntToStr(CommonType));
		WriteString('Common','Host', CommonHost);
		WriteString('Common','Port', IntToStr(CommonPort));
		WriteString('Common','DBName', CommonDB);
		WriteString('Common','Username', CommonUser);
		WriteString('Common','Password', CommonPass);

		//Game
		WriteString('Game','Type', IntToStr(GameType));
		WriteString('Game','Host', GameHost);
		WriteString('Game','Port', IntToStr(GamePort));
		WriteString('Game','DBName', GameDB);
		WriteString('Game','Username', GameUser);
		WriteString('Game','Password', GamePass);

		//Static
		WriteString('Static','Type', IntToStr(StaticType));
		WriteString('Static','Host', StaticHost);
		WriteString('Static','Port', IntToStr(StaticPort));
		WriteString('Static','DBName', StaticDB);
		WriteString('Static','Username', StaticUser);
		WriteString('Static','Password', StaticPass);

		UpdateFile;
	end;{Save}
//------------------------------------------------------------------------------
end{ServerOptions}.
