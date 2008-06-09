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

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
	uses
		IniFiles;

	type

  TDatabaseConfig = record
		Protocol							: string;
		Host									: string;
		Port									: Integer;
		Database							: string;
		User									: string;
		Pass									: string;
  end;

//------------------------------------------------------------------------------
//TDatabaseOptions	                                                        CLASS
//------------------------------------------------------------------------------
		TDatabaseOptions = class(TMemIniFile)
		protected
			fAccountConfig		: TDatabaseConfig;

			fGameConfig				: TDatabaseConfig;

		public
			property AccountConfig	: TDatabaseConfig read fAccountConfig;
			property GameConfig			: TDatabaseConfig read fGameConfig;

			Procedure Load;
			Procedure Save;
		end;
//------------------------------------------------------------------------------

implementation
	uses
		Classes,
		SysUtils,
		NetworkConstants,
		Math,
		Main;

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
		//LoadDatabaseConfig                                          SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadDatabaseConfig(const ADatabase : string; var ADatabaseConfig : TDatabaseConfig);
		begin
			ReadSectionValues(ADatabase, Section);
			if Section.Values['Protocol'] = '' then
			begin
				Section.Values['Protocol'] := 'sqlite-3';
			end;
			ADatabaseConfig.Protocol := Section.Values['Protocol'];
			ADatabaseConfig.Host := Section.Values['Host'];
			ADatabaseConfig.Port := EnsureRange(StrToIntDef(Section.Values['Port'], 3306), 1, MAX_PORT);
			if Section.Values['Database'] = '' then
			begin
				Section.Values['Database'] := MainProc.Options.DatabaseDirectory+'/'+ADatabase+'.db';
			end;
			ADatabaseConfig.Database := Section.Values['Database'];
			ADatabaseConfig.User := Section.Values['Username'];
			ADatabaseConfig.Pass := Section.Values['Password'];
		end;{Subroutine LoadOptions}
		//--------------------------------------------------------------------------

	begin
		Section    := TStringList.Create;

		Section.QuoteChar := '"';
		Section.Delimiter := ',';

		LoadDatabaseConfig('account', fAccountConfig);
		LoadDatabaseConfig('game', fGameConfig);

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

		procedure SaveDatabaseConfig(const ADatabase : string; var ADatabaseConfig : TDatabaseConfig);
		begin
			with ADatabaseConfig do
			begin
				WriteString(ADatabase,'Protocol', Protocol);
				WriteString(ADatabase,'Host', Host);
				WriteString(ADatabase,'Port', IntToStr(Port));
				WriteString(ADatabase,'Database', Database);
				WriteString(ADatabase,'Username', User);
				WriteString(ADatabase,'Password', Pass);
			end;
		end;

	begin
		SaveDatabaseConfig('Account', fAccountConfig);
		SaveDatabaseConfig('Game', fGameConfig);

		UpdateFile;
	end;{Save}
//------------------------------------------------------------------------------
end{ServerOptions}.
