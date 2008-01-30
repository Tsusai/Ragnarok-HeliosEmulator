//------------------------------------------------------------------------------
//HeliosOptions	                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This unit is used to gain access to configuration variables loaded from
//    Helios.ini.
//
//	Changes -
//		January 7th, 2007 - RaX - Broken out from ServerOptions.
//
//------------------------------------------------------------------------------
unit HeliosOptions;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
	uses
		IniFiles;

	type

//------------------------------------------------------------------------------
//THeliosOptions	                                                        CLASS
//------------------------------------------------------------------------------
		THeliosOptions = class(TMemIniFile)
		private
//private variables
			fMapDirectory				: String;
			fDatabaseDirectory	: String;
			fConfigDirectory		: String;
			fScriptDirectory		: String;

			fLoginEnabled				: Boolean;
			fCharaEnabled				: Boolean;
			fInterEnabled				: Boolean;
			fZoneEnabled				: Boolean;

			fReconnectDelay     : LongWord;

		public
			property MapDirectory				: String read fMapDirectory;
			property DatabaseDirectory	: String read fDatabaseDirectory;
			property ConfigDirectory		: String read fConfigDirectory;
			property ScriptDirectory		: String read fScriptDirectory;

			property LoginEnabled	: Boolean read fLoginEnabled;
			property CharaEnabled	: Boolean read fCharaEnabled;
			property InterEnabled	: Boolean read fInterEnabled;
			property ZoneEnabled	: Boolean read fZoneEnabled;

			property ReconnectDelay : LongWord read fReconnectDelay;

			//Public methods
			procedure Load;
			procedure Save;
		end;
//------------------------------------------------------------------------------

implementation
	uses
		Classes,
		SysUtils,
		WinLinux;

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
	procedure THeliosOptions.Load;
	var
		Section    : TStringList;


		//--------------------------------------------------------------------------
		//LoadFolderStructure                                      SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadFolderStructure;
		begin
			ReadSectionValues('Folder Structure', Section);
			if NOT IsValidFolderName(Section.Values['Maps']) then
			begin
				Section.Values['Maps']	:= './Maps';
			end;
			if NOT IsValidFolderName(Section.Values['Database']) then
			begin
				Section.Values['Database']	:= './Database';
			end;
			if NOT IsValidFolderName(Section.Values['Configuration']) then
			begin
				Section.Values['Configuration']	 := './Configuration';
			end;
			if NOT IsValidFolderName(Section.Values['Script']) then
			begin
				Section.Values['Script']	 := './Scripts';
			end;

			fMapDirectory				:= Section.Values['Maps'];
			fDatabaseDirectory	:= Section.Values['Database'];
			fConfigDirectory		:= Section.Values['Configuration'];
			fScriptDirectory		:= Section.Values['Script'];
		end;{Subroutine LoadZone}
		//--------------------------------------------------------------------------


		//--------------------------------------------------------------------------
		//LoadLogin                                               SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadLogin;
		begin
			ReadSectionValues('Login', Section);
			fLoginEnabled := StrToBoolDef(Section.Values['Enabled'] ,true);
		end;{Subroutine LoadLogin}
		//--------------------------------------------------------------------------


		//--------------------------------------------------------------------------
		//LoadChara                                          SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadChara;
		begin
			ReadSectionValues('Character', Section);
			fCharaEnabled := StrToBoolDef(Section.Values['Enabled'] ,true);
		end;{Subroutine LoadChara}
		//--------------------------------------------------------------------------


		//--------------------------------------------------------------------------
		//LoadInter                                                 SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadInter;
		begin
			ReadSectionValues('Inter', Section);
			fInterEnabled := StrToBoolDef(Section.Values['Enabled'] ,true);
		end;{Subroutine LoadInter}
		//--------------------------------------------------------------------------


		//--------------------------------------------------------------------------
		//LoadZone                                               SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadZone;
		begin
			ReadSectionValues('Zone', Section);
			fZoneEnabled := StrToBoolDef(Section.Values['Enabled'] ,true);
		end;{Subroutine LoadZone}
		//--------------------------------------------------------------------------

		//--------------------------------------------------------------------------
		//LoadGeneral                                               SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadGeneral;
		begin
			ReadSectionValues('General', Section);
			fReconnectDelay := StrToIntDef(Section.Values['Reconnect Delay'] , 3000);
		end;{Subroutine LoadZone}
		//--------------------------------------------------------------------------

	begin
		Section    := TStringList.Create;

		Section.QuoteChar := '"';
		Section.Delimiter := ',';

		LoadFolderStructure;

		LoadLogin;
		LoadChara;
		LoadInter;
		LoadZone;
		LoadGeneral;

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
	procedure THeliosOptions.Save;
	begin
		//Folder Structure
		WriteString('Folder Structure','Maps',MapDirectory);
		WriteString('Folder Structure','Database',DatabaseDirectory);
		WriteString('Folder Structure','Configuration',ConfigDirectory);
		WriteString('Folder Structure','Script',ScriptDirectory);

		//Login
		WriteString('Login','Enabled',BoolToStr(LoginEnabled));

		//Chara
		WriteString('Character','Enabled',BoolToStr(CharaEnabled));

		//Inter
		WriteString('Inter','Enabled',BoolToStr(InterEnabled));

		//Zone
		WriteString('Zone','Enabled',BoolToStr(ZoneEnabled));

		//General
		WriteString('General', 'Reconnect Delay', IntToStr(ReconnectDelay));

		UpdateFile;
	end;{Save}
//------------------------------------------------------------------------------

end{ServerOptions}.
