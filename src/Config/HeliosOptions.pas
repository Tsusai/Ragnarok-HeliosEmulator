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
			fInfoEnabled       : boolean;
			fNoticeEnabled     : boolean;
			fWarningEnabled    : boolean;
			fErrorEnabled      : boolean;

			fLoginEnabled      : boolean;
			fCharaEnabled      : boolean;
			fInterEnabled      : boolean;
			fZoneEnabled       : boolean;

		public
			property ShowInfo : boolean read fInfoEnabled;
			property ShowNotice : boolean read fNoticeEnabled;
			property ShowWarning : boolean read fWarningEnabled;
			property ShowError : boolean read fErrorEnabled;

			property LoginEnabled : boolean read fLoginEnabled;
      property CharaEnabled : boolean read fCharaEnabled;
			property InterEnabled : boolean read fInterEnabled;
      property ZoneEnabled : boolean read fZoneEnabled;

			//Public methods
			procedure Load;
			procedure Save;
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
	procedure THeliosOptions.Load;
	var
		Section    : TStringList;

		//--------------------------------------------------------------------------
		//LoadConsole                                               SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadConsole;
		begin
			ReadSectionValues('Console', Section);
			fInfoEnabled := StrToBoolDef(Section.Values['ShowInfo'] ,true);
			fNoticeEnabled := StrToBoolDef(Section.Values['ShowNotice'] ,true);
			fWarningEnabled := StrToBoolDef(Section.Values['ShowWarning'] ,true);
			fErrorEnabled := StrToBoolDef(Section.Values['ShowError'] ,true);
		end;{Subroutine LoadLogin}
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

	begin
		Section    := TStringList.Create;

		Section.QuoteChar := '"';
		Section.Delimiter := ',';

		LoadConsole;

		LoadLogin;
    LoadChara;
    LoadInter;
    LoadZone;

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
		//Console
		WriteString('Console','ShowInfo',BoolToStr(fInfoEnabled));
		WriteString('Console','ShowNotice',BoolToStr(fNoticeEnabled));
		WriteString('Console','ShowWarning',BoolToStr(fWarningEnabled));
		WriteString('Console','ShowError',BoolToStr(fErrorEnabled));

		//Login
		WriteString('Login','Enabled',BoolToStr(LoginEnabled));

    //Chara
		WriteString('Character','Enabled',BoolToStr(CharaEnabled));

    //Inter
    WriteString('Inter','Enabled',BoolToStr(InterEnabled));

    //Zone
		WriteString('Zone','Enabled',BoolToStr(ZoneEnabled));

		UpdateFile;
	end;{Save}
//------------------------------------------------------------------------------

end{ServerOptions}.
