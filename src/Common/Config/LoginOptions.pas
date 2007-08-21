//------------------------------------------------------------------------------
//LoginOptions	                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This unit is used to gain access to configuration variables loaded from
//    Login.ini.
//
//	Changes -
//		January 7th, 2007 - RaX - Broken out from ServerOptions.
//
//------------------------------------------------------------------------------
unit LoginOptions;

interface
	uses
		IniFiles;

	type

//------------------------------------------------------------------------------
//TLoginOptions	                                                        CLASS
//------------------------------------------------------------------------------
		TLoginOptions = class(TMemIniFile)
		private
//private variables
			fPort		      : Word;
      fKey          : String;
			fEnableMF			: Boolean;
      fIndySchedulerType  : Byte;
      fIndyThreadPoolSize : Word;
//Gets/Sets
			procedure SetPort(Value : Word);
			procedure SetEnableMF(Value : Boolean);

		public
			//Communication
      property Port    : Word read fPort write SetPort;

      //Security
      property Key     : String read fKey;

      //Options
      property EnableMF     : boolean read fEnableMF write SetEnableMF;

      //Performance
      property IndySchedulerType : Byte read fIndySchedulerType;
      property IndyThreadPoolSize : Word read fIndyThreadPoolSize;

			//Public methods
			procedure Load;
			procedure Save;
		end;
//------------------------------------------------------------------------------

implementation
	uses
		Classes,
		SysUtils,
		Math,
		NetworkConstants;

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
	procedure TLoginOptions.Load;
	var
		Section    : TStringList;

    //--------------------------------------------------------------------------
    //LoadCommunication                                          SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadCommunication;
		begin
			ReadSectionValues('Communication', Section);
			fPort   := EnsureRange(StrToIntDef(Section.Values['Port'], 6900), 1, MAX_PORT);
		end;{Subroutine LoadCommunication}
    //--------------------------------------------------------------------------


    //--------------------------------------------------------------------------
    //LoadSecurity                                              SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadSecurity;
		begin
			ReadSectionValues('Security', Section);
      fKey    := Section.Values['Key'];
		end;{Subroutine LoadSecurity}
    //--------------------------------------------------------------------------


    //--------------------------------------------------------------------------
    //LoadOptions                                               SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadOptions;
		begin
			ReadSectionValues('Options', Section);
      fEnableMF    := StrToBoolDef(Section.Values['EnableMF'] ,false);
		end;{Subroutine LoadOptions}
    //--------------------------------------------------------------------------


    //--------------------------------------------------------------------------
    //LoadPerformance                                             SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadPerformance;
		begin
			ReadSectionValues('Performance', Section);
      fIndySchedulerType := EnsureRange(StrToIntDef(Section.Values['Indy Scheduler Type'], 0), 0, 2);
      fIndyThreadPoolSize := EnsureRange(StrToIntDef(Section.Values['Indy Thread Pool Size'], 1), 1, High(Word));
		end;{Subroutine LoadPerformance}
    //--------------------------------------------------------------------------

	begin
		Section    := TStringList.Create;

		Section.QuoteChar := '"';
		Section.Delimiter := ',';

    LoadCommunication;
    LoadSecurity;
    LoadOptions;
    LoadPerformance;

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
	procedure TLoginOptions.Save;
	begin
    //Communication
		WriteString('Communication','Port',IntToStr(fPort));

    //Security
    WriteString('Security','Key',fKey);

    //Options
		WriteString('Options','EnableMF',BoolToStr(EnableMF));

    //Performance
    WriteString('Performance','Indy Scheduler Type',IntToStr(IndySchedulerType));
    WriteString('Performance','Indy Thread Pool Size',IntToStr(IndyThreadPoolSize));

		UpdateFile;
	end;{Save}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//SetPort()                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Property Set Routine for LoginPort. Ensures that if the login port is
//    changed for whatever reason, that it gets written to the .ini immediately.
//    The same is true for all communication variables.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TLoginOptions.SetPort(Value : word);
	begin
		if fPort <> Value then
		begin
			fPort := Value;
			WriteString('Communication', 'Port', IntToStr(fPort));
		end;
	end;{SetPort}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetEnableMF()                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Property Set Routine for EnableMF. Ensures that if _M/_F registration is
//    changed for whatever reason, that it gets written to the .ini immediately.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TLoginOptions.SetEnableMF(Value : boolean);
	begin
		if fEnableMF <> value then
		begin
			fEnableMF := value;
			WriteString('Options', 'EnableMF', BoolToStr(EnableMF));
		end;
	end;{SetEnableMF}
//------------------------------------------------------------------------------
end{ServerOptions}.
