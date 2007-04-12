//------------------------------------------------------------------------------
//ConsoleOptions	                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This unit is used to gain access to configuration variables loaded from
//    Console.ini.
//
//	Changes -
//		February 24th, 2007 - RaX - Broken out from HeliosOptions.
//
//------------------------------------------------------------------------------
unit ConsoleOptions;

interface
	uses
		IniFiles;

	type

//------------------------------------------------------------------------------
//TConsoleOptions	                                                        CLASS
//------------------------------------------------------------------------------
		TConsoleOptions = class(TMemIniFile)
		private
//private variables
			fInfoEnabled				: boolean;
			fNoticeEnabled			: boolean;
			fWarningEnabled			: boolean;
			fErrorEnabled				: boolean;
			fDebugEnabled				: boolean;

			fLogsEnabled				: boolean;
			fLogsFileName				: String;
			fInfoLogsEnabled		: boolean;
			fNoticeLogsEnabled	: boolean;
			fWarningLogsEnabled	: boolean;
			fErrorLogsEnabled		: boolean;
			fDebugLogsEnabled		: boolean;

		public
			property ShowInfo 		: boolean read fInfoEnabled;
			property ShowNotices	: boolean read fNoticeEnabled;
			property ShowWarnings	: boolean read fWarningEnabled;
			property ShowErrors		: boolean read fErrorEnabled;
			property ShowDebug		: boolean read fDebugEnabled;

			property LogsEnabled	: boolean read fLogsEnabled;
			property LogsFileName	: String	read fLogsFileName;
			property LogInfo			: boolean read fInfoLogsEnabled;
			property LogNotices		: boolean read fNoticeLogsEnabled;
			property LogWarnings	: boolean read fWarningLogsEnabled;
			property LogErrors		: boolean read fErrorLogsEnabled;
			property LogDebug			: boolean read fDebugLogsEnabled;
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
	procedure TConsoleOptions.Load;
	var
		Section    : TStringList;

		//--------------------------------------------------------------------------
		//LoadOutput                                               SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadOutput;
		begin
			ReadSectionValues('Output', Section);
			fInfoEnabled		:= StrToBoolDef(Section.Values['Info']		,true);
			fNoticeEnabled	:= StrToBoolDef(Section.Values['Notices']	,true);
			fWarningEnabled	:= StrToBoolDef(Section.Values['Warnings']	,true);
			fErrorEnabled		:= StrToBoolDef(Section.Values['Errors']		,true);
			fDebugEnabled		:= StrToBoolDef(Section.Values['Debug']		,true);
		end;{Subroutine LoadOutput}
		//--------------------------------------------------------------------------

		//--------------------------------------------------------------------------
		//LoadLogs                                               SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadLogs;
		begin
			ReadSectionValues('Logs', Section);
			fLogsEnabled 				:= StrToBoolDef(Section.Values['Enabled']	, false);
			fLogsFileName				:= 							Section.Values['File']						;
			fInfoLogsEnabled 		:= StrToBoolDef(Section.Values['Info'] 		, false);
			fNoticeLogsEnabled	:= StrToBoolDef(Section.Values['Notices'] 	, false);
			fWarningLogsEnabled	:= StrToBoolDef(Section.Values['Warnings'] , true);
			fErrorLogsEnabled		:= StrToBoolDef(Section.Values['Errors'] 	, true);
			fDebugLogsEnabled		:= StrToBoolDef(Section.Values['Debug'] 	, false);
		end;{Subroutine LoadOutput}
		//--------------------------------------------------------------------------
	begin
		Section    := TStringList.Create;

		Section.QuoteChar := '"';
		Section.Delimiter := ',';

		LoadOutput;
		LoadLogs;

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
	procedure TConsoleOptions.Save;
	begin
		//Output
		WriteString('Output','Info',BoolToStr(ShowInfo));
		WriteString('Output','Notices',BoolToStr(ShowNotices));
		WriteString('Output','Warnings',BoolToStr(ShowWarnings));
		WriteString('Output','Errors',BoolToStr(ShowErrors));
		WriteString('Output','Debug',BoolToStr(ShowDebug));

		//Logs
		WriteString('Logs','Enabled',BoolToStr(LogsEnabled));
		WriteString('Logs','File', LogsFileName);
		WriteString('Logs','Info',BoolToStr(LogInfo));
		WriteString('Logs','Notices',BoolToStr(LogNotices));
		WriteString('Logs','Warnings',BoolToStr(LogWarnings));
		WriteString('Logs','Errors',BoolToStr(LogErrors));
		WriteString('Logs','Debug',BoolToStr(LogDebug));

		UpdateFile;
	end;{Save}
//------------------------------------------------------------------------------

end{ServerOptions}.
