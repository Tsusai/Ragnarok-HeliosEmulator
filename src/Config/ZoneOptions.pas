//------------------------------------------------------------------------------
//ZoneOptions	                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This unit is used to gain access to configuration variables loaded from
//    Zone.ini.
//
//	Changes -
//		January 7th, 2007 - RaX - Broken out from ServerOptions.
//
//------------------------------------------------------------------------------
unit ZoneOptions;

interface
	uses
		IniFiles;

	type

//------------------------------------------------------------------------------
//TZoneOptions	                                                        CLASS
//------------------------------------------------------------------------------
		TZoneOptions = class(TMemIniFile)
		private
//private variables
      fID           : LongWord;
			fPort		      : Word;
			fEnabled      : boolean;
			fWANIP      	: String;
			fLANIP 	      : String;

      fCharaIP      : String;
      fCharaPort    : Word;
      fCharaKey     : String;

      fInterIP      : String;
      fInterPort    : Word;
      fInterKey     : String;

			fZoneTick			: Integer;//The amount of time in milliseconds to sleep
															// between packet processes.
			fEventTick		: Integer;//The amount of time in milliseconds to sleep
															// between event processes.
			fCharClickArea: Integer;//The number of cells away from a character that
															//They can click to move to.
			fCharShowArea	: Integer;//The distance in cells away from a character that
                            	//other entities appear in.



//Gets/Sets
			procedure SetPort(Value : Word);
			procedure SetWANIP(Value : String);
			procedure SetLANIP(Value : String);
			procedure SetCharaIP(Value : String);
			procedure SetCharaPort(Value : Word);

		public
      //Server
      property Enabled : boolean read fEnabled;
			property ID      : LongWord read fID;

			//Communication
      property Port    : Word read fPort write SetPort;
			property WANIP : String read fWANIP write SetWANIP;
			property LANIP : String read fLANIP write SetLANIP;

			property CharaIP : String read fCharaIP write SetCharaIP;
			property CharaPort : Word read fCharaPort write SetCharaPort;
			property CharaKey : string read fCharaKey;

			property InterIP : String read fInterIP;
			property InterPort : Word read fInterPort;
			property InterKey : string read fInterKey;

			//Security

      //Options

			//Performance
			property ZoneTick			: Integer read fZoneTick;
			property EventTick		: Integer read fEventTick;
			property CharClickArea: Integer read fCharClickArea;
			property CharShowArea	: Integer read fCharShowArea;

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
	procedure TZoneOptions.Load;
	var
		Section    : TStringList;

    //--------------------------------------------------------------------------
    //LoadServer                                               SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadServer;
		begin
			ReadSectionValues('Server', Section);
			fEnabled := StrToBoolDef(Section.Values['Enabled'] ,true);
      fID := StrToIntDef(Section.Values['ID'] ,1);
		end;{Subroutine LoadServer}
    //--------------------------------------------------------------------------


    //--------------------------------------------------------------------------
    //LoadCommunication                                          SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadCommunication;
		begin
			ReadSectionValues('Communication', Section);
			fPort     := StrToIntDef(Section.Values['Port'], 5121);

			if Section.Values['WANIP'] = '' then
			begin
				Section.Values['WANIP']			:= '127.0.0.1';
			end;
			fWANIP			:= Section.Values['WANIP'];

			if Section.Values['LANIP'] = '' then
			begin
				Section.Values['LANIP']			:= '127.0.0.1';
			end;
			fLANIP			:= Section.Values['LANIP'];

      if Section.Values['CharaIP'] = '' then
			begin
				Section.Values['CharaIP']			:= '127.0.0.1';
			end;
			fCharaIP			:= Section.Values['CharaIP'];

			fCharaPort		:= StrToIntDef(Section.Values['CharaPort'], 6121);
      fCharaKey    := Section.Values['CharaKey'];

      if Section.Values['InterIP'] = '' then
			begin
				Section.Values['InterIP']			:= '127.0.0.1';
			end;
			fInterIP			:= Section.Values['InterIP'];

			fInterPort		:= StrToIntDef(Section.Values['InterPort'], 4000);
      fInterKey    := Section.Values['InterKey'];
		end;{Subroutine LoadCommunication}
    //--------------------------------------------------------------------------


    //--------------------------------------------------------------------------
    //LoadSecurity                                              SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadSecurity;
		begin
			ReadSectionValues('Security', Section);

		end;{Subroutine LoadSecurity}
    //--------------------------------------------------------------------------

		//--------------------------------------------------------------------------
		//LoadPerformance                                             SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadPerformance;
		begin
			ReadSectionValues('Performance', Section);
			fZoneTick			:= StrToIntDef(Section.Values['Zone Tick'], 10);
			fEventTick		:= StrToIntDef(Section.Values['Event Tick'], 10);
			fCharClickArea:= StrToIntDef(Section.Values['Click Area'], 16);
			fCharShowArea	:= StrToIntDef(Section.Values['Show Area'], 16);
		end;{Subroutine LoadPerformance}
		//--------------------------------------------------------------------------

    //--------------------------------------------------------------------------
    //LoadOptions                                               SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadOptions;
		begin
			ReadSectionValues('Options', Section);

		end;{Subroutine LoadOptions}
    //--------------------------------------------------------------------------

	begin
		Section    := TStringList.Create;

		Section.QuoteChar := '"';
		Section.Delimiter := ',';

		LoadServer;
    LoadCommunication;
		LoadSecurity;
		LoadPerformance;
    LoadOptions;

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
	procedure TZoneOptions.Save;
	begin
    //Server
		WriteString('Server','Enabled',BoolToStr(Enabled));
    WriteString('Server','ID',IntToStr(ID));

    //Communication
		WriteString('Communication','WANIP',WANIP);
		WriteString('Communication','LANIP',LANIP);
		WriteString('Communication','Port', IntToStr(Port));

		WriteString('Communication','CharaIP',fCharaIP);
		WriteString('Communication','CharaPort',IntToStr(fCharaPort));
    WriteString('Communication','CharaKey',fCharaKey);

    WriteString('Communication','InterIP',fInterIP);
		WriteString('Communication','InterPort',IntToStr(fInterPort));
		WriteString('Communication','InterKey',fInterKey);

    //Security

		//Performance
		WriteString('Performance','Zone Tick',IntToStr(fZoneTick));
		WriteString('Performance','Event Tick',IntToStr(fEventTick));
		WriteString('Performance','Click Area',IntToStr(fCharClickArea));
		WriteString('Performance','ShowArea',IntToStr(fCharShowArea));

		//Options

		UpdateFile;
	end;{Save}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//SetPort()                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Property Set Routine for ZonePort. Ensures that if the Zone port is
//    changed for whatever reason, that it gets written to the .ini immediately.
//    The same is true for all communication variables.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TZoneOptions.SetPort(Value : word);
	begin
		if fPort <> Value then
		begin
			fPort := Value;
			WriteString('Communication', 'Port', IntToStr(Port));
		end;
	end;{SetPort}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetWANIP()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Property Set Routine for WAN IP, and save to .ini
//	Changes -
//		November 29th, 2006 - RaX - Created.
//		March 13th, 2007 - Aeomin - Modify Header
//
//------------------------------------------------------------------------------
	procedure TZoneOptions.SetWANIP(Value : String);
	begin
		if fWANIP <> Value then
		begin
			fWANIP := Value;
			WriteString('Communication', 'WANIP', WANIP);
		end;
	end;{SetWANIP}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetLANIP()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Property Set Routine for LAN IP, and save to .ini
//	Changes -
//		November 29th, 2006 - RaX - Created.
//		March 13th, 2007 - Aeomin - Modify Header
//
//------------------------------------------------------------------------------
	procedure TZoneOptions.SetLANIP(Value : String);
	begin
		if fWANIP <> Value then
		begin
			fWANIP := Value;
			WriteString('Communication', 'LANIP', LANIP);
		end;
	end;{SetLANIP}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//SetCharaPort()                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Property Set Routine for Character Server Port, and save to .ini
//	Changes -
//		January 14th, 2007 - Tsusai - Created.
//		March 13th, 2007 - Aeomin - Modify Header
//
//------------------------------------------------------------------------------
	procedure TZoneOptions.SetCharaPort(Value : Word);
	begin
		if fCharaPort <> Value then
		begin
			fCharaPort := Value;
			WriteString('Communication', 'CharaPort', IntToStr(CharaPort));
		end;
	end;{SetCharaPort}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//SetCharaIP()                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Property Set Routine for Character Server IP, and save to .ini
//	Changes -
//		January 14th, 2007 - Tsusai - Created.
//		March 13th, 2007 - Aeomin - Modify Header
//
//------------------------------------------------------------------------------
	procedure TZoneOptions.SetCharaIP(Value : String);
	begin
		if fCharaIP <> Value then
		begin
			fCharaIP := Value;
			WriteString('Communication', 'CharaIP', CharaIP);
		end;
	end;{SetLoginIP}
//------------------------------------------------------------------------------
end{ServerOptions}.
