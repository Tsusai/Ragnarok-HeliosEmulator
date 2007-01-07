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

//Gets/Sets
			procedure SetPort(Value : Word);
			procedure SetWANIP(Value : String);
			procedure SetLANIP(Value : String);

		public
      //Server
      property Enabled : boolean read fEnabled;

			//Communication
      property Port    : Word read fPort write SetPort;
			property WANIP : String read fWANIP write SetWANIP;
			property LANIP : String read fLANIP write SetLANIP;

      //Security

      //Options


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
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
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
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
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
end{ServerOptions}.
