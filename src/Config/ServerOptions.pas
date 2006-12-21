//------------------------------------------------------------------------------
//ServerOptions	                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This unit is used to gain access to configuration variables loaded from
//    ServerOptions.ini.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//		November 13th, 2006 - Tsusai - Added new options, covering the dual 
//		 databases, and rearangements.
//
//------------------------------------------------------------------------------
unit ServerOptions;

interface
	uses
		IniFiles;

	type

//------------------------------------------------------------------------------
//TServerOptions	                                                        CLASS
//------------------------------------------------------------------------------
		TServerOptions = class(TMemIniFile)
		private
//Login Config
			fLoginPort		: Word;
			fLoginEnabled : boolean;
			fEnableMF			: Boolean;

//Chara Config
			fCharaPort		: Word;
			fCharaEnabled : boolean;
			fCharaWANIP		: String;
			fCharaLANIP		: String;
			fLoginComIP		: String;
			fLoginComPort : Word;
			fServerName		: String;

//Zone Config
			fZonePort			: Word;
			fZoneEnabled	: boolean;
			fZoneWANIP		: String;
			fZoneLANIP		: String;
			fInterComIP 	: String;
			fInterComPort : Word;
			fCharaComIP		: String;
			fCharaComPort	: Word;

//Inter Config
			fInterPort 		: Word;
			fInterEnabled : boolean;
			fInterWANIP 	: String;
			fInterLANIP 	: String;

//Database Config
			fDatabaseType : Integer;

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

//Gets/Sets
//Login Related
			procedure SetLoginPort(Value : Word);
			procedure SetEnableMF(Value : Boolean);

//Character Related
			procedure SetCharaPort(Value : Word);
			procedure SetCharaWANIP(Value : String);
			procedure SetCharaLANIP(Value : String);
			procedure SetLoginComIP(Value : String);
			procedure SetLoginComPort(Value : Word);
			procedure SetServerName(Value : String);

//Zone Related
			procedure SetZonePort(Value : Word);
			procedure SetZoneWANIP(Value : String);
			procedure SetZoneLANIP(Value : String);
			procedure SetCharaComIP(Value : String);
			procedure SetCharaComPort(Value : Word);
			procedure SetInterComIP(Value : String);
			procedure SetInterComPort(Value : Word);

//Inter Related
			procedure SetInterPort(Value : Word);
			procedure SetInterWANIP(Value : String);
			procedure SetInterLANIP(Value : String);

//Database Related
			procedure SetDatabaseType(Value : Integer);

		public
			//Communication

			//LoginOptions
			property EnableMF : boolean read fEnableMF write SetEnableMF;
			property LoginPort : Word read fLoginPort write SetLoginPort;
			property LoginEnabled : boolean read fLoginEnabled;

			//CharaOptions
			property ServerName : String read fServerName write SetServerName;
			property CharaPort : Word read fCharaPort write SetCharaPort;
			property CharaEnabled : boolean read fCharaEnabled;
			property CharaWANIP : String read fCharaWANIP write SetCharaWANIP;
			property CharaLANIP : String read fCharaLANIP write SetCharaLANIP;
			property LoginComIP : String read fLoginComIP write SetLoginComIP;
			property LoginComPort : Word read fLoginComPort write SetLoginComPort;

			//ZoneOptions
			property ZonePort  : Word read fZonePort  write SetZonePort;
			property ZoneEnabled : boolean read fZoneEnabled;
			property ZoneLANIP : String read fZoneLANIP write SetZoneLANIP;
			property ZoneWANIP : String read fZoneWANIP write SetZoneWANIP;
			property CharaComIP : String read fCharaComIP write SetCharaComIP;
			property CharaComPort : Word read fCharaComPort write SetCharaComPort;
			property InterComIP : String read fInterComIP write SetInterComIP;
			property InterComPort : Word read fInterComPort write SetInterComPort;

			//InterOptions
			property InterPort  : Word read fInterPort  write SetInterPort;
			property InterEnabled : boolean read fInterEnabled;
			property InterWANIP : String read fInterWANIP write SetInterWANIP;
			property InterLANIP : String read fInterLANIP write SetInterLANIP;

			//Database - Best to turn off the server BEFORE editing this stuff anywho.
			//Common Information Database

			property DatabaseType : Integer read fDatabaseType write SetDatabaseType;
			property CommonHost : string Read fCommonHost;
			property CommonPort : integer read fCommonPort;
			property CommonDB   : string Read fCommonDB;
			property CommonUser : string Read fCommonUser;
			property CommonPass : string Read fCommonPass;

			//Game Specific Database
			property GameHost : string Read fGameHost;
			property GamePort : integer read fGamePort;
			property GameDB   : string Read fGameDB;
			property GameUser : string Read fGameUser;
			property GamePass : string Read fGamePass;

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
//TServerOptions.Load()                                               PROCEDURE
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
	procedure TServerOptions.Load;
	var
		Section    : TStringList;

    //--------------------------------------------------------------------------
    //LoadLoginOptions                                      SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadLoginOptions;
		begin
			ReadSectionValues('Login', Section);
			fEnableMF    := StrToBoolDef(Section.Values['EnableMF'] ,false);
			fLoginEnabled := StrToBoolDef(Section.Values['Enabled'] ,true);
			fLoginPort   := StrToIntDef(Section.Values['Port'], 6900);

		end;{Subroutine LoadLoginOptions}
    //--------------------------------------------------------------------------

		//--------------------------------------------------------------------------
		//LoadCharaOptions                                      SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadCharaOptions;
		begin
			ReadSectionValues('Chara', Section);

			if Section.Values['ServerName'] = '' then begin
				Section.Values['ServerName'] := 'Helios';
			end;
			fCharaEnabled		:= StrToBoolDef(Section.Values['Enabled'], TRUE);
			fServerName			:= Section.Values['ServerName'];
			fCharaPort			:= StrToIntDef(Section.Values['Port'], 6121);
			fLoginComPort		:= StrToIntDef(Section.Values['LoginPort'], 6900);

			if Section.Values['WANIP'] = '' then
			begin
				Section.Values['WANIP']			:= '127.0.0.1';
			end;
			fCharaWANIP			:= Section.Values['WANIP'];

			if Section.Values['LANIP'] = '' then
			begin
				Section.Values['LANIP']			:= '127.0.0.1';
			end;
			fCharaLANIP			:= Section.Values['LANIP'];

			if Section.Values['LoginIP'] = '' then
			begin
				Section.Values['LoginIP']			:= '127.0.0.1';
			end;
			fLoginComIP			:= Section.Values['LoginIP'];
		end;{Subroutine LoadCharaOptions}
		//--------------------------------------------------------------------------

		//--------------------------------------------------------------------------
		//LoadZoneOptions                                      SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadZoneOptions;
		begin
			ReadSectionValues('Zone', Section);

			fZoneEnabled  := StrToBoolDef(Section.Values['Enabled'], TRUE);
			fZonePort     := StrToIntDef(Section.Values['Port'], 5121);
			fCharaComPort		:= StrToIntDef(Section.Values['CharaPort'], 6121);
			fInterComPort		:= StrToIntDef(Section.Values['InterPort'], 4000);

			if Section.Values['WANIP'] = '' then
			begin
				Section.Values['WANIP']			:= '127.0.0.1';
			end;
			fZoneWANIP			:= Section.Values['WANIP'];

			if Section.Values['LANIP'] = '' then
			begin
				Section.Values['LANIP']			:= '127.0.0.1';
			end;
			fZoneLANIP			:= Section.Values['LANIP'];


			if Section.Values['CharaIP'] = '' then
			begin
				Section.Values['CharaIP']			:= '127.0.0.1';
			end;
			fCharaComIP			:= Section.Values['CharaIP'];


			if Section.Values['InterIP'] = '' then
			begin
				Section.Values['InterIP']			:= '127.0.0.1';
			end;
			fInterComIP			:= Section.Values['InterIP'];
		end;{Subroutine LoadZoneOptions}
		//--------------------------------------------------------------------------

		//--------------------------------------------------------------------------
		//LoadInterOptions                                      SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadInterOptions;
		begin
			ReadSectionValues('Inter', Section);

			fInterEnabled  := StrToBoolDef(Section.Values['Enabled'], TRUE);
			fInterPort     := StrToIntDef(Section.Values['Port'], 4000);

			if Section.Values['WANIP'] = '' then
			begin
				Section.Values['WANIP']			:= '127.0.0.1';
			end;
			fInterWANIP			:= Section.Values['WANIP'];

			if Section.Values['LANIP'] = '' then
			begin
				Section.Values['LANIP']			:= '127.0.0.1';
			end;
			fInterLANIP			:= Section.Values['LANIP'];
		end;{Subroutine LoadInterOptions}
		//--------------------------------------------------------------------------

		//--------------------------------------------------------------------------
		//LoadDatabase                                      SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadDatabase;
		begin
			ReadSectionValues('Database', Section);

			fDatabaseType := StrToIntDef(Section.Values['Type'], 1);
			//Accounts
			if Section.Values['Common_Host'] = '' then begin
				Section.Values['Common_Host'] := './save';
			end;
			fCommonHost := Section.Values['Common_Host'];
			fCommonPort := StrToIntDef(Section.Values['Common_Port'], 3306);
			if Section.Values['Common_Database'] = '' then begin
				Section.Values['Common_Database'] := 'helioscommon';
			end;
			fCommonDB := Section.Values['Common_Database'];
			if Section.Values['Common_Username'] = '' then begin
				Section.Values['Common_Username'] := 'root';
			end;
			fCommonUser := Section.Values['Common_Username'];
			fCommonPass := Section.Values['Common_Password'];

			//Gameserver stuff
			if Section.Values['Game_Host'] = '' then begin
				Section.Values['Game_Host'] := './save';
			end;
			fGameHost := Section.Values['Game_Host'];
			fGamePort := StrToIntDef(Section.Values['Game_Port'], 3306);
			if Section.Values['Game_Database'] = '' then begin
				Section.Values['Game_Database'] := 'heliosgame';
			end;
			fGameDB := Section.Values['Game_Database'];
			if Section.Values['Game_Username'] = '' then begin
				Section.Values['Game_Username'] := 'root';
			end;
			fGameUser := Section.Values['Game_Username'];
			fGamePass := Section.Values['Game_Password'];

		end;{Subroutine LoadMySQL}
		//--------------------------------------------------------------------------
	begin
		Section    := TStringList.Create;

		Section.QuoteChar := '"';
		Section.Delimiter := ',';

		LoadLoginOptions;
		LoadCharaOptions;
		LoadZoneOptions;
		LoadInterOptions;
		LoadDatabase;
		Section.Free;

	end;{TServerOptions.Load}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.Save()                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			This routine saves all configuration values defined here to the .ini
//    file.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.Save;
	begin
		WriteString('Login','Enabled',BoolToStr(LoginEnabled));
		WriteString('Login','Port',IntToStr(LoginPort));
		WriteString('Login','EnableMF',BoolToStr(EnableMF));

		WriteString('Chara','Enabled',BoolToStr(CharaEnabled));
		WriteString('Chara','WANIP',CharaWANIP);
		WriteString('Chara','LANIP',CharaLANIP);
		WriteString('Chara','Port', IntToStr(CharaPort));
		WriteString('Chara','LoginIP',LoginComIP);
		WriteString('Chara','LoginPort',IntToStr(LoginComPort));
		WriteString('Chara','ServerName',ServerName);

		WriteString('Zone','Enabled',BoolToStr(ZoneEnabled));
		WriteString('Zone','WANIP',ZoneWANIP);
		WriteString('Zone','LANIP',ZoneLANIP);
		WriteString('Zone','Port', IntToStr(ZonePort));
		WriteString('Zone','CharaIP',CharaComIP);
		WriteString('Zone','CharaPort',IntToStr(CharaComPort));
		WriteString('Zone','InterIP',InterComIP);
		WriteString('Zone','InterPort',IntToStr(InterComPort));

		WriteString('Inter','Enabled',BoolToStr(InterEnabled));
		WriteString('Inter','WANIP',InterWANIP);
		WriteString('Inter','LANIP',InterLANIP);
		WriteString('Inter','Port', IntToStr(InterPort));

		WriteString('Database', 'Type',IntToStr(DatabaseType));
		WriteString('Database','Common_Host', CommonHost);
		WriteString('Database','Common_Port', IntToStr(CommonPort));
		WriteString('Database','Common_Database', CommonDB);
		WriteString('Database','Common_Username', CommonUser);
		WriteString('Database','Common_Password', CommonPass);

		WriteString('Database','Game_Host', GameHost);
		WriteString('Database','Game_Port', IntToStr(GamePort));
		WriteString('Database','Game_Database', GameDB);
		WriteString('Database','Game_Username', GameUser);
		WriteString('Database','Game_Password', GamePass);

		UpdateFile;
	end;{TServerOptions.Save}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetLoginPort()                                       PROCEDURE
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
	procedure TServerOptions.SetLoginPort(Value : word);
	begin
		if fLoginPort <> Value then
		begin
			fLoginPort := Value;
			WriteString('Login', 'Port', IntToStr(LoginPort));
		end;
	end;{TServerOptions.SetLoginPort}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetCharaWANIP()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetCharaWANIP(Value : String);
	begin
		if fCharaWANIP <> Value then
		begin
			fCharaWANIP := Value;
			WriteString('Chara', 'WANIP', CharaWANIP);
		end;
	end;{TServerOptions.SetCharaWANIP}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetCharaLANIP()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetCharaLANIP(Value : String);
	begin
		if fCharaWANIP <> Value then
		begin
			fCharaWANIP := Value;
			WriteString('Chara', 'LANIP', CharaLANIP);
		end;
	end;{TServerOptions.SetCharaLANIP}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetZoneWANIP()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetZoneWANIP(Value : String);
	begin
		if fZoneWANIP <> Value then
		begin
			fZoneWANIP := Value;
			WriteString('Zone', 'WANIP', ZoneWANIP);
		end;
	end;{TServerOptions.SetZoneWANIP}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetZoneLANIP()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetZoneLANIP(Value : String);
	begin
		if fZoneWANIP <> Value then
		begin
			fZoneWANIP := Value;
			WriteString('Zone', 'LANIP', ZoneLANIP);
		end;
	end;{TServerOptions.SetLoginLANIP}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetInterWANIP()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetInterWANIP(Value : String);
	begin
		if fInterWANIP <> Value then
		begin
			fInterWANIP := Value;
			WriteString('Inter', 'WANIP', InterWANIP);
		end;
	end;{TServerOptions.SetInterWANIP}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetInterLANIP()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetInterLANIP(Value : String);
	begin
		if fInterWANIP <> Value then
		begin
			fInterWANIP := Value;
			WriteString('Inter', 'LANIP', InterLANIP);
		end;
	end;{TServerOptions.SetInterLANIP}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetLoginComPort()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetLoginComPort(Value : Word);
	begin
		if fLoginComPort <> Value then
		begin
			fLoginComPort := Value;
			WriteString('Chara', 'LoginPort', IntToStr(LoginComPort));
		end;
	end;{TServerOptions.SetLoginComPort}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetLoginComIP()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetLoginComIP(Value : String);
	begin
		if fLoginComIP <> Value then
		begin
			fLoginComIP := Value;
			WriteString('Chara', 'LoginIP', LoginComIP);
		end;
	end;{TServerOptions.SetLoginComIP}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetCharaComPort()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetCharaComPort(Value : Word);
	begin
		if fCharaComPort <> Value then
		begin
			fCharaComPort := Value;
			WriteString('Zone', 'CharaPort', IntToStr(CharaComPort));
		end;
	end;{TServerOptions.SetCharaComPort}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetCharaComIP()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetCharaComIP(Value : String);
	begin
		if fCharaComIP <> Value then
		begin
			fCharaComIP := Value;
			WriteString('Zone', 'CharaIP', CharaComIP);
		end;
	end;{TServerOptions.SetCharaComIP}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetInterComPort()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetInterComPort(Value : Word);
	begin
		if fInterComPort <> Value then
		begin
			fInterComPort := Value;
			WriteString('Zone', 'InterPort', IntToStr(InterComPort));
		end;
	end;{TServerOptions.SetInterComPort}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetInterComIP()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetInterComIP(Value : String);
	begin
		if fInterComIP <> Value then
		begin
			fInterComIP := Value;
			WriteString('Zone', 'InterIP', InterComIP);
		end;
	end;{TServerOptions.SetInterComIP}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetCharaPort()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Property Set Routine for CharaPort. Ensures that if the Chara port is
//    changed for whatever reason, that it gets written to the .ini immediately.
//    The same is true for all communication variables.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetCharaPort(Value : word);
	begin
		if fCharaPort <> value then
		begin
			fCharaPort := value;
			WriteString('Chara', 'Port', IntToStr(CharaPort));
		end;
	end;{TServerOptions.SetCharaPort}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetZonePort()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Property Set Routine for ZonePort. Ensures that if the zone port is
//    changed for whatever reason, that it gets written to the .ini immediately.
//    The same is true for all communication variables.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetZonePort(Value : word);
	begin
		if fZonePort <> value then
		begin
			fZonePort := value;
			WriteString('Zone', 'Port', IntToStr(ZonePort));
		end;
	end;{TServerOptions.SetZonePort}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetInterPort()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Property Set Routine for InterPort. Ensures that if the inter port is
//    changed for whatever reason, that it gets written to the .ini immediately.
//    The same is true for all communication variables.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetInterPort(Value : word);
	begin
		if fInterPort <> value then
		begin
			fInterPort := value;
			WriteString('Inter', 'Port', IntToStr(InterPort));
		end;
	end;{TServerOptions.SetInterPort}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetEnableMF()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Property Set Routine for EnableMF. Ensures that if _M/_F registration is
//    changed for whatever reason, that it gets written to the .ini immediately.
//    The same is true for all communication variables.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetEnableMF(Value : boolean);
	begin
		if fEnableMF <> value then
		begin
			fEnableMF := value;
			WriteString('Login', 'EnableMF', BoolToStr(EnableMF));
		end;
	end;{TServerOptions.SetEnableMF}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetServerName()                                      PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Property Set Routine for ServerName. Ensures that if the server name is
//    changed for whatever reason, that it gets written to the .ini immediately.
//    The same is true for all communication variables.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetServerName(Value : String);
	begin
		if fServerName <> Value then
		begin
			fServerName := Value;
			WriteString('Chara', 'ServerName', ServerName);
		end;
	end;{TServerOptions.SetServerName}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetDatabaseType()                                    PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Property Set Routine for DatabaseType.Ensures that if the server name is
//    changed for whatever reason, that it gets written to the .ini immediately.
//    The same is true for all communication variables.
//
//	Changes -
//		September 29th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetDatabaseType(Value : Integer);
	begin
		if fDatabaseType <> Value then
		begin
			fDatabaseType := Value;
			WriteString('Database', 'DatabaseType', IntToStr(DatabaseType));
		end;
	end;{TServerOptions.SetDatabaseType}
//------------------------------------------------------------------------------


end{ServerOptions}.
