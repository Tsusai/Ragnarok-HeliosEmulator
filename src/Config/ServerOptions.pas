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
			fLoginPort : Word;
			fCharaPort : Word;
			fZonePort  : Word;
			fInterPort : Word;

			fWAN_IP    : string;
			fLAN_IP    : string;

			fLoginEnabled : boolean;
			fCharaEnabled : boolean;
			fZoneEnabled : boolean;
			fInterEnabled : boolean;

			fEnableMF  : Boolean;

			fServerName : String;

			fDatabaseType : Integer;

			fCommonHost : string;
			fCommonPort : integer;
			fCommonDB   : string;
			fCommonUser : string;
			fCommonPass : string;

			fGameHost : string;
			fGamePort : integer;
			fGameDB   : string;
			fGameUser : string;
			fGamePass : string;

			procedure SetLoginPort(Value : word);
			procedure SetCharaPort(Value : word);
			procedure SetZonePort(Value : word);
			procedure SetInterPort(Value : Word);
			procedure SetLAN_IP(Value : string);
			procedure SetWAN_IP(Value : string);

			procedure SetEnableMF(Value : boolean);

			procedure SetServerName(Value : String);

      procedure SetDatabaseType(Value : Integer);

		public
			//Communication
			property LAN_IP : string read fLAN_IP write SetLAN_IP;
			property WAN_IP : string read fWAN_IP write SetWAN_IP;
			property DatabaseType : Integer read fDatabaseType write SetDatabaseType;

			//LoginOptions
			property EnableMF : boolean read fEnableMF write SetEnableMF;
			property LoginPort : Word read fLoginPort write SetLoginPort;
			property LoginEnabled : boolean read fLoginEnabled;

			//CharaOptions
			property ServerName : String read fServerName write SetServerName;
			property CharaPort : Word read fCharaPort write SetCharaPort;
			property CharaEnabled : boolean read fCharaEnabled;

			//ZoneOptions
			property ZonePort  : Word read fZonePort  write SetZonePort;
			property ZoneEnabled : boolean read fZoneEnabled;

			//InterOptions
			property InterPort  : Word read fInterPort  write SetInterPort;
			property InterEnabled : boolean read fInterEnabled;

			//MySQL - Best to turn off the server BEFORE editing this stuff anywho.
			//Common Information Database
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
    //LoadCommunication                                      SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadCommunication;
		begin
			ReadSectionValues('Communication', Section);

			fWAN_IP      := Section.Values['WAN_IP'];

			if Section.Values['LAN_IP'] = '' then fLAN_IP := '127.0.0.1'
			else fLAN_IP := Section.Values['LAN_IP'];

			fZonePort    := StrToIntDef(Section.Values['ZonePort'], 5121);

			fDatabaseType:= StrToIntDef(Section.Values['DatabaseType'], 1);

		end;{Subroutine LoadCommunication}
    //--------------------------------------------------------------------------

    //--------------------------------------------------------------------------
    //LoadLoginOptions                                      SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadLoginOptions;
		begin
			ReadSectionValues('LoginServer', Section);
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
			ReadSectionValues('CharacterServer', Section);

			if Section.Values['ServerName'] = '' then begin
				Section.Values['ServerName'] := 'Helios';
			end;
			fCharaEnabled  := StrToBoolDef(Section.Values['Enabled'], TRUE);
			fServerName    := Section.Values['ServerName'];
			fCharaPort     := StrToIntDef(Section.Values['Port'], 6121);
		end;{Subroutine LoadCharaOptions}
		//--------------------------------------------------------------------------

		//--------------------------------------------------------------------------
		//LoadZoneOptions                                      SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadZoneOptions;
		begin
			ReadSectionValues('ZoneServer', Section);

			fZoneEnabled  := StrToBoolDef(Section.Values['Enabled'], TRUE);
			fZonePort     := StrToIntDef(Section.Values['Port'], 5121);
		end;{Subroutine LoadZoneOptions}
		//--------------------------------------------------------------------------

		//--------------------------------------------------------------------------
		//LoadInterOptions                                      SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadInterOptions;
		begin
			ReadSectionValues('InterServer', Section);

			fInterEnabled  := StrToBoolDef(Section.Values['Enabled'], TRUE);
			fInterPort     := StrToIntDef(Section.Values['Port'], 4000);
		end;{Subroutine LoadInterOptions}
		//--------------------------------------------------------------------------

		//--------------------------------------------------------------------------
    //LoadMySQL                                      SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadMySQL;
		begin
			ReadSectionValues('Database', Section);

			//Accounts
			if Section.Values['Common_Host'] = '' then begin
				Section.Values['Common_Host'] := '127.0.0.1';
			end;
			fCommonHost := Section.Values['Common_Host'];
			fCommonPort := StrToIntDef(Section.Values['Common_Port'], 3306);
			if Section.Values['Common_Database'] = '' then begin
				Section.Values['Common_Database'] := 'HeliosCommon';
			end;
			fCommonDB := Section.Values['Common_Database'];
			if Section.Values['Common_Username'] = '' then begin
				Section.Values['Common_Username'] := 'root';
			end;
			fCommonUser := Section.Values['Common_Username'];
			fCommonPass := Section.Values['Common_Password'];

			//Gameserver stuff
			if Section.Values['Game_Host'] = '' then begin
				Section.Values['Game_Host'] := '127.0.0.1';
			end;
			fGameHost := Section.Values['Game_Host'];
			fGamePort := StrToIntDef(Section.Values['Game_Port'], 3306);
			if Section.Values['Game_Database'] = '' then begin
				Section.Values['Game_Database'] := 'HeliosGame';
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

		LoadCommunication;
		LoadLoginOptions;
		LoadCharaOptions;
		LoadZoneOptions;
		LoadInterOptions;
		LoadMySQL;
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


		WriteString('Communication', 'ZonePort',    IntToStr(ZonePort));
		WriteString('Communication', 'WAN_IP',      WAN_IP);
		WriteString('Communication', 'LAN_IP',      LAN_IP);
    WriteString('Communication', 'DatabaseType',IntToStr(DatabaseType));

		WriteString('LoginOptions','EnableMF',BoolToStr(EnableMF));
		WriteString('LoginOptions','Port',IntToStr(LoginPort));
		WriteString('LoginOptions','Enabled',BoolToStr(LoginEnabled));

		WriteString('CharaOptions','ServerName',ServerName);
		WriteString('CharaOptions','Port', IntToStr(CharaPort));
		WriteString('CharaOptions','Enabled',BoolToStr(CharaEnabled));

		WriteString('ZoneOptions','Port', IntToStr(ZonePort));
		WriteString('ZoneOptions','Enabled',BoolToStr(ZoneEnabled));

		WriteString('InterOptions','Port', IntToStr(InterPort));
		WriteString('InterOptions','Enabled',BoolToStr(InterEnabled));

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
		if fLoginPort <> value then
		begin
			fLoginPort := value;
			WriteString('Communication', 'LoginPort', IntToStr(LoginPort));
		end;
	end;{TServerOptions.SetLoginPort}
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
			WriteString('Communication', 'CharaPort', IntToStr(CharaPort));
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
			WriteString('Communication', 'ZonePort', IntToStr(ZonePort));
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
			WriteString('Communication', 'InterPort', IntToStr(InterPort));
		end;
	end;{TServerOptions.SetInterPort}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetLAN_IP()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Property Set Routine for LAN_IP. Ensures that if the LAN IP is
//    changed for whatever reason, that it gets written to the .ini immediately.
//    The same is true for all communication variables.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetLAN_IP(Value : string);
	begin
		if fLAN_IP <> value then
		begin
			fLAN_IP := value;
			WriteString('Communication', 'LAN_IP', LAN_IP);
		end;
	end;{TServerOptions.SetLAN_IP}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TServerOptions.SetWAN_IP()                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Property Set Routine for WAN_IP. Ensures that if the wan ip is
//    changed for whatever reason, that it gets written to the .ini immediately.
//    The same is true for all communication variables.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TServerOptions.SetWAN_IP(Value : string);
	begin
		if fWAN_IP <> value then
		begin
			fWAN_IP := value;
			WriteString('Communication', 'WAN_IP', WAN_IP);
		end;
	end;{TServerOptions.SetWAN_IP}
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
			WriteString('LoginOptions', 'EnableMF', BoolToStr(EnableMF));
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
			WriteString('CharaOptions', 'ServerName', ServerName);
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
			WriteString('Communication', 'DataaseType', IntToStr(DatabaseType));
		end;
	end;{TServerOptions.SetDatabaseType}
//------------------------------------------------------------------------------


end{ServerOptions}.
