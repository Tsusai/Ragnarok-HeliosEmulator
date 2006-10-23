//------------------------------------------------------------------------------
//ServerOptions	                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This unit is used to gain access to configuration variables loaded from
//    ServerOptions.ini.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit ServerOptions;

interface
	uses
		IniFiles;

	type
		{Tsusai [05/06/06]: No existing ini file needed nor pre creation to read
		writing to protected variables to set. I'm all for the prometheus way,
		save after multiple changes.  the properties update the ini file in memory,
		just need to do a update file afterwards to "set in stone"}

    //RaX - Agreed, that's how it's supposed to work. 5 months ago I was just
    //  crazy...=) Remove these comments whenever.

TSaveLoopConfig = record
  Enabled     : Boolean;
  Interval    : Int64;
end;
//------------------------------------------------------------------------------
//TServerOptions	                                                        CLASS
//------------------------------------------------------------------------------
		TServerOptions = class(TMemIniFile)
		private
			fLoginPort : Word;
			fCharaPort : Word;
			fZonePort  : Word;

			fWAN_IP    : string;
			fLAN_IP    : string;

			fEnableMF  : Boolean;

			fServerName : String;

			fDatabaseType : Integer;

			fMySQLHost : string;
			fMySQLPort : integer;
			fMySQLDB   : string;
			fMySQLUser : string;
			fMySQLPass : string;

			procedure SetLoginPort(Value : word);
			procedure SetCharaPort(Value : word);
			procedure SetZonePort(Value : word);

			procedure SetLAN_IP(Value : string);
			procedure SetWAN_IP(Value : string);

			procedure SetEnableMF(Value : boolean);

			procedure SetServerName(Value : String);

      procedure SetDatabaseType(Value : Integer);

		public
      SaveLoop  : TSaveLoopConfig;
			//Communication
			property LoginPort : Word read fLoginPort write SetLoginPort;
			property CharaPort : Word read fCharaPort write SetCharaPort;
			property ZonePort  : Word read fZonePort  write SetZonePort;

			property LAN_IP : string read fLAN_IP write SetLAN_IP;
			property WAN_IP : string read fWAN_IP write SetWAN_IP;
			//LoginOptions
			property EnableMF : boolean read fEnableMF write SetEnableMF;

			//CharaOptions
			property ServerName : String read fServerName write SetServerName;

      property DatabaseType : Integer read fDatabaseType write SetDatabaseType;

			//MySQL - Best to turn off the server BEFORE editing this stuff anywho.
			property MySQLHost : string Read fMySQLHost;
			property MySQLPort : integer read fMySQLPort;
			property MySQLDB   : string Read fMySQLDB;
			property MySQLUser : string Read fMySQLUser;
			property MySQLPass : string Read fMySQLPass;

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

			fLoginPort   := StrToIntDef(Section.Values['LoginPort'], 6900);
			fCharaPort   := StrToIntDef(Section.Values['CharaPort'], 6121);
			fZonePort    := StrToIntDef(Section.Values['ZonePort'], 5121);

      fDatabaseType:= StrToIntDef(Section.Values['DatabaseType'], 1);

		end;{Subroutine LoadCommunication}
    //--------------------------------------------------------------------------

    //--------------------------------------------------------------------------
    //LoadLoginOptions                                      SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadLoginOptions;
		begin
			ReadSectionValues('LoginOptions', Section);
			fEnableMF    := StrToBoolDef(Section.Values['EnableMF'] ,false);
		end;{Subroutine LoadLoginOptions}
    //--------------------------------------------------------------------------

    //--------------------------------------------------------------------------
    //LoadCharaOptions                                      SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadCharaOptions;
		begin
			ReadSectionValues('CharaOptions', Section);

			if Section.Values['ServerName'] = '' then begin
				Section.Values['ServerName'] := 'Helios';
			end;
			
			fServerName    := Section.Values['ServerName'];
		end;{Subroutine LoadCharaOptions}
    //--------------------------------------------------------------------------

    //--------------------------------------------------------------------------
    //LoadMySQL                                      SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadMySQL;
		begin
			ReadSectionValues('MySQL', Section);

			if Section.Values['Host'] = '' then begin
				Section.Values['Host'] := '127.0.0.1';
			end;
			fMySQLHost := Section.Values['Host'];
			fMySQLPort := StrToIntDef(Section.Values['Port'], 3306);
			if Section.Values['Database'] = '' then begin
				Section.Values['Database'] := 'Helios';
			end;
			fMySQLDB := Section.Values['Database'];
			if Section.Values['Username'] = '' then begin
				Section.Values['Username'] := 'root';
			end;
			fMySQLUser := Section.Values['Username'];
			fMySQLPass := Section.Values['Password'];
		end;{Subroutine LoadMySQL}
    //--------------------------------------------------------------------------

    //--------------------------------------------------------------------------
    //LoadMisc                                      SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadMisc;
		begin
			ReadSectionValues('Misc', Section);

			SaveLoop.Enabled  := StrToBoolDef(Section.Values['SaveLoop-Enabled'], TRUE);
      SaveLoop.Interval := StrToInt64Def(Section.Values['SaveLoop-Interval'], 60000);
		end;{Subroutine LoadMySQL}
    //--------------------------------------------------------------------------
	begin
		Section    := TStringList.Create;

		Section.QuoteChar := '"';
		Section.Delimiter := ',';

		LoadCommunication;
		LoadLoginOptions;
		LoadCharaOptions;
		LoadMySQL;
    LoadMisc;

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
		WriteString('Communication', 'LoginPort',   IntToStr(LoginPort));
		WriteString('Communication', 'CharaPort',   IntToStr(CharaPort));
		WriteString('Communication', 'ZonePort',    IntToStr(ZonePort));
		WriteString('Communication', 'WAN_IP',      WAN_IP);
		WriteString('Communication', 'LAN_IP',      LAN_IP);
    WriteString('Communication', 'DatabaseType',IntToStr(DatabaseType));

		WriteString('LoginOptions','EnableMF',BoolToStr(EnableMF));

		WriteString('CharaOptions','ServerName',ServerName);

		WriteString('MySQL','Host', MySQLHost);
		WriteString('MySQL','Port', IntToStr(MySQLPort));
		WriteString('MySQL','Database', MySQLDB);
		WriteString('MySQL','Username', MySQLUser);
		WriteString('MySQL','Password', MySQLPass);

    WriteString('Misc', 'SaveLoop-Enabled', BoolToStr(SaveLoop.Enabled));
    WriteString('Misc', 'SaveLoop-Interval', IntToStr(SaveLoop.Interval));

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
