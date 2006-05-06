unit ServerOptions;

interface
	uses
		IniFiles;

	type
		{Tsusai [05/06/06]: No existing ini file needed nor pre creation to read
		writing to protected variables to set. I'm all for the prometheus way,
		save after multiple changes.  the properties update the ini file in memory,
		just need to do a update file afterwards to "set in stone"}
		TServerOptions = class(TMemIniFile)
		private
			fLoginPort : Word;
			fCharaPort : Word;
			fZonePort : Word;

			fWAN_IP : string;
			fLAN_IP : string;

			fEnableMF : Boolean;

			procedure SetLoginPort(Value : word);
			procedure SetCharaPort(Value : word);
			procedure SetZonePort(Value : word);

			procedure SetLAN_IP(Value : string);
			procedure SetWAN_IP(Value : string);

			procedure SetEnableMF(Value : boolean);

		public
			property LoginPort : Word read fLoginPort write SetLoginPort;
			property CharaPort : Word read fCharaPort write SetCharaPort;
			property ZonePort  : Word read fZonePort  write SetZonePort;

			property LAN_IP : string read fLAN_IP write SetLAN_IP;
			property WAN_IP : string read fWAN_IP write SetWAN_IP;

			property EnableMF : boolean read fEnableMF write SetEnableMF;

			procedure Load;
			procedure Save;
		end;

implementation
	uses
		Classes,
		SysUtils,
		Console;

	procedure TServerOptions.Load;
	var
		Section    : TStringList;

		procedure LoadCommunication;
		begin
			ReadSectionValues('Communication', Section);

			fWAN_IP       := Section.Values['WAN_IP'];

			if Section.Values['LAN_IP'] = '' then fLAN_IP := '127.0.0.1'
			else fLAN_IP := Section.Values['LAN_IP'];

			fLoginPort   := StrToIntDef(Section.Values['LoginPort'], 6900);
			fCharaPort   := StrToIntDef(Section.Values['CharaPort'], 6121);
			fZonePort    := StrToIntDef(Section.Values['ZonePort'], 5121);
		end;

		procedure LoadLoginOptions;
		begin
			ReadSectionValues('LoginOptions', Section);
			fEnableMF    := StrToBoolDef(Section.Values['EnableMF'] ,false);
		end;

	begin
		Section    := TStringList.Create;

		Section.QuoteChar := '"';
		Section.Delimiter := ',';

		LoadCommunication;
		LoadLoginOptions;

		Section.Free;

	end;

	procedure TServerOptions.Save;
	begin
		WriteString('Communication', 'LoginPort', IntToStr(LoginPort));
		WriteString('Communication', 'CharaPort', IntToStr(CharaPort));
		WriteString('Communication', 'ZonePort',  IntToStr(ZonePort));
		WriteString('Communication', 'WAN_IP',    WAN_IP);
		WriteString('Communication', 'LAN_IP',    LAN_IP);

		WriteString('LoginOptions','EnableMF',BoolToStr(EnableMF));
		UpdateFile;
	end;

	procedure TServerOptions.SetLoginPort(Value : word);
	begin
		if fLoginPort <> value then
		begin
			fLoginPort := value;
			WriteString('Communication', 'LoginPort', IntToStr(LoginPort));
		end;
	end;

	procedure TServerOptions.SetCharaPort(Value : word);
	begin
		if fCharaPort <> value then
		begin
			fCharaPort := value;
			WriteString('Communication', 'CharaPort', IntToStr(CharaPort));
		end;
	end;

	procedure TServerOptions.SetZonePort(Value : word);
	begin
		if fZonePort <> value then
		begin
			fZonePort := value;
			WriteString('Communication', 'ZonePort', IntToStr(ZonePort));
		end;
	end;

	procedure TServerOptions.SetLAN_IP(Value : string);
	begin
		if fLAN_IP <> value then
		begin
			fLAN_IP := value;
			WriteString('Communication', 'LAN_IP', LAN_IP);
		end;
	end;

	procedure TServerOptions.SetWAN_IP(Value : string);
	begin
		if fWAN_IP <> value then
		begin
			fWAN_IP := value;
			WriteString('Communication', 'WAN_IP', WAN_IP);
		end;
	end;

	procedure TServerOptions.SetEnableMF(Value : boolean);
	begin
		if fEnableMF <> value then
		begin
			fEnableMF := value;
			WriteString('LoginOptions', 'EnableMF', BoolToStr(EnableMF));
		end;
	end;

end.
