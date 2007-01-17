//------------------------------------------------------------------------------
//CharaOptions	                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This unit is used to gain access to configuration variables loaded from
//    Character.ini.
//
//	Changes -
//		January 7th, 2007 - RaX - Broken out from ServerOptions.
//
//------------------------------------------------------------------------------
unit CharaOptions;

interface
	uses
		IniFiles,
    Types;

	type

//------------------------------------------------------------------------------
//TCharaOptions	                                                        CLASS
//------------------------------------------------------------------------------
		TCharaOptions = class(TMemIniFile)
		private
//private variables
			fPort		      : Word;
			fWANIP		    : String;
			fLANIP		    : String;
      fKey          : String;

			fLoginIP	    : String;
			fLoginPort    : Word;
      fLoginKey     : String;

			fServerName		: String;

      fDefaultZeny  : Integer;
      fDefaultMap   : String;
      fDefaultPoint : TPoint;
      fDefaultHeadTop  : Integer;
      fDefaultHeadMid : Integer;
      fDefaultHeadLow : Integer;
      fDefaultArmor : Integer;
      fDefaultRightHand: Integer;
      fDefaultLeftHand  : Integer;
      fDefaultShoes : Integer;
      fDefaultGarment : Integer;
      fDefaultAccessory1 : Integer;
      fDefaultAccessory2 : Integer;

//Gets/Sets
			procedure SetPort(Value : Word);
			procedure SetWANIP(Value : String);
			procedure SetLANIP(Value : String);
			procedure SetLoginIP(Value : String);
			procedure SetLoginPort(Value : Word);
			procedure SetServerName(Value : String);

		public

      //Communication
			property Port : Word read fPort write SetPort;
			property WANIP : String read fWANIP write SetWANIP;
			property LANIP : String read fLANIP write SetLANIP;

			property LoginIP : String read fLoginIP write SetLoginIP;
			property LoginPort : Word read fLoginPort write SetLoginPort;
      property LoginKey  : String read fLoginKey;

      //Security
      property Key : String read fKey;

      //Options
			property ServerName : String read fServerName write SetServerName;


      property DefaultZeny: Integer read fDefaultZeny write fDefaultZeny;
      property DefaultMap : String read fDefaultMap write fDefaultMap;
      property DefaultPoint : TPoint read fDefaultPoint write fDefaultPoint;
      property DefaultHeadTop: Integer read fDefaultHeadTop write fDefaultHeadTop;
      property DefaultHeadMid: Integer read fDefaultHeadMid write fDefaultHeadMid;
      property DefaultHeadLow: Integer read fDefaultHeadLow write fDefaultHeadLow;
      property DefaultRightHand: Integer read fDefaultRightHand write fDefaultRightHand;
      property DefaultLeftHand: Integer read fDefaultLeftHand write fDefaultLeftHand;
      property DefaultArmor: Integer read fDefaultArmor write fDefaultArmor;
      property DefaultGarment: Integer read fDefaultGarment write fDefaultGarment;
      property DefaultShoes: Integer read fDefaultShoes write fDefaultShoes;
      property DefaultAccessory1: Integer read fDefaultAccessory1 write fDefaultAccessory1;
      property DefaultAccessory2: Integer read fDefaultAccessory2 write fDefaultAccessory2;

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
	procedure TCharaOptions.Load;
	var
		Section    : TStringList;
    //--------------------------------------------------------------------------
    //LoadCommunication                                          SUB PROCEDURE
    //--------------------------------------------------------------------------
		procedure LoadCommunication;
		begin
			ReadSectionValues('Communication', Section);

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
      fPort			:= StrToIntDef(Section.Values['Port'], 6121);

			if Section.Values['LoginIP'] = '' then
			begin
				Section.Values['LoginIP']			:= '127.0.0.1';
			end;
			fLoginIP			:= Section.Values['LoginIP'];
			fLoginPort		:= StrToIntDef(Section.Values['LoginPort'], 6900);
      fLoginKey    := Section.Values['LoginKey'];
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
			if Section.Values['ServerName'] = '' then begin
				Section.Values['ServerName'] := 'Helios';
			end;
      fServerName			:= Section.Values['ServerName'];
		end;{Subroutine LoadOptions}
    //--------------------------------------------------------------------------


    //--------------------------------------------------------------------------
    //LoadCharacterDefaults                                       SUB PROCEDURE
    //--------------------------------------------------------------------------
    procedure LoadCharacterDefaults;
    begin
      ReadSectionValues('CharacterDefaults', Section);
      DefaultZeny       := StrToIntDef(Section.Values['Zeny'], 0);
      if Section.Values['Map'] = '' then begin
				Section.Values['Map'] := 'new_1-1';
			end;
      DefaultMap        := Section.Values['Map'];
      fDefaultPoint.X   := StrToIntDef(Section.Values['Point.X'], 53);
      fDefaultPoint.Y   := StrToIntDef(Section.Values['Point.Y'], 111);
      DefaultHeadTop    := StrToIntDef(Section.Values['HeadTop'], 0);
      DefaultHeadMid    := StrToIntDef(Section.Values['HeadMid'], 0);
      DefaultHeadLow    := StrToIntDef(Section.Values['HeadLow'], 0);
      DefaultRightHand  := StrToIntDef(Section.Values['RightHand'], 1201);
      DefaultLeftHand   := StrToIntDef(Section.Values['LeftHand'], 0);
      DefaultArmor      := StrToIntDef(Section.Values['Armor'], 2301);
      DefaultShoes      := StrToIntDef(Section.Values['Shoes'], 0);
      DefaultGarment    := StrToIntDef(Section.Values['Garment'], 0);
      DefaultAccessory1 := StrToIntDef(Section.Values['Accessory1'], 0);
      DefaultAccessory2 := StrToIntDef(Section.Values['Accessory2'], 0);
    end;{LoadCharacterDefaults}
    //--------------------------------------------------------------------------

	begin
		Section    := TStringList.Create;

		Section.QuoteChar := '"';
		Section.Delimiter := ',';
    
    LoadCommunication;
    LoadSecurity;
    LoadOptions;
    LoadCharacterDefaults;

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
	procedure TCharaOptions.Save;
	begin
    //Communication
		WriteString('Communication','WANIP',WANIP);
		WriteString('Communication','LANIP',LANIP);
		WriteString('Communication','Port', IntToStr(Port));
		WriteString('Communication','LoginIP',LoginIP);
		WriteString('Communication','LoginPort',IntToStr(LoginPort));
    WriteString('Communication','LoginKey',LoginKey);

    //Security
    WriteString('Security','Key',Key);

    //Options
		WriteString('Options','ServerName',ServerName);

    //CharacterDefaults
    WriteString('CharacterDefaults','Zeny',IntToStr(DefaultZeny));
    WriteString('CharacterDefaults','Map',DefaultMap);
    WriteString('CharacterDefaults','Point.X',IntToStr(fDefaultPoint.X));
    WriteString('CharacterDefaults','Point.Y',IntToStr(DefaultPoint.Y));
    WriteString('CharacterDefaults','HeadTop',IntToStr(DefaultHeadTop));
    WriteString('CharacterDefaults','HeadMid',IntToStr(DefaultHeadMid));
    WriteString('CharacterDefaults','HeadLow',IntToStr(DefaultHeadLow));
    WriteString('CharacterDefaults','RightHand',IntToStr(DefaultRightHand));
    WriteString('CharacterDefaults','LeftHand',IntToStr(DefaultLeftHand));
    WriteString('CharacterDefaults','Armor',IntToStr(DefaultArmor));
    WriteString('CharacterDefaults','Shoes',IntToStr(DefaultShoes));
    WriteString('CharacterDefaults','Garment',IntToStr(DefaultGarment));
    WriteString('CharacterDefaults','Accessory1',IntToStr(DefaultAccessory1));
    WriteString('CharacterDefaults','Accessory2',IntToStr(DefaultAccessory2));

		UpdateFile;
	end;{Save}
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
	procedure TCharaOptions.SetWANIP(Value : String);
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
	procedure TCharaOptions.SetLANIP(Value : String);
	begin
		if fWANIP <> Value then
		begin
			fWANIP := Value;
			WriteString('Communication', 'LANIP', LANIP);
		end;
	end;{SetLANIP}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetServerName()                                                    PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Property Set Routine for ServerName. Ensures that if the server name is
//    changed for whatever reason, that it gets written to the .ini immediately.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TCharaOptions.SetServerName(Value : String);
	begin
		if fServerName <> Value then
		begin
			fServerName := Value;
			WriteString('Options', 'ServerName', ServerName);
		end;
	end;{SetServerName}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetPort()                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Property Set Routine for Port. Ensures that if the  port is changed for
//    whatever reason, that it gets written to the .ini immediately.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TCharaOptions.SetPort(Value : word);
	begin
		if fPort <> value then
		begin
			fPort := value;
			WriteString('Communication', 'Port', IntToStr(Port));
		end;
	end;{SetPort}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetLoginPort()                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
	procedure TCharaOptions.SetLoginPort(Value : Word);
	begin
		if fLoginPort <> Value then
		begin
			fLoginPort := Value;
			WriteString('Communication', 'LoginPort', IntToStr(LoginPort));
		end;
	end;{SetLoginPort}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//SetLoginIP()                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
	procedure TCharaOptions.SetLoginIP(Value : String);
	begin
		if fLoginIP <> Value then
		begin
			fLoginIP := Value;
			WriteString('Communication', 'LoginIP', LoginIP);
		end;
	end;{SetLoginIP}
//------------------------------------------------------------------------------
end{CharaOptions}.
