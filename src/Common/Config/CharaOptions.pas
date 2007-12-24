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
			fID		: LongWord;
			fPort		      : Word;
			fWANIP		    : String;
			fLANIP		    : String;
			fKey          : String;

			fLoginIP	    : String;
			fLoginPort    : Word;
			fLoginKey     : String;

			fServerName		: String;
			fUse108LengthForReply : boolean;
			fShowFriendlyMessageOnDupLogin : Boolean;

			fDefaultZeny  		: LongWord;
			fDefaultMap   		: String;
			fDefaultPoint 		: TPoint;
			fDefaultHeadTop 	: Word;
			fDefaultHeadMid 	: Word;
			fDefaultHeadLow 	: Word;
			fDefaultArmor			: Word;
			fDefaultRightHand	: Word;
			fDefaultLeftHand  : Word;
			fDefaultShoes 		: Word;
			fDefaultGarment 	: Word;
			fDefaultAccessory1: Word;
			fDefaultAccessory2: Word;

			fIndySchedulerType  : Byte;
			fIndyThreadPoolSize : Word;

//Gets/Sets
			procedure SetPort(Value : Word);
			procedure SetWANIP(Value : String);
			procedure SetLANIP(Value : String);
			procedure SetLoginIP(Value : String);
			procedure SetLoginPort(Value : Word);
			procedure SetServerName(Value : String);

		public

			//Communication
			property ID      : LongWord read fID;
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
			property Use108LengthForReply : boolean read fUse108LengthForReply write fUse108LengthForReply;
			property ShowFriendlyMessageOnDupLogin : Boolean read fShowFriendlyMessageOnDupLogin write fShowFriendlyMessageOnDupLogin;


			property DefaultZeny: LongWord read fDefaultZeny write fDefaultZeny;
			property DefaultMap : String read fDefaultMap write fDefaultMap;
			property DefaultPoint : TPoint read fDefaultPoint write fDefaultPoint;
			property DefaultHeadTop: Word read fDefaultHeadTop write fDefaultHeadTop;
			property DefaultHeadMid: Word read fDefaultHeadMid write fDefaultHeadMid;
			property DefaultHeadLow: Word read fDefaultHeadLow write fDefaultHeadLow;
			property DefaultRightHand: Word read fDefaultRightHand write fDefaultRightHand;
			property DefaultLeftHand: Word read fDefaultLeftHand write fDefaultLeftHand;
			property DefaultArmor: Word read fDefaultArmor write fDefaultArmor;
			property DefaultGarment: Word read fDefaultGarment write fDefaultGarment;
			property DefaultShoes: Word read fDefaultShoes write fDefaultShoes;
			property DefaultAccessory1: Word read fDefaultAccessory1 write fDefaultAccessory1;
			property DefaultAccessory2: Word read fDefaultAccessory2 write fDefaultAccessory2;

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
	procedure TCharaOptions.Load;
	var
		Section    : TStringList;
	//--------------------------------------------------------------------------
	//LoadServer                                               SUB PROCEDURE
	//--------------------------------------------------------------------------
		procedure LoadServer;
		begin
			ReadSectionValues('Server', Section);
			fID := EnsureRange(StrToIntDef(Section.Values['ID'] ,1), Low(LongWord), High(LongWord));
		end;{Subroutine LoadServer}
	//--------------------------------------------------------------------------

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
			fPort			:= EnsureRange(StrToIntDef(Section.Values['Port'], 6121), 1, MAX_PORT);

			if Section.Values['LoginIP'] = '' then
			begin
				Section.Values['LoginIP']			:= '127.0.0.1';
			end;
			fLoginIP			:= Section.Values['LoginIP'];
			fLoginPort		:= EnsureRange(StrToIntDef(Section.Values['LoginPort'], 6900), 1, MAX_PORT);
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

			(*Tsusai Mar 24th 2007
			This boolean helps decide to use either the original 106 byte length base
			for character information, or add on an extra word for 108 character data
			lengths.  A MIX MODE LIKE ZONE IS IMPOSSIBLE.  MUST BE ONE OR THE OTHER*)
			fUse108LengthForReply := StrToBoolDef(Section.Values['Support_Dec06_AndNewerClients'] ,false);
			{* Aeomin April 12th, 2007
			 If this Boolean set as false, char server will directly DC the client when
			 attempt duplicate login, else wil send "Someone has already logged in with this ID"*}
			ShowFriendlyMessageOnDupLogin := StrToBoolDef(Section.Values['Show_FriendlyMessage_On_DuplicateLogin'] ,false);
		end;{Subroutine LoadOptions}
		//--------------------------------------------------------------------------


		//--------------------------------------------------------------------------
		//LoadCharacterDefaults                                       SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadCharacterDefaults;
		begin
			ReadSectionValues('CharacterDefaults', Section);
			DefaultZeny       := EnsureRange(StrToIntDef(Section.Values['Zeny'], 0), Low(LongWord), High(LongWord));
			if Section.Values['Map'] = '' then begin
				Section.Values['Map'] := 'new_1-1';
			end;
			DefaultMap        := Section.Values['Map'];
			fDefaultPoint.X   := EnsureRange(StrToIntDef(Section.Values['Point.X'], 53), Low(Word), High(Word));
			fDefaultPoint.Y   := EnsureRange(StrToIntDef(Section.Values['Point.Y'], 111), Low(Word), High(Word));
			DefaultHeadTop    := EnsureRange(StrToIntDef(Section.Values['HeadTop'], 0), Low(Word), High(Word));
			DefaultHeadMid    := EnsureRange(StrToIntDef(Section.Values['HeadMid'], 0), Low(Word), High(Word));
			DefaultHeadLow    := EnsureRange(StrToIntDef(Section.Values['HeadLow'], 0), Low(Word), High(Word));
			DefaultRightHand  := EnsureRange(StrToIntDef(Section.Values['RightHand'], 1201), Low(Word), High(Word));
			DefaultLeftHand   := EnsureRange(StrToIntDef(Section.Values['LeftHand'], 0), Low(Word), High(Word));
			DefaultArmor      := EnsureRange(StrToIntDef(Section.Values['Armor'], 2301), Low(Word), High(Word));
			DefaultShoes      := EnsureRange(StrToIntDef(Section.Values['Shoes'], 0), Low(Word), High(Word));
			DefaultGarment    := EnsureRange(StrToIntDef(Section.Values['Garment'], 0), Low(Word), High(Word));
			DefaultAccessory1 := EnsureRange(StrToIntDef(Section.Values['Accessory1'], 0), Low(Word), High(Word));
			DefaultAccessory2 := EnsureRange(StrToIntDef(Section.Values['Accessory2'], 0), Low(Word), High(Word));
		end;{LoadCharacterDefaults}
		//--------------------------------------------------------------------------


		//--------------------------------------------------------------------------
		//LoadPerformance                                             SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadPerformance;
		begin
			ReadSectionValues('Performance', Section);
			fIndySchedulerType := EnsureRange(StrToIntDef(Section.Values['Indy Scheduler Type'], 0), 0, 1);
			fIndyThreadPoolSize := EnsureRange(StrToIntDef(Section.Values['Indy Thread Pool Size'], 1), 1, High(Word));
		end;{Subroutine LoadPerformance}
		//--------------------------------------------------------------------------

	begin
		Section    := TStringList.Create;

		Section.QuoteChar := '"';
		Section.Delimiter := ',';

		LoadServer;
		LoadCommunication;
		LoadSecurity;
		LoadOptions;
		LoadCharacterDefaults;
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
	procedure TCharaOptions.Save;
	begin
		//Server
		WriteString('Server','ID',IntToStr(ID));
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
		WriteString('Options','Support_Dec06_AndNewerClients',BoolToStr(fUse108LengthForReply));
		WriteString('Options','Show_FriendlyMessage_On_DuplicateLogin',BoolToStr(fShowFriendlyMessageOnDupLogin));

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

		//Performance
		WriteString('Performance','Indy Scheduler Type',IntToStr(IndySchedulerType));
		WriteString('Performance','Indy Thread Pool Size',IntToStr(IndyThreadPoolSize));

		UpdateFile;
	end;{Save}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetWANIP()                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Property Set Routine for WAN IP of Character Server
//	Changes -
//		November 29th, 2006 - RaX - Created.
//		March 13th, 2007 - Aeomin - Modify Header.
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
//SetLANIP()                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Property Set Routine for LAN IP of Character Server
//	Changes -
//		November 29th, 2006 - RaX - Created.
//		March 13th, 2007 - Aeomin - Modify Header.
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
//		Property Set Routine for Login Server Port.
//	Changes -
//		November 29th, 2006 - RaX - Created.
//		March 13th, 2007 - Aeomin - Modify Header.
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
//		Property Set Routine for Login Server IP.
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
