//------------------------------------------------------------------------------
//ZoneOptions                                                               UNIT
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

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
	uses
		IniFiles;

	type

//------------------------------------------------------------------------------
//TZoneOptions                                                             CLASS
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

			fZoneTick			: Word;//The amount of time in milliseconds to sleep
															// between packet processes.
			fEventTick		: Word;//The amount of time in milliseconds to sleep
															// between event processes.
			fCharClickArea: Word;//The number of cells away from a character that
															//They can click to move to.
			fCharShowArea	: Word;//The distance in cells away from a character that
															//other entities appear in.

			fIndySchedulerType  : Byte;
			fIndyThreadPoolSize : Word;

			fBaseXPMultiplier: LongWord;
			fJobXPMultiplier: LongWord;

			fMaxBaseLevel : Word;
			fMaxJobLevel  : Word;
			fMaxBaseLevelsPerEXPGain : Word;
			fMaxJobLevelsPerEXPGain : Word;

			fFullHPOnLevelUP : Boolean;
			fFullSPOnLevelUP : Boolean;

			fKickOnShutdown : Boolean;

			fMaxStats : Integer;

			fMaxStackItem : Word;
			fMaxItems : Word;
			fGroundItemTimeout : LongWord;

			fDynamicMapLoading : Boolean;

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
			property ZoneTick			: Word read fZoneTick;
			property EventTick		: Word read fEventTick;
			property CharClickArea: Word read fCharClickArea;
			property CharShowArea	: Word read fCharShowArea;
			property IndySchedulerType : Byte read fIndySchedulerType;
			property IndyThreadPoolSize : Word read fIndyThreadPoolSize;

			//Game
			property KickOnShutdown : Boolean read fKickOnShutdown;

			property BaseXPMultiplier: LongWord read fBaseXPMultiplier;
			property JobXPMultiplier: LongWord read fJobXPMultiplier;

			property MaxBaseLevel : Word read fMaxBaseLevel;
			property MaxJobLevel  : Word read fMaxJobLevel;
			property MaxBaseLevelsPerEXPGain : Word read fMaxBaseLevelsPerEXPGain;
			property MaxJobLevelsPerEXPGain : Word read fMaxJobLevelsPerEXPGain;

			property FullHPOnLevelUp : Boolean read fFullHPOnLevelUp;
			property FullSPOnLevelUp : Boolean read fFullSPOnLevelUp;

			property MaxCharacterStats : Integer read fMaxStats;

			property MaxStackItem : Word read fMaxStackItem;
			property MaxItems : Word read fMaxItems;
			property GroundItemTimeout : LongWord read fGroundItemTimeout;

			property DynamicMapLoading : Boolean read fDynamicMapLoading;

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
//Load                                                                 PROCEDURE
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
			fID := EnsureRange(StrToIntDef(Section.Values['ID'] ,1), Low(LongWord), High(LongWord));
		end;{Subroutine LoadServer}
		//--------------------------------------------------------------------------


		//--------------------------------------------------------------------------
		//LoadCommunication                                          SUB PROCEDURE
		//--------------------------------------------------------------------------
		procedure LoadCommunication;
		begin
			ReadSectionValues('Communication', Section);
			fPort     := EnsureRange(StrToIntDef(Section.Values['Port'], 5121), 1, MAX_PORT);

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

			fCharaPort		:= EnsureRange(StrToIntDef(Section.Values['CharaPort'], 6121), 1, MAX_PORT);
			fCharaKey    := Section.Values['CharaKey'];

			if Section.Values['InterIP'] = '' then
			begin
				Section.Values['InterIP']			:= '127.0.0.1';
			end;
			fInterIP			:= Section.Values['InterIP'];

			fInterPort		:= EnsureRange(StrToIntDef(Section.Values['InterPort'], 4000), 1, MAX_PORT);
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
	//LoadPerformance                                              SUB PROCEDURE
	//--------------------------------------------------------------------------
		procedure LoadPerformance;
		begin
			ReadSectionValues('Performance', Section);
			fZoneTick			:= EnsureRange(StrToIntDef(Section.Values['Zone Tick'], 10), Low(Word), High(Word));
			fEventTick		:= EnsureRange(StrToIntDef(Section.Values['Event Tick'], 10), Low(Word), High(Word));
			fCharClickArea:= EnsureRange(StrToIntDef(Section.Values['Click Area'], 16), Low(Word), High(Word));
			fCharShowArea	:= EnsureRange(StrToIntDef(Section.Values['Show Area'], 16), Low(Word), High(Word));
			fIndySchedulerType := EnsureRange(StrToIntDef(Section.Values['Indy Scheduler Type'], 0), 0, 1);
			fIndyThreadPoolSize := EnsureRange(StrToIntDef(Section.Values['Indy Thread Pool Size'], 1), 1, High(Word));
		end;{Subroutine LoadPerformance}
	//--------------------------------------------------------------------------


	//--------------------------------------------------------------------------
	//LoadGame                                              SUB PROCEDURE
	//--------------------------------------------------------------------------
		procedure LoadGame;
		begin
			ReadSectionValues('Game', Section);
			fKickOnShutdown := StrToBoolDef(Section.Values['Kick On Shutdown'] ,False);

			fBaseXPMultiplier  := Min(StrToIntDef(Section.Values['Base EXP Multiplier'], 1), High(LongWord));
			fJobXPMultiplier  := Min(StrToIntDef(Section.Values['Job EXP Multiplier'], 1), High(LongWord));

			fMaxBaseLevel   := Min(StrToIntDef(Section.Values['Max. Base Level'], 99), High(Word));
			fMaxJobLevel    := Min(StrToIntDef(Section.Values['Max. Job Level'], 99), High(Word));
			fMaxBaseLevelsPerEXPGain := Min(StrToIntDef(Section.Values['Max. Base Levels Allowed Per EXP Gain'], 1), High(Word));
			fMaxJobLevelsPerEXPGain := Min(StrToIntDef(Section.Values['Max. Job Levels Allowed Per EXP Gain'], 1), High(Word));
			fFullHPOnLevelUp := StrToBoolDef(Section.Values['Full HP on Level up?'], TRUE);
			fFullSPOnLevelUp := StrToBoolDef(Section.Values['Full SP on Level up?'], TRUE);
			fMaxStats := StrToIntDef(Section.Values['Max. Character Stats'], 99);
			fMaxStackItem := EnsureRange(StrToIntDef(Section.Values['MaxStackItem'], 30000),0,High(Word));
			fMaxItems := EnsureRange(StrToIntDef(Section.Values['MaxItems'], 100),0,High(Word));
			fGroundItemTimeout := EnsureRange(StrToIntDef(Section.Values['GroundItemTimeout'], 60),1,High(Word));
			fDynamicMapLoading := StrToBoolDef(Section.Values['Dynamic Map Loading'], true);
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
		LoadGame;
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
		WriteString('Performance','Indy Scheduler Type',IntToStr(IndySchedulerType));
		WriteString('Performance','Indy Thread Pool Size',IntToStr(IndyThreadPoolSize));


		//Game
		WriteString('Game','Kick On Shutdown',BoolToStr(fKickOnShutdown));
		WriteString('Game','Base EXP Multiplier',IntToStr(fBaseXPMultiplier));
		WriteString('Game','Job EXP Multiplier',IntToStr(fJobXPMultiplier));

		WriteString('Game','Max. Base Level',IntToStr(fMaxBaseLevel));
		WriteString('Game','Max. Job Level',IntToStr(fMaxJobLevel));
		WriteString('Game','Max. Base Levels Allowed Per EXP Gain',IntToStr(fMaxBaseLevelsPerEXPGain));
		WriteString('Game','Max. Job Levels Allowed Per EXP Gain',IntToStr(fMaxJobLevelsPerEXPGain));

		WriteString('Game','Full HP On Level Up?',BoolToStr(fFullHPOnLevelUp));
		WriteString('Game','Full SP On Level Up?',BoolToStr(fFullSPOnLevelUp));

		WriteString('Game','Max. Character Stats',IntToStr(fMaxStats));

		WriteString('Game','MaxStackItem',IntToStr(fMaxStackItem));
		WriteString('Game','MaxItems',IntToStr(fMaxItems));
		WriteString('Game','GroundItemTimeout', IntToStr(fGroundItemTimeout));

		WriteString('Game','Dynamic Map Loading',BoolToStr(fDynamicMapLoading));
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
