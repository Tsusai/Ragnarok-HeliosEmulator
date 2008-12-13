//------------------------------------------------------------------------------
//GMCommandExe                                                              UNIT
//------------------------------------------------------------------------------
//	What it does-
//      	Actual GM command execution code
//
//	Changes -
//		[2007/08/08] - Aeomin - Created.
//
//------------------------------------------------------------------------------
unit GMCommandExe;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
uses
	Classes,
	{Project}
	Character,
	Account
	;

	{WARNING: FromChar is there for reference, you are NOT suppose to treat as controlling real character}

	procedure GMZoneStatus(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMWarp(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMWarpDev(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMJump(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMCharMove(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMRecall(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);

	procedure GMGiveBaseExperience(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMGiveJobExperience(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMBaseLevelUp(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMJobLevelUp(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMAddStatusPoints(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMAddSkillPoints(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMGiveZeny(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMGiveStat(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMResetStats(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMSpeed(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMDie(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);

	procedure GMResetLook(const Arguments	:	array of String;const	FromChar:TCharacter;const	TargetChar:	TCharacter;const	Error	:	TStringList);

	procedure GMBroadCast(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMBroadCastNoName(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMBroadCastLocal(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMBroadCastLocalNoName(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMBroadCastLocalBlue(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);

	procedure GMBroadCastColor(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMBroadCastRed(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMBroadCastPurple(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMBroadCastGreen(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMBroadCastBlack(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMBroadCastBlue(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMBroadCastWhite(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);

	procedure GMKick(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMKickAll(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMBan(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);

	procedure GMEffect(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
	procedure GMWhere(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);

	procedure GMItem(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);

	procedure GMJob(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);

	procedure GMCreateInstance(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);

	procedure GMSpawn(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);

implementation
uses
	{RTL/VCL}
	SysUtils,
	Math,
	Types,
	{Project}
	Main,
	ZoneSend,
	PacketTypes,
	Map,
	GameConstants,
	Item,
	ItemTypes,
	ItemInstance,
	Mob,
	AreaLoopEvents,
	ZoneInterCommunication
	{Third Party}
	;

//------------------------------------------------------------------------------
//GMZoneStatus                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Response zone status, only when zone is online.
//
//	Changes-
//		[2007/?/?] RaX - Create (Rax, correct date if you remember...)
//		[2007/8/8] Aeomin - Moved from GMCommands.pas and create header.
//------------------------------------------------------------------------------
procedure GMZoneStatus(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	Error.Add('Zone '+ IntToStr(MainProc.ZoneServer.Options.ID) + ' : ' + IntToStr(MainProc.ZoneServer.CharacterList.Count) + ' Online!');
end;{GMZoneStatus}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMWarp                                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Warp the character, a wrapper of #WarpDev
//
//	Changes-
//		[2007/08/13] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMWarp(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	MapName        : String;
	MapZoneID      : SmallInt;
	Index          : Integer;
	Map            : TMap;
	APoint         : TPoint;
	IsInstance     : Boolean;
begin
	if (Length(Arguments) >= 2) then
	begin
		IsInstance := False;
		MapName :=Arguments[0];
		if Pos('#', MapName) > 0 then
		begin
			if MainProc.ZoneServer.InstanceMapList.IndexOf(MapName) > -1 then
			begin
				MapZoneID := MainProc.ZoneServer.Options.ID;
				IsInstance := True;
			end else
			begin
				MapZoneID := TThreadLink(TargetChar.ClientInfo.Data).DatabaseLink.Map.GetZoneID(MapName);
			end;
		end else
		begin
			MapZoneID := TThreadLink(TargetChar.ClientInfo.Data).DatabaseLink.Map.GetZoneID(MapName);
		end;

		//Map not found
		if MapZoneID < 0 then
		begin
			Error.Add('Map ' + Arguments[0] + ' not found!');
		end else
		begin
			if (Length(Arguments) >= 3) then
			begin
				//Lets just as player's wish (first)
				APoint.X := EnsureRange(StrToIntDef(Arguments[1], 0), 0, High(Word));
				APoint.Y := EnsureRange(StrToIntDef(Arguments[2], 0), 0, High(Word));
			end else
			begin
				APoint.X := -1;
				APoint.Y := -1;
			end;

			if LongWord(MapZoneID) = MainProc.ZoneServer.Options.ID then
			begin
				//The map is at same zone, so lets find the index id
				if IsInstance then
					Index := MainProc.ZoneServer.InstanceMapList.IndexOf(MapName)
				else
					Index := MainProc.ZoneServer.MapList.IndexOf(MapName);

				if Index < 0 then
				begin
					//This is impossible as i know of, but IF it happened...
					Error.Add('Map ' + MapName + ' not found!');
				end else
				begin
					//So, found it!
					if IsInstance then
						Map := MainProc.ZoneServer.InstanceMapList.Items[Index]
					else
						Map := MainProc.ZoneServer.MapList.Items[Index];

					if Map.SafeLoad then
					begin
						if (APoint.X > Map.Size.X -1) or (APoint.Y > Map.Size.Y -1)
						or (APoint.X < 0) or (APoint.Y < 0) or Map.IsBlocked(APoint) then
						begin
							APoint := Map.RandomCell;
						end;

						if not ZoneSendWarp(
								TargetChar,
								MapName,
								APoint.X,
								APoint.Y
							)
						then begin
							Error.Add('Map ' + MapName + ' not found!');
						end else
						begin
							Error.Add('Warped to ' + MapName);
						end;
					end else
					begin
						//Well.. it just failed...
						Error.Add('Warp Failed!');
						Error.Add('An unexpected error occured during map load...');
					end;
				end;
			end else
			begin
				if APoint.X < 0 then
					APoint.X := {Random}(High(Word));
				if APoint.Y < 0 then
					APoint.Y := {Random}(High(Word));
				//The map is not in same zone..~
				ZoneSendWarp(
					TargetChar,
					MapName,
					APoint.X,
					APoint.Y
					)
			end;
		end;
	end else
	begin
		//Not enough parameters
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMWarpDev                                                            PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Warp the character
//
//	Changes-
//		[2007/08/08] Aeomin - Create.
//		[2007/08/13] Aeomin - Renamed from GMWarp
//------------------------------------------------------------------------------
procedure GMWarpDev(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	if (Length(Arguments) >= 4) then
	begin
		if not ZoneSendWarp(
				TargetChar,
				Arguments[0],
				EnsureRange(StrToIntDef(Arguments[1], 0), 0, High(Word)),
				EnsureRange(StrToIntDef(Arguments[2], 0), 0, High(Word))
			) then
		begin
			Error.Add('Map ' + Arguments[0] + ' not found!');
		end else
		begin
			Error.Add('Warped to ' + Arguments[0]);
		end;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMWarpDev}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMJump                                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		jump to random or specific cell
//
//	Changes-
//		[2007/08/14] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMJump(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	APoint : TPoint;
begin
	if (Length(Arguments) >= 2) then
	begin
		APoint.X := EnsureRange(StrToIntDef(Arguments[0], 0), 0, High(Word));
		APoint.Y := EnsureRange(StrToIntDef(Arguments[1], 0), 0, High(Word));
	end else
	begin
		//Just try it, if works then you are on LUCK!
		APoint.X := Random(High(Word));
		APoint.Y := Random(High(Word));
	end;

	//Lets see.. XD
	if (APoint.X > TargetChar.MapInfo.Size.X -1) or (APoint.Y > TargetChar.MapInfo.Size.Y -1)
	or (APoint.X < 0) or (APoint.Y < 0) or TargetChar.MapInfo.IsBlocked(APoint) then
	begin
		APoint := TargetChar.MapInfo.RandomCell;
	end;

	if not ZoneSendWarp(
			TargetChar,
			TargetChar.Map,
			APoint.X,
			APoint.Y
		)
	then begin
		//I'm not sure what just happened..
		Error.Add('Failed to Jump');
	end else
	begin
		Error.Add('Jumped to ' + IntToStr(APoint.X) + ',' + IntToStr(APoint.Y));
	end;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//GMCharMove                                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Warp a player to you.
//
//	Changes-
//		[2008/08/11] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMCharMove(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	if (Length(Arguments) >= 5) then
	begin
		if not ZoneSendWarp(
				TargetChar,
				Arguments[1],
				EnsureRange(StrToIntDef(Arguments[2], 0), 0, High(Word)),
				EnsureRange(StrToIntDef(Arguments[3], 0), 0, High(Word))
			) then
		begin
			Error.Add('Map ' + Arguments[0] + ' not found!');
		end else
		begin
			Error.Add('Warped ' + TargetChar.Name);
		end;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMRecall}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMRecall                                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Warp a player to you.
//
//	Changes-
//		[2008/08/11] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMRecall(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	if not ZoneSendWarp(
			TargetChar,
			FromChar.Map,
			FromChar.Position.X,
			FromChar.Position.Y
		)
	then begin
		Error.Add('Failed to recall');
	end else
	begin
		Error.Add('Recalled '+TargetChar.Name);
	end;
end;{GMRecall}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMGiveBaseExperience                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Give character base experience XD
//
//	Changes-
//		[2007/8/8] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMGiveBaseExperience(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	ToChange : LongWord;
begin
	if (Length(Arguments) >= 2) then
	begin
		ToChange := EnsureRange(StrToIntDef(Arguments[1], 0), 0, High(Integer));
		if ToChange > 0 then
		begin
			TargetChar.BaseEXP := TargetChar.BaseEXP + ToChange;
			Error.Add(IntToStr(ToChange) + ' base experiences Given to ' + TargetChar.Name);
		end else
		begin
			Error.Add('Amount of experience can only between 1-2147483647');
		end;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMGiveBaseExperience}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMGiveJobExperience                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Give character job experience
//
//	Changes-
//		[2007/8/10] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMGiveJobExperience(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	ToChange : LongWord;
begin
	if (Length(Arguments) >= 2) then
	begin
		ToChange := EnsureRange(StrToIntDef(Arguments[1], 0), 0, High(Integer));
		if ToChange > 0 then
		begin
			TargetChar.JobEXP := TargetChar.JobEXP + ToChange;
			Error.Add(IntToStr(ToChange) + ' job experiences Given to ' + TargetChar.Name);
		end else
		begin
			Error.Add('Amount of experience can only between 1-2147483647');
		end;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMGiveJobExperience}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBaseLevelUp                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Give a player base level
//
//	Changes-
//		[2007/8/11] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMBaseLevelUp(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	ToChange : SmallInt;
	OldLevel : SmallInt;
begin
	if (Length(Arguments) >= 2) then
	begin
		ToChange := EnsureRange(StrToIntDef(Arguments[1], 0), -32767, 32767);
		if ToChange = 0 then
		begin
			Error.Add('Amount of level can only between -32767 to 32767 (Can not be 0)');
		end else
		begin
			OldLevel := TargetChar.BaseLV;

			ToChange := Min(Max(TargetChar.BaseLV + ToChange, 1), Min(MainProc.ZoneServer.Options.MaxBaseLevel, CHAR_BLEVEL_MAX));

			TargetChar.BaseLV := ToChange;

			Error.Add(IntToStr(TargetChar.BaseLV - OldLevel) + ' base levels Given to ' + TargetChar.Name);
		end;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMBaseLevelUp}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMJobLevelUp                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Give a player job level
//
//	Changes-
//		[2007/8/11] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMJobLevelUp(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	ToChange : SmallInt;
	OldLevel : SmallInt;
begin
	if (Length(Arguments) >= 2) then
	begin
		ToChange := EnsureRange(StrToIntDef(Arguments[1], 0), -32767, 32767);
		if ToChange = 0 then
		begin
			Error.Add('Amount of level can only between -32767 to 32767 (Can not be 0)');
		end else
		begin
			OldLevel := TargetChar.JobLV;

			ToChange := Min(Max(TargetChar.JobLV + ToChange, 1), Min(MainProc.ZoneServer.Options.MaxJobLevel, CHAR_JLEVEL_MAX));

			TargetChar.JobLV := ToChange;

			Error.Add(IntToStr(TargetChar.JobLV - OldLevel) + ' job levels Given to ' + TargetChar.Name);
		end;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMJobLevelUp}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMAddStatusPoints                                                    PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Give target player status points
//
//	Changes-
//		[2007/8/14] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMAddStatusPoints(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	ToChange : Integer;
	OldPoint : Integer;
begin
	if (Length(Arguments) >= 2) then
	begin
		ToChange := EnsureRange(StrToIntDef(Arguments[1], 0), -CHAR_STATPOINT_MAX, CHAR_STATPOINT_MAX);
		if ToChange = 0 then
		begin
			Error.Add('Amount of points must be between -' + IntToStr(CHAR_STATPOINT_MAX) + ' and ' + IntToStr(CHAR_STATPOINT_MAX) + ' (Can not be 0)');
		end else
		begin
			OldPoint := TargetChar.StatusPts;

			ToChange := EnsureRange(TargetChar.StatusPts + ToChange, 0, CHAR_STATPOINT_MAX);

			TargetChar.StatusPts := ToChange;

			Error.Add(IntToStr(TargetChar.StatusPts - OldPoint) + ' status points given to ' + TargetChar.Name);
		end;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMAddSkillPoints                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Give target player skill points
//
//	Changes-
//		[2007/8/14] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMAddSkillPoints(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	ToChange : Integer;
	OldPoint : Integer;
begin
	if (Length(Arguments) >= 2) then
	begin
		ToChange := EnsureRange(StrToIntDef(Arguments[1], 0), -CHAR_SKILLPOINT_MAX, CHAR_SKILLPOINT_MAX);
		if ToChange = 0 then
		begin
			Error.Add('Amount of points can only between -' + IntToStr(CHAR_SKILLPOINT_MAX) + ' to ' + IntToStr(CHAR_SKILLPOINT_MAX) + ' (Can not be 0)');
		end else
		begin
			OldPoint := TargetChar.SkillPts;

			ToChange := EnsureRange(TargetChar.SkillPts + ToChange, 0, CHAR_SKILLPOINT_MAX);

			TargetChar.SkillPts := ToChange;

			Error.Add(IntToStr(TargetChar.SkillPts - OldPoint) + ' skill points given to ' + TargetChar.Name);
		end;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMGiveZeny                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Give target player zeny (XD)
//
//	Changes-
//		[2007/8/14] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMGiveZeny(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	ToChange  : Integer;
	OldAmount : Integer;
begin
	if (Length(Arguments) >= 2) then
	begin
		ToChange := EnsureRange(StrToIntDef(Arguments[1], 0), Low(Integer), High(Integer));
		if ToChange = 0 then
		begin
			Error.Add('Amount of zeny can only between -2147483647 to 2147483647 (Can not be 0) at a time');
		end else
		begin
			OldAmount := TargetChar.Zeny;

			if High(Integer) - OldAmount < ToChange then
				ToChange := High(Integer)
			else
				ToChange := EnsureRange(TargetChar.Zeny + ToChange, 0, 2147483647);

			TargetChar.Zeny := ToChange;

			Error.Add(IntToStr(TargetChar.Zeny - OldAmount) + ' zeny given to ' + TargetChar.Name);
		end;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMGiveStat                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Give target player stat, Type can be str/int/dex/ bla bla.. or number
//
//	Changes-
//		[2007/8/14] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMGiveStat(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	ToChange  : Integer;
	OldAmount : Integer;
	State     : String;
	SType     : Byte;
	OK        : Boolean;
begin
	if (Length(Arguments) >= 4) then
	begin
		ToChange := EnsureRange(StrToIntDef(Arguments[2], 0), -CHAR_STAT_MAX, CHAR_STAT_MAX);
		if ToChange = 0 then
		begin
			Error.Add('Amount of stats can only between -' + IntToStr(CHAR_STAT_MAX) + ' to ' + IntToStr(CHAR_STAT_MAX) + ' (Can not be 0) at a time');
		end else
		begin
			State := UpperCase(Arguments[1]);

			OK := False;
			SType := 0;
			if (State = 'STR') or (State = '0') then
			begin
				State := 'STR';
				SType := 0;
				OK    := True;
			end else
			if (State = 'AGI') or (State = '1') then
			begin
				State := 'AGI';
				SType := 1;
				OK    := True;
			end else
			if (State = 'VIT') or (State = '2') then
			begin
				State := 'VIT';
				SType := 2;
				OK    := True;
			end else
			if (State = 'INT') or (State = '3') then
			begin
				State := 'INT';
				SType := 3;
				OK    := True;
			end else
			if (State = 'DEX') or (State = '4') then
			begin
				State := 'DEX';
				SType := 4;
				OK    := True;
			end else
			if (State = 'LUK') or (State = '5') then
			begin
				State := 'LUK';
				SType := 5;
				OK    := True;
			end else
			begin
				Error.Add(State + ' is an invalide type');
			end;
			if OK then
			begin
				OldAmount := TargetChar.ParamBase[SType];
				if High(Integer) - OldAmount < ToChange then
					ToChange := High(Integer)
				else
					ToChange := EnsureRange(TargetChar.ParamBase[SType] + ToChange, 1, CHAR_STAT_MAX);
				TargetChar.ParamBase[SType] := ToChange;
				Error.Add(IntToStr(TargetChar.ParamBase[SType] - OldAmount) + ' ' + State + ' stats given to ' + TargetChar.Name);
			end;
		end;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMResetStats                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Reset target character's stats
//
//	Changes-
//		[2007/8/20] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMResetStats(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	TargetChar.ResetStats;
	Error.Add('Status reset successful!');
end;{GMResetStats}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMSpeed                                                              PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Control player's walking speed
//
//	Changes-
//		[2007/11/26] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMSpeed(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	ToChange : Integer;
begin
	if (Length(Arguments) >= 2) then
	begin
		ToChange := EnsureRange(StrToIntDef(Arguments[0], 150), 25, 1000);
		TargetChar.Speed := ToChange;
		Error.Add('Walking speed changed to ' + IntToStr(ToChange));
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMSpeed}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMDie                                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		A simple procedure drains all player's HP
//
//	Changes-
//		[2007/12/27] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMDie(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	TargetChar.HP := 0;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//GMBroadCast                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Broadcast GM Announce (With name)
//
//	Changes-
//		[2007/8/9] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMBroadCast(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	Announce  : String;
begin
	Announce := FromChar.Name + ': ' + Arguments[0];
	SendGMAnnounce(TargetChar.ClientInfo, Announce);
end;{GMBroadCast}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBroadCastNoName                                                    PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Broadcast GM Announce (Without name)
//
//	Changes-
//		[2007/8/9] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMBroadCastNoName(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	SendGMAnnounce(TargetChar.ClientInfo, Arguments[0]);
end;{GMBroadCastNoName}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBroadCastLocal                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Broadcast GM Announce to current map (With name)
//	have to loop here.. since TYPE_TARGETMAP and GMFLAG_NOSPLIT can't used at
//	same time
//
//	Changes-
//		[2007/8/11] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMBroadCastLocal(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	Announce  : String;
	AChar     : TCharacter;
	idxY      : SmallInt;
	idxX      : SmallInt;
	Index     : Integer;
begin
	Announce := FromChar.Name + ': ' + Arguments[0];
	for idxY := TargetChar.MapInfo.Size.Y - 1 downto 0 do
	begin
		for idxX := TargetChar.MapInfo.Size.X - 1 downto 0 do
		begin
			for Index := TargetChar.MapInfo.Cell[idxX, idxY].Beings.Count - 1 downto 0 do
			begin
				if not (TargetChar.MapInfo.Cell[idxX,idxY].Beings.Objects[Index] is TCharacter) then
				begin
					Continue;
				end;
				AChar := TargetChar.MapInfo.Cell[idxX,idxY].Beings.Objects[Index] as TCharacter;
				SendGMAnnounce(AChar.ClientInfo, Announce);
			end;
		end;
	end;
end;{GMBroadCastLocal}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBroadCastLocalNoName                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Broadcast GM Announce to current map (Without name)
//	have to loop here.. since TYPE_TARGETMAP and GMFLAG_NOSPLIT can't used at
//	same time
//
//	Changes-
//		[2007/8/11] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMBroadCastLocalNoName(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	AChar     : TCharacter;
	idxY      : SmallInt;
	idxX      : SmallInt;
	Index     : Integer;
begin
	for idxY := TargetChar.MapInfo.Size.Y - 1 downto 0 do
	begin
		for idxX := TargetChar.MapInfo.Size.X - 1 downto 0 do
		begin
			for Index := TargetChar.MapInfo.Cell[idxX, idxY].Beings.Count - 1 downto 0 do
			begin
				if not (TargetChar.MapInfo.Cell[idxX,idxY].Beings.Objects[Index] is TCharacter) then
				begin
					Continue;
				end;
				AChar := TargetChar.MapInfo.Cell[idxX,idxY].Beings.Objects[Index] as TCharacter;
				SendGMAnnounce(AChar.ClientInfo, Arguments[0]);
			end;
		end;
	end;
end;{GMBroadCastLocalNoName}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBroadCastLocalBlue                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Broadcast GM Announce to current map (Without name)
//	have to loop here.. since TYPE_TARGETMAP and GMFLAG_NOSPLIT can't used at
//	same time
//
//	Changes-
//		[2007/8/11] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMBroadCastLocalBlue(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	AChar     : TCharacter;
	idxY      : SmallInt;
	idxX      : SmallInt;
	Index     : Integer;
begin
	for idxY := TargetChar.MapInfo.Size.Y - 1 downto 0 do
	begin
		for idxX := TargetChar.MapInfo.Size.X - 1 downto 0 do
		begin
			for Index := TargetChar.MapInfo.Cell[idxX, idxY].Beings.Count - 1 downto 0 do
			begin
				if not (TargetChar.MapInfo.Cell[idxX,idxY].Beings.Objects[Index] is TCharacter) then
				begin
					Continue;
				end;
				AChar := TargetChar.MapInfo.Cell[idxX,idxY].Beings.Objects[Index] as TCharacter;
				SendGMAnnounce(AChar.ClientInfo, Arguments[0], True);
			end;
		end;
	end;
end;{GMBroadCastLocalBlue}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBroadCastColor                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Broadcasts a message in a color.
//
//	Changes-
//		[2007/11/12] RabidChocobo - Create.
//------------------------------------------------------------------------------
procedure GMBroadCastColor(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	Color : Integer;
begin
	if (Length(Arguments) >= 3) then
	begin
		Color := StrToIntDef('$'+ Arguments[0], -1);
		if (Color < $000000) or (Color > $FFFFFF) then
		begin
			Error.Add('Invalid color, must be in between 000000 and FFFFFF.');
			Error.Add(Arguments[Length(Arguments)-1]);
		end else
		begin
			SendGMAnnounceColor(TargetChar.ClientInfo, Arguments[1], Color);
		end;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMBroadCastColor}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBroadCastRed                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Broadcasts a message in red without a name.
//
//	Changes-
//		[2007/11/12] RabidChocobo - Create.
//------------------------------------------------------------------------------
procedure GMBroadCastRed(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	if (Length(Arguments) >= 2) then
	begin
		SendGMAnnounceColor(TargetChar.ClientInfo, Arguments[0], $FF3333);
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMBroadCastRed}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBroadCastPurple                                                    PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Broadcasts a message in purple without a name.
//
//	Changes-
//		[2007/11/12] RabidChocobo - Create.
//------------------------------------------------------------------------------
procedure GMBroadCastPurple(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	if (Length(Arguments) >= 2) then
	begin
		SendGMAnnounceColor(TargetChar.ClientInfo, Arguments[0], $CC66FF);
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMBroadCastPurple}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBroadCastGreen                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Broadcasts a message in green without a name.
//
//	Changes-
//		[2007/11/12] RabidChocobo - Create.
//------------------------------------------------------------------------------
procedure GMBroadCastGreen(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	if (Length(Arguments) >= 2) then
	begin
		SendGMAnnounceColor(TargetChar.ClientInfo, Arguments[0], $66FF66);
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMBroadCastGreen}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBroadCastBlack                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Broadcasts a message in black without a name.
//
//	Changes-
//		[2007/11/12] RabidChocobo - Create.
//------------------------------------------------------------------------------
procedure GMBroadCastBlack(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	if (Length(Arguments) >= 2) then
	begin
		SendGMAnnounceColor(TargetChar.ClientInfo, Arguments[0], $202020);
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMBroadCastBlack}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBroadCastBlue                                                      PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Broadcasts a message in blue without a name.
//
//	Changes-
//		[2007/11/12] RabidChocobo - Create.
//------------------------------------------------------------------------------
procedure GMBroadCastBlue(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	if (Length(Arguments) >= 2) then
	begin
		SendGMAnnounceColor(TargetChar.ClientInfo, Arguments[0], $3399CC);
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMBroadCastBlue}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBroadCastWhite                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Broadcasts a message in white without a name.
//
//	Changes-
//		[2007/11/12] RabidChocobo - Create.
//------------------------------------------------------------------------------
procedure GMBroadCastWhite(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	if (Length(Arguments) >= 2) then
	begin
		SendGMAnnounceColor(TargetChar.ClientInfo, Arguments[0], $FFFFFF);
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMBroadCastWhite}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMKick                                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Kick a player
//
//	Changes-
//		[2007/12/5] Aeomin - Created.
//------------------------------------------------------------------------------
procedure GMKick(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	if (Length(Arguments) >= 2) then
	begin
		Kick(TargetChar);
		Error.Add('Player '''+TargetChar.Name+''' kicked.');
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMKick}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMKickAll                                                            PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Kick everyone
//
//	Changes-
//		[2007/12/5] Aeomin - Created.
//------------------------------------------------------------------------------
procedure GMKickAll(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	Kick(TargetChar);
end;{GMKick}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//GMBan                                                            PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		bans Specified User
//
//	Changes-
//
//------------------------------------------------------------------------------
procedure GMBan(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
		TClientLink(TargetChar.ClientInfo.Data).AccountLink.PermanantBan();
 		kick(TargetChar);
		Error.Add('Player '''+TargetChar.Name+''' Banned.');
end;{GMBan}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMEffect                                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Send effect packet (mostly for debug purpose)
//
//	Changes-
//		[2007/11/24] Aeomin - Created.
//------------------------------------------------------------------------------
procedure GMEffect(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	EffectID : Integer;
begin
	if (Length(Arguments) >= 2) then
	begin
		EffectID := StrToIntDef(Arguments[0], -1);
		if EffectID < 0 then
		begin
			Error.Add('Invalid Effect ID.');
			Error.Add(Arguments[Length(Arguments)-1]);
		end else
		begin
			TargetChar.ShowEffect(EffectID);
		end;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMEffect}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMWhere                                                              PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Get character current map name & coordinate
//
//	Changes-
//		[2008/01/01] Aeomin - Created.
//------------------------------------------------------------------------------
procedure GMWhere(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	Error.Add('Map : '+TargetChar.Map);
	Error.Add('At  : ('+IntToStr(TargetChar.Position.X)+','+IntToStr(TargetChar.Position.Y)+')');
end;{GMWhere}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMItem                                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Get item
//
//	Changes-
//		[2008/10/20] Aeomin - Created.
//------------------------------------------------------------------------------
procedure GMItem(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	ID : Word;
	Amount : Word;
	Valid : Boolean;
	AnItem : TItemInstance;
begin
	if (Length(Arguments) >= 2) then
	begin
		ID := StrToIntDef(Arguments[0], 0);
		if (Length(Arguments) >= 3) then
			Amount := EnsureRange(StrToIntDef(Arguments[1], 1),1,High(Word))
		else
			Amount := 1;
		if ID >0 then
			Valid := TThreadLink(TargetChar.ClientInfo.Data).DatabaseLink.Items.Find(ID)
		else
		begin
			ID := TThreadLink(TargetChar.ClientInfo.Data).DatabaseLink.Items.Find(Arguments[0]);
			Valid := ID > 0;
		end;
		if Valid then
		begin
			AnItem := TItemInstance.Create;
			AnItem.Item := TItem.Create;
			AnItem.Item.ID := ID;
			AnItem.Quantity := Amount;
			AnItem.Identified := True;
			TThreadLink(TargetChar.ClientInfo.Data).DatabaseLink.Items.Load(AnItem.Item);
			TargetChar.Inventory.Add(AnItem);
		end else
		begin
			Error.Add('Item not found!');
		end;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMItem}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMJob                                                                 ROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Changes a character's job
//
//	Changes-
//		[2008/9/27] RaX - Created.
//------------------------------------------------------------------------------
procedure GMJob(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	NewJob : Word;
begin
	if (Length(Arguments) >= 2) then
	begin
		NewJob := EnsureRange(StrToIntDef(Arguments[0], 0), 0, High(Word));
		TargetChar.ChangeJob(NewJob);
		Error.Add('Job changed to ' + TargetChar.JobName + '.');
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMJob}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMResetLook                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Reset target character's Look
//
//	Changes-
//		[2008/09/24] Spre - Create.
//		[2008/09/24] Spre - Added New check!
//------------------------------------------------------------------------------
procedure GMResetLook(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	if (Length(Arguments) >= 2) then
	begin
//		TargetChar.ResetLook := 0;
		Error.Add('Look reset successful!');
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMResetLook}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMCreateInstance                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Create an instance map
//
//	Changes-
//		[2008/12/?] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMCreateInstance(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
begin
	if (Length(Arguments) >= 3) then
	begin
		ZoneSendCreateInstanceMapRequest(
			MainProc.ZoneServer.ToInterTCPClient,
			Arguments[0],
			Arguments[1],
			TargetChar.ID,
			0
		);
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMCreateInstance}
//------------------------------------------------------------------------------


procedure GMSpawn(const Arguments : array of String;const FromChar:TCharacter;const TargetChar: TCharacter;const Error : TStringList);
var
	AMob : TMob;
begin
	if (Length(Arguments) >= 2) then
	begin
		AMob := TMob.Create;
		AMob.JID := StrToIntDef(Arguments[0],0);
		AMob.Name := 'KFC Rejected';
		AMob.SpriteName := Arguments[0];
		AMob.Position:=TargetChar.Position;
		AMob.ID:= TargetChar.MapInfo.NewObjectID;
		TThreadLink(TargetChar.ClientInfo.Data).DatabaseLink.Mob.Load(AMob);
		AMob.MapInfo := TargetChar.MapInfo;
		AMob.Initiate;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;


end;
//------------------------------------------------------------------------------
end{GMCommandExe}.
