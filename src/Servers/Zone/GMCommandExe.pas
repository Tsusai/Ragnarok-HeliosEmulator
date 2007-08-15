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

interface
uses
	Classes,
	{Project}
	Character
	;

	procedure GMZoneStatus(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMWarp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMWarpDev(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMJump(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMGiveBaseExperience(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMGiveJobExperience(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMBaseLevelUp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMJobLevelUp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMAddStatusPoints(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMAddSkillPoints(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMGiveZeny(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMBroadCast(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMBroadCastNoName(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMBroadCastLocal(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMBroadCastLocalNoName(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMBroadCastLocalBlue(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
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
	BufferIO,
	Map,
	MapTypes,
	ZoneInterCommunication,
	{Third Party}
	IdContext
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
procedure GMZoneStatus(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
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
procedure GMWarp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
var
	MapZoneID      : SmallInt;
	Index          : Integer;
	Map            : TMap;
	APoint         : TPoint;
begin
	if (Length(Arguments) >= 2) then
	begin
		TThreadLink(TargetChar.ClientInfo.Data).DatabaseLink.StaticData.Connect;
		MapZoneID := TThreadLink(TargetChar.ClientInfo.Data).DatabaseLink.StaticData.GetMapZoneID(Arguments[0]);
		TThreadLink(TargetChar.ClientInfo.Data).DatabaseLink.StaticData.Disconnect;
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

			if Cardinal(MapZoneID) = MainProc.ZoneServer.Options.ID then
			begin
				//The map is at same zone, so lets find the index id
				Index := MainProc.ZoneServer.MapList.IndexOf(Arguments[0]);
				if Index < 0 then
				begin
					//This is impossible as i know of, but IF it happened...
					Error.Add('Map ' + Arguments[0] + ' not found!');
				end else
				begin
					//So, found it!
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
								Arguments[0],
								APoint.X,
								APoint.Y
							)
						then begin
							Error.Add('Map ' + Arguments[0] + ' not found!');
						end else
						begin
							Error.Add('Warped to ' + Arguments[0]);
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
					APoint.X := Random(High(Word));
				if APoint.Y < 0 then
					APoint.Y := Random(High(Word));
				//The map is not in same zone..~
				ZoneSendMapWarpRequestToInter(MainProc.ZoneServer.ToInterTCPClient, TargetChar.CID, Cardinal(MapZoneID), Arguments[0], APoint);
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
procedure GMWarpDev(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
begin
	if (Length(Arguments) >= 3) then
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
procedure GMJump(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
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
//GMGiveBaseExperience                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Give character base experience XD
//
//	Changes-
//		[2007/8/8] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMGiveBaseExperience(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
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
procedure GMGiveJobExperience(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
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
procedure GMBaseLevelUp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
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

			ToChange := Min(Max(TargetChar.BaseLV + ToChange, 1), Min(MainProc.ZoneServer.Options.MaxBaseLevel, High(Word)));

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
procedure GMJobLevelUp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
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

			ToChange := Min(Max(TargetChar.JobLV + ToChange, 1), Min(MainProc.ZoneServer.Options.MaxJobLevel, High(Word)));

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
procedure GMAddStatusPoints(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
var
	ToChange : SmallInt;
	OldPoint : SmallInt;
begin
	if (Length(Arguments) >= 2) then
	begin
		ToChange := EnsureRange(StrToIntDef(Arguments[1], 0), -32767, 32767);
		if ToChange = 0 then
		begin
			Error.Add('Amount of points can only between -32767 to 32767 (Can not be 0)');
		end else
		begin
			OldPoint := TargetChar.StatusPts;

			ToChange := Min(Max(TargetChar.StatusPts + ToChange, 0), 32767);

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
procedure GMAddSkillPoints(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
var
	ToChange : SmallInt;
	OldPoint : SmallInt;
begin
	if (Length(Arguments) >= 2) then
	begin
		ToChange := EnsureRange(StrToIntDef(Arguments[1], 0), -32767, 32767);
		if ToChange = 0 then
		begin
			Error.Add('Amount of points can only between -32767 to 32767 (Can not be 0)');
		end else
		begin
			OldPoint := TargetChar.SkillPts;

			ToChange := Min(Max(TargetChar.SkillPts + ToChange, 0), 32767);

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
procedure GMGiveZeny(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
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
//GMBroadCast                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Broadcast GM Announce (With name)
//
//	Changes-
//		[2007/8/9] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMBroadCast(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
var
	Announce  : String;
begin
	Announce := FromChar + ': ' + Arguments[0];
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
procedure GMBroadCastNoName(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
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
procedure GMBroadCastLocal(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
var
	Announce  : String;
	AChar     : TCharacter;
	idxY      : SmallInt;
	idxX      : SmallInt;
	Index     : Integer;
begin
	Announce := FromChar + ': ' + Arguments[0];
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
procedure GMBroadCastLocalNoName(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
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
procedure GMBroadCastLocalBlue(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
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
end{GMCommandExe}.
