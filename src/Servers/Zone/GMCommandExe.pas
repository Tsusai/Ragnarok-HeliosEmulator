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
	procedure GMGiveBaseExperience(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMGiveJobExperience(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMBaseLevelUp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
	procedure GMJobLevelUp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
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
	{Project}
	Main,
	ZoneSend,
	PacketTypes,
	BufferIO,
	{Third Party}
	IdContext
	;

//------------------------------------------------------------------------------
//ZoneStatus                                                           PROCEDURE
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
//ZoneWarp                                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Warp the character
//
//	Changes-
//		[2007/8/8] Aeomin - Create.
//------------------------------------------------------------------------------
procedure GMWarp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList);
begin
	if (Length(Arguments) >= 3) then
	begin
		if not ZoneSendWarp(
				TargetChar,
				Arguments[0],
				StrToIntDef(Arguments[1], 0),
				StrToIntDef(Arguments[2], 0)
			)
		then begin
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
end;{GMWarp}
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
