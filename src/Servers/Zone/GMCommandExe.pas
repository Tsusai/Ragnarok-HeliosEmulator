//------------------------------------------------------------------------------
//GMCommandExe                                                              UNIT
//------------------------------------------------------------------------------
//	What it does-
//      	Actual GM command code
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

	function GMZoneStatus(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList) : Boolean;
	function GMWarp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList) : Boolean;
	function GMGiveBaseExperience(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList) : Boolean;
	function GMGiveJobExperience(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList) : Boolean;
	function GMBaseLevelUp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList) : Boolean;
	function GMJobLevelUp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList) : Boolean;
	function GMBroadCast(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList) : Boolean;
	function GMBroadCastNoName(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList) : Boolean;
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
//ZoneStatus                                                            FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Response zone status, only when zone is online.
//
//	Changes-
//		[2007/?/?] RaX - Create (Rax, correct date if you remember...)
//		[2007/8/8] Aeomin - Moved from GMCommands.pas and create header.
//------------------------------------------------------------------------------
function GMZoneStatus(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList) : Boolean;
begin
	Result := TRUE;
	Error.Add('Zone '+ IntToStr(MainProc.ZoneServer.Options.ID) + ' : ' + IntToStr(MainProc.ZoneServer.CharacterList.Count) + ' Online!');
end;{GMZoneStatus}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneWarp                                                              FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Warp the character
//
//	Changes-
//		[2007/8/8] Aeomin - Create.
//------------------------------------------------------------------------------
function GMWarp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList) : Boolean;
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
			Result := False;
		end else
		begin
			Error.Add('Warped to ' + Arguments[0]);
			Result := True;
		end;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
		Result := False;
	end;
end;{GMWarp}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMGiveBaseExperience                                                  FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Give character base experience XD
//
//	Changes-
//		[2007/8/8] Aeomin - Create.
//------------------------------------------------------------------------------
function GMGiveBaseExperience(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList) : Boolean;
var
	ToChange : LongWord;
begin
	Result := False;
	if (Length(Arguments) >= 2) then
	begin
		ToChange := EnsureRange(StrToIntDef(Arguments[1], 0), 0, High(Integer));
		if ToChange > 0 then
		begin
			TargetChar.BaseEXP := TargetChar.BaseEXP + ToChange;
			Error.Add(IntToStr(ToChange) + ' base experiences Given to ' + TargetChar.Name);
			Result := True;
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
//GMGiveJobExperience                                                   FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Give character job experience
//
//	Changes-
//		[2007/8/10] Aeomin - Create.
//------------------------------------------------------------------------------
function GMGiveJobExperience(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList) : Boolean;
var
	ToChange : LongWord;
begin
	Result := False;
	if (Length(Arguments) >= 2) then
	begin
		ToChange := EnsureRange(StrToIntDef(Arguments[1], 0), 0, High(Integer));
		if ToChange > 0 then
		begin
			TargetChar.JobEXP := TargetChar.JobEXP + ToChange;
			Error.Add(IntToStr(ToChange) + ' job experiences Given to ' + TargetChar.Name);
			Result := True;
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
//GMBaseLevelUp                                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Give a player base level
//
//	Changes-
//		[2007/8/11] Aeomin - Create.
//------------------------------------------------------------------------------
function GMBaseLevelUp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList) : Boolean;
var
	ToChange : SmallInt;
	OldLevel : Byte;
begin
	Result := False;
	if (Length(Arguments) >= 2) then
	begin
		ToChange := EnsureRange(StrToIntDef(Arguments[1], 0), -254, 254);
		if ToChange = 0 then
		begin
			Error.Add('Amount of level can only between -254 to 254');
		end else
		begin
			OldLevel := TargetChar.BaseLV;
			//Keep number safe
			if (TargetChar.BaseLV + ToChange) > High(Byte) then
			begin
				TargetChar.BaseLV := High(Byte);
			end else if (TargetChar.BaseLV + ToChange) < 1 then
			begin
				TargetChar.BaseLV := 1;
			end else
			begin
				TargetChar.BaseLV := TargetChar.BaseLV + ToChange;
			end;
			Error.Add(IntToStr(TargetChar.BaseLV - OldLevel) + ' base levels Given to ' + TargetChar.Name);
			Result := True;
		end;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMBaseLevelUp}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMJobLevelUp                                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Give a player job level
//
//	Changes-
//		[2007/8/11] Aeomin - Create.
//------------------------------------------------------------------------------
function GMJobLevelUp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList) : Boolean;
var
	ToChange : SmallInt;
	OldLevel : Byte;
begin
	Result := False;
	if (Length(Arguments) >= 2) then
	begin
		ToChange := EnsureRange(StrToIntDef(Arguments[1], 0), -254, 254);
		if ToChange = 0 then
		begin
			Error.Add('Amount of level can only between -254 to 254');
		end else
		begin
			OldLevel := TargetChar.JobLV;
			//Keep number safe
			if (TargetChar.JobLV + ToChange) > High(Byte) then
			begin
				TargetChar.JobLV := High(Byte);
			end else if (TargetChar.JobLV + ToChange) < 1 then
			begin
				TargetChar.JobLV := 1;
			end else
			begin
				TargetChar.JobLV := TargetChar.JobLV + ToChange;
			end;
			Error.Add(IntToStr(TargetChar.JobLV - OldLevel) + ' job levels Given to ' + TargetChar.Name);
			Result := True;
		end;
	end else
	begin
		Error.Add('Syntax Help:');
		Error.Add(Arguments[Length(Arguments)-1]);
	end;
end;{GMJobLevelUp}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBroadCast                                                           FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Broadcast GM Announce (With name)
//
//	Changes-
//		[2007/8/9] Aeomin - Create.
//------------------------------------------------------------------------------
function GMBroadCast(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList) : Boolean;
var
	Announce  : String;
begin
	Announce := FromChar + ': ' + Arguments[0];
	SendGMAnnounce(TargetChar.ClientInfo, Announce);
	
	Result := True;
end;{GMBroadCast}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBroadCastNoName                                                     FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Broadcast GM Announce (Without name)
//
//	Changes-
//		[2007/8/9] Aeomin - Create.
//------------------------------------------------------------------------------
function GMBroadCastNoName(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : TStringList) : Boolean;
begin
	SendGMAnnounce(TargetChar.ClientInfo, Arguments[0]);
	
	Result := True;
end;{GMBroadCastNoName}
//------------------------------------------------------------------------------
end.
