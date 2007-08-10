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
	{Project}
	Character
	;

	function GMZoneStatus(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : String) : Boolean;
	function GMWarp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : String) : Boolean;
	function GMGiveBaseExperience(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : String) : Boolean;
	function GMBroadCast(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : String) : Boolean;
	function GMBroadCastNoName(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : String) : Boolean;
implementation
uses
	{RTL/VCL}
	SysUtils,
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
function GMZoneStatus(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : String) : Boolean;
begin
	Result := TRUE;
	Error := 'Zone '+ IntToStr(MainProc.ZoneServer.Options.ID) + ' : ' + IntToStr(MainProc.ZoneServer.CharacterList.Count) + ' Online!';
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
function GMWarp(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : String) : Boolean;
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
			Error := 'Map ' + Arguments[0] + ' not found!';
			Result := False;
		end else
		begin
			Error := 'Warped to ' + Arguments[0];
			Result := True;
		end;
	end else
	begin
		Error := 'Command Warp Failed';
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
function GMGiveBaseExperience(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : String) : Boolean;
begin
	if (Length(Arguments) >= 2) then
	begin
		//Thats the solution i know of..
		TargetChar.BaseEXP := TargetChar.BaseEXP + Cardinal(StrToIntDef(Arguments[1], 0));
		Error := 'Experience Given to ' + TargetChar.Name;
		Result := True;
	end else
	begin
		Error := 'Command GiveBaseExperience Failed';
		Result := False;
	end;
end;{GMGiveBaseExperience}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBroadCast                                                           FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Broadcast GM Announce
//
//	Changes-
//		[2007/8/9] Aeomin - Create.
//------------------------------------------------------------------------------
function GMBroadCast(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : String) : Boolean;
var
	Announce  : String;
begin
	Announce := FromChar + ': ' + Arguments[0];
	SendGMAnnounce(TargetChar.ClientInfo, Announce);
	
	Result := True;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBroadCastNoName                                                     FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Broadcast GM Announce and not add name
//
//	Changes-
//		[2007/8/9] Aeomin - Create.
//------------------------------------------------------------------------------
function GMBroadCastNoName(const Arguments : array of String;FromChar:String;TargetChar: TCharacter; var Error : String) : Boolean;
begin
	SendGMAnnounce(TargetChar.ClientInfo, Arguments[0]);
	
	Result := True;
end;
//------------------------------------------------------------------------------
end.
