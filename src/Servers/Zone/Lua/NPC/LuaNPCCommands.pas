//Contains all script commands used by the lua system
unit LuaNPCCommands;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
	LuaCoreRoutines;

function LoadNPCCommands(var ALua : TLua) : boolean;

implementation
uses
	//RTL
	SysUtils,
	Types,
	//Project
	BufferIO,
	Character,
	GameConstants,
	LuaNPCCore,
	LuaPas,
	Main,
	Math,
	NPC,
	PacketTypes,
	ZoneSend,
	LuaVarConstants
	;

//Forward declarations of delphi procedures that are added to the lua engine.
function addnpc(ALua : TLua) : integer; cdecl; forward;
function addwarp(ALua : TLua) : integer; cdecl; forward;
function addhiddenwarp(ALua : TLua) : integer; cdecl; forward;
//Standard NPC Commands
function script_moveto(ALua : TLua) : integer; cdecl; forward;
function script_dialog(ALua : TLua) : integer; cdecl; forward;
function script_wait(ALua : TLua) : integer; cdecl; forward;
function script_close(ALua : TLua) : integer; cdecl; forward;
function script_checkpoint(ALua : TLua) : integer; cdecl; forward;
function script_menu(ALua : TLua) : integer; cdecl; forward;
function script_getcharavar(ALua : TLua) : integer; cdecl; forward;
function script_setcharavar(ALua : TLua) : integer; cdecl; forward;
function script_SetItem(ALua : TLua) : integer; cdecl; forward;
function script_GetItem(ALua : TLua) : integer; cdecl; forward;
function script_getgold(ALua : TLua) : integer; cdecl; forward;
function script_dropgold(ALua : TLua) : Integer; cdecl; forward;
function script_getexp(ALua : TLua) : integer; cdecl; forward;
function script_getJexp(ALua : TLua) : integer; cdecl; forward;
function script_ResetStat(ALua : TLua) : integer; cdecl; forward;
function script_HpDrain(ALua : TLua) : integer; cdecl; forward;
function script_HpHeal(ALua : TLua) : integer; cdecl; forward;
function script_SpDrain(ALua : TLua) : integer; cdecl; forward;
function script_SpHeal(ALua : TLua) : integer; cdecl; forward;
function script_HPFullheal(ALua : TLua) : integer; cdecl; forward;
function script_SPFullheal(ALua : TLua) : integer; cdecl; forward;
function script_Compass(ALua : TLua) : integer; cdecl; forward;
function script_ShowImage(ALua : TLua) : integer; cdecl; forward;
function script_CompassCheck(ALua : TLua) : integer; cdecl; forward;
function script_Emotion(ALua : TLua) : integer; cdecl; forward;
function script_OpenMailBox(ALua : TLua) : integer; cdecl; forward;
function script_CloseMailBox(ALua : TLua) : integer; cdecl; forward;
function script_JobChange(ALua : TLua) : integer; cdecl; forward;
//Special Commands
function script_get_charaname(ALua : TLua) : integer; cdecl; forward;
function lua_print(ALua : TLua) : integer; cdecl; forward;

const
	NPCCommandCount = 33;

const
	//"Function name in lua" , Delphi function name
	//This assigns our procedures here, with the lua system
	NPCCommandList : array [1..NPCCommandCount] of lual_reg = (
		//TNPC adding
		(name:'npc';func:addnpc),
		(name:'warp';func:addwarp),
		(name:'hiddenwarp';func:addhiddenwarp),
		//NPC Commands
		(name:'moveto';func:script_moveto),
		(name:'dialog';func:script_dialog),
		(name:'wait';func:script_wait),
		(name:'close';func:script_close),
		(name:'checkpoint';func:script_checkpoint),
		(name:'menu';func:script_menu),
		(name:'getvar';func:script_getcharavar),
		(name:'setvar';func:script_setcharavar),
		(name:'setitem';func:script_SetItem),
		(name:'getitem';func:script_GetItem),
		(name:'getgold';func:script_getgold),
		(name:'dropgold';func:script_dropgold),
		(name:'getexp';func:script_getexp),
		(name:'getJexp';func:script_getJexp),
		(name:'ResetStat';func:script_ResetStat),
		(name:'hpdrain';func:script_HpDrain),
		(name:'hpheal';func:script_HpHeal),
		(name:'spdrain';func:script_SpDrain),
		(name:'spheal';func:script_SpHeal),
		(name:'hpfullheal';func:script_HPFullHeal),
		(name:'spfullheal';func:script_SPFullHeal),
		(name:'compass';func:script_Compass),
		(name:'showimage';func:script_ShowImage),
		(name:'compass_check';func:script_CompassCheck),
		(name:'emotion';func:script_Emotion),
		(name:'OpenMailing';func:script_OpenMailBox),
		(name:'CloseMailing';func:script_CloseMailBox),
		(name:'JobChange';func:script_JobChange),
		//Special Variable retrieving functions
		(name:'PcName';func:script_get_charaname),
		//Misc tools.
		(name:'print';func:lua_print)
	);

//[2007/04/23] Tsusai - Added result
//Registers all the NPC Commands into the Lua instance
function LoadNPCCommands(var ALua : TLua) : boolean;
var
	idx : integer;
begin
	for idx := 1 to NPCCommandCount do
	begin
		lua_register(
			ALua,
			NPCCommandList[idx].name,
			NPCCommandList[idx].func
		);
	end;
	Result := true;
end;

//npc("new_1-1","Bulletin Board",spr_HIDDEN_NPC,66,114,4,0,0,"new_1-1_Bulletin_Board_66114"[,"optional ontouch"])
function CheckLuaNPCSyntax(ALua : TLua) : boolean;
var
	ParamCount : word;
begin
	//Assume true
	Result := true;
	ParamCount := lua_gettop(ALua);
	if not ParamCount in [9..10] then
	begin
		if (ParamCount = 10) and
			not Lua_isNonNumberString(ALua,10) then
		begin
			Result := false;
		end;
		if not Lua_isNonNumberString(ALua, 1) and
			not Lua_isNonNumberString(ALua, 2) and
			not lua_isnumber(ALua, 3) and
			not lua_isnumber(ALua, 4) and
			not lua_isnumber(ALua, 5) and
			not lua_isnumber(ALua, 6) and
			not lua_isnumber(ALua, 7) and
			not lua_isnumber(ALua, 8) and
			not Lua_isNonNumberString(ALua, 9) then
		begin
			Result := false;
		end;
	end;
	if not Result then
	begin
		lual_error(ALua,'NPC Script Syntax Error');
	end;
end;

//[2007/04/23] Tsusai - Added result
//[2007/05/28] Tsusai - Fixed npc point reading
//npc("new_1-1","Bulletin Board",spr_HIDDEN_NPC,66,114,4,0,0,"new_1-1_Bulletin_Board_66114"[,"optional ontouch"])
function addnpc(ALua : TLua) : integer; cdecl;
var
	ANPC : TScriptNPC;
	OnTouch : string;
begin
	OnTouch := '';
	if CheckLuaNPCSyntax(ALua) then
	begin
		if lua_gettop(ALua) = 10 then
		begin
			OnTouch := lua_tostring(ALua,10);
		end;
		ANPC := TSCriptNPC.Create(lua_tostring(ALua,9),OnTouch);
		ANPC.Map := lua_tostring(ALua,1);
		ANPC.Name := lua_tostring(ALua,2);
		ANPC.JID := lua_tointeger(ALua,3);
		ANPC.Position :=
			Point(lua_tointeger(ALua,4) , lua_tointeger(ALua,5));
		ANPC.Direction := lua_tointeger(ALua,6);
		ANPC.OnTouchXRadius := lua_tointeger(ALua,7);
		ANPC.OnTouchYRadius := lua_tointeger(ALua,8);
		MainProc.ZoneServer.NPCList.AddObject(ANPC.ID,ANPC);
	end;
	Result := 0;
end;

//Verifies all lua information on the warp syntax
//(hidden)warp("map","name",x,y,xradius,yradius)
function CheckLuaWarpSyntax(
	ALua : TLua
) : boolean;
var
	ParamCount : word;
begin
	Result := true;
	ParamCount := lua_gettop(ALua);
	if not (ParamCount = 6) and
		not (Lua_isNonNumberString(ALua,1)) and
		not (Lua_isNonNumberString(ALua,2)) and
		not (lua_isnumber(ALua,3)) and
		not (lua_isnumber(ALua,4)) and
		not (lua_isnumber(ALua,5)) and
		not (lua_isnumber(ALua,6)) then
	begin
		lual_error(ALua,'NPC Script Syntax Error');
		Result := false;
	end;
end;

//Takes lua information and makes the warp
procedure MakeNPCWarp(
	ALua : TLua;
	const JID : Word
);
var
	AWarpNPC : TWarpNPC;
begin
	AWarpNPC := TWarpNPC.Create(lua_tostring(ALua,2));
	AWarpNPC.Map := lua_tostring(ALua,1);
	AWarpNPC.Name := AWarpNPC.TouchFunction;
	AWarpNPC.JID := JID;
	AWarpNPC.Position :=
		Point(
			lua_tointeger(ALua,3),
			lua_tointeger(ALua,4)
		);
	AWarpNPC.OnTouchXRadius := lua_tointeger(ALua,5);
	AWarpNPC.OnTouchYRadius := lua_tointeger(ALua,6);
	MainProc.ZoneServer.NPCList.AddObject(AWarpNPC.ID,AWarpNPC);
end;

//[2007/04/23] Tsusai - Added result
//warp("new_1-1","novicetraining1warp001",148,112,2,3)
function addwarp(ALua : TLua) : integer; cdecl;
begin
	Result := 0;
	//Check Syntax
	if CheckLuaWarpSyntax(ALua) then
	begin
		//Make the warp
		MakeNPCWarp(ALua,NPC_WARPSPRITE);
	end;
end;

//[2007/05/03] Tsusai - Added
//hiddenwarp("new_1-1","novicetraining1warp001",148,112,2,3)
function addhiddenwarp(ALua : TLua) : integer; cdecl;
begin
	Result := 0;
	//Check Syntax
	if CheckLuaWarpSyntax(ALua) then
	begin
		//Make the warp
		MakeNPCWarp(ALua,NPC_INVISIBLE);
	end;
end;

//moveto("map",x,y)
//Warps the character to the given destination
function script_moveto(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
begin

	if (lua_gettop(ALua) = 3) and
		(Lua_isNonNumberString(ALua,1)) and
		(lua_isnumber(ALua,2)) and
		(lua_isnumber(ALua,3)) then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			ZoneSendWarp(
				AChara,
				lua_tostring(ALua,1),
				lua_tointeger(ALua,2),
				lua_tointeger(ALua,3)
			);
		end;
		lua_yield(ALua,0);//end the script
	end else
	begin
		luaL_error(ALua,'script moveto syntax error');
	end;
	Result := 0;
end;

//dialog "this is my text"
//NPC Dialog to the character
function script_dialog(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
	Len : integer;
	Dialog : string;
	OutBuffer : TBuffer;
begin
	Result := 0;
	if (lua_gettop(ALua) = 1) and
		(lua_isstring(ALua,1)) then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			Dialog := lua_tostring(ALua,1);
			Len := Length(Dialog);
			WriteBufferWord(0, $00b4, OutBuffer);
			WriteBufferWord(2, Len + 8, OutBuffer);
			WriteBufferLongWord(4, AChara.ScriptBeing.ID, OutBuffer);
			WriteBufferString(8, Dialog, Len, OutBuffer);
			SendBuffer(AChara.ClientInfo, OutBuffer, len + 8);
		end;
	end else
	begin
		luaL_error(ALua,'script dialog syntax error');
	end;
end;

//wait()
//Sends the next button to the client
function script_wait(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
	OutBuffer : TBuffer;
begin
	Result := 0;
	if (lua_gettop(ALua) = 0) then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			WriteBufferWord(0, $00b5, OutBuffer);
			WriteBufferLongWord(2, AChara.ScriptBeing.ID, OutBuffer);
			SendBuffer(AChara.ClientInfo, OutBuffer, 6);
			AChara.ScriptStatus := SCRIPT_YIELD_WAIT;
			Result := lua_yield(ALua,0);
		end;
	end else
	begin
		luaL_error(ALua,'script wait syntax error');
	end;
end;

//close()
//Sends the close button to the client and flags the script user as not running.
function script_close(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
	OutBuffer : TBuffer;
begin
	Result := 0;
	if lua_gettop(ALua) = 0 then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			WriteBufferWord(0, $00b6, OutBuffer);
			WriteBufferLongWord(2, AChara.ScriptBeing.ID, OutBuffer);
			SendBuffer(AChara.ClientInfo, OutBuffer, 6);
			AChara.ScriptStatus := SCRIPT_NOTRUNNING;
		end;
	end else
	begin
		luaL_error(ALua,'script close syntax error');
	end;
end;

//checkpoint("map",x,y)
//Sets the specified save point to the character
function script_checkpoint(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
begin
	Result := 0;
	if (lua_gettop(ALua) = 3) and
		(Lua_isNonNumberString(ALua,1)) and
		(lua_isnumber(ALua,2)) and
		(lua_isnumber(ALua,3)) then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			AChara.SaveMap := lua_tostring(ALua,1);
			AChara.SavePoint := Point(
				lua_tointeger(ALua,2), lua_tointeger(ALua,3)
			);
		end;
	end else
	begin
		luaL_error(ALua,'script checkpoint syntax error');
	end;
end;

//menu("choice1","choice2","choice3",etc)
//Sets the specified save point to the character
//R 00b7 <len>.w <ID>.l <str>.?B
//Each menu choice is delimited by ":"
function script_menu(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
	ParamCount : word;
	MenuString : string;
	Size : word;
	OutBuffer : TBuffer;
	idx : word;
begin
	Result := 0;
	MenuString := '';
	ParamCount := lua_gettop(ALua);
	if ParamCount > 0 then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			for idx := 1 to ParamCount do
			begin
				if idx = 1 then
				begin
					MenuString := lua_tostring(Alua,idx);
				end else
				begin
					MenuString := MenuString + ':' + lua_tostring(Alua,idx);
				end;
			end;
			WriteBufferWord(0, $00b7, OutBuffer);
			Size := Length(MenuString);
			WriteBufferWord(2, Size + 8, OutBuffer);
			WriteBufferLongWord(4, AChara.ScriptBeing.ID, OutBuffer);
			WriteBufferString(8, MenuString, Size, OutBuffer);
			SendBuffer(AChara.ClientInfo, OutBuffer, Size + 8);
			AChara.ScriptStatus := SCRIPT_YIELD_MENU;
			Result := lua_yield(ALua,1);
		end;
	end else
	begin
		luaL_error(ALua,'script menu syntax error');
	end;
end;

//getvar(key)
//Returns a character variable.
function script_getcharavar(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
	Value : String;
	StrValue : String;
	Key : string;
	KeyConst : Integer;
begin
	//Returns 1 result
	Result := 1;
	if (lua_gettop(ALua) = 1) and //1 parameter count
		(lua_isString(ALua,1)) then //first param is a string
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			Key := lua_tostring(ALua, 1);
			if lua_isNumber(ALua, 1) then
			begin
				KeyConst := lua_toInteger(ALua, 1);
				case KeyConst of
					VAR_ASPD: begin
						lua_pushinteger(ALua, AChara.ASpeed);
					end;

					VAR_CURXPOS: begin
						lua_pushinteger(ALua, AChara.Position.X);
					end;

					VAR_CURYPOS: begin
						lua_pushinteger(ALua, AChara.Position.Y);
					end;

					VAR_CLEVEL: begin
						lua_pushinteger(ALua, AChara.BaseLV);
					end;

					VAR_EXP: begin
						lua_pushinteger(ALua, AChara.BaseEXP);
					end;

					VAR_HAIRCOLOR: begin
						lua_pushinteger(ALua, AChara.HairColor);
					end;

					VAR_HP: begin
						lua_pushinteger(ALua, AChara.HP);
					end;

					VAR_JOB: begin
						lua_pushinteger(ALua, AChara.JID);
					end;

					VAR_JOBEXP: begin
						lua_pushinteger(ALua, AChara.JobEXP);
					end;

					VAR_JOBLEVEL: begin
						lua_pushinteger(ALua, AChara.JobLV);
					end;

					VAR_MAXHP: begin
						lua_pushinteger(ALua, AChara.MaxHP);
					end;

					VAR_MAXSP: begin
						lua_pushinteger(ALua, AChara.MaxSP);
					end;

					VAR_MAXWEIGHT: begin
						lua_pushinteger(ALua, AChara.MaxWeight);
					end;

					VAR_MONEY : begin
						lua_pushinteger(ALua, AChara.Zeny);
					end;

					VAR_POINT: begin
						lua_pushinteger(ALua, AChara.StatusPts);
					end;

					VAR_SEX : begin
						lua_pushinteger(ALua, TClientLink(AChara.ClientInfo.Data).AccountLink.GenderNum);
					end;

					VAR_SP: begin
						lua_pushinteger(ALua, AChara.SP);
					end;

					VAR_SPEED: begin
						lua_pushinteger(ALua, AChara.Speed);
					end;

					VAR_SPPOINT: begin
						lua_pushinteger(ALua, AChara.SkillPts);
					end;

					VAR_WEIGHT: begin
						lua_pushinteger(ALua, AChara.Weight);
					end;

					//Ismarried Variable
					//Implemented by Spre 2007/12/31
					//Comment Here Incase changes need to be made, easily Identifiable
					VAR_ISMARRIED: begin
						lua_pushinteger(ALua, Byte(AChara.IsMarried));
					end;

					VAR_STR: begin
						lua_pushinteger(ALua, AChara.ParamBase[STR] + AChara.ParamBonus[STR]);
					end;

					VAR_AGI: begin
						lua_pushinteger(ALua, AChara.ParamBase[AGI] + AChara.ParamBonus[AGI]);
					end;

					VAR_VIT: begin
						lua_pushinteger(ALua, AChara.ParamBase[VIT] + AChara.ParamBonus[VIT]);
					end;

					VAR_INT: begin
						lua_pushinteger(ALua, AChara.ParamBase[INT] + AChara.ParamBonus[INT]);
					end;

					VAR_DEX: begin
						lua_pushinteger(ALua, AChara.ParamBase[DEX] + AChara.ParamBonus[DEX]);
					end;

					VAR_LUK: begin
						lua_pushinteger(ALua, AChara.ParamBase[LUK] + AChara.ParamBonus[LUK]);
					end;

					VAR_STANDARD_STR: begin
						lua_pushinteger(ALua, AChara.ParamBase[STR]);
					end;

					VAR_STANDARD_AGI: begin
						lua_pushinteger(ALua, AChara.ParamBase[AGI]);
					end;

					VAR_STANDARD_VIT: begin
						lua_pushinteger(ALua, AChara.ParamBase[VIT]);
					end;

					VAR_STANDARD_INT: begin
						lua_pushinteger(ALua, AChara.ParamBase[INT]);
					end;

					VAR_STANDARD_DEX: begin
						lua_pushinteger(ALua, AChara.ParamBase[DEX]);
					end;

					VAR_STANDARD_LUK: begin
						lua_pushinteger(ALua, AChara.ParamBase[LUK]);
					end;

					VAR_CURDIR: begin
						lua_pushinteger(ALua, AChara.Direction);
					end;

					VAR_CHARACTERID: begin
						lua_pushinteger(ALua, AChara.ID);
					end;

					VAR_ACCOUNTID: begin
						lua_pushinteger(ALua, AChara.AccountID);
					end;

					VAR_MAPNAME: begin
						lua_pushstring(ALua, PChar(AChara.Map));
					end;

					VAR_ACCOUNTNAME: begin
						// sigh.. i'm not sure why but it doesnt work directly.
						StrValue := TClientLink(AChara.ClientInfo.Data).AccountLink.Name;
						lua_pushstring(ALua, PChar(StrValue));
					end;

					VAR_CHARACTERNAME: begin
						lua_pushstring(ALua, PChar(AChara.Name));
					end;

					VAR_HEADPALETTE: begin
						lua_pushinteger(ALua, AChara.HairColor);
					end;

					VAR_BODYPALETTE: begin
						lua_pushinteger(ALua, AChara.ClothesColor);
					end;
				end;
			end else
			begin
				Value := TThreadLink(AChara.ClientInfo.Data).DatabaseLink.Character.GetVariable(AChara,Key);
				lua_pushstring(ALua, PChar(Value));
			end;
		end;
	end else
	begin
		luaL_error(ALua,'script getvar syntax error');
	end;
end;

//setvar(key,value)
//Sets a character variable
function script_setcharavar(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
	Value : String;
	Key : string;
begin
	//Returns 0 results
	Result := 0;
	if (lua_gettop(ALua) = 2) and
		(lua_isString(ALua,1)) and
		(lua_isnumber(ALua,2)) then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			Key := lua_tostring(ALua, 1);
			Value := lua_tostring(ALua, 2);
			TThreadLink(AChara.ClientInfo.Data).DatabaseLink.Character.SetVariable(AChara,Key,Value);
		end;
	end else
	begin
		luaL_error(ALua,'script setvar syntax error');
	end;
end;

//SetItem - Apparently is set vriable..
//so I just made it wrapper of setvar
function script_SetItem(ALua : TLua) : Integer;
begin
	Result := script_setcharavar(ALua);
end;

//getitem- see above
function script_GetItem(ALua : TLua) : Integer;
begin
	Result := script_getcharavar(ALua);
end;

//getgold(value)
//Gives or takes money/zeny to/from the character
function script_getgold(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
	Zeny : LongInt;
begin
	//Returns 0 results
	Result := 0;
	if (lua_gettop(ALua) = 1) and
		(lua_isnumber(ALua,1)) then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			//it can be a negative number.  It should be alright w/ the +
			// 5 +-1 = 4 :)
			//combining signed and unsigned types warning.  Ignoring
			{$WARNINGS OFF}
			Zeny := EnsureRange(lua_tointeger(ALua, 1),Low(LongInt),High(LongInt));
			AChara.Zeny := EnsureRange(AChara.Zeny + Zeny,
											Low(AChara.Zeny),
											High(AChara.Zeny)
			);
			{$WARNINGS ON}
		end;
	end;
end;

//dropgold
function script_dropgold(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
	Zeny : LongInt;
begin
	//Returns 0 results
	Result := 0;
	if (lua_gettop(ALua) = 1) and
		(lua_isnumber(ALua,1)) then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			{$WARNINGS OFF}
			Zeny := EnsureRange(lua_tointeger(ALua, 1),Low(LongInt),High(LongInt));
			AChara.Zeny := EnsureRange(AChara.Zeny - Zeny,
											Low(AChara.Zeny),
											High(AChara.Zeny)
			);
			{$WARNINGS ON}
		end;
	end;
end;


//getexp(value)
//Gives or takes money/zeny to/from the character
function script_getexp(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
begin
	//Returns 0 results
	Result := 0;
	if (lua_gettop(ALua) = 1) and
		(lua_isnumber(ALua,1)) then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			AChara.BaseEXP := AChara.BaseEXP +
						Cardinal(EnsureRange(lua_tointeger(ALua, 1),0,High(Integer)));
		end;
	end;
end;

//getJexp(value)
//Gives or takes money/zeny to/from the character
function script_getJexp(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
begin
	//Returns 0 results
	Result := 0;
	if (lua_gettop(ALua) = 1) and
		(lua_isnumber(ALua,1)) then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			AChara.JobEXP := AChara.JobEXP +
						Cardinal(EnsureRange(lua_tointeger(ALua, 1),0,High(Integer)));
		end;
	end;
end;


//ResetStat
//Reset character's status
function script_ResetStat(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
begin
	Result := 0;
	if GetCharaFromLua(ALua,AChara) then
	begin
		AChara.ResetStats;
	end;
end;

// Lose some % hp for player
function script_HpDrain(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
	Percentage : Byte;
begin
	Result := 0;
	if lua_gettop(ALua) = 1 then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			Percentage := EnsureRange(lua_tointeger(ALua, 1),0,100);
			if AChara.HP > 0 then
				AChara.HPPercent := AChara.HPPercent - Percentage;
		end;
	end;
end;

//Gain some % HP for player
function script_HpHeal(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
	Percentage : Byte;
begin
	Result := 0;
	if lua_gettop(ALua) = 1 then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			Percentage := EnsureRange(lua_tointeger(ALua, 1),0,100);
			if AChara.HP > 0 then
				AChara.HPPercent := AChara.HPPercent + Percentage;
		end;
	end;
end;

// Lose some sp for player
function script_SpDrain(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
	Percentage : Byte;
begin
	Result := 0;
	if lua_gettop(ALua) = 1 then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			Percentage := EnsureRange(lua_tointeger(ALua, 1),0,100);
			if AChara.SP > 0 then
				AChara.SPPercent := AChara.SPPercent - Percentage;
		end;
	end;
end;

//Gain some % SP for player
function script_SpHeal(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
	Percentage : Byte;
begin
	Result := 0;
	if lua_gettop(ALua) = 1 then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			Percentage := EnsureRange(lua_tointeger(ALua, 1),0,100);
			if AChara.SP > 0 then
				AChara.SPPercent := AChara.SPPercent + Percentage;
		end;
	end;
end;

//Full heal player's HP
function script_HPFullHeal(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
begin
	Result := 0;
	if lua_gettop(ALua) = 0 then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			AChara.HP := AChara.MaxHP;
		end;
	end else
	begin
		luaL_error(ALua,'script hpfullheal syntax error');
	end;
end;

//Full heal player's SP
function script_SPFullHeal(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
begin
	Result := 0;
	if lua_gettop(ALua) = 0 then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			AChara.SP := AChara.MaxSP;
		end;
	end else
	begin
		luaL_error(ALua,'script spfullheal syntax error');
	end;
end;

//Compass - mark on mini map
function script_Compass(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
	PointType : Byte;
begin
	Result := 0;
	if lua_gettop(ALua) = 5 then
	begin
		if lua_isnumber(ALua, 1) AND
		lua_isnumber(ALua, 2) AND
		lua_isnumber(ALua, 3) AND
		lua_isnumber(ALua, 4) then
		begin
			if GetCharaFromLua(ALua,AChara) then
			begin
				PointType := EnsureRange(lua_tointeger(ALua, 4),0,255);
				if PointType = 0 then
					PointType := 2;
				SendCompass(
					AChara.ClientInfo,
					AChara.ScriptBeing.ID,
					lua_tointeger(ALua, 1),
					lua_tointeger(ALua, 2),
					lua_tointeger(ALua, 3),
					PointType,
					Lua_toLongWord(ALua, 5)
				);
			end;
		end;
	end;
end;

//Show cutin thingy
function script_ShowImage(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
begin
	Result := 0;
	if lua_gettop(ALua) = 2 then
	begin
		if lua_isString(ALua, 1) AND
		lua_isNumber(ALua, 2) then
		begin
			if GetCharaFromLua(ALua,AChara) then
			begin
				SendCutin(
					AChara.ClientInfo,
					lua_toString(ALua, 1),
					EnsureRange(lua_toInteger(ALua, 2),0,255)
				);
			end;
		end;
	end;
end;

//Compass Check - I have no idea what it does..
function script_CompassCheck(ALua : TLua) : integer; cdecl;
begin
	Result := 0;
	if lua_gettop(ALua) = 2 then
	begin
		// Do nothing...
	end;
end;

//Trigger emotion
function script_Emotion(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
begin
	Result := 0;
	if lua_gettop(ALua) = 1 then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			AChara.ScriptBeing.ShowEmotion(lua_tointeger(ALua,1));
		end;
	end;
end;

//Open Mailbox
function script_OpenMailBox(ALua : TLua) : integer;
var
	AChara : TCharacter;
begin
	Result := 0;
	if GetCharaFromLua(ALua,AChara) then
	begin
		ToggleMailWindow(
			AChara,
			True
		);
	end;
end;

//Close Mailbox
function script_CloseMailBox(ALua : TLua) : integer;
var
	AChara : TCharacter;
begin
	Result := 0;
	if GetCharaFromLua(ALua,AChara) then
	begin
		ToggleMailWindow(
			AChara,
			False
		);
	end;
end;

//Change Jobs
//Change Job script code [Spre]
function script_JobChange(ALua : TLua) : integer;
var
	AChara : TCharacter;
begin
	if (lua_gettop(ALua) = 1) then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			AChara.JID := lua_tointeger(ALua, 1);
			AChara.JobLv := 0;
		end else
		begin
			luaL_error(ALua,'script SetJob syntax error');
		end;
	end;
	Result := 0;
end;

//Special commands here
function script_get_charaname(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
begin
	Result := 1; //we are going to return 1 result
	if (lua_gettop(ALua) = 0) then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			lua_pushstring(ALua,PChar(AChara.Name));
		end;
	end else
	begin
		luaL_error(ALua,'script PcName syntax error');
	end;
end;

//Random usage for testing.  Will take its string and output it on
//the console.
function lua_print(ALua : TLua) : integer; cdecl;
var
	i, n: Integer;
begin
	n := lua_gettop(ALua);
	for i := 1 to n do
	begin
		if i > 1 then
			Write(#9);
		if lua_isstring(ALua, i) then
			Write(lua_tostring(ALua, i))
		else
			Write(Format('%s:%p', [lua_type(ALua, i), lua_topointer(ALua, i)]));
	end;
	WriteLn;
	Result := 0;
end;

end.
