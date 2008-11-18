//Contains all script commands used by the lua system
//Note the command lual_argcheck will cause the procedure it is in to call exit;
//So be aware of your code!

unit LuaNPCCommands;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
	LuaCoreRoutines,
	LuaTypes;

function LoadNPCCommands(var ALua : TLua) : boolean;

implementation
uses
	//RTL
	SysUtils,
	Types,
	//Project
	Character,
	GameConstants,
	LuaNPCCore,
	LuaPas,
	Main,
	Math,
	Item,
	ItemInstance,
	NPC,
	PacketTypes,
	ZoneSend,
	LuaVarConstants,
	ZoneInterCommunication
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
function script_DropItem(ALua : TLua) : integer; cdecl; forward;
function script_GetItemQuantity(ALua : TLua) : integer; cdecl; forward;
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
function script_ResetLook(ALua	:TLua)	:	Integer; cdecl;	forward;
function script_Input(ALua : TLua) : integer; cdecl; forward;
function script_InputStr(ALua : TLua) : integer; cdecl; forward;
function script_BroadcastInMap(ALua : TLua) : integer; cdecl; forward;
//Special Commands
function script_get_charaname(ALua : TLua) : integer; cdecl; forward;
function lua_print(ALua : TLua) : integer; cdecl; forward;

const
	NPCCommandCount = 39;

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
		(name:'dropitem';func:script_DropItem),
		(name:'getitemquantity';func:script_GetItemQuantity),
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
		(name:'ResetLook';func:script_ResetLook),
		(name:'input';func:script_Input),
		(name:'inputstr';func:script_InputStr),
		(name:'broadcastinmap';func:script_BroadcastInMap),
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
//[2008/10/18] Tsusai - Implemented lua argchecks
function CheckLuaNPCSyntax(ALua : TLua) : boolean;
var
	ParamCount : word;
begin
	//Assume false
	Result := false;
	ParamCount := lua_gettop(ALua);
	//Check parameter count
	if not ParamCount in [9..10] then lual_error(ALua,'Invalid number of NPC Script parameters');
	//Check parameters
	lual_argcheck(ALua,Lua_isNonNumberString(ALua,1),1,'NPC Map parameter must be a string');
	lual_argcheck(ALua,lua_isNonNumberString(ALua,2),2,'NPC Name parameter must be a string');
	lual_argcheck(ALua,lua_isposnumber(ALua,3),3,'NPC Sprite parameter must be a integer');
	lual_argcheck(ALua,lua_isposnumber(ALua,4),4,'NPC X Coord parameter must be a integer');
	lual_argcheck(ALua,lua_isposnumber(ALua,5),5,'NPC Y Coord parameter must be a integer');
	lual_argcheck(ALua,lua_isposnumber(ALua,6),6,'NPC Direction parameter must be a integer');
	lual_argcheck(ALua,lua_isposnumber(ALua,7),7,'NPC X Radius parameter must be a integer');
	lual_argcheck(ALua,lua_isposnumber(ALua,8),8,'NPC Y Radius parameter must be a integer');
	lual_argcheck(ALua,Lua_isNonNumberString(ALua,9),9,'NPC Function Name parameter must be a string');
	//Optional Ontouch
	if ParamCount = 10 then lual_argcheck(ALua,Lua_isNonNumberString(ALua,10),10,'NPC OnTouch Function Name parameter must be a string');

	//Made it through, set to true
	Result := not Result;
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
//[2008/10/18] Tsusai - Implemented lua argchecks
function CheckLuaWarpSyntax(
	ALua : TLua
) : boolean;
var
	ParamCount : word;
begin
	//Assume false, since argchecks will cause Exit; to happen....somehow
	Result := False;
	//Validate
	ParamCount := lua_gettop(ALua);
	if not ParamCount = 6 then lual_error(ALua,'Invalid number of NPC Warp parameters');
	lual_argcheck(ALua,Lua_isNonNumberString(ALua,1),1,'Warp Map parameter must be a string');
	lual_argcheck(ALua,Lua_isNonNumberString(ALua,2),2,'Warp Name parameter must be a string');
	lual_argcheck(ALua,lua_isposnumber(ALua,3),3,'Warp X Coord parameter must be a integer');
	lual_argcheck(ALua,lua_isposnumber(ALua,4),4,'Warp Y Coord parameter must be a integer');
	lual_argcheck(ALua,lua_isposnumber(ALua,5),5,'Warp X Radius parameter must be a integer');
	lual_argcheck(ALua,lua_isposnumber(ALua,6),6,'Warp Y Radius parameter must be a integer');
	//Made it through, set true
	Result := NOT Result;
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
	Result := 0;
	//Validate
	if not ParamCount = 3 then lual_error(ALua,'Invalid number of NPC moveto parameters');
	lual_argcheck(ALua,Lua_isNonNumberString(ALua,1),1,'NPC moveto map parameter must be a string');
	lual_argcheck(ALua,lua_isposnumber(ALua,2),2,'NPC moveto X coord parameter must be a integer');
	lual_argcheck(ALua,lua_isposnumber(ALua,3),3,'NPC moveto Y coord parameter must be a integer');
	//Run if validated
	if GetCharaFromLua(ALua,AChara) then
	begin
		ZoneSendWarp(
			AChara,
			lua_tostring(ALua,1),
			lua_tointeger(ALua,2),
			lua_tointeger(ALua,3)
		);
	end;
	//Tsusai Oct 11 2008: We can't end the script.  There are some stupid Aegis
	//scripts that still continue doing behind the scenes stuff after warping.
	//The script maker must verify it ends on its own.  However, we warked, so,
	//assume we aren't running a script.  The previous script should end quickly
	//anyways

	//lua_yield(ALua,0);//end the script
	AChara.ScriptStatus := SCRIPT_NOTRUNNING;
end;

//dialog "this is my text"
//NPC Dialog to the character
function script_dialog(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
begin
	Result := 0;
	if not (lua_gettop(ALua) = 1) then luaL_error(ALua,'NPC Dialog parameter count error');
	lual_argcheck(ALua,lua_isstring(ALua,1),1,'NPC Dialog must be string with quotes');
	if GetCharaFromLua(ALua,AChara) then
	begin
		SendNPCDialog(AChara,AChara.ScriptBeing.ID,lua_tostring(ALua,1));
	end;
end;

//wait()
//Sends the next button to the client
function script_wait(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
begin
	Result := 0;
	if not (lua_gettop(ALua) = 0) then luaL_error(ALua,'NPC wait must not have any parameters');
	if GetCharaFromLua(ALua,AChara) then
	begin
		//Send the next button
		SendNPCNext(AChara,AChara.ScriptBeing.ID);
		//Pause lua.  Tell it to wait on 0 parameters in return.
		AChara.ScriptStatus := SCRIPT_YIELD_WAIT;
		Result := lua_yield(ALua,0);
	end;
end;

//close()
//Sends the close button to the client and flags the script user as not running.
function script_close(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
begin
	Result := 0;
	if not lua_gettop(ALua) = 0 then luaL_error(ALua,'Close command error');
	if GetCharaFromLua(ALua,AChara) then
	begin
		SendNPCClose(AChara,AChara.ScriptBeing.ID);
		//Make lua stop
		AChara.ScriptStatus := SCRIPT_NOTRUNNING;
	end;
end;

//checkpoint("map",x,y)
//Sets the specified save point to the character
function script_checkpoint(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
begin
	Result := 0;
	//Validate
	if not ParamCount = 3 then lual_error(ALua,'Invalid number of NPC checkpoint parameters');
	lual_argcheck(ALua,Lua_isNonNumberString(ALua,1),1,'NPC checkpoint map parameter must be a string');
	lual_argcheck(ALua,lua_isposnumber(ALua,2),2,'NPC checkpoint X coord parameter must be a integer');
	lual_argcheck(ALua,lua_isposnumber(ALua,3),3,'NPC checkpoint Y coord parameter must be a integer');
	//Save data
	if GetCharaFromLua(ALua,AChara) then
	begin
		AChara.SaveMap := lua_tostring(ALua,1);
		AChara.SavePoint := Point(
		lua_tointeger(ALua,2), lua_tointeger(ALua,3));
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
	idx : word;

begin
	Result := 0;
	MenuString := '';
	//Validate
	ParamCount := lua_gettop(ALua);
	if not ParamCount > 0 then lual_error(ALua,'NPC Menu needs at least one parameter');
	for idx := 1 to ParamCount do lual_argcheck(ALua,lua_isString(ALua,idx),idx,'NPC Menu parameter must be a integer or string');
	//Run
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
		SendNPCMenu(AChara,AChara.ScriptBeing.ID,MenuString);
		AChara.ScriptStatus := SCRIPT_YIELD_MENU;
		Result := lua_yield(ALua,1);
	end;
end;

//getvar(key)
//Returns a character variable.
function script_getcharavar(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
	Value : String;
	Key : string;
	KeyConst : Integer;
	VarType : Byte;
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
						Value := TClientLink(AChara.ClientInfo.Data).AccountLink.Name;
						lua_pushstring(ALua, PChar(Value));
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
				Value := TThreadLink(AChara.ClientInfo.Data).DatabaseLink.Character.GetVariable(AChara,Key, VarType);
				//Check variable type and push appropriate type back to script.
				case VarType of
					CHARAVAR_STRING :
						begin
							lua_pushstring(ALua, PChar(Value));
						end;
					else
						begin
							lua_pushinteger(ALua, StrToIntDef(Value, 0));
						end;
				end;
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
	Key : string;
	Value : String;
	AType : Byte;
begin
	//Returns 0 results
	Result := 0;
	if (lua_gettop(ALua) = 2) and
		(lua_isString(ALua,1)) then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			Key := lua_tostring(ALua, 1);
			if lua_isnumber(ALua, 2) then
			begin
				AType := CHARAVAR_INTEGER;
			end else
			begin
      	AType := CHARAVAR_STRING;
      end;
			Value := lua_tostring(ALua, 2);

			TThreadLink(AChara.ClientInfo.Data).DatabaseLink.Character.SetVariable(AChara,Key,Value, AType);
		end;
	end else
	begin
		luaL_error(ALua,'script setvar syntax error');
	end;
end;

//SetItem - Apparently is set variable..
//so I just made it wrapper of setvar
function script_SetItem(ALua : TLua) : Integer;
begin
	Result := script_setcharavar(ALua);
end;

//gives the character an item of quantity
function script_GetItem(ALua : TLua) : Integer;
var
	AChara : TCharacter;
	AnItem : TItemInstance;
	ItemID	: Word;
begin
	Result := 0;
	//check number of parameters
	if lua_gettop(ALua) = 2 then
	begin
		//check to make sure it was a character who executed this script
		if GetCharaFromLua(ALua,AChara) then
		begin

			ItemID := 0;
			//type check first parameter to see if we're dealing with an item id or name
			if lua_isnumber(ALua, 1) then
			begin
				//if we're dealing with an id, we try to find the item.
				if	TThreadLink(AChara.ClientInfo.Data).DatabaseLink.Items.Find(
							lua_tointeger(ALua, 1)
						) then
				begin
					ItemID := lua_tointeger(ALua, 1);
				end;
			//else, if we're dealing with a name, we try to find the id.
			end else
			begin
				ItemID :=  TThreadLink(AChara.ClientInfo.Data).DatabaseLink.Items.Find(lua_tostring(ALua, 1));
			end;
			//if an id was found, we check the second parameter
			if ItemID > 0 then
			begin
				//type check the second parameter, quantity
				if lua_isnumber(ALua, 2) then
				begin
					//since we passed all checks, create the item instance and add it to the inventory.
					AnItem := TItemInstance.Create;
					AnItem.Item := TItem.Create;
					AnItem.Item.ID := ItemID;
					AnItem.Quantity := EnsureRange(lua_tointeger(ALua, 2), 1, High(Word));
					AnItem.Identified := true;
					TThreadLink(AChara.ClientInfo.Data).DatabaseLink.Items.Load(AnItem.Item);
					AChara.Inventory.Add(AnItem);
				end else
				begin
					luaL_error(ALua,'script getitem syntax error, second parameter should be numeric');
				end;
			end else
			begin
				luaL_error(ALua,'script getitem syntax error, item not found');
			end;
		end;
	end else
	begin
		luaL_error(ALua,'script getitem syntax error, incorrect number of parameters');
	end;
end;

function script_GetItemQuantity(ALua : TLua) : Integer;
var
	AChara : TCharacter;
	ItemID	: Word;
begin
	Result := 0;
	//check number of parameters
	if lua_gettop(ALua) = 1 then
	begin
		//check to make sure it was a character who executed this script
		if GetCharaFromLua(ALua,AChara) then
		begin

			ItemID := 0;
			//type check first parameter to see if we're dealing with an item id or name
			if lua_isnumber(ALua, 1) then
			begin
				//if we're dealing with an id, we try to find the item.
				if	TThreadLink(AChara.ClientInfo.Data).DatabaseLink.Items.Find(
							lua_tointeger(ALua, 1)
						) then
				begin
					ItemID := lua_tointeger(ALua, 1);
				end;
			//else, if we're dealing with a name, we try to find the id.
			end else
			begin
				ItemID :=  TThreadLink(AChara.ClientInfo.Data).DatabaseLink.Items.Find(lua_tostring(ALua, 1));
			end;
			//if an id was found, we check the second parameter
			if ItemID > 0 then
			begin
					lua_pushinteger(ALua, AChara.Inventory.AmountOf(ItemId));
			end else
			begin
				luaL_error(ALua,'script getitemquantity syntax error, item not found');
			end;
		end;
	end else
	begin
		luaL_error(ALua,'script getitemquantity syntax error, incorrect number of parameters');
	end;
end;

function script_DropItem(ALua : TLua) : Integer;
var
	AChara : TCharacter;
	ItemID	: Word;
	Quantity : Word;
begin
	Result := 0;
	//check number of parameters
	if lua_gettop(ALua) = 2 then
	begin
		//check to make sure it was a character who executed this script
		if GetCharaFromLua(ALua,AChara) then
		begin

			ItemID := 0;
			//type check first parameter to see if we're dealing with an item id or name
			if lua_isnumber(ALua, 1) then
			begin
				//if we're dealing with an id, we try to find the item.
				if	TThreadLink(AChara.ClientInfo.Data).DatabaseLink.Items.Find(
							lua_tointeger(ALua, 1)
						) then
				begin
					ItemID := lua_tointeger(ALua, 1);
				end;
			//else, if we're dealing with a name, we try to find the id.
			end else
			begin
				ItemID :=  TThreadLink(AChara.ClientInfo.Data).DatabaseLink.Items.Find(lua_tostring(ALua, 1));
			end;
			//if an id was found, we check the second parameter
			if ItemID > 0 then
			begin
				//type check the second parameter, quantity
				if lua_isnumber(ALua, 2) then
				begin
					Quantity := EnsureRange(lua_toInteger(ALua, 2), 1, High(Word));
					//since we passed all checks, create the item instance and add it to the inventory.
					AChara.Inventory.Remove(ItemID, Quantity);
				end else
				begin
					luaL_error(ALua,'script dropitem syntax error, second parameter should be numeric');
				end;
			end else
			begin
				luaL_error(ALua,'script dropitem syntax error, item not found');
			end;
		end;
	end else
	begin
		luaL_error(ALua,'script dropitem syntax error, incorrect number of parameters');
	end;
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
	//Validate
	if not (lua_gettop(ALua) = 1) then lual_error(ALua, 'NPC getgold error, there must be one parameter');
	lual_argcheck(ALua, lua_isposnumber(ALua,1), 1, 'NPC getgold parameter must be a positive integer.');
	//Run
	if GetCharaFromLua(ALua,AChara) then
	begin
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

//dropgold
function script_dropgold(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
	Zeny : LongInt;
begin
	//Returns 0 results
	Result := 0;
	//Validate
	if not (lua_gettop(ALua) = 1) then lual_error(ALua, 'NPC dropgold error, there must be one parameter');
	lual_argcheck(ALua, lua_isposnumber(ALua,1), 1, 'NPC dropgold parameter must be a positive integer.');
	//Run
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


//getexp(value)
//Gives or takes base exp to/from the character
function script_getexp(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
begin
	//Returns 0 results
	Result := 0;
	if not (lua_gettop(ALua) = 1) then lual_error(ALua, 'NPC getexp error, there must be one parameter');
	lual_argcheck(ALua, lua_isnumber(ALua,1), 1, 'NPC getexp parameter must be a integer.');
	if GetCharaFromLua(ALua,AChara) then
	begin
		AChara.BaseEXP := AChara.BaseEXP +
					Cardinal(EnsureRange(lua_tointeger(ALua, 1),0,High(Integer)));
	end;
end;

//getJexp(value)
//Gives or takes job exp to/from the character
function script_getJexp(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
begin
	//Returns 0 results
	Result := 0;
	if not (lua_gettop(ALua) = 1) then lual_error(ALua, 'NPC getjexp error, there must be one parameter');
	lual_argcheck(ALua, lua_isnumber(ALua,1), 1, 'NPC getjexp parameter must be a integer.');
	if GetCharaFromLua(ALua,AChara) then
	begin
		AChara.JobEXP := AChara.JobEXP +
					Cardinal(EnsureRange(lua_tointeger(ALua, 1),0,High(Integer)));
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
	if not (lua_gettop(ALua) = 1) then lual_error(ALua, 'NPC HPDrain error, there must be one parameter');
	lual_argcheck(ALua, lua_isposnumber(ALua,1), 1, 'NPC HPDrain parameter must be a positive integer.');
	if GetCharaFromLua(ALua,AChara) then
	begin
		Percentage := EnsureRange(lua_tointeger(ALua, 1),0,100);
		if AChara.HP > 0 then
			AChara.HPPercent := AChara.HPPercent - Percentage;
	end;
end;

//Gain some % HP for player
function script_HpHeal(ALua : TLua) : integer; cdecl;
var
	AChara : TCharacter;
	Percentage : Byte;
begin
	Result := 0;
	if not (lua_gettop(ALua) = 1) then lual_error(ALua, 'NPC HPHeal error, there must be one parameter');
	lual_argcheck(ALua, lua_isposnumber(ALua,1), 1, 'NPC HPHeal parameter must be a positive integer.');

	if GetCharaFromLua(ALua,AChara) then
	begin
		Percentage := EnsureRange(lua_tointeger(ALua, 1),0,100);
		if AChara.HP > 0 then
			AChara.HPPercent := AChara.HPPercent + Percentage;
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
			AChara.ChangeJob(EnsureRange(lua_tointeger(ALua, 1), 0, high(word)));
		end;
	end else
	begin
		luaL_error(ALua,'script ChangeJob syntax error');
	end;
	Result := 0;
end;

//Reset Look
//Code I am working on to reset ones look to 0 [Spre]
function script_ResetLook(ALua	:TLua)	:	Integer;	cdecl;
var
	AChara	:	TCharacter;
begin
	Result := 0;
	if GetCharaFromLua(ALua,AChara) then
	begin
//		AChara.ResetLook;
	end;
	luaL_error(ALua,'script Reset Look syntax error');
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

//Input a number
function script_Input(ALua : TLua) : integer;
var
	AChara : TCharacter;
begin
	Result := 0;
	if (lua_gettop(ALua) = 0) then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			SendNPCInput(
				AChara,
				AChara.ScriptBeing.ID
			);
			AChara.ScriptStatus := SCRIPT_YIELD_INPUT;
			Result := lua_yield(ALua,1);
		end;
	end else
	begin
		luaL_error(ALua,'script Input syntax error');
	end;
end;

//Input of a string
function script_InputStr(ALua : TLua) : integer;
var
	AChara : TCharacter;
begin
	Result := 0;
	if (lua_gettop(ALua) = 0) then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			SendNPCInputStr(
				AChara,
				AChara.ScriptBeing.ID
			);
			AChara.ScriptStatus := SCRIPT_YIELD_INPUT;
			Result := lua_yield(ALua,1);
		end;
	end else
	begin
		luaL_error(ALua,'script Inputstr syntax error');
	end;
end;

//Broadcast in current map
function script_BroadcastInMap(ALua : TLua) : integer;
var
	AChara : TCharacter;
begin
	Result := 0;
	if (lua_gettop(ALua) = 1) then
	begin
		if GetCharaFromLua(ALua,AChara) then
		begin
			ZoneSendGMCommandtoInter(
				MainProc.ZoneServer.ToInterTCPClient,
				AChara.AccountID,
				AChara.ID,
				'#BroadCastLN ' + lua_tostring(ALua,1)
			);
		end;
	end else
	begin
		luaL_error(ALua,'script Inputstr syntax error');
	end;
end;
end.
