unit LuaNPCCommands;

interface

uses
	LuaCoreRoutines;

function LoadNPCCommands(var ALua : TLua) : boolean;

implementation
uses
	GameConstants,
	LuaPas,
	NPC,
	Types,
	Main,
	SysUtils;

//Forward declarations of delphi procedures that are added to the lua engine.
function addnpc(ALua : TLua) : integer; cdecl; forward;
function addwarp(ALua : TLua) : integer; cdecl; forward;
function addhiddenwarp(ALua : TLua) : integer; cdecl; forward;
function lua_print(ALua : TLua) : integer; cdecl; forward;

const
	NPCCommandCount = 4;

const
	//"Function name in lua" , Delphi function name
	NPCCommandList : array [1..NPCCommandCount] of lual_reg = (
		(name:'npc';func:addnpc),
		(name:'warp';func:addwarp),
		(name:'hiddenwarp';func:addhiddenwarp),
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
			Point(lua_tointeger(ALua,4) , lua_tointeger(ALua,6));
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
