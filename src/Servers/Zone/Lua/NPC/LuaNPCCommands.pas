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

//[2007/04/23] Tsusai - Added result
//Dummy
function addnpc(ALua : TLua) : integer; cdecl;
begin
	//Get Data
	//Create TNPC
	//Assign Data from stack to TNPC
	Writeln('NPC Loaded');
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
	Result := false;
	ParamCount := lua_gettop(ALua);
	if (ParamCount <> 6)  then
	begin
		lual_error(ALua,'NPC Warp: Incorrect number of parameters');
		exit;
	end;
	if not Boolean(Lua_isNonNumberString(ALua,1)) then
	begin
		lual_error(ALua,'NPC Warp: First parameter (mapname) must be a string');
		exit;
	end;
	if not Boolean(lua_isstring(ALua,2)) then
	begin
		lual_error(ALua,'NPC Warp: Second parameter (name) must be a string or number');
		exit;
	end;
	if not Boolean(lua_isnumber(ALua,3)) then
	begin
		lual_error(ALua,'NPC Warp: Third parameter (X) must be a integer');
		exit;
	end;
	if not Boolean(lua_isnumber(ALua,4)) then
	begin
		lual_error(ALua,'NPC Warp: Fourth parameter (Y) must be a integer');
		exit;
	end;
	if not Boolean(lua_isnumber(ALua,5)) then
	begin
		lual_error(ALua,'NPC Warp: Fifth parameter (X Radius) must be a integer');
		exit;
	end;
	if not Boolean(lua_isnumber(ALua,6)) then
	begin
		lual_error(ALua,'NPC Warp: Sixth parameter (Y Radius) must be a integer');
		exit;
	end;
	Result := true;
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
	AWarpNPC.XRadius := lua_tointeger(ALua,5);
	AWarpNPC.YRadius := lua_tointeger(ALua,6);
	MainProc.ZoneServer.NPCList.AddObject(AWarpNPC.ID,AWarpNPC);
end; 

//[2007/04/23] Tsusai - Added result
//warp("map","name",x,y,xradius,yradius)
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
//hiddenwarp("map","name",x,y,xradius,yradius)
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
