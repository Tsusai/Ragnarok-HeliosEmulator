unit LuaNPCCommands;

interface

uses
	LuaCoreRoutines;

function LoadNPCCommands(var ALua : TLua) : boolean;

implementation
uses
	LuaPas,
	NPC,
	Types,
	Main,
	SysUtils;

function addnpc(ALua : TLua) : integer; cdecl; forward;
function addwarp(ALua : TLua) : integer; cdecl; forward;
function lua_print(ALua : TLua) : integer; cdecl; forward;

const
	NPCCommandCount = 3;

const
	//"Function name in lua" , Delphi function name
	NPCCommandList : array [1..NPCCommandCount] of lual_reg = (
		(name:'npc';func:addnpc),
		(name:'warp';func:addwarp),
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

//[2007/04/23] Tsusai - Added result
function addwarp(ALua : TLua) : integer; cdecl;
var
	AWarpNPC : TWarpNPC;
	ParamCount : Word;
	OnTouchFunction : string;
begin
	Result := 0;
	OnTouchFunction := '';
	ParamCount := lua_gettop(ALua);
	//warp("new_1-1","Start-Castle",148,112,2,3,"new_1-2",100,9)
	if not (ParamCount in [9..10])  then
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
	if not Boolean(Lua_isNonNumberString(ALua,7)) then
	begin
		lual_error(ALua,'NPC Warp: Seventh parameter (Destination Map) must be a string');
		exit;
	end;
	if not Boolean(lua_isnumber(ALua,8)) then
	begin
		lual_error(ALua,'NPC Warp: Eighth parameter (Destination X) must be a integer');
		exit;
	end;
	if not Boolean(lua_isnumber(ALua,9)) then
	begin
		lual_error(ALua,'NPC Warp: Nineth parameter (Destination Y) must be a integer');
		exit;
	end;
	if ParamCount = 10 then
	begin
		if not Lua_isNonNumberString(ALua,10) then
		begin
			lual_error(ALua,'NPC Warp: Optional Tenth parameter (OnTouch function) must be a string');
			exit;
		end else
		begin
			OnTouchFunction := lua_tostring(ALua,10);
		end;
	end;

	AWarpNPC := TWarpNPC.Create(OnTouchFunction);
	AWarpNPC.Map := lua_tostring(ALua,1);
	AWarpNPC.Name := lua_tostring(ALua,2);
	AWarpNPC.Position :=
		Point(
			lua_tointeger(ALua,3),
			lua_tointeger(ALua,4)
		);
	AWarpNPC.XRadius := lua_tointeger(ALua,5);
	AWarpNPC.YRadius := lua_tointeger(ALua,6);
	AWarpNPC.DestMap := lua_tostring(ALua,7);
		AWarpNPC.DestPoint :=
		Point(
			lua_tointeger(ALua,8),
			lua_tointeger(ALua,9)
		);
	MainProc.ZoneServer.NPCList.AddObject(AWarpNPC.ID,AWarpNPC); 

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