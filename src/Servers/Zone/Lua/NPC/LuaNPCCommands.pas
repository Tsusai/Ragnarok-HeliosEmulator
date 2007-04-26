unit LuaNPCCommands;

interface

uses
	LuaCoreRoutines;

function LoadNPCCommands(var ALua : TLua) : boolean;

implementation
uses
	LuaPas,
	SysUtils;

function addnpc(ALua : TLua) : integer; cdecl; forward;
function addwarp(ALua : TLua) : integer; cdecl; forward;
function lua_print(ALua : TLua) : integer; cdecl; forward;

const
	NPCCommandCount = 3;
	
const
	//"Function name in lua" , Delphi function name
	NPCCommandList : array [1..NPCCommandCount] of lual_reg = (
		(name:'addnpc';func:addnpc),
		(name:'addwarp';func:addwarp),
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
//Dummy
function addwarp(ALua : TLua) : integer; cdecl;
begin
	//Get Data
	//Create TNPC
	//Assign Data from stack to TNPC
	Writeln('Warp Loaded');
	Result := 0;
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