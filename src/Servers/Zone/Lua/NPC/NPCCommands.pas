unit NPCCommands;

//[2007/04/23] Tsusai - Changed lua filename
interface

uses
	LuaPas;


function LoadNPCCommands(var ALua : Plua_state) : boolean;
function addnpc(ALua : Plua_state) : integer; cdecl;
function addwarp(ALua : Plua_state) : integer; cdecl;
function lua_print(ALua : Plua_state) : integer; cdecl;

const
	NPCCommandCount = 3;

const
	//"Function name in lua" , Delphi function name
	NPCCommandList : array [1..NPCCommandCount] of lual_reg = (
		(name:'addnpc';func:addnpc),
		(name:'addwarp';func:addwarp),
		(name:'print';func:lua_print)
	);

implementation
uses
	SysUtils;

//[2007/04/23] Tsusai - Added result
function LoadNPCCommands(var ALua : Plua_state) : boolean;
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
function addnpc(ALua : Plua_state) : integer; cdecl;
begin
	//Get Data
	//Create TNPC
	//Assign Data from stack to TNPC
	Writeln('NPC Loaded');
	Result := 0;
end;

//[2007/04/23] Tsusai - Added result
function addwarp(ALua : Plua_state) : integer; cdecl;
begin
	//Get Data
	//Create TNPC
	//Assign Data from stack to TNPC
	Writeln('Warp Loaded');
	Result := 0;
end;

function lua_print(ALua : Plua_state) : integer; cdecl;
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