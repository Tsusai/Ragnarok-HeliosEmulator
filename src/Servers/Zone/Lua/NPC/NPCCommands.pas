unit NPCCommands;

interface

uses
	Lua;


function LoadNPCCommands(ALua : Plua_state) : boolean;
function addnpc(ALua : Plua_state) : integer; cdecl;
function addwarp(ALua : Plua_state) : integer; cdecl;


const
	NPCCommandCount = 2;

const
	//"Function name in lua" , Delphi function name
	NPCCommandList = array [1..NPCCommandCount] of lual_reg = (
		(name:'addnpc',func:addnpc),
		(name:'addwarp',func:addwarp)
	);

implementation

function LoadNPCCommands(ALua : Plua_state) : boolean;
var
	idx : integer;
begin
	for idx := 1 to NPCCommandList do
	begin
		lua_register(
			ALua, 
			NPCCommandList.name,
			NPCCommandList.func
		);
	end;
end;

function addnpc(ALua : Plua_state) : integer; cdecl;
begin
	//Get Data
	//Create TNPC
	//Assign Data from stack to TNPC
	Writeln('NPC Loaded');
end;

function addwarp(ALua : Plua_state) : integer; cdecl;
begin
	//Get Data
	//Create TNPC
	//Assign Data from stack to TNPC
	Writeln('Warp Loaded');
end;