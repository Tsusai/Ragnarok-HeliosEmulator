//Contains all script commands used by the lua system
//Note the command lual_argcheck will cause the procedure it is in to call exit;
//So be aware of your code!

unit LuaItemCommands;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
//	LuaCoreRoutines,
	LuaTypes;

function LoadItemCommands(var ALua : TLua) : boolean;

implementation


//Forward declarations of delphi procedures that are added to the lua engine.
//example function addnpc(ALua : TLua) : integer; cdecl; forward;

(*const
	ItemCommandCount = 1;*)

(*const
	//"Function name in lua" , Delphi function name
	//This assigns our procedures here, with the lua system
	ItemCommandList : array [1..ItemCommandCount] of lual_reg = (
		//Example
		(name:'npc';func:addnpc),
		(name:'warp';func:addwarp)

	);*)

//[2008/11/17] Tsusai - Added result
//Registers all the Item Commands into the Lua instance
function LoadItemCommands(var ALua : TLua) : boolean;
{var
	idx : integer;}
begin
{	for idx := 1 to ItemCommandCount do
	begin
		lua_register(
			ALua,
			ItemCommandList[idx].name,
			ItemCommandList[idx].func
		);
	end;}
	Result := true;
end;

end.
