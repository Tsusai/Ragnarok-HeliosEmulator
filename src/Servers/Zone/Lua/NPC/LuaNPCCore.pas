unit LuaNPCCore;

interface

uses
	Character,
	LuaCoreRoutines;

	procedure RunLuaNPCScript(
		var AChara : TCharacter;
		const LuaFunc : string
	);
	procedure SetCharaToLua(var AChara : TCharacter);
	function GetCharaFromLua(var ALua : TLua; var AChara : TCharacter) : boolean;

implementation
uses
	Main,
	LuaPas;


procedure RunLuaNPCScript(
	var AChara : TCharacter;
	const LuaFunc : string
);
begin
	//Set the player's lua thread
	MakeLuaThread(MainProc.ZoneServer.NPCLua,AChara.LuaInfo);
	//Set the character pointer to the global list
	SetCharaToLua(AChara);
	//Get the function
	lua_getglobal(AChara.LuaInfo.Lua,PChar(LuaFunc));
	//run the function
	lua_resume(AChara.LuaInfo.Lua,0);
	//Terminate the thread
	TerminateLuaThread(AChara.LuaInfo);
end;

//Pushes the Character Pointer onto the global array of the lua
//This is meant so that when a character executes a script, we can retrieve the TCharacter object
procedure SetCharaToLua(var AChara : TCharacter);
begin
	lua_pushlightuserdata(AChara.LuaInfo.Lua, @AChara); // Push pointer for the character
	lua_setglobal(AChara.LuaInfo.Lua, 'character'); // Tell Lua to set it as global
end;

//Pulls the Character object pointer from the lua global array.
function GetCharaFromLua(var ALua : TLua; var AChara : TCharacter) : boolean;
var
	PCharacter : ^TCharacter;
begin
	Result := false;
	lua_getglobal(ALua, 'character');
	PCharacter := lua_topointer(ALua, -1);
	if Assigned(PCharacter) then
	begin
		AChara := PCharacter^;
		Result := true;
	end;
	//lua_pop(ALua, 1);
end;

end.
