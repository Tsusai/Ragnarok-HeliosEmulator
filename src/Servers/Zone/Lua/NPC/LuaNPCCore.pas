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
	function GetCharaFromLua(var ALua : TLua) : TCharacter;

implementation
uses
	Main,
	LuaPas,
	LuaNPCCommands;


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
	lua_resume(AChara.LuaInfo.Lua,0);
end;

//Pushes the Character Pointer onto the global array of the lua
//This is meant so that when a character executes a script, we can retrieve the TCharacter object
procedure SetCharaToLua(var AChara : TCharacter);
begin
	lua_pushlightuserdata(AChara.LuaInfo.Lua, @AChara); // Push pointer for the character
	lua_setglobal(AChara.LuaInfo.Lua, 'character'); // Tell Lua to set it as global
end;

//Pulls the Character object pointer from the lua global array.
function GetCharaFromLua(var ALua : TLua) : TCharacter;
var
	PCharacter : ^TCharacter;
begin
	lua_getglobal(ALua, 'character');
	PCharacter := lua_topointer(ALua, -1);
	if Assigned(PCharacter) then
	begin
		Result := PCharacter^;
	end else
	begin
		Writeln('Error retrieving a valid character data pointer!!!');
		Result := nil;
	end;
	lua_pop(ALua, 1);
end;

end.
