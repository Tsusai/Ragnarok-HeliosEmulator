unit LuaNPCCore;

interface

uses
	LuaCoreRoutines;

	procedure PrepLuaForNPCScripts(ALua : TLua; CharaID : LongWord = 0);
	procedure SetCharaIDToLua(ALua : TLua; CharaID : LongWord);
	function GetCharaIDFromLua(ALua : TLua) : LongWord;

implementation
uses
	LuaPas,
	LuaNPCCommands;


//Takes the passed Lua and applicable character id, and
//loads the registers our delphi functions into it.
procedure PrepLuaForNPCScripts(ALua : TLua; CharaID : LongWord = 0);
begin
	//Register our npc commands
	LoadNPCCommands(ALua);
	//Push character ID into the global variables.
	SetCharaIDToLua(ALua,CharaID)
end;

procedure SetCharaIDToLua(ALua : TLua; CharaID : LongWord);
begin
	lua_pushliteral(ALua, 'char_id'); // Push global key for char_id
	lua_pushnumber(ALua, CharaID); // Push value for char_id
	lua_rawset(ALua,LUA_GLOBALSINDEX); // Tell Lua to set char_id as a global var
end;

function GetCharaIDFromLua(ALua : TLua) : LongWord;
begin
	lua_pushliteral(ALua, 'char_id');
	lua_rawget(ALua, LUA_GLOBALSINDEX);
	Result := lua_tointeger(ALua, -1);
	lua_pop(ALua, 1);
end;

end.
