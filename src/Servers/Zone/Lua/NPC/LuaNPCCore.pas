unit LuaNPCCore;

interface

uses
	LuaCoreRoutines;

	procedure PrepLuaForNPCScripts(ALua : TLua; CharaID : integer = 0);

implementation
uses
	LuaPas,
	LuaNPCCommands;


procedure PrepLuaForNPCScripts(ALua : TLua; CharaID : integer = 0);
begin
	LoadNPCCommands(ALua);
	lua_pushliteral(ALua, 'char_id'); // Push global key for char_id
	lua_pushnumber(ALua, CharaID); // Push value for char_id
	lua_rawset(ALua,LUA_GLOBALSINDEX); // Tell Lua to set char_id as a global var
end;

end.
