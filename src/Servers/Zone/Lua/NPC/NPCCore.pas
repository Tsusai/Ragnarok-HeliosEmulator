unit NPCCore;

interface

uses
	lua,
	Character,
	NPCCommands;

procedure MakeCharacterLuaThread(
	ACharacter : TCharacter;
	var GlobalLua : Plua_state
);

procedure LuaSetup(var ALua : Plua_state; const LuaFile : string);
procedure LuaCleanup(var ALua : Plua_state);

implementation
uses
	lualib,
	lauxlib;

procedure MakeCharacterLuaThread(
	ACharacter : TCharacter;
	var GlobalLua : Plua_state
);
begin
	try
		lua_close(ACharacter.Lua); //close existing if it exists
	except
		//no clue if it would err
	end;
	
	ACharacter.Lua := lua_newthread(GlobalLua);
	lua_pushliteral(ACharacter.Lua, 'char_id'); // Push global key for char_id
	lua_pushnumber(ACharacter.Lua, ACharacter.CID); // Push value for char_id
	lua_rawset(ACharacter.Lua,LUA_GLOBALSINDEX); // Tell Lua to set char_id as a global var
end;


procedure LoadLuaLibs(var ALua : Plua_state);
begin
	luaopen_io(ALua);
	luaopen_base(ALua);
	luaopen_table(ALua);
	luaopen_string(ALua);
	luaopen_math(ALua);
end;

procedure LuaSetup(var ALua : Plua_state; const LuaFile : string);
begin
	ALua := lua_open;

	LoadLuaLibs(ALua);

	LoadNPCCommands(ALua);

	if luaL_loadfile(ALua, PChar(LuaFile)) <> 0 then //0 = no errors
	begin
		WriteLn(lua_tostring(ALua, -1));
		lua_pop(ALua, 1); //Remove the error string
	end;

	if lua_pcall(ALua,0,0,0) <> 0 then //0 = no errors
	begin
		WriteLn(lua_tostring(ALua, -1));
		lua_pop(ALua, 1); //Remove the error string
	end;

end;

procedure LuaCleanup(var ALua : Plua_state);
begin
	lua_close(ALua);
end;

end.