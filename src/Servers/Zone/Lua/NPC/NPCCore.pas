unit NPCCore;

//[2007/04/23] Tsusai - Changed lua unit
//[2007/04/23] Tsusai - Changed MakeCharacterLuaThread to
//	MakeLuaThread
//[2007/04/23] Tsusai - 
interface

uses
	LuaPas;

procedure MakeLuaThread(
	var SourceLua : Plua_state;
	var DestLua   : Plua_state;
	CharaID : LongWord = 0
);

procedure LuaSetup(var ALua : Plua_state; const LuaFile : string);
procedure RunLuaScript(var ALua : Plua_state; Const LuaFile : String);

implementation
uses
	NPCCommands;

procedure RunLuaScript(var ALua : Plua_state; Const LuaFile : String);
begin
	//Load the script
	if luaL_loadfile(ALua, PChar(LuaFile)) <> 0 then //0 = no errors
	begin
		WriteLn(lua_tostring(ALua, -1));
		lua_pop(ALua, 1); //Remove the error string
	end;

	//Executes the script
	if lua_pcall(ALua,0,0,0) <> 0 then //0 = no errors
	begin
		WriteLn(lua_tostring(ALua, -1));
		lua_pop(ALua, 1); //Remove the error string
	end;
end;

procedure MakeLuaThread(
	var SourceLua : Plua_state;
	var DestLua   : Plua_state;
	CharaID : LongWord = 0
);
begin
	if DestLua <> nil then
	begin
		lua_close(DestLua); //close existing if it exists
	end;

	DestLua := lua_newthread(SourceLua);
	lua_pushliteral(DestLua, 'char_id'); // Push global key for char_id
	lua_pushnumber(DestLua, CharaID); // Push value for char_id
	lua_rawset(DestLua,LUA_GLOBALSINDEX); // Tell Lua to set char_id as a global var

end;

procedure LuaSetup(var ALua : Plua_state; const LuaFile : string);
begin
	ALua := lua_open;

	luaL_openlibs(ALua);

	LoadNPCCommands(ALua);

	lua_pushliteral(ALua, 'char_id'); // Push global key for char_id
	lua_pushnumber(ALua, 0); // Push value for char_id
	lua_rawset(ALua,LUA_GLOBALSINDEX); // Tell Lua to set char_id as a global var

	RunLuaScript(ALua,LuaFile);
end;

end.
