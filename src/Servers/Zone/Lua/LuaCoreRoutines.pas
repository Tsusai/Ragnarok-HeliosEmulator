unit LuaCoreRoutines;

interface
uses
	LuaPas;

	Type TLua = Plua_state;

	Type TLuaInfo = record
		Lua   : TLua;
		LuaID : Integer;
		ParentLua     : TLua;
	end;

	procedure MakeLuaThread(
		var SourceLua : TLua;
		var DestLua   : TLuaInfo
	);

	procedure RunLuaScript(
		var ALua : TLua;
		Const LuaFile : String
	);

	procedure InitLuaState(
		var ALua : TLua
	);

	procedure TerminateLuaThread(
		const LuaInfo : TLuaInfo
	);

	procedure TerminateLua(
		var RootLuaOnly : TLua
	);


implementation

procedure MakeLuaThread(
	var SourceLua : TLua;
	var DestLua   : TLuaInfo
);
begin
	DestLua.Lua := lua_newthread(SourceLua);
	DestLua.LuaID := luaL_ref(SourceLua, LUA_REGISTRYINDEX);
	DestLua.ParentLua := SourceLua;
end;

procedure InitLuaState(var ALua : TLua);
begin
	ALua := lua_open;
	luaL_openlibs(ALua);
end;

procedure RunLuaScript(var ALua : TLua; Const LuaFile : String);
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

procedure TerminateLuaThread(
	const LuaInfo : TLuaInfo
);
begin
	luaL_unref(
		LuaInfo.ParentLua,
		LUA_REGISTRYINDEX,
		LuaInfo.LuaID
	);
end;

procedure TerminateLua(
	var RootLuaOnly : TLua
);
begin
	lua_close(RootLuaOnly);
end;

end.
