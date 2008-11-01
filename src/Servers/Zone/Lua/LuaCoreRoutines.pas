//Wrapper to keep lua from using all the lua units in various units.  This way,
//lua is only required in the zone.
unit LuaCoreRoutines;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
uses
	LuaPas;

	//Wrapper for the lua state.
	Type TLua = Plua_state;

	//Used mainly for descending lua threads off a root one
	//so that it may be dereferenced later on and freed.
	Type TLuaInfo = record
		Lua       : TLua;
		LuaID     : Integer;
		ParentLua : TLua;
	end;

	function Lua_isNonNumberString(
		ALua : TLua;
		Index : word
	) : boolean;

	function Lua_toLongWord(
		var ALua : TLua;
		Index : byte
	) : LongWord;

	function Lua_isposnumber(
		ALua : TLua;
		Index : word
	) : boolean;

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
uses
	Math;

//Since 23235 is a valid number and string to lua, this routine checks for
//non pure number string checking
//345345 = false
//Test = true
//"test 123" = true
function Lua_isNonNumberString(
	ALua : TLua;
	Index : word
) : boolean;
begin
	Result := (Boolean(lua_isstring(ALua,Index)) and
	not Boolean(lua_isnumber(ALua,Index)));
end;


// Convert to LongWord
function Lua_toLongWord(
	var ALua : TLua; 
	Index : byte
) : LongWord;
begin
	Result := 
	EnsureRange(
		Round(lua_tonumber(ALua, Index)),
		Low(LongWord), High(LongWord)
	);
end;

// Check to see if it is a positive integer (including 0)
function Lua_isposnumber(
		ALua : TLua;
		Index : word
	) : boolean;
begin
	Result := false;
	if lua_isnumber(ALua,Index) then
	begin
		Result := (lua_tonumber(ALua,Index) >= 0);
	end;
end;


//Takes an existing lua, and makes a new execution thread for a
//descendant.  Also stores the parent's info so that it can
//be deferenced and freed.
//Also Creates a table environment with a reference to the sourcelua's global
//functions, variables etc, so that the creation of new globals do not
//becomed shared with the source lua or other descendant threads.
procedure MakeLuaThread(
	var SourceLua : TLua;
	var DestLua   : TLuaInfo
);
begin
	DestLua.Lua := lua_newthread(SourceLua);
	lua_pushvalue(SourceLua, -1);
	DestLua.LuaID := luaL_ref(SourceLua, LUA_REGISTRYINDEX);
	DestLua.ParentLua := SourceLua;
	//Create a local environment with a link to global environment via
	//    __index metamethod
	//This allows for thread only globals
	lua_newtable(SourceLua);
	lua_pushvalue(SourceLua, -1);
	lua_setmetatable(SourceLua, -2); //Set itself as metatable
	lua_pushvalue(SourceLua, LUA_GLOBALSINDEX);
	lua_setfield(SourceLua, -2, '__index');
	lua_setfenv(SourceLua, -2);
	lua_pop(SourceLua, 1);
end;

//Initializes a brand new lua, not for descendant threads.
procedure InitLuaState(var ALua : TLua);
begin
	ALua := lua_open;
	luaL_openlibs(ALua);
end;

//Tells a lua instance to load and execute a script file
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

//Using the stored parent info, we are able to dereference and free
//up a lua descendant thread.
procedure TerminateLuaThread(
	const LuaInfo : TLuaInfo
);
begin
	if not (LuaInfo.Lua = nil) then
	begin
		luaL_unref(
			LuaInfo.ParentLua,
			LUA_REGISTRYINDEX,
			LuaInfo.LuaID
		);
	end;
end;

//Closes out the head only lua.  Make sure descendant threads are
//taken care of first.
procedure TerminateLua(
	var RootLuaOnly : TLua
);
begin
	lua_close(RootLuaOnly);
end;

end.
