//------------------------------------------------------------------------------
//LuaCoreRoutines                                                          UNIT
//------------------------------------------------------------------------------
//	What it does -
//			Contains core routines for Lua.
//    Wrapper to keep lua from using all the lua units in various units.
//	 	This way, lua is only required in the zone.
//	Changes -
//  	[2008/12/10] RabidCh - Created header for documentation.
//------------------------------------------------------------------------------
unit LuaCoreRoutines;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
uses
	{Project}
	Character,
	{Lua}
	LuaTypes,
	LuaPas;

	const LUA_NPC_CORE_FILE = '/Core/NPCCore.lua';
	const LUA_ITEM_CORE_FILE = '/Core/ItemCore.lua';

	type TLuaEnv = (LUA_NPC, LUA_ITEM);

  // Additional checks for Lua values
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

	// Core Routines
	procedure MakeLuaThread(
		var SourceLua : TLua;
		var DestLua   : TLuaInfo
	);

	procedure LoadAndRunLuaScript(
		var ALua : TLua;
		Const LuaFile : String
	);

	procedure LuaRunPlayerScript(
		var AChara : TCharacter;
		const LuaMode : TLuaEnv;
		const LuaFunc : string
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

	procedure SetCharaToLua(var AChara : TCharacter);
	function GetCharaFromLua(var ALua : TLua; var AChara : PCharacter) : boolean;


implementation
uses
	{RTL/VCL}
	Globals,
	Terminal,
	{Project}
	LuaTCharacter,
	Main,
	Math,
	WinLinux;

//------------------------------------------------------------------------------
//Lua_isNonNumberString                                                 FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//			Checks for non-pure number strings since
//		23235 is a valid number and string to lua.
//		Examples:
//			345345 = false
//			Test = true
//			"test 123" = true
//	Changes -
//------------------------------------------------------------------------------
function Lua_isNonNumberString(
	ALua : TLua;
	Index : word
) : boolean;
begin
	Result := (Boolean(lua_isstring(ALua,Index)) and
	not Boolean(lua_isnumber(ALua,Index)));
end;{Lua_isNonNumberString}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Lua_toLongWord                                                        FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Converts lua value to LongWord
//	Changes -
//------------------------------------------------------------------------------
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
end;{Lua_toLongWord}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Lua_isposnumber                                                       FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//			Checks if lua value is a positive integer (including 0) and returns
//		true/false
//	Changes -
//------------------------------------------------------------------------------
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
end;{Lua_isposnumber}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//MakeLuaThread                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//			Takes an existing lua, and makes a new execution thread for a descendant.
//		Also stores the parent's info so that it can be deferenced and freed.
//		Also Creates a table environment with a reference to the sourcelua's global
//		functions, variables etc, so that the creation of new globals do not
//		becomed shared with the source lua or other descendant threads.
//		THIS WAS A BITCH TO MAKE, SHOULD NOT NEED ALTERING IN ANY FORM
//
//	Changes -
//		Feb 16 2009 - Tsusai: Changed to better example from lua mailing lists
//------------------------------------------------------------------------------
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
	lua_newtable(SourceLua); //new table for globals
	lua_newtable(SourceLua); //metatable for new globals
	lua_pushliteral(SourceLua, '__index');
	lua_pushvalue(SourceLua, LUA_GLOBALSINDEX); //__index tries old common globals
	lua_settable(SourceLua, -3);
	lua_setmetatable(SourceLua, -2);
	lua_replace(SourceLua, LUA_GLOBALSINDEX);

end;{MakeLuaThread}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//InitLuaState                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//			Initializes a brand new lua, not for descendant threads.
//	Changes -
//------------------------------------------------------------------------------
procedure InitLuaState(var ALua : TLua);
begin
	ALua := lua_open;
	luaL_openlibs(ALua);
end;{InitLuaState}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadAndRunLuaScript                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//			Tells a lua instance to load and execute a script file
//	Changes -
//------------------------------------------------------------------------------
procedure LoadAndRunLuaScript(var ALua : TLua; Const LuaFile : String);
begin
	RegisterClassTCharacter(ALua);
	//Load the script
	if luaL_loadfile(ALua, PChar(LuaFile)) <> 0 then //0 = no errors
	begin
		if Length(lua_tostring(ALua, -1)) > 0 then
		begin
			//If something went wrong, get the error message off the stack
			Console.Message(lua_tostring(ALua, -1), 'Zone Server - Lua', MS_ERROR);
			Exit;
		end;
		lua_pop(ALua, 1); //Remove the error string
	end;

	//Executes the script
	if lua_pcall(ALua,0,0,0) <> 0 then //0 = no errors
	begin
		if Length(lua_tostring(ALua, -1)) > 0 then
		begin
			//If something went wrong, get the error message off the stack
			Console.Message(lua_tostring(ALua, -1), 'Zone Server - Lua', MS_ERROR);
			Exit;
		end;
		lua_pop(ALua, 1); //Remove the error string
	end;
end;{LoadAndRunLuaScript}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TerminateLuaThread                                                   PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//			Using the stored parent info, we are able to dereference and free
//		up a lua descendant thread.
//	Changes -
//------------------------------------------------------------------------------
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
end;{TerminateLuaThread}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LuaRunPlayerScript                                                   PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//			Creates and runs a player based execution environment based on
//		the function name
//	Changes -
//------------------------------------------------------------------------------
procedure LuaRunPlayerScript(
	var AChara : TCharacter;
	const LuaMode : TLuaEnv;
	const LuaFunc : string
);
begin
	//Set the player's lua thread, setting up its own unique runtime enviro.
	case LuaMode of
		LUA_NPC: MakeLuaThread(MainProc.ZoneServer.NPCLua,AChara.LuaInfo);
		LUA_ITEM: MakeLuaThread(MainProc.ZoneServer.ItemLua,AChara.LuaInfo);
	end;
	//Set the character pointer to the global list
	SetCharaToLua(AChara);
	//Get the function
	lua_getglobal(AChara.LuaInfo.Lua,PChar(LuaFunc));
	//run the function
	if lua_resume(AChara.LuaInfo.Lua,0) <> 0 then
	begin
		if Length(lua_tostring(AChara.LuaInfo.Lua, -1)) > 0 then
		begin
			//If something went wrong, get the error message off the stack
			Console.Message(lua_tostring(AChara.LuaInfo.Lua, -1), 'Zone Server - Lua', MS_ERROR);
		end;
		lua_pop(AChara.LuaInfo.Lua, 1); //Remove the error string
	end;
end;{LuaRunPlayerScript}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetCharaToLua                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//			Pushes the Character Pointer onto the global array of the lua
//	Changes -
//------------------------------------------------------------------------------
//	This is meant so that when a character executes a script,
//	we can retrieve the TCharacter object
procedure SetCharaToLua(var AChara : TCharacter);
begin
	lua_pushlightuserdata(AChara.LuaInfo.Lua, @AChara); // Push pointer for the character
	lua_setglobal(AChara.LuaInfo.Lua, 'character'); // Tell Lua to set it as global
	//RegisterClassTCharacter(AChara.LuaInfo.Lua);
	RegisterExistingClassTCharacter(AChara.LuaInfo.Lua,AChara,'LuaChara');
end;{SetCharaToLua}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetCharaFromLua                                                      FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//			Pulls the Character object pointer from the lua global array.
//	Changes -
//------------------------------------------------------------------------------
function GetCharaFromLua(var ALua : TLua; var AChara : PCharacter) : boolean;
begin
	lua_getglobal(ALua, 'character');
	AChara := lua_topointer(ALua, -1);
	Result := Assigned(AChara); //Need something better
end;{GetCharaFromLua}
//------------------------------------------------------------------------------
end{LuaCoreRoutines}.
