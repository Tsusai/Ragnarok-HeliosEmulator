//Core routine for setting up the lua system.  All scripts are loaded, but
//multiple runtime environments are setup per character and removed after
//completion.  Each of these individual runtime environments reads global vars
//which are script related, but have their own pointer as as a global variable,
//so each command can access the character runing it.
unit LuaNPCCore;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
	Character,
	LuaCoreRoutines;

  procedure RunLuaNPCScript(
		var AChara : TCharacter;
		const LuaFunc : string
	);

	procedure ResumeLuaNPCScript(
		var AChara : TCharacter;
		const ReturnAValue : Boolean = false
	);

	procedure ResumeLuaNPCScriptWithString(
		var AChara : TCharacter;
		const ReturnString : string
	);

	procedure ResumeLuaNPCScriptWithInteger(
		var AChara : TCharacter;
		const ReturnInteger : integer
	);

	procedure ResumeLuaNPCScriptWithDouble(
		var AChara : TCharacter;
		const ReturnDouble : double
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
	//Set the player's lua thread, setting up its own unique runtime enviro.
	MakeLuaThread(MainProc.ZoneServer.NPCLua,AChara.LuaInfo);
	//Set the character pointer to the global list
	SetCharaToLua(AChara);
	//Get the function
	lua_getglobal(AChara.LuaInfo.Lua,PChar(LuaFunc));
	//run the function
	lua_resume(AChara.LuaInfo.Lua,0);
end;

procedure ResumeLuaNPCScript(
	var AChara : TCharacter;
	const ReturnAValue : Boolean = false
);
begin
	SetCharaToLua(AChara); //put the global back on the stack
	lua_resume(AChara.LuaInfo.Lua, Byte(ReturnAValue));
end;

procedure ResumeLuaNPCScriptWithString(
	var AChara : TCharacter;
	const ReturnString : string
);
begin
	lua_pushstring(AChara.LuaInfo.Lua, PChar(ReturnString));
	ResumeLuaNPCScript(AChara,true);
end;

procedure ResumeLuaNPCScriptWithInteger(
	var AChara : TCharacter;
	const ReturnInteger : integer
);
begin
	lua_pushinteger(AChara.LuaInfo.Lua, ReturnInteger);
	ResumeLuaNPCScript(AChara,true);
end;

procedure ResumeLuaNPCScriptWithDouble(
	var AChara : TCharacter;
	const ReturnDouble : Double
);
begin
	lua_pushnumber(AChara.LuaInfo.Lua, ReturnDouble);
	ResumeLuaNPCScript(AChara,true);
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
		if Assigned(AChara) then
		begin
			Result := true;
		end;
	end;
end;

end.
