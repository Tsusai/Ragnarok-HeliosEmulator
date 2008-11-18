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
	LuaCoreRoutines,
	LuaTypes;

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




implementation
uses
	Main,
	Globals,
	LuaPas;


procedure ResumeLuaNPCScript(
	var AChara : TCharacter;
	const ReturnAValue : Boolean = false
);
begin
	SetCharaToLua(AChara); //put the global back on the stack
	if lua_resume(AChara.LuaInfo.Lua, Byte(ReturnAValue)) <> 0 then
	begin
		if Length(lua_tostring(AChara.LuaInfo.Lua, -1)) > 0 then
		begin
		//If something went wrong, get the error message off the stack
			Console.Message(lua_tostring(AChara.LuaInfo.Lua, -1), 'Zone Server - Lua', MS_ERROR);
    end;
		lua_pop(AChara.LuaInfo.Lua, 1); //Remove the error string
	end;
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

end.
