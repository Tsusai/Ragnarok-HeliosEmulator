unit LuaTCharacter;

interface

uses
	Character,
	LuaPas;


//Class Wrappers
procedure RegisterClassTCharacter(var LuaScript:PLua_State);
procedure RegisterExistingClassTCharacter(L: PLua_State; var Instance : TCharacter; InstanceName : AnsiString = '');

implementation

uses
	{Classes,
	GameConstants, EventList,
	Map,
	ParameterList,}
	SysUtils,
	PointList,

	LuaUtils,
	Types,
	TypInfo;

const
	LuaTCharacterClassName = 'LuaChara';


procedure LuaCopyTable(L: Plua_State; IdxFrom, IdxTo, MtTo : Integer);
var
	id:Integer;
	tbl : Integer;
	key, val : Variant;
	cf : lua_CFunction;
begin
	lua_pushnil(L);
	while(lua_next(L, IdxFrom)<>0)do
		begin
			key := LuaToVariant(L, -2);
			if CompareText(key, '__') = 1 then
				tbl := MtTo
			else
				tbl := IdxTo;
			case lua_type(L, -1) of
				LUA_TFUNCTION : begin
					cf := lua_tocfunction(L, -1);
					LuaPushVariant(L, key);
					lua_pushcfunction(L, cf);
					lua_rawset(L, tbl);
				end;
				LUA_TTABLE    : begin
					id := lua_gettop(L);
					LuaCopyTable(L, id, IdxTo, MtTo);
				end;
			else
				val := LuaToVariant(L, -1);
				LuaPushVariant(L, key);
				LuaPushVariant(L, val);
				lua_rawset(L, tbl);
			end;
			lua_pop(L, 1);
		end;
end;




function LuaToTCharacter(var L: Plua_State; Idx : Integer) : TCharacter;
var
	T : ^TCharacter;
begin
	result := nil;
	if lua_type(L, Idx) = LUA_TTABLE then
	begin
		T := LuaRawGetTableLightUserData(L, Idx, '_Self');//LuaGetTableLightUserData(L, Idx, '_Self');
		result := T^;
	end
	else
		luaL_error(L, PChar('Class table expected.'));
end;


//Need to cleanup, to prevent a real tcharacter creation.  We don't need to make
//them at all.  Find stack size, stack objects, etc to determine how to make it.
function new_TCharacter(L: Plua_State): Integer; cdecl;
var
	c : TCharacter;
	Index: Integer;
	mt, idx: Integer;
	PCharacter : ^TCharacter;
begin
	if lua_type(L, 1) <> LUA_TTABLE then
		lua_remove(L, 1);
		
	PCharacter := lua_topointer(L,2);
	if Assigned(PCharacter) then
	begin
		lua_newtable(L);
		idx := lua_gettop(L);

		lua_pushliteral(L, '_Self');
		lua_pushlightuserdata(L, PCharacter);
		lua_rawset(L, idx);
	end else
	begin;
		c:= TCharacter.Create(nil);
		lua_newtable(L);
		idx := lua_gettop(L);

		lua_pushliteral(L, '_Self');
		lua_pushlightuserdata(L, (Pointer(c)));
		lua_rawset(L, idx);
	end;


	lua_newtable(L);
	mt := lua_gettop(L);
	luaL_getmetatable(L, LuaTCharacterClassName);
	index := lua_gettop(L);
	LuaCopyTable(L, index, idx, mt);
	lua_setmetatable(L, idx);

	lua_pop(L, 1);
	result := 1;


end;

function luaTCharacterWalk(L: Plua_State): Integer; cdecl;
var
	c : TCharacter;
begin
	c := LuaToTCharacter(L, 1);
	lua_remove(L, 1);
	if lua_gettop(L)<>0 then
		luaL_error(L, 'Walk expects no parameters.')
	else
		begin
			c.Walk();
		end;
	result := 0
end;

function luaTCharacterCalcMaxHP(L: Plua_State): Integer; cdecl;
var
	c : TCharacter;
begin
	c := LuaToTCharacter(L, 1);
	lua_remove(L, 1);
	if lua_gettop(L)<>0 then
		luaL_error(L, 'CalcMaxHP expects no parameters.')
	else
		begin
			c.CalcMaxHP();
		end;
	result := 0
end;

function luaTCharacterCalcMaxSP(L: Plua_State): Integer; cdecl;
var
	c : TCharacter;
begin
	c := LuaToTCharacter(L, 1);
	lua_remove(L, 1);
	if lua_gettop(L)<>0 then
		luaL_error(L, 'CalcMaxSP expects no parameters.')
	else
		begin
			c.CalcMaxSP();
		end;
	result := 0
end;

function luaTCharacterCalcSpeed(L: Plua_State): Integer; cdecl;
var
	c : TCharacter;
begin
	c := LuaToTCharacter(L, 1);
	lua_remove(L, 1);
	if lua_gettop(L)<>0 then
		luaL_error(L, 'CalcSpeed expects no parameters.')
	else
		begin
			c.CalcSpeed();
			//Writeln('CalcSpeed Hack, showing the name: '+c.Name);
		end;
	result := 0
end;

function luaTCharacterCalcASpeed(L: Plua_State): Integer; cdecl;
var
	c : TCharacter;
begin
	c := LuaToTCharacter(L, 1);
	lua_remove(L, 1);
	if lua_gettop(L)<>0 then
		luaL_error(L, 'CalcASpeed expects no parameters.')
	else
		begin
			c.CalcASpeed();
		end;
	result := 0
end;

function luaTCharacterUpdateDirection(L: Plua_State): Integer; cdecl;
var
	c : TCharacter;
begin
	c := LuaToTCharacter(L, 1);
	lua_remove(L, 1);
	if lua_gettop(L)<>0 then
		luaL_error(L, 'UpdateDirection expects no parameters.')
	else
		begin
			c.UpdateDirection();
		end;
	result := 0
end;

function LuaToLongWord(L: Plua_State; idx :integer) : LongWord;
begin
	Result := Round(lua_tonumber(L,idx));
end;

function luaTCharacterShowEffect(L: Plua_State): Integer; cdecl;
var
	c : TCharacter;
	v_EffectID : LongWord;
begin
	c := LuaToTCharacter(L, 1);
	lua_remove(L, 1);
	if lua_gettop(L)<>1 then
		luaL_error(L, 'ShowEffect expects 1 parameter (EffectID).')//123
	else
		begin
			v_EffectID := LuaToLongWord(L, 1);
			c.ShowEffect(v_EffectID);
		end;
	result := 0
end;

function LuaToWord(L: Plua_State; idx : integer) : word;
begin
	Result := lua_tointeger(L,idx);
end;

function LuaToByte(L: Plua_State; idx : integer) : word;
begin
	Result := lua_tointeger(L,idx);
end;

function luaTCharacterInPointRange(L: Plua_State): Integer; cdecl;
var
	c : TCharacter;
	v_TargetPoint : TPoint;
begin
	c := LuaToTCharacter(L, 1);
	lua_remove(L, 1);
	if lua_gettop(L)<>1 then
		luaL_error(L, 'InPointRange expects 1 parameter (TargetPoint).')//123
	else
		begin
			//v_TargetPoint := LuaToTPoint(L, 1);
			LuaPushBoolean(L, c.InPointRange(v_TargetPoint));
		end;
	result := 1
end;

procedure LuaPushWord(L: PLua_State; val : integer);
begin
	lua_pushnumber(L, val);
end;

procedure LuaPushByte(L: PLua_State; val : integer);
begin
	lua_pushnumber(L, val);
end;
procedure LuaPushLongWord(L: PLua_State; val : integer);
begin
	lua_pushnumber(L, val);
end;

function index_TCharacter(L: Plua_State): Integer; cdecl;
var
	c : TCharacter;
	propName : AnsiString;
	v        : Variant;
begin
	c := LuaToTCharacter(L, 1);
	lua_remove(L, 1);
	propName := LuaToString(L, 1);
			if (CompareText('HeadDirection', propName) = 0) then
				LuaPushWord(L, c.HeadDirection)
			else if (CompareText('Direction', propName) = 0) then
				LuaPushByte(L, c.Direction)
			else if (CompareText('AttackRange', propName) = 0) then
				LuaPushWord(L, c.AttackRange)
			else if (CompareText('ATK', propName) = 0) then
				LuaPushWord(L, c.ATK)
			else if (CompareText('MATK1', propName) = 0) then
				LuaPushWord(L, c.MATK1)
			else if (CompareText('MATK2', propName) = 0) then
				LuaPushWord(L, c.MATK2)
			else if (CompareText('DEF1', propName) = 0) then
				LuaPushWord(L, c.DEF1)
			else if (CompareText('DEF2', propName) = 0) then
				LuaPushWord(L, c.DEF2)
			else if (CompareText('MDEF1', propName) = 0) then
				LuaPushWord(L, c.MDEF1)
			else if (CompareText('MDEF2', propName) = 0) then
				LuaPushWord(L, c.MDEF2)
			else if (CompareText('HIT', propName) = 0) then
				LuaPushWord(L, c.HIT)
			else if (CompareText('FLEE1', propName) = 0) then
				LuaPushWord(L, c.FLEE1)
			else if (CompareText('PerfectDodge', propName) = 0) then
				LuaPushByte(L, c.PerfectDodge)
			else if (CompareText('FalseCritical', propName) = 0) then
				LuaPushWord(L, c.FalseCritical)
			else if (CompareText('Critical', propName) = 0) then
				LuaPushWord(L, c.Critical)
			else if (CompareText('SaveMapID', propName) = 0) then
				LuaPushLongWord(L, c.SaveMapID)
			else if (CompareText('PathIndex', propName) = 0) then
				LuaPushWord(L, c.PathIndex)
			else if (CompareText('MoveTick', propName) = 0) then
				LuaPushLongWord(L, c.MoveTick)
			else if (CompareText('TargetID', propName) = 0) then
				LuaPushLongWord(L, c.TargetID)
			else
			if (CompareText('Name', propName) = 0) then
				LuaPushstring(L, c.Name)
			else if (CompareText('JID', propName) = 0) then
				LuaPushWord(L, c.JID)
			else if (CompareText('BaseLV', propName) = 0) then
				LuaPushWord(L, c.BaseLV)
			else if (CompareText('JobLV', propName) = 0) then
				LuaPushWord(L, c.JobLV)
			else if (CompareText('BaseEXP', propName) = 0) then
				LuaPushLongWord(L, c.BaseEXP)
			else if (CompareText('JobEXP', propName) = 0) then
				LuaPushLongWord(L, c.JobEXP)
			else if (CompareText('BaseEXPToNextLevel', propName) = 0) then
				LuaPushLongWord(L, c.BaseEXPToNextLevel)
			else if (CompareText('JobEXPToNextLevel', propName) = 0) then
				LuaPushLongWord(L, c.JobEXPToNextLevel)
			else if (CompareText('MaxHP', propName) = 0) then
				LuaPushWord(L, c.MaxHP)
			else if (CompareText('HP', propName) = 0) then
				LuaPushWord(L, c.HP)
			else if (CompareText('HPPercent', propName) = 0) then
				LuaPushByte(L, c.HPPercent)
			else if (CompareText('SPPercent', propName) = 0) then
				LuaPushByte(L, c.SPPercent)
			else if (CompareText('MaxSP', propName) = 0) then
				LuaPushWord(L, c.MaxSP)
			else if (CompareText('SP', propName) = 0) then
				LuaPushWord(L, c.SP)
			else if (CompareText('Status', propName) = 0) then
				LuaPushWord(L, c.Status)
			else if (CompareText('Ailments', propName) = 0) then
				LuaPushWord(L, c.Ailments)
			else if (CompareText('Option', propName) = 0) then
				LuaPushWord(L, c.Option)
			else if (CompareText('Speed', propName) = 0) then
				LuaPushWord(L, c.Speed)
			else if (CompareText('ASpeed', propName) = 0) then
				LuaPushWord(L, c.ASpeed)
			else if (CompareText('MinimumHit', propName) = 0) then
				LuaPushInteger(L, c.MinimumHit)
			else if (CompareText('MaximumHit', propName) = 0) then
				LuaPushInteger(L, c.MaximumHit)
			else if (CompareText('AttackDmgTime', propName) = 0) then
				LuaPushInteger(L, c.AttackDmgTime)
			else if (CompareText('Attack_Motion', propName) = 0) then
				LuaPushInteger(L, c.Attack_Motion)
			else if (CompareText('EnemySight', propName) = 0) then
				LuaPushByte(L, c.EnemySight)
			else if (CompareText('Element', propName) = 0) then
				LuaPushWord(L, c.Element)

			else
	if IsPublishedProp(c, propName) then
		begin
			v := GetPropValue(c, propName);
			LuaPushVariant(L, v);
		end
		else
				luaL_error(L, PChar('Property "'+propName+'" does not exist'));

	result := 1;
end;

function newindex_TCharacter(L:PLua_State): Integer; cdecl;
var
	c        : TCharacter;
	propName : AnsiString;
	v        : Variant;
	i        : Integer;
	obj      : TObject;
begin
	c := LuaToTCharacter(L, 1);
	lua_remove(L, 1);
	propName := LuaToString(L, 1);
	v        := LuaToVariant(L, 2);
	if IsPublishedProp(c, propName) then
		begin
			if PropIsType(c, propName, tkClass) then
				begin
					case lua_type(L, 2) of
						LUA_TTABLE : begin
							obj := TObject(LuaRawGetTableLightUserData(L, 2, '_Self'));
						end;
						LUA_TNUMBER: begin
							i := LuaToInteger(L, 2);
							obj := TObject(Pointer(i));
						end;
					else
						obj := nil;
					end;
					if Assigned(obj) then
						SetObjectProp(c, propName, obj)
					else
						luaL_error(L, PChar('Property "'+propName+'" expects and object or object reference.'));
				end
			else
				SetPropValue(c, propName, v);
		end
	else
		begin
			if (CompareText('HeadDirection', propName) = 0) then
				c.HeadDirection := LuaToWord(L, 2)
			else if (CompareText('Direction', propName) = 0) then
				c.Direction := LuaToByte(L, 2)
			else if (CompareText('AttackRange', propName) = 0) then
				c.AttackRange := LuaToWord(L, 2)
			else if (CompareText('ATK', propName) = 0) then
				c.ATK := LuaToWord(L, 2)
			else if (CompareText('MATK1', propName) = 0) then
				c.MATK1 := LuaToWord(L, 2)
			else if (CompareText('MATK2', propName) = 0) then
				c.MATK2 := LuaToWord(L, 2)
			else if (CompareText('DEF1', propName) = 0) then
				c.DEF1 := LuaToWord(L, 2)
			else if (CompareText('DEF2', propName) = 0) then
				c.DEF2 := LuaToWord(L, 2)
			else if (CompareText('MDEF1', propName) = 0) then
				c.MDEF1 := LuaToWord(L, 2)
			else if (CompareText('MDEF2', propName) = 0) then
				c.MDEF2 := LuaToWord(L, 2)
			else if (CompareText('HIT', propName) = 0) then
				c.HIT := LuaToWord(L, 2)
			else if (CompareText('FLEE1', propName) = 0) then
				c.FLEE1 := LuaToWord(L, 2)
			else if (CompareText('PerfectDodge', propName) = 0) then
				c.PerfectDodge := LuaToByte(L, 2)
			else if (CompareText('FalseCritical', propName) = 0) then
				c.FalseCritical := LuaToWord(L, 2)
			else if (CompareText('Critical', propName) = 0) then
				c.Critical := LuaToWord(L, 2)
			else if (CompareText('SaveMapID', propName) = 0) then
				c.SaveMapID := LuaToLongWord(L, 2)
			else if (CompareText('PathIndex', propName) = 0) then
				c.PathIndex := LuaToWord(L, 2)
			else if (CompareText('MoveTick', propName) = 0) then
				c.MoveTick := LuaToLongWord(L, 2)
			else if (CompareText('TargetID', propName) = 0) then
				c.TargetID := LuaToLongWord(L, 2)
			else
			if (CompareText('Name', propName) = 0) then
				c.Name := LuaTostring(L, 2)
			else if (CompareText('JID', propName) = 0) then
				c.JID := LuaToWord(L, 2)
			else if (CompareText('BaseLV', propName) = 0) then
				c.BaseLV := LuaToWord(L, 2)
			else if (CompareText('JobLV', propName) = 0) then
				c.JobLV := LuaToWord(L, 2)
			else if (CompareText('BaseEXP', propName) = 0) then
				c.BaseEXP := LuaToLongWord(L, 2)
			else if (CompareText('JobEXP', propName) = 0) then
				c.JobEXP := LuaToLongWord(L, 2)
			else if (CompareText('BaseEXPToNextLevel', propName) = 0) then
				c.BaseEXPToNextLevel := LuaToLongWord(L, 2)
			else if (CompareText('JobEXPToNextLevel', propName) = 0) then
				c.JobEXPToNextLevel := LuaToLongWord(L, 2)
			else if (CompareText('MaxHP', propName) = 0) then
				c.MaxHP := LuaToWord(L, 2)
			else if (CompareText('HP', propName) = 0) then
				c.HP := LuaToWord(L, 2)
			else if (CompareText('HPPercent', propName) = 0) then
				c.HPPercent := LuaToByte(L, 2)
			else if (CompareText('SPPercent', propName) = 0) then
				c.SPPercent := LuaToByte(L, 2)
			else if (CompareText('MaxSP', propName) = 0) then
				c.MaxSP := LuaToWord(L, 2)
			else if (CompareText('SP', propName) = 0) then
				c.SP := LuaToWord(L, 2)
			else if (CompareText('Status', propName) = 0) then
				c.Status := LuaToWord(L, 2)
			else if (CompareText('Ailments', propName) = 0) then
				c.Ailments := LuaToWord(L, 2)
			else if (CompareText('Option', propName) = 0) then
				c.Option := LuaToWord(L, 2)
			else if (CompareText('Speed', propName) = 0) then
				c.Speed := LuaToWord(L, 2)
			else if (CompareText('ASpeed', propName) = 0) then
				c.ASpeed := LuaToWord(L, 2)
			else if (CompareText('MinimumHit', propName) = 0) then
				c.MinimumHit := LuaToInteger(L, 2)
			else if (CompareText('MaximumHit', propName) = 0) then
				c.MaximumHit := LuaToInteger(L, 2)
			else if (CompareText('AttackDmgTime', propName) = 0) then
				c.AttackDmgTime := LuaToInteger(L, 2)
			else if (CompareText('Attack_Motion', propName) = 0) then
				c.Attack_Motion := LuaToInteger(L, 2)
			else if (CompareText('EnemySight', propName) = 0) then
				c.EnemySight := LuaToByte(L, 2)
			else if (CompareText('Element', propName) = 0) then
				c.Element := LuaToWord(L, 2)
			else
				luaL_error(L, PChar('Property "'+propName+'" does not exist'));
		end;
	result := 0;
end;

function gc_TCharacter(L: Plua_State): Integer; cdecl;
var
	c : TCharacter;
begin
	c := LuaToTCharacter(L, 1);
	c.Free;
	result := 0;
end;

procedure RegisterClassTCharacter(var LuaScript: PLua_State);
var
	MetaTable,
	MethodTable,
	Methods : Integer;
begin
	lua_newtable(LuaScript);
	Methods := lua_gettop(LuaScript);

	luaL_newmetatable(LuaScript, LuaTCharacterClassName);
	MetaTable := lua_gettop(LuaScript);

	lua_pushstring(LuaScript, LuaTCharacterClassName);
	lua_pushvalue(LuaScript, Methods);
	lua_settable(LuaScript, LUA_GLOBALSINDEX);

	lua_pushliteral(LuaScript, '__metatable');
	lua_pushvalue(LuaScript, Methods);
	lua_settable(LuaScript, metatable);

	lua_pushliteral(LuaScript, '__index');
	lua_pushcfunction(LuaScript, @index_TCharacter);
	lua_settable(LuaScript, MetaTable);

	lua_pushliteral(LuaScript, '__newindex');
	lua_pushcfunction(LuaScript, @newindex_TCharacter);
	lua_settable(LuaScript, MetaTable);

	lua_pushliteral(LuaScript, '__gc');
	lua_pushcfunction(LuaScript, @gc_TCharacter);
	lua_settable(LuaScript, MetaTable);

	lua_newtable(LuaScript);
	MethodTable := lua_gettop(LuaScript);
	lua_pushliteral(LuaScript, '__call');
	lua_pushcfunction(LuaScript, @new_TCharacter);
	lua_pushliteral(LuaScript, 'new');
	lua_pushvalue(LuaScript, -2);
	lua_settable(LuaScript, Methods);
	lua_settable(LuaScript, MethodTable);
	lua_setmetatable(LuaScript, Methods);

	lua_pushstring(LuaScript, 'Walk');
	lua_pushcfunction(LuaScript, @luaTCharacterWalk);
	lua_settable(LuaScript, Methods);
	lua_pushstring(LuaScript, 'CalcMaxHP');
	lua_pushcfunction(LuaScript, @luaTCharacterCalcMaxHP);
	lua_settable(LuaScript, Methods);
	lua_pushstring(LuaScript, 'CalcMaxSP');
	lua_pushcfunction(LuaScript, @luaTCharacterCalcMaxSP);
	lua_settable(LuaScript, Methods);
	lua_pushstring(LuaScript, 'CalcSpeed');
	lua_pushcfunction(LuaScript, @luaTCharacterCalcSpeed);
	lua_settable(LuaScript, Methods);
	lua_pushstring(LuaScript, 'CalcASpeed');
	lua_pushcfunction(LuaScript, @luaTCharacterCalcASpeed);
	lua_settable(LuaScript, Methods);
	lua_pushstring(LuaScript, 'UpdateDirection');
	lua_pushcfunction(LuaScript, @luaTCharacterUpdateDirection);
	lua_settable(LuaScript, Methods);
	lua_pushstring(LuaScript, 'ShowEffect');
	lua_pushcfunction(LuaScript, @luaTCharacterShowEffect);
	lua_settable(LuaScript, Methods);
	lua_pushstring(LuaScript, 'InPointRange');
	lua_pushcfunction(LuaScript, @luaTCharacterInPointRange);
	lua_settable(LuaScript, Methods);
	lua_pop(LuaScript, 2);
end;

procedure RegisterExistingClassTCharacter(L: PLua_State; var Instance : TCharacter; InstanceName : AnsiString = '');
var
	index, mt, idx : Integer;
	bHasName : Boolean;
begin
	bHasName := InstanceName <> '';
	if bHasName then
		lua_pushliteral(L, PChar(InstanceName));
	lua_newtable(L);
	idx := lua_gettop(L);

	lua_pushliteral(L, '_Self');
	Lua_pushlightuserdata(L, @Instance);
	lua_rawset(L, idx);


	lua_newtable(L);
	mt := lua_gettop(L);
	luaL_getmetatable(L, LuaTCharacterClassName);
	index := lua_gettop(L);
	LuaCopyTable(L, index, idx, mt);
	lua_setmetatable(L, idx);
	lua_pushstring(L, 'Walk');
	lua_pushcfunction(L, @luaTCharacterWalk);
	lua_settable(L, mt);
	lua_pushstring(L, 'CalcMaxHP');
	lua_pushcfunction(L, @luaTCharacterCalcMaxHP);
	lua_settable(L, mt);
	lua_pushstring(L, 'CalcMaxSP');
	lua_pushcfunction(L, @luaTCharacterCalcMaxSP);
	lua_settable(L, mt);
	lua_pushstring(L, 'CalcSpeed');
	lua_pushcfunction(L, @luaTCharacterCalcSpeed);
	lua_settable(L, mt);
	lua_pushstring(L, 'CalcASpeed');
	lua_pushcfunction(L, @luaTCharacterCalcASpeed);
	lua_settable(L, mt);
	lua_pushstring(L, 'UpdateDirection');
	lua_pushcfunction(L, @luaTCharacterUpdateDirection);
	lua_settable(L, mt);
	lua_pushstring(L, 'ShowEffect');
	lua_pushcfunction(L, @luaTCharacterShowEffect);
	lua_settable(L, mt);
	lua_pushstring(L, 'InPointRange');
	lua_pushcfunction(L, @luaTCharacterInPointRange);
	lua_settable(L, mt);
	lua_pop(L, 1);
	if bHasName then
		lua_settable(L, LUA_GLOBALSINDEX);
end;

end.
