(*
** $Id: lualib.h,v 1.28 2003/03/18 12:24:26 roberto Exp $
** Lua standard libraries
** See Copyright Notice in lua.h
*)
(*
** Translated to pascal by Lavergne Thomas
** Bug reports :
**    - thomas.lavergne@laposte.net
**   In french or in english
*)
unit lualib;

interface

uses
  Lua;

const
  LUA_COLIBNAME = 'coroutine';

function luaopen_base(L: Plua_State): LongBool; cdecl;

const
  LUA_TABLIBNAME = 'table';

function luaopen_table(L: Plua_State): LongBool; cdecl;

const
  LUA_IOLIBNAME = 'io';
  LUA_OSLIBNAME = 'os';

function luaopen_io(L: Plua_State): LongBool; cdecl;

const
  LUA_STRLINAME = 'string';

function luaopen_string(L: Plua_State): LongBool; cdecl;

const
  LUA_MATHLIBNAME = 'math';

function luaopen_math(L: Plua_State): LongBool; cdecl;

const
  LUA_DBLIBNAME = 'debug';

function luaopen_debug(L: Plua_State): LongBool; cdecl;

function luaopen_loadlib(L: Plua_State): LongWord; cdecl;

(* compatibility code *)

function lua_baselibopen(L: Plua_State): LongBool;
function lua_tablibopen(L: Plua_State): LongBool;
function lua_iolibopen(L: Plua_State): LongBool;
function lua_strlibopen(L: Plua_State): LongBool;
function lua_mathlibopen(L: Plua_State): LongBool;
function lua_dblibopen(L: Plua_State): LongBool;

implementation

function luaopen_base(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_table(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_io(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_string(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_math(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_debug(L: Plua_State): LongBool; cdecl; external LUA_LIB_NAME;
function luaopen_loadlib(L: Plua_State): LongWord; cdecl; external LUA_LIB_NAME;

function lua_baselibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_base(L);
end;

function lua_tablibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_table(L);
end;

function lua_iolibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_io(L);
end;

function lua_strlibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_string(L);
end;

function lua_mathlibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_math(L);
end;

function lua_dblibopen(L: Plua_State): LongBool;
begin
  Result := luaopen_debug(L);
end;

end.
