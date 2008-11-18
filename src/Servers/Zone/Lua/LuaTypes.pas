unit LuaTypes;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
	uses LuaPas;

	//Wrapper for the lua state.
	Type TLua = Plua_state;

	//Used mainly for descending lua threads off a root one
	//so that it may be dereferenced later on and freed.
	Type TLuaInfo = record
		Lua       : TLua;
		LuaID     : Integer;
		ParentLua : TLua;
	end;

implementation

end.