//------------------------------------------------------------------------------
//DatabaseConstants                                                       UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Houses our database related constants, usually names of databases
//    translated to integers.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit DatabaseConstants;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

const
	SQLITE			= 1;//SQLite.
	MYSQL				= 2;//MySQL database.
	MAX_DBTYPE	= 2;//The highest database type, used for ensurerange.

implementation



end.
 