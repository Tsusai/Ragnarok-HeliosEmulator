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

interface

const
	TEXT				= 1;//TEXT database, set ServerConfig.DatabaseType to this integer.
	MYSQL				= 2;//MySQL database.
	SQLITE			= 3;//SQLite.  Will be #1 soon.
	MAX_DBTYPE	= 3;//The highest database type, used for ensurerange.

implementation



end.
 