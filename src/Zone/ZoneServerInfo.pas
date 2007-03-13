//------------------------------------------------------------------------------
//ZoneServerInfo			                                    UNIT
//------------------------------------------------------------------------------
//	What it does-
//      	Data holder which needed when connect to Char/Inter Server.
//
//	Changes -
//		March 13th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
unit ZoneServerInfo;

interface
uses
	ServerInfo;

type
//------------------------------------------------------------------------------
//TZoneServerInfo			                     Class (TServerInfo)
//------------------------------------------------------------------------------
//	What it does-
//      	Zone ID is used to identification.
//
//	Changes -
//		March 13th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
	TZoneServerInfo = class(TServerInfo)
	public
		ZoneID : LongWord;
	end;

implementation

end.
