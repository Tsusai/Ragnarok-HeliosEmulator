//------------------------------------------------------------------------------
//CharacterServerInfo			                                    UNIT
//------------------------------------------------------------------------------
//	What it does-
//      A Class stores character server's very basic informations.
//    It is used to store Char Server list for Login Server.
//
//	Changes -
//		March 12th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
unit CharacterServerInfo;

interface
uses
	ServerInfo;

type
//------------------------------------------------------------------------------
//TCharaServerInfo                                                        CLASS
//------------------------------------------------------------------------------
	TCharaServerInfo = class(TServerInfo)
	public
		ServerID   : LongWord;
		ServerName : String;
		OnlineUsers : Word;
	end;

implementation

end.
