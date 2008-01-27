//------------------------------------------------------------------------------
//ZoneServerInfo			                                    UNIT
//------------------------------------------------------------------------------
//	What it does-
//      	Data holder which needed when connect to Char/Inter Server.
//
//	Changes -
//		March 13th, 2007 - Aeomin - Created Header
//		[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
//
//------------------------------------------------------------------------------
unit ZoneServerInfo;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	//none
	{Project}
	ServerInfo
	{3rd Party}
	//none
	;


type
//------------------------------------------------------------------------------
//TZoneServerInfo			                     Class (TServerInfo)
//------------------------------------------------------------------------------
//	What it does-
//      	Zone ID is used to identification.
//
//	Changes -
//		March 13th, 2007 - Aeomin - Created Header
//		March 30th, 2007 - Tsusai - Added OnlineUsers
//
//------------------------------------------------------------------------------
	TZoneServerInfo = class(TServerInfo)
	public
		ZoneID : LongWord;
		OnlineUsers : word;
	end;

implementation

end.
