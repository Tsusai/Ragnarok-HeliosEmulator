//------------------------------------------------------------------------------
//ZoneInterCommunication	                                                 UNIT
//------------------------------------------------------------------------------
//  What it does -
//      Houses our Zone<->Inter server communication routines. These are
//    used in ZoneServer.pas and also in InterServer.pas.
//
//  Changes -
//    March 18th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
unit ZoneInterCommunication;

interface
uses
	ZoneServer,
	CommClient,
	IdContext;

	procedure ValidateWithInterServer(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	procedure SendValidateFlagToZone(AClient : TIdContext; Validated : byte);
	procedure SendZoneWANIPToInter(AClient : TInterClient; ZoneServer : TZoneServer);
	procedure SendZoneLANIPToInter(AClient : TInterClient; ZoneServer : TZoneServer);
	procedure SendZoneOnlineUsersToInter(AClient : TInterClient; ZoneServer : TZoneServer);

implementation
uses
	BufferIO,
	Globals,
	PacketTypes,
	TCPServerRoutines;

//------------------------------------------------------------------------------
//ValidateWithInterServer                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the Inter server key, zone id, and zoneport to the inter
//    server for validation.
//
//  Changes -
//    March 18th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
	procedure ValidateWithInterServer(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0, $2200, OutBuffer);
		WriteBufferLongWord(2, ZoneServer.Options.ID, OutBuffer);
		WriteBufferWord(6, ZoneServer.Port, OutBuffer);
		WriteBufferMD5String(8, GetMD5(ZoneServer.Options.InterKey), OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2200));
	end;//ValidateWithInterServer
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendValidateFlagToZone                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      After validation, if a zone server checks out it is sent this flag back
//    which contains a simple TRUE/FALSE flag that lets a zone know that it
//    passed or not.
//
//  Changes -
//    March 18th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
	procedure SendValidateFlagToZone(
		AClient : TIdContext;
		Validated : byte
	);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0, $2201, OutBuffer);
		WriteBufferByte(2, Validated, OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2201));
	end;//SendValidateFlagToZone
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendZoneWANIPToChara                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the zone's WANIP to the inter server.
//
//  Changes -
//    March 18th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
	procedure SendZoneWANIPToInter(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	var
		OutBuffer : TBuffer;
		Size : integer;
	begin
		Size := Length(ZoneServer.Options.WANIP);
		WriteBufferWord(0,$2202,OutBuffer);
		WriteBufferWord(2,Size+4,OutBuffer);
		WriteBufferString(4,ZoneServer.Options.WANIP,Size,OutBuffer);
		SendBuffer(AClient,OutBuffer,Size+4);
	end;//SendZoneWANIPToInter
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendZoneLANIPToInter                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the zone's LANIP to the inter server.
//
//  Changes -
//    March 18th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
	procedure SendZoneLANIPToInter(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	var
		OutBuffer : TBuffer;
		Size : integer;
	begin
		Size := Length(ZoneServer.Options.LANIP);
		WriteBufferWord(0,$2203,OutBuffer);
		WriteBufferWord(2,Size+4,OutBuffer);
		WriteBufferString(4,ZoneServer.Options.LANIP,Size,OutBuffer);
		SendBuffer(AClient,OutBuffer,Size+4);
	end;//SendZoneLANIPToInter
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendZoneOnlineUsersToInter                                          PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Updates the inter server's internal list of online inters.
//
//  Changes -
//    March 18th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
	procedure SendZoneOnlineUsersToInter(AClient : TInterClient; ZoneServer : TZoneServer);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0,$2204,OutBuffer);
		WriteBufferWord(2,ZoneServer.OnlineUsers,OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2204));
	end;//SendZoneOnlineUsersToInter
//------------------------------------------------------------------------------
end.
 