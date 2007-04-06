//------------------------------------------------------------------------------
//ZoneCharaCommunication                                                   UNIT
//------------------------------------------------------------------------------
//  What it does -
//      Houses our Zone<->Character server communication routines. These are
//    used in ZoneServer.pas and also in CharacterServer.pas.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header.
//------------------------------------------------------------------------------
unit ZoneCharaCommunication;

interface
uses
	ZoneServer,
	CommClient,
	IdContext;

	procedure ValidateWithCharaServer(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	procedure SendValidateFlagToZone(AClient : TIdContext; Validated : byte);
	procedure SendZoneWANIPToChara(AClient : TInterClient; ZoneServer : TZoneServer);
	procedure SendZoneLANIPToChara(AClient : TInterClient; ZoneServer : TZoneServer);
	procedure SendZoneOnlineUsersToChara(AClient : TInterClient; ZoneServer : TZoneServer);
	procedure SendZoneCharaIncrease(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	procedure SendZoneCharaDecrease(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	procedure SendOnlineCountToZone(
		AClient : TIdContext;
		Count : Word
	);

implementation
uses
	BufferIO,
	Globals,
	PacketTypes,
	TCPServerRoutines,sysutils;

//------------------------------------------------------------------------------
//ValidateWithCharaServer                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the character server key, zone id, and zoneport to the character
//    server for validation.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header.
//------------------------------------------------------------------------------
	procedure ValidateWithCharaServer(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0, $2100, OutBuffer);
		WriteBufferLongWord(2, ZoneServer.Options.ID, OutBuffer);
		WriteBufferWord(6, ZoneServer.Port, OutBuffer);
		WriteBufferMD5String(8, GetMD5(ZoneServer.Options.CharaKey), OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2100));
	end;//ValidateWithCharaServer 
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
//    January 18th, 2007 - RaX - Created Header.
//------------------------------------------------------------------------------
	procedure SendValidateFlagToZone(
		AClient : TIdContext;
		Validated : byte
	);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0, $2101, OutBuffer);
		WriteBufferByte(2, Validated, OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2101));
	end;//SendValidateFlagToZone
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendZoneWANIPToChara                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the zone's WANIP to the character server.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header.
//------------------------------------------------------------------------------
	procedure SendZoneWANIPToChara(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	var
		OutBuffer : TBuffer;
		Size : integer;
	begin
		Size := Length(ZoneServer.Options.WANIP);
		WriteBufferWord(0,$2102,OutBuffer);
		WriteBufferWord(2,Size+4,OutBuffer);
		WriteBufferString(4,ZoneServer.Options.WANIP,Size,OutBuffer);
		SendBuffer(AClient,OutBuffer,Size+4);
	end;//SendZoneWANIPToChara
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendZoneLANIPToChara                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the zone's LANIP to the character server.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header.
//------------------------------------------------------------------------------
	procedure SendZoneLANIPToChara(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	var
		OutBuffer : TBuffer;
		Size : integer;
	begin
		Size := Length(ZoneServer.Options.LANIP);
		WriteBufferWord(0,$2103,OutBuffer);
		WriteBufferWord(2,Size+4,OutBuffer);
		WriteBufferString(4,ZoneServer.Options.LANIP,Size,OutBuffer);
		SendBuffer(AClient,OutBuffer,Size+4);
	end;//SendZoneLANIPToChara
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendZoneOnlineUsersToChara                                          PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Updates the character server's internal list of online characters.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header.
//------------------------------------------------------------------------------
	procedure SendZoneOnlineUsersToChara(AClient : TInterClient; ZoneServer : TZoneServer);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0,$2104,OutBuffer);
		WriteBufferWord(2,ZoneServer.OnlineUsers,OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2104));
	end;//SendZoneOnlineUsersToChara
//------------------------------------------------------------------------------

	procedure SendZoneCharaIncrease(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0,$2105,OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2105));
	end;

	procedure SendZoneCharaDecrease(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0,$2106,OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2106));
	end;
	
	procedure SendOnlineCountToZone(
		AClient : TIdContext;
		Count : Word
	);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0,$2107,OutBuffer);
		WriteBufferWord(2,Count,OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2107));
	end;
end.
