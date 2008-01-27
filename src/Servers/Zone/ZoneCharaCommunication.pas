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

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
uses
	ZoneServer,
	CommClient,
	Character,
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
	procedure SendZoneCharaLogon(
		AClient : TInterClient;
		AChara  : TCharacter
	);
	procedure SendZoneCharaLogOut(
		AClient : TInterClient;
		AChara  : TCharacter;
		Action  : Byte
	);
	procedure SendKickAccountToZone(
		AClient : TIdContext;
		CharId  : LongWord
	);

implementation
uses
	BufferIO,
	Globals,
	Main,
	PacketTypes,
	TCPServerRoutines
	;

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


//------------------------------------------------------------------------------
//SendZoneCharaLogon                                                   PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Update account in Char server's Account List.
//      Zone --> Char Server
//  Changes -
//    April 10th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
	procedure SendZoneCharaLogon(
		AClient : TInterClient;
		AChara  : TCharacter
	);
	var
		OutBuffer : TBuffer;
	begin
		FillChar(OutBuffer, GetPacketLength($2108), 0);
		WriteBufferWord(0,$2108,OutBuffer);
		WriteBufferLongWord(2, AChara.ID, OutBuffer);
		WriteBufferWord(6, MainProc.ZoneServer.Options.ID, OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2108));
	end;
//------------------------------------------------------------------------------

	procedure SendZoneCharaLogOut(
		AClient : TInterClient;
		AChara  : TCharacter;
		Action  : Byte
	);
	var
		OutBuffer : TBuffer;
	begin
		FillChar(OutBuffer, GetPacketLength($2109), 0);
		WriteBufferWord(0,$2109,OutBuffer);
		WriteBufferLongWord(2, AChara.ID, OutBuffer);
		WriteBufferByte(6, Action, OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2109));
	end;

	procedure SendKickAccountToZone(
		AClient : TIdContext;
		CharId  : LongWord
	);
	var
		OutBuffer : TBuffer;
	begin
		FillChar(OutBuffer, GetPacketLength($2110), 0);
		WriteBufferWord(0,$2110,OutBuffer);
		WriteBufferLongWord(2, CharId, OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2110));
	end;
end.
