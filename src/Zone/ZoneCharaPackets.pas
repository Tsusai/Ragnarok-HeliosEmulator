unit ZoneCharaPackets;

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

implementation
uses
	BufferIO,
	Globals,
	PacketTypes,
	TCPServerRoutines;

	procedure ValidateWithCharaServer(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0, $2100, OutBuffer);
		WriteBufferCardinal(2, ZoneServer.Options.ID, OutBuffer);
		WriteBufferWord(6, ZoneServer.Port, OutBuffer);
		WriteBufferMD5String(8, GetMD5(ZoneServer.Options.CharaKey), OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2100));
	end;

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
	end;

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
	end;

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
	end;

	procedure SendZoneOnlineUsersToChara(AClient : TInterClient; ZoneServer : TZoneServer);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0,$2104,OutBuffer);
		WriteBufferWord(2,ZoneServer.OnlineUsers,OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2104));
	end;

end.
 