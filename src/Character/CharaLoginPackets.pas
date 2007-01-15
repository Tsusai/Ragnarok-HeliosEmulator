unit CharaLoginPackets;

interface
uses
	CharacterServer,
	CommClient,
	IdContext;

	procedure ValidateWithLoginServer(
		AClient : TInterClient;
		CharacterServer : TCharacterServer
	);
	procedure SendValidateFlagToChara(AClient : TIdContext; Validated : boolean);
	procedure SendCharaWANIPToLogin(AClient : TInterClient; CharacterServer : TCharacterServer);
	procedure SendCharaLANIPToLogin(AClient : TInterClient; CharacterServer : TCharacterServer);
	procedure SendCharaOnlineUsersToLogin(AClient : TInterClient; CharacterServer : TCharacterServer);

implementation
uses
	BufferIO,
	Globals,
	PacketTypes,
	TCPServerRoutines;

	procedure ValidateWithLoginServer(
		AClient : TInterClient;
		CharacterServer : TCharacterServer
	);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0, $2000, OutBuffer);
		WriteBufferMD5String(2, GetMD5(CharacterServer.Options.LoginKey), OutBuffer);
		WriteBufferString(18, CharacterServer.Servername, 24, OutBuffer);
		WriteBufferWord(42, CharacterServer.WANPort, OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2000));
	end;

	procedure SendValidateFlagToChara(AClient : TIdContext; Validated : boolean);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0, $2001, OutBuffer);
		WriteBufferByte(2, Byte(Validated), OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2001));
	end;

	procedure SendCharaWANIPToLogin(
		AClient : TInterClient;
		CharacterServer : TCharacterServer
	);
	var
		OutBuffer : TBuffer;
		Size : integer;
	begin
		Size := Length(CharacterServer.WANIP);
		WriteBufferWord(0,$2002,OutBuffer);
		WriteBufferWord(2,Size+4,OutBuffer);
		WriteBufferString(4,CharacterServer.WANIP,Size,OutBuffer);
		SendBuffer(AClient,OutBuffer,Size+4);
	end;

	procedure SendCharaLANIPToLogin(
		AClient : TInterClient;
		CharacterServer : TCharacterServer
	);
	var
		OutBuffer : TBuffer;
		Size : integer;
	begin
		Size := Length(CharacterServer.LANIP);
		WriteBufferWord(0,$2003,OutBuffer);
		WriteBufferWord(2,Size+4,OutBuffer);
		WriteBufferString(4,CharacterServer.LANIP,Size,OutBuffer);
		SendBuffer(AClient,OutBuffer,Size+4);
	end;

	procedure SendCharaOnlineUsersToLogin(
		AClient : TInterClient;
		CharacterServer : TCharacterServer
	);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0,$2004,OutBuffer);
		WriteBufferWord(2,CharacterServer.OnlineUsers,OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2004));
	end;

end.
 