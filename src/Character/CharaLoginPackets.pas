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
	procedure SendValidateFlag(AClient : TIdContext; Validated : boolean);
	procedure SendWANIP(AClient : TInterClient; CharacterServer : TCharacterServer);
	procedure SendLANIP(AClient : TInterClient; CharacterServer : TCharacterServer);
	procedure SendOnlineUsers(AClient : TInterClient; CharacterServer : TCharacterServer);

implementation
uses
	BufferIO,
	Globals,
	PacketTypes;

	procedure ValidateWithLoginServer(
		AClient : TInterClient;
		CharacterServer : TCharacterServer
	);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0, $2000, OutBuffer);
		WriteBufferMD5String(2, GetMD5('helioscserver'), OutBuffer);
		WriteBufferString(18, CharacterServer.Servername, 24, OutBuffer);
		WriteBufferWord(42, CharacterServer.WANPort, OutBuffer);
		SendBuffer(AClient,OutBuffer,44);
	end;

	procedure SendValidateFlag(AClient : TIdContext; Validated : boolean);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0, $2001, OutBuffer);
		WriteBufferByte(2, Byte(Validated), OutBuffer);
		SendBuffer(AClient,OutBuffer,3);
	end;

	procedure SendWANIP(AClient : TInterClient; CharacterServer : TCharacterServer);
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

	procedure SendLANIP(AClient : TInterClient; CharacterServer : TCharacterServer);
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

	procedure SendOnlineUsers(AClient : TInterClient; CharacterServer : TCharacterServer);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0,$2004,OutBuffer);
		WriteBufferWord(2,CharacterServer.OnlineUsers,OutBuffer);
		SendBuffer(AClient,OutBuffer,4);
	end;

end.
 