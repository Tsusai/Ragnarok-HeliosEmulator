//------------------------------------------------------------------------------
//CharaLoginCommunication							                                    UNIT
//------------------------------------------------------------------------------
//	What it does-
//      This unit houses routines for Character Server to Login Server
//		communication.
//
//	Changes -
//		March 12th, 2007 - Aeomin - Created Header
//		March 18th, 2007 - RaX - Updated Header.
//
//------------------------------------------------------------------------------
unit CharaLoginCommunication;

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
//------------------------------------------------------------------------------
//ValidateWithLoginServer                                              PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Send verification to Login Server.
//
//	Changes -
//		March 12th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
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
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendValidateFlagToChara                                              PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			An exception procedure which used by Login Server.
//	Tell Character Server whether the request is validated or not.
//
//	Changes -
//		March 12th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
	procedure SendValidateFlagToChara(AClient : TIdContext; Validated : boolean);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0, $2001, OutBuffer);
		WriteBufferByte(2, Byte(Validated), OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2001));
	end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendCharaWANIPToLogin                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Send the WAN IP of Character Server to Login Server.
//
//	Changes -
//		March 12th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
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
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendCharaLANIPToLogin                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Send the LAN IP of Character Server to Login Server.
//
//	Changes -
//		March 12th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
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
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendCharaOnlineUsersToLogin                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Send the number of online players to Login Server.
//
//	Changes -
//		March 12th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
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
//------------------------------------------------------------------------------
end.
 
