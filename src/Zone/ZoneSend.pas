unit ZoneSend;

interface
uses
	Character,
	{Third Party}
	IdContext
	;

	procedure ZoneSendMapConnectReply(ACharacter : TCharacter);
	procedure ZoneSendMapConnectDeny(AClient : TIdContext);
	procedure ZoneSendTickToClient(AClient : TIdContext);
	procedure ZoneSendObjectNameAndIDBasic(
		AClient : TIdContext;
		ID : Cardinal;
		Name : String
	);


implementation
uses
	BufferIO,
	PacketTypes,
	TCPServerRoutines,
	WinLinux;

	procedure ZoneSendMapConnectReply(ACharacter : TCharacter);
	var
		ReplyBuffer : TBuffer;
	begin
		WriteBufferWord(0, $0073, ReplyBuffer);
		WriteBufferCardinal(2, GetTick, ReplyBuffer);
		WriteBufferPointAndDirection(6, ACharacter.Point,ReplyBuffer,ACharacter.Direction);
		WriteBufferByte(9, 5, ReplyBuffer);
		WriteBufferByte(10, 5, ReplyBuffer);
		SendBuffer(ACharacter.ClientInfo,ReplyBuffer,11);
	end;

	procedure ZoneSendMapConnectDeny(AClient : TIdContext);
	var
		ReplyBuffer : TBuffer;
	begin
		WriteBufferWord(0, $0074, ReplyBuffer);
		WriteBufferByte(2, 0 ,    ReplyBuffer);
		SendBuffer(AClient,ReplyBuffer,3);
	end;

	procedure ZoneSendTickToClient(AClient : TIdContext);
	var
		ReplyBuffer : TBuffer;
	begin
		WriteBufferWord(0, $007f, ReplyBuffer);
		WriteBufferCardinal(2, GetTick, ReplyBuffer);
		SendBuffer(AClient, ReplyBuffer, 6);
	end;

	procedure ZoneSendObjectNameAndIDBasic(
		AClient : TIdContext;
		ID : Cardinal;
		Name : String
	);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord    (0, $0095, OutBuffer);
		WriteBufferCardinal(2, ID, OutBuffer);
		WriteBufferString  (6, Name, 24, OutBuffer);
		SendBuffer(AClient, OutBuffer, GetPacketLength($0095));
	end;

end.