unit ZoneSend;

interface
uses
	Character,
	{Third Party}
	IdContext
	;

	procedure ZoneSendMapConnectReply(ACharacter : TCharacter);
	procedure ZoneSendMapConnectDeny(AClient : TIdContext);


implementation
uses
	PacketTypes,
	Socket,
	WinLinux;

	procedure ZoneSendMapConnectReply(ACharacter : TCharacter);
	var
		ReplyBuffer : TBuffer;
	begin
		WriteBufferWord(0, $0073, ReplyBuffer);
		WriteBufferCardinal(2, GetTick, ReplyBuffer);
		WriteBufferPointAndDirection(6, ACharacter.Point,ReplyBuffer);
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

end.