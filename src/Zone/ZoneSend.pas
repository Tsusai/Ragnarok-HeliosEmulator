//------------------------------------------------------------------------------
//ZoneSend                                                                 UNIT
//------------------------------------------------------------------------------
//  What it does -
//      On events executing internally, or by other characters on the server, we
//    will have to send information to the client to tell it what happened. This
//    unit houses the routines to do so.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
//ZoneSendMapConnectReply                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Replies to a clients map connect request, sends the clients position and
//    direction.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
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
	end;//ZoneSendMapConnectReply
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendMapConnectDeny                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Denys a client access to the map server.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
	procedure ZoneSendMapConnectDeny(AClient : TIdContext);
	var
		ReplyBuffer : TBuffer;
	begin
		WriteBufferWord(0, $0074, ReplyBuffer);
		WriteBufferByte(2, 0 ,    ReplyBuffer);
		SendBuffer(AClient,ReplyBuffer,3);
	end;//ZoneSendMapConnectReply
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendTickToClient                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends a ping or "tick" to the client to make sure that it's still there.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
	procedure ZoneSendTickToClient(AClient : TIdContext);
	var
		ReplyBuffer : TBuffer;
	begin
		WriteBufferWord(0, $007f, ReplyBuffer);
		WriteBufferCardinal(2, GetTick, ReplyBuffer);
		SendBuffer(AClient, ReplyBuffer, 6);
	end;//ZoneSendTickToClient
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendObjectNameAndIDBasic                                        PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends a characters name and ID to the client.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
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
	end;//ZoneSendObjectNameAndIDBasic
//------------------------------------------------------------------------------
end.