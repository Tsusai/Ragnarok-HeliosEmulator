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
	procedure ZoneSendTickToClient(ACharacter : TCharacter);
	procedure ZoneSendWalkReply(ACharacter : TCharacter);
	procedure ZoneSendObjectNameAndIDBasic(
		ACharacter : TCharacter;
		ID : LongWord;
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
		WriteBufferLongWord(2, GetTick, ReplyBuffer);
		WriteBufferPointAndDirection(6, ACharacter.Point,ReplyBuffer,ACharacter.Direction);
		WriteBufferByte(9, 5, ReplyBuffer);
		WriteBufferByte(10, 5, ReplyBuffer);
		SendBuffer(ACharacter.ClientInfo,ReplyBuffer,GetPacketLength($0073,ACharacter.ClientVersion));
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
		SendBuffer(AClient,ReplyBuffer,GetPacketLength($0074));
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
	procedure ZoneSendTickToClient(ACharacter : TCharacter);
	var
		ReplyBuffer : TBuffer;
	begin
		WriteBufferWord(0, $007f, ReplyBuffer);
		WriteBufferLongWord(2, GetTick, ReplyBuffer);
		SendBuffer(ACharacter.ClientInfo, ReplyBuffer, GetPacketLength($007f,ACharacter.ClientVersion));
	end;//ZoneSendTickToClient
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendWalkReply		                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends a packet that tells the client to go ahead and walk.
//
//  Changes -
//    February 27th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
	procedure ZoneSendWalkReply(ACharacter : TCharacter);
	var
		ReplyBuffer : TBuffer;
	begin
		WriteBufferWord(0, $0087, ReplyBuffer);
		WriteBufferLongWord(2, ACharacter.MoveTick, ReplyBuffer);
		WriteBufferTwoPoints( 6, ACharacter.DestinationPoint, ACharacter.Point, ReplyBuffer);
		WriteBufferByte(11, 0, ReplyBuffer);
		SendBuffer(ACharacter.ClientInfo, ReplyBuffer, GetPacketLength($0087, ACharacter.ClientVersion));
	end;//ZoneSendWalkReply
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
		ACharacter : TCharacter;
		ID : LongWord;
		Name : String
	);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord   (0, $0095, OutBuffer);
		WriteBufferLongWord(2, ID, OutBuffer);
		WriteBufferString (6, Name, 24, OutBuffer);
		SendBuffer(ACharacter.ClientInfo, OutBuffer, GetPacketLength($0095,ACharacter.ClientVersion));
	end;//ZoneSendObjectNameAndIDBasic
//------------------------------------------------------------------------------
end.