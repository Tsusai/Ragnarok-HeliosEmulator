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
	Types,
         Math,
	Character,
	{Third Party}
	IdContext
	;

	procedure ZoneSendMapConnectReply(ACharacter : TCharacter);
	procedure ZoneSendMapConnectDeny(AClient : TIdContext);
	procedure ZoneSendTickToClient(ACharacter : TCharacter);
	procedure ZoneSendWalkReply(ACharacter : TCharacter; DestPoint : TPoint);
	procedure ZoneSendObjectNameAndIDBasic(
		ACharacter : TCharacter;
		ID : LongWord;
		Name : String
	);
	procedure SendCharacterSelectResponse(ACharacter : TCharacter);
	procedure SendQuitGameResponse(ACharacter : TCharacter);
	procedure SendAreaChat(Chat : String;Length : Word;ACharacter : TCharacter);

	procedure ZoneSendGMCommandtoInter(ACharacter : TCharacter; Command : String);

	implementation
uses
        Main,
	BufferIO,
	PacketTypes,
	TCPServerRoutines,
	WinLinux,
        Being;

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
		WriteBufferPointAndDirection(6, ACharacter.Position,ReplyBuffer,ACharacter.Direction);
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
	procedure ZoneSendWalkReply(ACharacter : TCharacter; DestPoint : TPoint);
	var
		ReplyBuffer : TBuffer;
	begin
		WriteBufferWord(0, $0087, ReplyBuffer);
		WriteBufferLongWord(2, ACharacter.MoveTick, ReplyBuffer);
		WriteBufferTwoPoints( 6, DestPoint, ACharacter.Position, ReplyBuffer);
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


//------------------------------------------------------------------------------
//SendCharacterSelectResponse                                        PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Tells the client to return to character select. Triggered by ZoneRecv ->
//		ReturnToCharacterSelect.
//
//  Changes -
//    March 17th, 2007 - RaX - Created;
//------------------------------------------------------------------------------
	procedure SendCharacterSelectResponse(
		ACharacter : TCharacter
	);
	var
		OutBuffer : TBuffer;
	begin
		//send leave 2
		WriteBufferWord(0, $00b3,OutBuffer);
		WriteBufferByte(2, 1,OutBuffer);
		SendBuffer(ACharacter.ClientInfo, OutBuffer, GetPacketLength($00b3,ACharacter.ClientVersion));
	end;//SendCharacterSelectResponse
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendQuitGameResponse				                                        PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//    	Tells the client to "Exit to windows". Triggered by ZoneRecv ->
//    QuitGame.
//
//  Changes -
//    March 17th, 2007 - RaX - Created;
//------------------------------------------------------------------------------
	procedure SendQuitGameResponse(
		ACharacter : TCharacter
	);
	var
		OutBuffer : TBuffer;
	begin
		//send leave 2
		WriteBufferWord(0, $018b, OutBuffer);
		WriteBufferWord(2, 0, OutBuffer);
		Sendbuffer(ACharacter.ClientInfo, OutBuffer, GetPacketLength($018b,ACharacter.ClientVersion));
	end;//SendQuitGameResponse
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendAreaChat				                       												PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//    	Send chat message to local area
//
//  Changes -
//    March 18th, 2007 - Aeomin - Created
//------------------------------------------------------------------------------
procedure SendAreaChat(
	Chat				: String;
	Length			: Word;
	ACharacter	: TCharacter
);
var
	OutBuffer : TBuffer;
	ABeing		: TBeing;
	idxY			: SmallInt;
	idxX			: SmallInt;
	BeingIdx	: integer;
begin
	//16 covers the old 15x15 grid
	for idxY := Max(ACharacter.Position.Y-MainProc.ZoneServer.Options.CharShowArea,0) to Min(ACharacter.Position.Y+MainProc.ZoneServer.Options.CharShowArea,ACharacter.MapInfo.Size.Y) do
	begin
		for idxX := Max(ACharacter.Position.X-MainProc.ZoneServer.Options.CharShowArea,0) to Min(ACharacter.Position.X+MainProc.ZoneServer.Options.CharShowArea,ACharacter.MapInfo.Size.X) do
		begin
			for BeingIdx := ACharacter.MapInfo.Cell[idxX,idxY].Beings.Count - 1 downto 0 do
			begin
				ABeing := ACharacter.MapInfo.Cell[idxX,idxY].Beings.Objects[BeingIdx] as TBeing;
				if ABeing = ACharacter then
				begin
					WriteBufferWord(0, $008e, OutBuffer);
					WriteBufferWord(2, Length+5, OutBuffer);
					WriteBufferString(4, Chat+#0, Length+1, OutBuffer);
					Sendbuffer(ACharacter.ClientInfo, OutBuffer, Length+5);
				end else
				begin
					WriteBufferWord(0, $008d, OutBuffer);
					WriteBufferWord(2, Length+9, OutBuffer);
					WriteBufferLongWord(4, ACharacter.ID, OutBuffer);
					WriteBufferString(8, Chat+#0, Length+1, OutBuffer);
					Sendbuffer(TCharacter(ABeing).ClientInfo, OutBuffer, Length+9);
				end;
			end;
		end;
	end;
end;        
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendGMCommandToInter                                           PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the received gm command to the inter server.
//
//  Changes -
//    March 19th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
	procedure ZoneSendGMCommandtoInter(ACharacter : TCharacter; Command : String);
	var
		ReplyBuffer : TBuffer;
		TotalLength	: Integer;
	begin
		//See Notes/GM Command Packets.txt
		TotalLength := 19+Length(Command);
		WriteBufferWord(0, $2205, ReplyBuffer);
		WriteBufferWord(2, TotalLength, ReplyBuffer);
		WriteBufferLongWord(4, ACharacter.Account.ID, ReplyBuffer);
		WriteBufferLongWord(8, ACharacter.CID, ReplyBuffer);
		WriteBufferWord(12, Length(Command), ReplyBuffer);
		WriteBufferString(14, Command, Length(Command), ReplyBuffer);
		SendBuffer(MainProc.ZoneServer.ToInterTCPClient, ReplyBuffer, TotalLength);
	end;//ZoneSendGMCommandToInter
//------------------------------------------------------------------------------

end.