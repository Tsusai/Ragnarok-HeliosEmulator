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
	CommClient,
	SysUtils,
	Being,
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
	procedure ZoneSendGMCommandResultToInter(AccountID : LongWord; CharacterID : LongWord; Success : Boolean; Error : String);

	procedure ZoneSendCharacterMessage(
		ACharacter	: TCharacter;
		AMessage		: String
	);

	procedure ZoneWalkingBeing(Who:TBeing;Point1,Point2:TPoint;AClient : TIdContext);
	procedure ZoneUpdateDirection(Who:TCharacter;AClient : TIdContext);
	procedure ZoneSendBeing(Who:TBeing;AClient : TIdContext;Logon:Boolean=False);
	procedure ZoneDisappearBeing(Who:TBeing;AClient : TIdContext;Effect:Byte=0);

	implementation
uses
	Main,
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
					ZoneSendCharacterMessage(ACharacter, Chat);
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


//------------------------------------------------------------------------------
//ZoneSendGMCommandResultToInter                                           PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the received gm command to the inter server.
//
//  Changes -
//    March 19th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
	procedure ZoneSendGMCommandResultToInter(
		AccountID : LongWord;
		CharacterID : LongWord;
		Success : Boolean;
		Error : String
	);
	var
		ReplyBuffer : TBuffer;
	begin
		if Success then
		begin
			WriteBufferWord(0, $2207, ReplyBuffer);
			WriteBufferLongWord(2, 18+Length(Error), ReplyBuffer);
			WriteBufferLongWord(6, AccountID, ReplyBuffer);
			WriteBufferLongWord(10, CharacterID, ReplyBuffer);
			WriteBufferLongWord(14, Length(Error), ReplyBuffer);
			WriteBufferString(18, Error, Length(Error), ReplyBuffer);
			SendBuffer(MainProc.ZoneServer.ToInterTCPClient,ReplyBuffer,18+Length(Error));
		end;
	end;//ZoneSendGMCommandToInter
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendGMCommandResultToInter                                           PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the received gm command to the inter server.
//
//  Changes -
//    March 19th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
	procedure ZoneSendCharacterMessage(
		ACharacter	: TCharacter;
		AMessage		: String
	);
	var
		OutBuffer	: TBuffer;
	begin
		WriteBufferWord(0, $008e, OutBuffer);
		WriteBufferWord(2, Length(AMessage)+5, OutBuffer);
		WriteBufferString(4, AMessage+#0, Length(AMessage)+1, OutBuffer);
		Sendbuffer(ACharacter.ClientInfo, OutBuffer, Length(AMessage)+5);
	end;//ZoneSendCharacterMessage
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//ZoneSendBeing                                                        PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      make character visible
//
//  Changes -
//    March 18th, 2007 - Aeomin - Created Header
//    March 23th, 2007 - Aeomin - Renamed from ZoneSendChar to ZoneSendBeing
//                                and support for Npc/Mob etc
//------------------------------------------------------------------------------
 procedure ZoneSendBeing(Who:TBeing;AClient : TIdContext;Logon:Boolean=False);
	var
		ReplyBuffer : TBuffer;
		Chara	    : TCharacter;
	begin
		//Old Packet Version
		FillChar(ReplyBuffer,54,0);
		if Logon then
		begin
		WriteBufferWord(0, $0079, ReplyBuffer);
		end else begin
		WriteBufferWord(0, $0078, ReplyBuffer);
		end;
		WriteBufferLongWord(2, Who.ID, ReplyBuffer);
		WriteBufferWord(6, Who.Speed, ReplyBuffer);
		WriteBufferWord(8, Who.Status, ReplyBuffer);
		WriteBufferWord(10, Who.Ailments, ReplyBuffer);
		WriteBufferWord(12, Who.Option, ReplyBuffer); 
		WriteBufferWord(14, Who.JID, ReplyBuffer);
		WriteBufferWord(32, Who.Direction, ReplyBuffer);
		if Who is TCharacter then
		begin
		Chara:=TCharacter(Who);
		{Todo: Hair stlye, head bottom needed for Pet...}
		WriteBufferWord(16, Chara.Hair, ReplyBuffer);
		WriteBufferWord(18, Chara.RightHand, ReplyBuffer);  //Weapon
		WriteBufferWord(20, Chara.LeftHand, ReplyBuffer);  //Shield
		WriteBufferWord(22, Chara.HeadBottom, ReplyBuffer);  //Head bottom
		WriteBufferWord(24, Chara.HeadTop, ReplyBuffer);  //Head top
		WriteBufferWord(26, Chara.HeadMid, ReplyBuffer);  //head mid
		WriteBufferWord(28, Chara.HairColor, ReplyBuffer);
		WriteBufferWord(30, Chara.ClothesColor, ReplyBuffer);
		WriteBufferLongWord(34, Chara.GuildID, ReplyBuffer);
		WriteBufferWord(42, Chara.Karma, ReplyBuffer);
		WriteBufferByte(45, Chara.Account.GenderNum, ReplyBuffer);
		end;
		WriteBufferWord(38, 0, ReplyBuffer);  //Emblem ID
		WriteBufferByte(44, 0, ReplyBuffer);  //Normal/Ready to fight
		WriteBufferPointAndDirection(46, Who.Position, ReplyBuffer,Who.Direction);
		WriteBufferByte(49, 5, ReplyBuffer);
		WriteBufferByte(50, 5, ReplyBuffer);
		if Logon then
		begin
		WriteBufferWord(51, Who.BaseLV, ReplyBuffer);
		SendBuffer(AClient,ReplyBuffer,GetPacketLength($0079));
		end else begin
		WriteBufferByte(51, 0, ReplyBuffer);   //Standing/Dead/Sit
		WriteBufferWord(52, Who.BaseLV, ReplyBuffer);
		SendBuffer(AClient,ReplyBuffer,GetPacketLength($0078));
		end;
               //Require packet version >=6
//		WriteBufferWord(0, $022A, ReplyBuffer);
//                WriteBufferLongWord(2, Who.ID, ReplyBuffer);
//                WriteBufferWord(6, Who.Speed, ReplyBuffer);
//                WriteBufferWord(8, 0, ReplyBuffer);     //Options
//                WriteBufferWord(10, 0, ReplyBuffer);    //Options
//                WriteBufferLongWord(12, 0, ReplyBuffer);//Options
//                WriteBufferLongWord(44, 0, ReplyBuffer);//Options
//                WriteBufferWord(16, Who.JID, ReplyBuffer);
//                WriteBufferWord(18, Who.Hair, ReplyBuffer);
//                WriteBufferWord(20, Who.RightHand, ReplyBuffer);  //Weapon
//                WriteBufferWord(22, Who.LeftHand, ReplyBuffer);  //Shield
//                WriteBufferWord(24, Who.HeadBottom, ReplyBuffer);  //Head bottom
//                WriteBufferWord(26, Who.HeadTop, ReplyBuffer);  //Head top
//                WriteBufferWord(28, Who.HeadMid, ReplyBuffer);  //head mid
//                WriteBufferWord(30, Who.HairColor, ReplyBuffer);
//                WriteBufferWord(32, Who.ClothesColor, ReplyBuffer);
//                WriteBufferWord(34, Who.Direction, ReplyBuffer);
//                WriteBufferLongWord(36, Who.GuildID, ReplyBuffer);
//                WriteBufferWord(40, 0, ReplyBuffer);  //Emblem ID
//                WriteBufferWord(42, Who.Manner, ReplyBuffer);
//                WriteBufferByte(48, Who.Karma, ReplyBuffer);
//                WriteBufferByte(49, Who.Account.GenderNum, ReplyBuffer);
//                WriteBufferPointAndDirection(50, Who.Position, ReplyBuffer,Who.Direction);
//                WriteBufferByte(53, 5, ReplyBuffer);
//                WriteBufferByte(54, 5, ReplyBuffer);
//                WriteBufferByte(55, 0, ReplyBuffer);   //Standing?
//                WriteBufferWord(56, Who.BaseLV, ReplyBuffer);
//                SendBuffer(AClient,ReplyBuffer,GetPacketLength($022A));
	end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneWalkingBeing                                                     PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Make character disappear
//
//  Changes -
//    March 18th, 2007 - Aeomin - Created Header
//    March 23th, 2007 - Aeomin - Renamed from ZoneWalkingChar to ZoneWalkingBeing
//------------------------------------------------------------------------------
	 procedure ZoneDisappearBeing(Who:TBeing;AClient : TIdContext;Effect:Byte=0);
	var
		ReplyBuffer : TBuffer;
	begin
		FillChar(ReplyBuffer,GetPacketLength($0080),0);
		WriteBufferWord(0, $0080, ReplyBuffer);
		WriteBufferLongWord(2, Who.ID, ReplyBuffer);
		WriteBufferByte(6, Effect, ReplyBuffer);
		SendBuffer(AClient,ReplyBuffer,GetPacketLength($0080));
	end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneWalkingBeing                                                     PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Send the destination of charater.
//
//  Changes -
//    March 18th, 2007 - Aeomin - Created Header
//    march 23th, 2007 - Aeomin - Renamed from ZoneWalkingChar to ZoneWalkingBeing
//------------------------------------------------------------------------------
procedure ZoneWalkingBeing(Who:TBeing;Point1,Point2:TPoint;AClient : TIdContext);
	var
		ReplyBuffer : TBuffer;
	begin
		FillChar(ReplyBuffer,GetPacketLength($0086),0);
		WriteBufferWord(0, $0086, ReplyBuffer);
		WriteBufferLongWord(2, Who.ID, ReplyBuffer);
		WriteBufferTwoPoints(6, Point1, Point2, ReplyBuffer);
		WriteBufferLongWord(12, GetTick, ReplyBuffer);
		SendBuffer(AClient,ReplyBuffer,GetPacketLength($0086));
	end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneUpdateDirection                                                  PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Update Character Direction to other characters
//
//  Changes -
//    March 20th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure ZoneUpdateDirection(Who:TCharacter;AClient : TIdContext);
var
	ReplyBuffer : TBuffer;
begin
	FillChar(ReplyBuffer,GetPacketLength($009c),0);
	WriteBufferWord(0, $009c, ReplyBuffer);
	WriteBufferLongWord(2, Who.ID, ReplyBuffer);
	WriteBufferWord(6, Who.HeadDirection, ReplyBuffer);
	WriteBufferByte(8, Who.Direction, ReplyBuffer);
	SendBuffer(AClient,ReplyBuffer,GetPacketLength($009c));
end;

end.
