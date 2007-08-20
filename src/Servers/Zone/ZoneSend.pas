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
//		[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
//		May 1st, 2007 - Tsusai - Added const to the parameters of almost all
//			routines
//
//------------------------------------------------------------------------------
unit ZoneSend;


interface


uses
	{RTL/VCL}
	Types,
	Classes,
	{Project}
	Being,
	Character,
	PacketTypes,
	{Third Party}
	IdContext
	;


	procedure ZoneSendMapConnectReply(const ACharacter : TCharacter);
	procedure ZoneSendMapConnectDeny(AClient : TIdContext);
	procedure ZoneSendTickToClient(const ACharacter : TCharacter);
	procedure ZoneSendWalkReply(const ACharacter : TCharacter; const DestPoint : TPoint);
	procedure ZoneSendObjectNameAndIDBasic(
		Const ACharacter : TCharacter;
		const ID : LongWord;
		const Name : String
	);
	procedure SendCharacterSelectResponse(const ACharacter : TCharacter);
	procedure SendQuitGameResponse(const ACharacter : TCharacter);
	procedure SendAreaChat(const Chat : String; const Length : Word; const ACharacter : TCharacter);


	procedure ZoneSendGMCommandResultToInter(
		const AccountID : LongWord;
		const CharacterID : LongWord;
		const ZoneID : LongWord;
		const Error : TStringList
	);

	procedure ZoneSendCharacterMessage(
		const ACharacter	: TCharacter;
		const AMessage		: String
	);

	procedure ZoneSendBeing(
		const Who:TBeing;
		AClient : TIdContext;
		const Logon:Boolean=False
	);
	procedure ZoneDisappearBeing(
		const Who:TBeing;
		AClient : TIdContext;
		const Effect:Byte=0
	);
	procedure ZoneWalkingBeing(
		const Who:TBeing;
		const Point1,Point2:TPoint;
		AClient : TIdContext
	);
	procedure ZoneUpdateDirection(
		const Who:TBeing;
		AClient : TIdContext

	);
	procedure ZoneSendConnectionsCount(AClient : TIdContext);

	function ZoneSendWarp(
		const ACharacter : TCharacter;
		const MapName : String;
		const X : Word;
		const Y : Word
	):Boolean;

	procedure Kick(const Who:TCharacter);
	procedure KickAll;
	procedure DuplicateSessionKick(InBuffer : TBuffer);
	procedure SendWhisper(const FromName,Whisper: String;AClient : TIdContext);
	procedure SendWhisperReply(const AChara:TCharacter; const Code : Byte);
	procedure SendGMAnnounce(AClient : TIdContext;const Announce:String;const Blue:boolean=False);
	procedure SendStatUPResult(const AChara:TCharacter;const Failed:Boolean;const Amount:Byte);
implementation


uses
	{RTL/VCL}
	Math,
	SysUtils,
	{Project}
	BufferIO,
	GameConstants,
	Main,
	TCPServerRoutines,
	WinLinux,
	MovementEvent
	{3rd Party}
	//none
	;


//------------------------------------------------------------------------------
//ZoneSendMapConnectReply                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Replies to a clients map connect request, sends the clients position and
//    direction.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//		May 1st, 2007 - Tsusai - Added const to the parameters
//------------------------------------------------------------------------------
	procedure ZoneSendMapConnectReply(const ACharacter : TCharacter);
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
//		May 1st, 2007 - Tsusai - Added const to the parameters
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
//		May 1st, 2007 - Tsusai - Added const to the parameters
//------------------------------------------------------------------------------
	procedure ZoneSendTickToClient(const ACharacter : TCharacter);
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
//		May 1st, 2007 - Tsusai - Added const to the parameters
//------------------------------------------------------------------------------
	procedure ZoneSendWalkReply(
		const ACharacter : TCharacter;
		const DestPoint : TPoint
	);
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
//		May 1st, 2007 - Tsusai - Added const to the parameters
//------------------------------------------------------------------------------
	procedure ZoneSendObjectNameAndIDBasic(
		const ACharacter : TCharacter;
		const ID : LongWord;
		const Name : String
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
//    April 10th, 2007 - Aeomin - Add DelayDisconnect
//		May 1st, 2007 - Tsusai - Added const to the parameters
//------------------------------------------------------------------------------
	procedure SendCharacterSelectResponse(
		const ACharacter : TCharacter
	);
	var
		OutBuffer : TBuffer;
	begin
		ACharacter.DcAndKeepData := True;
		//send leave 2
		WriteBufferWord(0, $00b3,OutBuffer);
		WriteBufferByte(2, 1,OutBuffer);
		SendBuffer(ACharacter.ClientInfo, OutBuffer, GetPacketLength($00b3,ACharacter.ClientVersion));
		ACharacter.DelayDisconnect(10000);
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
//    April 10th, 2007 - Aeomin - Add DelayDisconnect
//		May 1st, 2007 - Tsusai - Added const to the parameters
//------------------------------------------------------------------------------
	procedure SendQuitGameResponse(
		const ACharacter : TCharacter
	);
	var
		OutBuffer : TBuffer;
	begin
//		ACharacter.DcAndKeepData := False;
		//send leave 2
		WriteBufferWord(0, $018b, OutBuffer);
		WriteBufferWord(2, 0, OutBuffer);
		Sendbuffer(ACharacter.ClientInfo, OutBuffer, GetPacketLength($018b, ACharacter.ClientVersion));
		ACharacter.DelayDisconnect(10000);
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
//		May 1st, 2007 - Tsusai - Added const to the parameters
//		May 25th, 2007 - Tsusai - Removed ABeing, changed to
//			APerson, and added TCharacter conditionals
//------------------------------------------------------------------------------
procedure SendAreaChat(
	const Chat				: String;
	const Length			: Word;
	const ACharacter	: TCharacter
);
var
	OutBuffer : TBuffer;
	APerson   : TCharacter;
	idxY			: SmallInt;
	idxX			: SmallInt;
	BeingIdx	: integer;
begin
	//16 covers the old 15x15 grid
	for idxY := Max(ACharacter.Position.Y-MainProc.ZoneServer.Options.CharShowArea,0) to Min(ACharacter.Position.Y+MainProc.ZoneServer.Options.CharShowArea,ACharacter.MapInfo.Size.Y - 1) do
	begin
		for idxX := Max(ACharacter.Position.X-MainProc.ZoneServer.Options.CharShowArea,0) to Min(ACharacter.Position.X+MainProc.ZoneServer.Options.CharShowArea,ACharacter.MapInfo.Size.X - 1) do
		begin
			for BeingIdx := ACharacter.MapInfo.Cell[idxX,idxY].Beings.Count - 1 downto 0 do
			begin
				if not (ACharacter.MapInfo.Cell[idxX,idxY].Beings.Objects[BeingIdx] is TCharacter) then
				begin
					Continue;
				end;
				APerson := ACharacter.MapInfo.Cell[idxX,idxY].Beings.Objects[BeingIdx] as TCharacter;
				if APerson = ACharacter then
				begin
					ZoneSendCharacterMessage(ACharacter, Chat);
				end else
				begin
					WriteBufferWord(0, $008d, OutBuffer);
					WriteBufferWord(2, Length+9, OutBuffer);
					WriteBufferLongWord(4, ACharacter.ID, OutBuffer);
					WriteBufferString(8, Chat+#0, Length+1, OutBuffer);
					Sendbuffer(APerson.ClientInfo, OutBuffer, Length+9);
				end;
			end;
		end;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendGMCommandResultToInter                                           PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the received gm command to the inter server.
//
//  Changes -
//    March 19th, 2007 - RaX - Created Header;
//		May 1st, 2007 - Tsusai - Added const to the parameters
//------------------------------------------------------------------------------
	procedure ZoneSendGMCommandResultToInter(
		const AccountID : LongWord;
		const CharacterID : LongWord;
		const ZoneID : LongWord;
		const Error : TStringList
	);
	var
		ReplyBuffer : TBuffer;
		BufferIndex : Integer;
		CSLength    : Integer;
		Index       : Integer;
	begin
		WriteBufferWord(0, $2207, ReplyBuffer);
		WriteBufferLongWord(4, AccountID, ReplyBuffer);
		WriteBufferLongWord(8, CharacterID, ReplyBuffer);
		WriteBufferLongWord(12, ZoneID, ReplyBuffer);
		WriteBufferWord(16, Error.Count, ReplyBuffer);
		BufferIndex := 18;

		for Index := 0 to Error.Count - 1 do
		begin
			CSLength := Length(Error[Index]);

			WriteBufferWord(BufferIndex, CSLength, ReplyBuffer);
			Inc(BufferIndex, 2);

			WriteBufferString(
				BufferIndex,
				Error[Index],
				CSLength,
				ReplyBuffer
			);
			Inc(BufferIndex, CSLength);
		end;
		WriteBufferWord(2, BufferIndex + 1, ReplyBuffer);
		SendBuffer(MainProc.ZoneServer.ToInterTCPClient,ReplyBuffer,BufferIndex + 1);
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
//		May 1st, 2007 - Tsusai - Added const to the parameters
//------------------------------------------------------------------------------
	procedure ZoneSendCharacterMessage(
		const ACharacter	: TCharacter;
		const AMessage		: String
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
//		May 1st, 2007 - Tsusai - Added const to the parameters
//		May 25th, 2007 - Tsusai - Invisible npcs aren't sent
//------------------------------------------------------------------------------
	procedure ZoneSendBeing(
		const Who:TBeing;
		AClient : TIdContext;
		const Logon:Boolean=False
	);
	var
		ReplyBuffer : TBuffer;
		Chara	    : TCharacter;
	begin
		if Who.JID = NPC_INVISIBLE then
		begin
			exit;
		end;
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
			WriteBufferByte(45, TClientLink(Chara.ClientInfo.Data).AccountLink.GenderNum, ReplyBuffer);
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
//		May 1st, 2007 - Tsusai - Added const to the parameters
//------------------------------------------------------------------------------
	procedure ZoneDisappearBeing(
		const Who:TBeing;
		AClient : TIdContext;
		const Effect:Byte=0
	);
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
//    March 23th, 2007 - Aeomin - Renamed from ZoneWalkingChar to ZoneWalkingBeing
//		May 1st, 2007 - Tsusai - Added const to the parameters
//------------------------------------------------------------------------------
	procedure ZoneWalkingBeing(
		const Who:TBeing;
		const Point1,Point2:TPoint;
		AClient : TIdContext
	);
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
//		May 1st, 2007 - Tsusai - Added const to the parameters
//------------------------------------------------------------------------------
procedure ZoneUpdateDirection(
	Const Who:TBeing;
	AClient : TIdContext
);
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
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendConnectionsCount                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Send total online count to client
//
//  Changes -
//    April 6th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure ZoneSendConnectionsCount(AClient : TIdContext);
var
	ReplyBuffer : TBuffer;
begin
	FillChar(ReplyBuffer,GetPacketLength($00C2),0);
	WriteBufferWord(0, $00C2, ReplyBuffer);
	WriteBufferLongWord(2, MainProc.ZoneServer.TotalOnlinePlayers, ReplyBuffer);
	SendBuffer(AClient,ReplyBuffer,GetPacketLength($00C2));
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Kick                                                                 PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Kick one player
//
//  Changes -
//    March 27th, 2007 - Aeomin - Created Header
//    April 5th, 2007 - Aeomin - Added DelayDisconnect to kill connect if still there.
//------------------------------------------------------------------------------
procedure Kick(const Who:TCharacter);
var
	OutBuffer : TBuffer;
begin
	FillChar(OutBuffer,GetPacketLength($018B),0);
	WriteBufferWord(0, $018B, OutBuffer);
	WriteBufferWord(2, 0, OutBuffer);
	SendBuffer(Who.ClientInfo,OutBuffer,GetPacketLength($018B));
	Who.DelayDisconnect(10000);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//KickAll                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Kick all players in current zone server
//
//  Changes -
//    March 27th, 2007 - Aeomin - Created Header
//		March 29th, 2007 - Tsusai - Reversed loop to prevent Index out of Bounds.
//------------------------------------------------------------------------------
procedure KickAll;
var
	Idx : Integer;
	Chara : TCharacter;
begin
	for Idx := MainProc.ZoneServer.CharacterList.Count-1 downto 0 do
	begin
		Chara := MainProc.ZoneServer.CharacterList[Idx] as TCharacter;
		Kick(Chara);
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//DuplicateSessionKick                                                 PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Show "Someone has already logged in with this ID" and DC.
//
//  Changes -
//    April 12th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure DuplicateSessionKick(InBuffer : TBuffer);
var
	OutBuffer : TBuffer;
	CharID    : LongWord;
	Idx       : Integer;
	Chara : TCharacter;
begin
	CharID := BufferReadLongWord(2,InBuffer);
	Idx := MainProc.ZoneServer.CharacterList.IndexOf(CharID);
	if Idx > -1 then
	begin
		Chara := MainProc.ZoneServer.CharacterList[Idx] as TCharacter;
		FillChar(OutBuffer, GetPacketLength($0081), 0);
		WriteBufferWord(0, $0081, OutBuffer);
		WriteBufferByte(2, 2, OutBuffer);
		SendBuffer(Chara.ClientInfo,OutBuffer,GetPacketLength($0081));
		Chara.DelayDisconnect(10000);
	end;
end;
//------------------------------------------------------------------------------




//------------------------------------------------------------------------------
//ZoneSendWarpRequest                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends a request to the inter server that asks if it's ok for a character
//		to warp to the specified zone.
//
//  Changes -
//    April 26th, 2007 - RaX - Created.
//		May 1st, 2007 - Tsusai - Renamed, and modified parameters recieved.
//		May 25th, 2007 - Tsusai - Removes player from current cell
//------------------------------------------------------------------------------
function ZoneSendWarp(
	const ACharacter : TCharacter;
	const MapName : String;
	const X : Word;
	const Y : Word
):boolean;
var
	OutBuffer : TBuffer;
	Size : Word;
	MapNameSize : Word;
	ClientIPSize : Word;
	MapZoneID			: SmallInt;
	Index  : Integer;

procedure RemoveFromList;
begin
	with ACharacter do
	begin
		//Quick change to prevent characters from being added in bad places.
		if (Position.X < MapInfo.Size.X) AND
			 (Position.Y < MapInfo.Size.Y) AND
			 (Position.X >= 0) AND
			 (Position.X >= 0) then
		begin
			MapInfo.Cell[Position.X, Position.Y].Beings.Delete(
			MapInfo.Cell[Position.X, Position.Y].Beings.IndexOfObject(ACharacter));
		end;
	end;
end;

begin
	TThreadLink(ACharacter.ClientInfo.Data).DatabaseLink.StaticData.Connect;
  try
	MapZoneID := TThreadLink(ACharacter.ClientInfo.Data).DatabaseLink.StaticData.GetMapZoneID(MapName);
	if MapZoneID < 0 then
	begin
		Result := False;
	end else
	begin
		if Cardinal(MapZoneID) = MainProc.ZoneServer.Options.ID then
		begin
			for Index := ACharacter.EventList.Count -1 downto 0 do
			begin
				if ACharacter.EventList.Items[Index] is TMovementEvent then
				begin
					ACharacter.EventList.Delete(Index);
				end;
			end;
			RemoveFromList;
			ACharacter.Map := MapName;
			ACharacter.Position := Point(X,Y);
			WriteBufferWord(0, $0091, OutBuffer);
			WriteBufferString(2, MapName+'.rsw', 16, OutBuffer);
			WriteBufferWord(18, X, OutBuffer);
			WriteBufferWord(20, Y, OutBuffer);
			SendBuffer(ACharacter.ClientInfo, OutBuffer, 22);
		end else
		begin
			MapNameSize := Length(MapName);
			ClientIPSize := Length(ACharacter.ClientInfo.Binding.PeerIP);
			Size := ClientIPSize + MapNameSize + 16;
			//<id>,<size>,<cid>,<mapnamesize>,<mapname>,<clientipsize>,<clientip>
			WriteBufferWord(0, $2208, OutBuffer);
			WriteBufferWord(2, Size, OutBuffer);
			WriteBufferLongWord(4, ACharacter.CID, OutBuffer);
			WriteBufferWord(8, X, OutBuffer);
			WriteBufferWord(10, Y, OutBuffer);
			WriteBufferWord(12, MapNameSize, OutBuffer);
			WriteBufferString(14, MapName, Length(MapName), OutBuffer);
			WriteBufferWord(14+MapNameSize, ClientIPSize, OutBuffer);
			WriteBufferString(
				16+MapNameSize,
			ACharacter.ClientInfo.Binding.PeerIP,
			Length(ACharacter.ClientInfo.Binding.PeerIP),
				OutBuffer
				);
			SendBuffer(MainProc.ZoneServer.ToInterTCPClient,OutBuffer,Size);
		end;
		Result := True;
	end;
  finally
	  TThreadLink(ACharacter.ClientInfo.Data).DatabaseLink.StaticData.Disconnect;
  end;
end;
//------------------------------------------------------------------------------

procedure SendWhisper(const FromName,Whisper: String;AClient : TIdContext);
var
	OutBuffer : TBuffer;
	Len : Integer;
begin
	Len := StrLen(PChar(Whisper));
	WriteBufferWord(0, $0097, OutBuffer);
	WriteBufferWord(2, Len + 29, OutBuffer);
	WriteBufferString(4, FromName, 24, OutBuffer);
	WriteBufferString(28, Whisper, Len + 1, OutBuffer);
	SendBuffer(AClient,OutBuffer, Len + 29);
end;

procedure SendWhisperReply(const AChara:TCharacter; const Code : Byte);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $0098, OutBuffer);
	WriteBufferByte(2, Code, OutBuffer);
	SendBuffer(AChara.ClientInfo,OutBuffer,GetPacketLength($0098));
end;


//------------------------------------------------------------------------------
//SendGMAnnounce                                                       PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends a GM announcement =p
//
//  Changes -
//	[2007/08/09] Aeomin - Created.
//
//------------------------------------------------------------------------------
procedure SendGMAnnounce(AClient : TIdContext;const Announce:String;const Blue:boolean=False);
var
	OutBuffer : TBuffer;
	Size      : Word;
	OffSet    : Byte;
begin
	Size := StrLen(PChar(Announce));
	if Blue then
		Offset := 8
	else
		Offset := 4;
	WriteBufferWord(0, $009a, OutBuffer);
	WriteBufferWord(2, Size + Offset, OutBuffer);
	if Blue then
		WriteBufferLongWord(4, $65756c62, OutBuffer);
	WriteBufferString(Offset, Announce, Size, OutBuffer);
	SendBuffer(AClient, OutBuffer, Size + Offset);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendStatUPResult                                                     PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Tell client if stat up was success or not
//
//  Changes -
//	[2007/08/20] Aeomin - Created.
//
//------------------------------------------------------------------------------
procedure SendStatUPResult(const AChara:TCharacter;const Failed:Boolean;const Amount:Byte);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $00bc, OutBuffer);
	if Failed then
		WriteBufferByte(2, 0, OutBuffer)
	else
		WriteBufferByte(2, 1, OutBuffer);
	WriteBufferByte(3, Amount, OutBuffer);
	SendBuffer(AChara.ClientInfo,OutBuffer,GetPacketLength($00bc));
end;
//------------------------------------------------------------------------------
end.
