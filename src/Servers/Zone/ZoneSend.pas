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
//		[2007/10/25] Aeomin - major clean up #1
//		June 28th, 2008 - Tsusai - Updated GetPacketLength to PacketDB.GetLength
//			in various calls
//
//------------------------------------------------------------------------------
unit ZoneSend;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	Classes,
	Types,
	{Project}
	Being,
	GameTypes,
	Character,
	PacketTypes,
	Mailbox,
	Inventory,
	Item,
	ItemInstance,
	{Third Party}
	IdContext
	;

	procedure SendCharID(
		const Who:TCharacter
	);

	procedure DuplicateSessionKick(
		InBuffer : TBuffer
	);
	procedure Kick(
		const Who:TCharacter
	);
	procedure KickAll;


	procedure SendAreaChat(
		const Chat : String;
		const Length : Word;
		const ACharacter : TCharacter
	);
	procedure SendCharacterSelectResponse(
		const ACharacter : TCharacter
	);
	procedure SendGMAnnounce(
		AClient : TIdContext;
		const Announce:String;
		const Blue:boolean=False
	);
	procedure SendGMAnnounceColor(
		AClient : TIdContext;
		const Announce:String;
		Color:Integer
	);
	procedure SendQuitGameResponse(
		const ACharacter : TCharacter
	);
	procedure SendStatUPResult(
		const AChara:TCharacter;
		const Failed:Boolean;
		const Amount:Byte
	);
	procedure SendDeleteFriend(
		AClient : TIdContext;
		const AccID : LongWord;
		const CharID : LongWord
	);


	procedure SendWhisper(
		const FromName,Whisper: String;
		AClient : TIdContext
	);
	procedure SendWhisperReply(
		const AChara:TCharacter;
		const Code : Byte
	);

	procedure SendSpecialEffect(
		const Who:TBeing;
		AClient : TIdContext;
		const EffectID : LongWord
	);

	procedure SendEmotion(
		const Who:TBeing;
		AClient : TIdContext;
		const EmotionID : Byte
	);

	procedure ZoneDisappearBeing(
		const Who:TBeing;
		AClient : TIdContext;
		const Effect:Byte=0
	);
	procedure ZoneSendBeing(
		const Who:TBeing;
		const AClient : TCharacter;
		const Spawn:Boolean=False
	);
	procedure ZoneSendCharacterMessage(
		const ACharacter	: TCharacter;
		const AMessage		: String
	);
	procedure ZoneSendConnectionsCount(
		AClient : TIdContext
	);
	procedure ZoneSendMapConnectReply(
		const ACharacter : TCharacter
	);
	procedure ZoneSendMapConnectDeny(
		AClient : TIdContext
	);
	procedure ZoneSendObjectNameAndIDBasic(
		Const ACharacter : TCharacter;
		const ID : LongWord;
		const Name : String
	);
	procedure ZoneSendTickToClient(
		const ACharacter : TCharacter
	);
	procedure ZoneSendWalkReply(
		const ACharacter : TCharacter;
		const DestPoint : TPoint
	);
	function ZoneSendWarp(
		const ACharacter : TCharacter;
		const MapName : String;
		const X : Word;
		const Y : Word
	):Boolean;
	procedure ZoneWalkingBeing(
		const Who:TBeing;
		const Point1,Point2:TPoint;
		AClient : TIdContext
	);
	procedure ZoneUpdateDirection(
		const Who:TBeing;
		AClient : TIdContext
	);
	procedure SendAddFriendRequest(
		AClient : TIdContext;
		const ReqAID, ReqID  : LongWord;
		const ReqName        : String
	);
	procedure SendAddFriendRequestReply(
		AClient : TIdContext;
		const AccID  : LongWord;
		const CharID : LongWord;
		const CharName : String;
		const Reply  : Byte
	);
	procedure SendFirendOnlineStatus(
		AClient       : TIdContext;
		const AID     : LongWord;
		const CID     : LongWord;
		const Offline : Byte
	);
	procedure DoAction(
		AClient			: TIdContext;
		SourceID		: LongWord;
		TargetID		: LongWord;
		SourceSpeed	: LongWord;
		TargetSpeed	: LongWord;
		ActionType	: Byte;
		Parameter1	: Word;
		Parameter2	: Word;
		Parameter3	: Word
	);
	procedure SendCompass(
		AClient		: TIdContext;
		const NPCID	: LongWord;
		const PointID	: Byte;
		const X		: LongWord;
		const Y		: LongWord;
		const PointType : LongWord;
		const Color     : LongWord
	);
  //Spres attempt to make a SetJob Packet thingie
 { procedure SendJID(
    AClient   : TIdContext;
    const jobchange  : Integer;
  );     }
	procedure SendCutin(
		AClient		: TIdContext;
		const Image	: String;
		const ImageType : Byte
	);
	procedure ToggleMailWindow(
		const AChara	: TCharacter;
		const Open : Boolean
	);
	procedure SendMailList(
		const AChara : TCharacter
	);
	procedure SendMailContent(
		AClient		: TIdContext;
		const Mail : TMail
	);
	procedure SendDeleteMailResult(
		const AChara	: TCharacter;
		const MailID:LongWord;
		const Flag : Boolean
	);
	procedure SendMailResult(
		const AChara	: TCharacter;
		const Fail  :Boolean
	);
	procedure SendNewMailNotify(
		const AChara	: TCharacter;
		const MailID	: LongWord;
		const Sender	: String;
		const Title	: String
	);
	procedure SendInventory(
		AClient		: TIdContext;
		const AInventory : TInventory
	);
	procedure SendNewItem(
		AClient		: TIdContext;
		const AInventory : TItemInstance;
		const Index	: Word;
		const Amount	: Word
	);
	procedure SendNewItemFailed(
		AClient		: TIdContext;
		const Flag	: Byte
	);
	procedure SendStatusIcon(
		const AChara	: TCharacter;
		const IconID	: Word;
		const Active	: Boolean
	);

	procedure SendUpdatedLook(
		const AChara			: TCharacter;
		const CharacterID	: LongWord;
		const AType				: TLookTypes;
		const Value1			: Word;
		const Value2			: Word
);
implementation


uses
	{RTL/VCL}
	Math,
	SysUtils,
	{Project}
	BufferIO,
	GameConstants,
	Globals,
	Main,
	ItemTypes,
	UseableItem,
	EquipmentItem,
	MiscItem,
	TCPServerRoutines,
	WinLinux
	{3rd Party}
	//none
	;

//------------------------------------------------------------------------------
//SendCharID                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Old client uses padding, this is for new client, which send
//	right after zone auth.
//--
//	Pre:
//		TODO
//	Post:
//		TODO
//--
//	Changes -
//		[2007/0608] Aeomin - Created
//------------------------------------------------------------------------------
procedure SendCharID(
		const Who:TCharacter
	);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $0283, OutBuffer);
	WriteBufferLongWord(2, Who.ID, OutBuffer);
	SendBuffer(Who.ClientInfo,OutBuffer,PacketDB.GetLength($0283,Who.ClientVersion));
end;{SendCharID}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//DuplicateSessionKick                                                 PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Show "Someone has already logged in with this ID" and DC.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    April 12th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure DuplicateSessionKick(
	InBuffer : TBuffer
	);
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
		WriteBufferWord(0, $0081, OutBuffer);
		WriteBufferByte(2, 2, OutBuffer);
		SendBuffer(Chara.ClientInfo,OutBuffer,PacketDB.GetLength($0081));
		Chara.DelayDisconnect(10000);
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Kick                                                                 PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Kick one player
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    March 27th, 2007 - Aeomin - Created Header
//    April 5th, 2007 - Aeomin - Added DelayDisconnect to kill connect if still there.
//------------------------------------------------------------------------------
procedure Kick(
	const Who:TCharacter
	);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $018B, OutBuffer);
	WriteBufferWord(2, 0, OutBuffer);
	SendBuffer(Who.ClientInfo,OutBuffer,PacketDB.GetLength($018B));
	Who.DelayDisconnect(GetTick + 10000);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//KickAll                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Kick all players in current zone server
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
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
		if MainProc.ZoneServer.CharacterList.Count > Idx then
		begin
			Chara := MainProc.ZoneServer.CharacterList[Idx] as TCharacter;
			Kick(Chara);
		end;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendAreaChat                                                         PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//    	Send chat message to local area
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    March 18th, 2007 - Aeomin - Created
//		May 1st, 2007 - Tsusai - Added const to the parameters
//		May 25th, 2007 - Tsusai - Removed ABeing, changed to
//			APerson, and added TCharacter conditionals
//	[2008/06/08] Aeomin - Updated packet structure
//------------------------------------------------------------------------------
procedure SendAreaChat(
	const Chat		: String;
	const Length		: Word;
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
end;{SendAreaChat}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendCharacterSelectResponse                                          PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Tells the client to return to character select. Triggered by ZoneRecv ->
//		ReturnToCharacterSelect.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
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
	SendBuffer(ACharacter.ClientInfo, OutBuffer, PacketDB.GetLength($00b3,ACharacter.ClientVersion));
	ACharacter.DelayDisconnect(10000);
end;//SendCharacterSelectResponse
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendGMAnnounce                                                       PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends a GM announcement =p
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/08/09] Aeomin - Created.
//
//------------------------------------------------------------------------------
procedure SendGMAnnounce(
	AClient : TIdContext;
	const Announce:String;
	const Blue:boolean=False
	);
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
//SendGMAnnounceColor                                                  PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends a GM announcement in color.
//      Should probably combine with SendGMAnnounce later.
//
//  Changes -
//	[2007/11/12] RabidChocobo - Created.
//
//------------------------------------------------------------------------------
procedure SendGMAnnounceColor(
	AClient : TIdContext;
	const Announce : String;
	Color : Integer
);
var
	OutBuffer : TBuffer;
	Size      : Word;
	OffSet    : Byte;
begin
	Size := StrLen(PChar(Announce)) + 1;
	Offset := 16;
	WriteBufferWord(0, $01c3, OutBuffer);
	WriteBufferWord(2, Size + Offset, OutBuffer);
	WriteBufferLongWord(4, Color, OutBuffer);
	WriteBufferWord(8, $0190, OutBuffer);
	WriteBufferWord(10, $000c, OutBuffer);
	WriteBufferLongWord(12, 0, OutBuffer);
	WriteBufferString(Offset, Announce, Size, OutBuffer);
	SendBuffer(AClient, OutBuffer, Offset + Size);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendQuitGameResponse                                                 PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//    	Tells the client to "Exit to windows". Triggered by ZoneRecv ->
//    QuitGame.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
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
	//ACharacter.DcAndKeepData := False;
	//send leave 2
	WriteBufferWord(0, $018b, OutBuffer);
	WriteBufferWord(2, 0, OutBuffer);
	Sendbuffer(ACharacter.ClientInfo, OutBuffer, PacketDB.GetLength($018b, ACharacter.ClientVersion));
	ACharacter.DelayDisconnect(GetTick + 10000);
end;{SendQuitGameResponse}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendStatUPResult                                                     PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Tell client if stat up was success or not
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/08/20] Aeomin - Created.
//
//------------------------------------------------------------------------------
procedure SendStatUPResult(
	const AChara:TCharacter;
	const Failed:Boolean;
	const Amount:Byte
	);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $00bc, OutBuffer);
	if Failed then
		WriteBufferByte(2, 0, OutBuffer)
	else
		WriteBufferByte(2, 1, OutBuffer);
	WriteBufferByte(3, Amount, OutBuffer);
	SendBuffer(AChara.ClientInfo,OutBuffer,PacketDB.GetLength($00bc));
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendDeleteFriend                                                     PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Tell client to delete a friend from friend list.
//--
//   Pre:
//   Post:
//	TODO
//--
//  Changes -
//	[2007/12/06] Aeomin - Created.
//
//------------------------------------------------------------------------------
procedure SendDeleteFriend(
	AClient : TIdContext;
	const AccID : LongWord;
	const CharID : LongWord
);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $020a, OutBuffer);
	WriteBufferLongWord(2, AccID, OutBuffer);
	WriteBufferLongWord(6, CharID, OutBuffer);
	SendBuffer(AClient, OutBuffer, PacketDB.GetLength($020a));
end;{SendDeleteFriend}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendWhisper                                                          PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Send whisper to client
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/10/25] Aeomin - Added header
//
//------------------------------------------------------------------------------
procedure SendWhisper(
	const FromName,Whisper: String;
	AClient : TIdContext
	);
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
end;{SendWhisper}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendWhisperReply                                                     PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Send whisper status to client (success or fail)
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/10/25] Aeomin - Added header
//
//------------------------------------------------------------------------------
procedure SendWhisperReply(
	const AChara:TCharacter;
	const Code : Byte
	);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $0098, OutBuffer);
	WriteBufferByte(2, Code, OutBuffer);
	SendBuffer(AChara.ClientInfo,OutBuffer,PacketDB.GetLength($0098));
end;{SendWhisperReply}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendSpecialEffect                                                    PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Send effect packet
//
//	Changes-
//		[2007/11/24] Aeomin - Created.
//------------------------------------------------------------------------------
procedure SendSpecialEffect(
	const Who:TBeing;
	AClient : TIdContext;
	const EffectID : LongWord
);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $01f3, OutBuffer);
	WriteBufferLongWord(2, Who.ID, OutBuffer);
	WriteBufferLongWord(6, EffectID, OutBuffer);
	SendBuffer(AClient, OutBuffer, PacketDB.GetLength($01f3));
end;{SendSpecialEffec}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//SendEmotion                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Send emotion packet
//
//	Changes-
//		[2007/11/24] Aeomin - Created.
//------------------------------------------------------------------------------
procedure SendEmotion(
	const Who:TBeing;
	AClient : TIdContext;
	const EmotionID : Byte
);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $00c0, OutBuffer);
	WriteBufferLongWord(2, Who.ID, OutBuffer);
	WriteBufferByte(6, EmotionID, OutBuffer);
	SendBuffer(AClient, OutBuffer, PacketDB.GetLength($00c0));
end;{SendEmotion}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//ZoneWalkingBeing                                                     PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Make character disappear
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
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
	WriteBufferWord(0, $0080, ReplyBuffer);
	WriteBufferLongWord(2, Who.ID, ReplyBuffer);
	WriteBufferByte(6, Effect, ReplyBuffer);
	SendBuffer(AClient,ReplyBuffer,PacketDB.GetLength($0080));
end;{ZoneDisappearBeing}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendBeing                                                        PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      make character visible
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    March 18th, 2007 - Aeomin - Created Header
//    March 23th, 2007 - Aeomin - Renamed from ZoneSendChar to ZoneSendBeing
//                                and support for Npc/Mob etc
//		May 1st, 2007 - Tsusai - Added const to the parameters
//		May 25th, 2007 - Tsusai - Invisible npcs aren't sent
//	[2008/06/08] Aeomin - Redo!
//------------------------------------------------------------------------------
procedure ZoneSendBeing(
	const Who:TBeing;
	const AClient : TCharacter;
	const Spawn:Boolean=False
	);
var
	ReplyBuffer : TBuffer;
	Chara	    : TCharacter;
	procedure SubSendPlayer;
	begin
		if Spawn then
		begin
			WriteBufferWord(0, $022b, ReplyBuffer);
		end else
		begin
			WriteBufferWord(0, $022a, ReplyBuffer);
		end;
		WriteBufferLongWord(2, Who.ID, ReplyBuffer);
		WriteBufferWord(6, Who.Speed, ReplyBuffer);
		WriteBufferWord(8, Who.Status, ReplyBuffer);
		WriteBufferWord(10, Who.Ailments, ReplyBuffer);
		WriteBufferWord(12, Who.Option, ReplyBuffer);
		{2 bytes shiftload}
		WriteBufferWord(16, Who.JID, ReplyBuffer);
		WriteBufferWord(34, Who.Direction, ReplyBuffer);
		Chara:=TCharacter(Who);
		{Todo: Hair stlye, head bottom needed for Pet...}
		WriteBufferWord(18, Chara.Hair, ReplyBuffer);
		WriteBufferWord(20, Chara.RightHand, ReplyBuffer);  //Weapon
		WriteBufferWord(22, Chara.LeftHand, ReplyBuffer);  //Shield
		WriteBufferWord(24, Chara.HeadBottom, ReplyBuffer);  //Head bottom
		WriteBufferWord(26, Chara.HeadTop, ReplyBuffer);  //Head top
		WriteBufferWord(28, Chara.HeadMid, ReplyBuffer);  //head mid
		WriteBufferWord(30, Chara.HairColor, ReplyBuffer);
		WriteBufferWord(32, Chara.ClothesColor, ReplyBuffer);
		WriteBufferWord(34, Chara.HeadDirection, ReplyBuffer);
		WriteBufferLongWord(36, Chara.GuildID, ReplyBuffer);
		WriteBufferWord(40, 0, ReplyBuffer);  //Emblem ID
		WriteBufferWord(42, 0, ReplyBuffer);   //Manner
		WriteBufferLongWord(44, 0, ReplyBuffer); //opt3?
		{2 more shiftload}
		WriteBufferByte(48, Chara.Karma, ReplyBuffer);
		WriteBufferByte(49, TClientLink(Chara.ClientInfo.Data).AccountLink.GenderNum, ReplyBuffer);
		//WriteBufferByte(44, 0, ReplyBuffer);  //Normal/Ready to fight
		WriteBufferPointAndDirection(50, Who.Position, ReplyBuffer,Who.Direction);
		WriteBufferByte(53, 5, ReplyBuffer);
		WriteBufferByte(54, 5, ReplyBuffer);
		if Spawn then
		begin
			WriteBufferWord(55, Who.BaseLV, ReplyBuffer);
			SendBuffer(AClient.ClientInfo,ReplyBuffer,PacketDB.GetLength($022b,AClient.ClientVersion));
		end else
		begin
			WriteBufferByte(55, 0, ReplyBuffer);   //Standing/Dead/Sit
			WriteBufferWord(56, Who.BaseLV, ReplyBuffer);
			SendBuffer(AClient.ClientInfo,ReplyBuffer,PacketDB.GetLength($022a,AClient.ClientVersion));
		end;
	end;
	procedure SubSendNPC;
	begin
		FillChar(ReplyBuffer,PacketDB.GetLength($0078),0);
		WriteBufferWord(0, $0078, ReplyBuffer);
		WriteBufferByte(2, 0, ReplyBuffer);
		WriteBufferLongWord(3, Who.ID, ReplyBuffer);
		WriteBufferWord(7, Who.Speed, ReplyBuffer);
		WriteBufferWord(13, Who.Option, ReplyBuffer);
		WriteBufferWord(15, Who.JID, ReplyBuffer);
		WriteBufferWord(17, 0, ReplyBuffer);   //hair_style
		WriteBufferWord(19, 0, ReplyBuffer);   //Weapon
		WriteBufferWord(21, 0, ReplyBuffer);   //Shield
		WriteBufferWord(23, 0, ReplyBuffer);   //headbottom
		WriteBufferPointAndDirection(47, Who.Position, ReplyBuffer,Who.Direction);
		WriteBufferByte(50, 5, ReplyBuffer);
		WriteBufferByte(51, 5, ReplyBuffer);
		SendBuffer(AClient.ClientInfo,ReplyBuffer,PacketDB.GetLength($0078,AClient.ClientVersion));
	end;
begin
	if Who.JID = NPC_INVISIBLE then
	begin
		Exit;
	end;
	//Old Packet Version
//	FillChar(ReplyBuffer,54,0);
	if Who is TCharacter then
		SubSendPlayer
	else
		SubSendNPC;
end;{ZoneSendBeing}

//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendGMCommandResultToInter                                       PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the received gm command to the inter server.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
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
end;{ZoneSendCharacterMessage}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendConnectionsCount                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Send total online count to client
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    April 6th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure ZoneSendConnectionsCount(
	AClient : TIdContext
	);
var
	ReplyBuffer : TBuffer;
begin
	FillChar(ReplyBuffer,PacketDB.GetLength($00C2),0);
	WriteBufferWord(0, $00C2, ReplyBuffer);
	WriteBufferLongWord(2, MainProc.ZoneServer.TotalOnlinePlayers, ReplyBuffer);
	SendBuffer(AClient,ReplyBuffer,PacketDB.GetLength($00C2));
end;{ZoneSendConnectionsCount}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendMapConnectReply                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Replies to a clients map connect request, sends the clients position and
//    direction.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//		May 1st, 2007 - Tsusai - Added const to the parameters
//------------------------------------------------------------------------------
procedure ZoneSendMapConnectReply(
	const ACharacter : TCharacter
	);
var
	ReplyBuffer : TBuffer;
begin
	WriteBufferWord(0, $0073, ReplyBuffer);
	WriteBufferLongWord(2, GetTick, ReplyBuffer);
	WriteBufferPointAndDirection(6, ACharacter.Position,ReplyBuffer,ACharacter.Direction);
	WriteBufferByte(9, 5, ReplyBuffer);
	WriteBufferByte(10, 5, ReplyBuffer);
	SendBuffer(ACharacter.ClientInfo,ReplyBuffer,PacketDB.GetLength($0073,ACharacter.ClientVersion));
end;{ZoneSendMapConnectReply}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendMapConnectDeny                                               PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Denys a client access to the map server.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//		May 1st, 2007 - Tsusai - Added const to the parameters
//------------------------------------------------------------------------------
procedure ZoneSendMapConnectDeny(
	AClient : TIdContext
	);
var
	ReplyBuffer : TBuffer;
begin
	WriteBufferWord(0, $0074, ReplyBuffer);
	WriteBufferByte(2, 0 ,    ReplyBuffer);
	SendBuffer(AClient,ReplyBuffer,PacketDB.GetLength($0074));
end;{ZoneSendMapConnectReply}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendObjectNameAndIDBasic                                         PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends a characters name and ID to the client.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
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
	SendBuffer(ACharacter.ClientInfo, OutBuffer, PacketDB.GetLength($0095,ACharacter.ClientVersion));
end;{ZoneSendObjectNameAndIDBasic}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendTickToClient                                                 PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends a ping or "tick" to the client to make sure that it's still there.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//		May 1st, 2007 - Tsusai - Added const to the parameters
//------------------------------------------------------------------------------
procedure ZoneSendTickToClient(
	const ACharacter : TCharacter
	);
var
	ReplyBuffer : TBuffer;
begin
	WriteBufferWord(0, $007f, ReplyBuffer);
	WriteBufferLongWord(2, GetTick, ReplyBuffer);
	SendBuffer(ACharacter.ClientInfo, ReplyBuffer, PacketDB.GetLength($007f,ACharacter.ClientVersion));
end;{ZoneSendTickToClient}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendWalkReply                                                    PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends a packet that tells the client to go ahead and walk.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
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
	SendBuffer(ACharacter.ClientInfo, ReplyBuffer, PacketDB.GetLength($0087, ACharacter.ClientVersion));
end;{ZoneSendWalkReply}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendWarp                                                  PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends a request to the inter server that asks if it's ok for a character
//		to warp to the specified zone, or warp them within
//		the current zone.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
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
	MapZoneID	: SmallInt;

begin
		MapZoneID := TThreadLink(ACharacter.ClientInfo.Data).DatabaseLink.Map.GetZoneID(MapName);
		if MapZoneID < 0 then
		begin
			Result := False;
		end else
		begin

			ACharacter.EventList.DeleteMovementEvents;

			if Cardinal(MapZoneID) = MainProc.ZoneServer.Options.ID then
			begin
				ACharacter.RemoveFromMap;
				ACharacter.ShowTeleportOut;
				ACharacter.ZoneStatus := isOffline;
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
				WriteBufferLongWord(4, ACharacter.ID, OutBuffer);
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
end;{ZoneSendWarp}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneWalkingBeing                                                     PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Send the destination of charater.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
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
	FillChar(ReplyBuffer,PacketDB.GetLength($0086),0);
	WriteBufferWord(0, $0086, ReplyBuffer);
	WriteBufferLongWord(2, Who.ID, ReplyBuffer);
	WriteBufferTwoPoints(6, Point1, Point2, ReplyBuffer);
	WriteBufferLongWord(12, GetTick, ReplyBuffer);
	SendBuffer(AClient,ReplyBuffer,PacketDB.GetLength($0086));
end;{ZoneWalkingBeing}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneUpdateDirection                                                  PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Update Character Direction to other characters
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
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
	WriteBufferWord(0, $009c, ReplyBuffer);
	WriteBufferLongWord(2, Who.ID, ReplyBuffer);
	WriteBufferWord(6, Who.HeadDirection, ReplyBuffer);
	WriteBufferByte(8, Who.Direction, ReplyBuffer);
	SendBuffer(AClient,ReplyBuffer,PacketDB.GetLength($009c));
end;{ZoneUpdateDirection}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendAddFriendRequest                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Send client request...
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    [2007/12/07] Aeomin - Created.
//------------------------------------------------------------------------------
procedure SendAddFriendRequest(
	AClient : TIdContext;
	const ReqAID, ReqID  : LongWord;
	const ReqName        : String
);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $0207, OutBuffer);
	WriteBufferLongWord(2, ReqAID, OutBuffer);
	WriteBufferLongWord(6, ReqID, OutBuffer);
	WriteBufferString(10, ReqName, NAME_LENGTH, OutBuffer);
	SendBuffer(AClient,OutBuffer,PacketDB.GetLength($0207));
end;{ZoneSendAddFriendRequest}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendAddFriendRequestReply                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Send reply to client
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    [2007/12/08] Aeomin - Created.
//------------------------------------------------------------------------------
procedure SendAddFriendRequestReply(
	AClient : TIdContext;
	const AccID  : LongWord;
	const CharID : LongWord;
	const CharName : String;
	const Reply  : Byte
);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $0209, OutBuffer);
	WriteBufferWord(2, Reply, OutBuffer);
	WriteBufferLongWord(4, AccID, OutBuffer);
	WriteBufferLongWord(8, CharID, OutBuffer);
	WriteBufferString(12, CharName, NAME_LENGTH, OutBuffer);
	SendBuffer(AClient,OutBuffer,PacketDB.GetLength($0209));
end;{SendAddFriendRequestReply}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendFirendOnlineStatus                                               PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//    Send online status of a friend to client.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/12/??] - Aeomin - Created
//------------------------------------------------------------------------------
procedure SendFirendOnlineStatus(
	AClient       : TIdContext;
	const AID     : LongWord;
	const CID     : LongWord;
	const Offline : Byte
);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $0206, OutBuffer);
	WriteBufferLongWord(2, AID, OutBuffer);
	WriteBufferLongWord(6, CID, OutBuffer);
	WriteBufferByte(10, Offline, OutBuffer);
	SendBuffer(AClient,OutBuffer,PacketDB.GetLength($0206));
end;{SendFirendOnlineStatus}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//DoAction                                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      tells a client to show an action made by another entity or itself.
//		SEE TACTIONTYPE FOR A LIST OF ACTIONS.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    [2007/12/24] RaX - Created.
//------------------------------------------------------------------------------
procedure DoAction(
	AClient			: TIdContext;
	SourceID		: LongWord;
	TargetID		: LongWord;
	SourceSpeed	: LongWord;
	TargetSpeed	: LongWord;
	ActionType	: Byte;
	Parameter1	: Word;
	Parameter2	: Word;
	Parameter3	: Word
);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $008a, OutBuffer);
	WriteBufferLongWord(2, SourceID, OutBuffer);
	WriteBufferLongWord(6, TargetID, OutBuffer);
	WriteBufferLongWord(10, GetTick(), OutBuffer);
	WriteBufferLongWord(14, SourceSpeed, OutBuffer);
	WriteBufferLongWord(18, TargetSpeed, OutBuffer);
	WriteBufferWord(22, Parameter1, OutBuffer);
	WriteBufferWord(24, Parameter2, OutBuffer);
	WriteBufferByte(26, ActionType, OutBuffer);
	WriteBufferWord(27, Parameter3, OutBuffer);

	SendBuffer(AClient,OutBuffer,PacketDB.GetLength($008a));
end;{DoAction}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendCompass                                                          PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Add/Remove "+" mark in mini map
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    [2007/12/31] Aeomin - Created.
//------------------------------------------------------------------------------
procedure SendCompass(
	AClient		: TIdContext;
	const NPCID	: LongWord;
	const PointID	: Byte;
	const X		: LongWord;
	const Y		: LongWord;
	const PointType : LongWord;
	const Color     : LongWord
);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $0144, OutBuffer);
	WriteBufferLongWord(2, NPCID, OutBuffer);
	WriteBufferLongWord(6, PointType, OutBuffer);
	WriteBufferLongWord(10, X, OutBuffer);
	WriteBufferLongWord(14, Y, OutBuffer);
	WriteBufferByte(18, PointID, OutBuffer);
	WriteBufferLongWord(19, Color, OutBuffer);
	SendBuffer(AClient,OutBuffer,PacketDB.GetLength($0144));
end;{SendCompass}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//SendCutin                                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//	Send cutin thingy to client.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    [2007/12/31] Aeomin - Created.
//------------------------------------------------------------------------------
procedure SendCutin(
	AClient		: TIdContext;
	const Image	: String;
	const ImageType : Byte
);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $01b3, OutBuffer);
	WriteBufferString(2, Image, 64, OutBuffer);
	WriteBufferByte(66, ImageType, OutBuffer);
	SendBuffer(AClient,OutBuffer,PacketDB.GetLength($01b3));
end;{SendCutin}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ToggleMailWindow                                                     PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//	Open/Close mail window on client
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    [2008/06/11] Aeomin - Created.
//------------------------------------------------------------------------------
procedure ToggleMailWindow(
	const AChara	: TCharacter;
	const Open : Boolean
);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $0260, OutBuffer);
	WriteBufferLongWord(2, Byte(not Open), OutBuffer);
	{TODO:Packet version...}
	SendBuffer(AChara.ClientInfo,OutBuffer,PacketDB.GetLength($0260,AChara.ClientVersion));
end;{ToggleMailWindow}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendMailList                                                         PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//		Send a list of mails
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//		[2008/06/12] - Aeomin - Created
//------------------------------------------------------------------------------
procedure SendMailList(
	const AChara : TCharacter
);
var
	Index : Integer;
	Mail : TMail;
	OutBuffer : TBuffer;
	Len : Word;
begin
	{This procedure will check if load from database was needed}
	AChara.Mails.LoadMails;
	if AChara.Mails.Mails > 0 then
	begin
		Len := (73*AChara.Mails.Mails) + 8;

		WriteBufferWord(0, $0240, OutBuffer);
		WriteBufferWord(2, Len, OutBuffer);
		WriteBufferLongWord(4, AChara.Mails.Mails, OutBuffer);

		for Index := 0 to AChara.Mails.Mails - 1 do
		begin
			Mail := AChara.Mails.Item[Index];
			WriteBufferLongWord(73*Index+8, Mail.ID, OutBuffer);
			WriteBufferString(73*Index+12, Mail.Title, 40, OutBuffer);
			WriteBufferByte(73*Index+52, Byte(Mail.Read), OutBuffer);
			WriteBufferString(73*Index+53, Mail.SenderName, NAME_LENGTH, OutBuffer);
			WriteBufferLongWord(73*Index+77, Mail.SendTime, OutBuffer);
		end;
		SendBuffer(AChara.ClientInfo,OutBuffer,Len);
	end;
end;{SendMailList}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendMailContent                                                      PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//		Send a mail
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//		[2008/06/12] - Aeomin - Created
//------------------------------------------------------------------------------
procedure SendMailContent(
	AClient		: TIdContext;
	const Mail : TMail
);
var
	MessageLen : Byte;
	Len : Word;
	OutBuffer : TBuffer;
begin
	MessageLen := Length(Mail.Content);
	Len := MessageLen + 101;

	WriteBufferWord(0, $0242, OutBuffer);
	WriteBufferWord(2, Len, OutBuffer);
	WriteBufferLongWord(4, Mail.ID, OutBuffer);
	WriteBufferString(8, Mail.Title, 40, OutBuffer);
	WriteBufferString(48, Mail.SenderName, NAME_LENGTH, OutBuffer);
	WriteBufferLongWord(72, 0, OutBuffer);{??}
	WriteBufferLongWord(76, 0, OutBuffer);{Zeny}
	FillChar(OutBuffer[80],19,$0); {No item support yet}
	WriteBufferByte(99, MessageLen, OutBuffer);
	WriteBufferString(100, Mail.Content, MessageLen, OutBuffer);
	SendBuffer(AClient,OutBuffer,Len);
end;{SendMailContent}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendDeleteMailResult                                                 PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//		Send result of delete mail.
//	Packet size still need improve -.-"
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//		[2008/08/09] - Aeomin - Created
//------------------------------------------------------------------------------
procedure SendDeleteMailResult(
	const AChara		: TCharacter;
	const MailID:LongWord;
	const Flag : Boolean
);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $0257, OutBuffer);
	WriteBufferLongWord(2, MailID, OutBuffer);
	WriteBufferWord(6, Byte(NOT Flag), OutBuffer);
	SendBuffer(AChara.ClientInfo,OutBuffer,PacketDB.GetLength($0257,AChara.ClientVersion));
end;{SendDeleteMailResult}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendMailResult                                                       PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//		Is sending mail successful?
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//		[2008/08/10] - Aeomin - Created
//------------------------------------------------------------------------------
procedure SendMailResult(
	const AChara	: TCharacter;
	const Fail  :Boolean
);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $0249, OutBuffer);
	WriteBufferByte(2,Byte(Fail), OutBuffer);
	SendBuffer(AChara.ClientInfo,OutBuffer,PacketDB.GetLength($0249,AChara.ClientVersion));
end;{SendMailResult}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendNewMailNotify                                                    PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//		Notify player a new mail arrived
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//		[2008/08/11] - Aeomin - Created
//------------------------------------------------------------------------------
procedure SendNewMailNotify(
	const AChara	: TCharacter;
	const MailID	: LongWord;
	const Sender	: String;
	const Title	: String
);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $024a, OutBuffer);
	WriteBufferLongWord(2,MailID, OutBuffer);
	WriteBufferString(6, Sender, NAME_LENGTH, OutBuffer);
	WriteBufferString(6+NAME_LENGTH, Title, 40, OutBuffer);
	SendBuffer(AChara.ClientInfo,OutBuffer,PacketDB.GetLength($024a,AChara.ClientVersion));
end;{SendNewMailNotify}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendInventory                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Send items in inventory
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//		[2008/09/19] Aeomin - Created
//------------------------------------------------------------------------------
procedure SendInventory(
	AClient		: TIdContext;
	const AInventory : TInventory
);
var
	OutBuffer : TBuffer;
	OutBufferEquip : TBuffer;
	Len   : Word;
	Index : Word;
	OffSet : Word;
	OffSetEquip : Word;
	ItemCount : Word;
	EquipmentCount : Word;
	InventoryItem : TItemInstance;
	EquipmentItem : TEquipmentItem;
begin
	ItemCount := 0;
	EquipmentCount := 0;
	OffSet := 4;
	OffSetEquip := 4;

	WriteBufferWord(0, $01ee, OutBuffer);
	WriteBufferWord(0, $02d0, OutBufferEquip);
	if AInventory.ItemList.Count > 0 then
	begin
		for Index := 0 to AInventory.ItemList.Count - 1 do
		begin
			InventoryItem := AInventory.ItemList.Items[Index];
			if (InventoryItem.Item is TUseableItem) OR
			(InventoryItem.Item is TMiscItem) then
			begin
				with InventoryItem.Item do
				begin
					WriteBufferWord(OffSet, Index+1, OutBuffer);    //Index
					WriteBufferWord(OffSet+2, ID, OutBuffer);  //ID
					WriteBufferByte(OffSet+4, ItemTypeToByte(ItemType), OutBuffer);    //Type
					WriteBufferByte(OffSet+5, 1, OutBuffer);    //Identified?
					WriteBufferWord(OffSet+6, InventoryItem.Quantity, OutBuffer); //Amount
					WriteBufferWord(OffSet+8, 0, OutBuffer);     //For ammo
					WriteBufferWord(OffSet+10, 0, OutBuffer);   //Card 1
					WriteBufferWord(OffSet+12, 0, OutBuffer);   //2
					WriteBufferWord(OffSet+14, 0, OutBuffer);   //3
					WriteBufferWord(OffSet+16, 0, OutBuffer);   //4
					Inc(OffSet,18);
				end;
				Inc(ItemCount);
			end else
			if InventoryItem.Item is TEquipmentItem then
			begin
				EquipmentItem := InventoryItem.Item as TEquipmentItem;
				WriteBufferWord(OffSetEquip, Index+1, OutBufferEquip);    //Index
				WriteBufferWord(OffSetEquip+2, EquipmentItem.ID, OutBufferEquip);  //ID
				WriteBufferByte(OffSetEquip+4, 5, OutBufferEquip);    //Type
//				WriteBufferByte(OffSetEquip+5, Byte(InventoryItem.Identified), OutBufferEquip);    //Identified
				//Identified for now
				WriteBufferByte(OffSetEquip+5, 1, OutBufferEquip);
				WriteBufferWord(OffSetEquip+6, EquipTypeToByte(EquipmentItem.EquipmentType), OutBufferEquip);
				WriteBufferWord(OffSetEquip+8, 0, OutBufferEquip); //Equiped?
				WriteBufferByte(OffSetEquip+10, 0, OutBufferEquip); //Broken?
				WriteBufferByte(OffSetEquip+11, InventoryItem.Refined, OutBufferEquip);
				WriteBufferWord(OffSetEquip+12, 0, OutBufferEquip);   //Card 1
				WriteBufferWord(OffSetEquip+14, 0, OutBufferEquip);   //2
				WriteBufferWord(OffSetEquip+16, 0, OutBufferEquip);   //3
				WriteBufferWord(OffSetEquip+18, 0, OutBufferEquip);   //4
				WriteBufferWord(OffSetEquip+24, 0, OutBufferEquip);   //??
				Inc(OffSetEquip,26);
				Inc(EquipmentCount);
			end;
		end;
	end;
	//Items
	Len := (ItemCount * 18) + 4;
	WriteBufferWord(2, Len, OutBuffer);
	SendBuffer(AClient, OutBuffer, Len);
	//Equipments
	Len := (EquipmentCount * 26) + 4;
	WriteBufferWord(2, Len, OutBufferEquip);
	SendBuffer(AClient, OutBufferEquip, Len);
end;{SendInventory}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendNewItem                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		New item!!
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//		[2008/09/20] Aeomin - Created
//------------------------------------------------------------------------------
procedure SendNewItem(
	AClient		: TIdContext;
	const AInventory : TItemInstance;
	const Index	: Word;
	const Amount	: Word
);
var
	OutBuffer : TBuffer;
begin
	with AInventory.Item do
	begin
		WriteBufferWord(0, $02d4, OutBuffer);
		WriteBufferWord(2, Index+1, OutBuffer);
		WriteBufferWord(4, Amount, OutBuffer);
		WriteBufferWord(6, ID, OutBuffer);
		WriteBufferByte(8, 1, OutBuffer);
		WriteBufferByte(9, 0, OutBuffer); //Broken?
		WriteBufferByte(10, AInventory.Refined, OutBuffer);
		WriteBufferWord(11, 0, OutBuffer);   //Card 1
		WriteBufferWord(13, 0, OutBuffer);   //2
		WriteBufferWord(15, 0, OutBuffer);   //3
		WriteBufferWord(17, 0, OutBuffer);   //4
		if AInventory.Item is TEquipmentItem then
		begin
			WriteBufferWord(19, EquipTypeToByte(TEquipmentItem(AInventory.Item).EquipmentType), OutBuffer);
		end;
		WriteBufferByte(21, ItemTypeToByte(ItemType), OutBuffer);;
		WriteBufferByte(22, 0, OutBuffer);  //Fail?
		WriteBufferLongWord(23, 0, OutBuffer);
		WriteBufferLongWord(27, 0, OutBuffer);
		SendBuffer(AClient, OutBuffer, 29);
	end;
end;{SendNewItem}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendNewItemFailed                                                    PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Failed to add item
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//		[2008/09/25] Aeomin - Created
//------------------------------------------------------------------------------
procedure SendNewItemFailed(
	AClient		: TIdContext;
	const Flag	: Byte
);
var
	OutBuffer : TBuffer;
begin
	if Flag > 0 then
	begin
		WriteBufferWord(0, $02d4, OutBuffer);
		WriteBufferByte(22, Flag, OutBuffer);
		SendBuffer(AClient, OutBuffer, 29);
	end;
end;{SendNewItemFailed}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendStatusIcon                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Send icons on right side
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//		[2008/09/25] Aeomin - Created
//------------------------------------------------------------------------------
procedure SendStatusIcon(
	const AChara	: TCharacter;
	const IconID	: Word;
	const Active	: Boolean
);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $0196, OutBuffer);
	WriteBufferWord(2, IconID, OutBuffer);
	WriteBufferLongWord(4, AChara.AccountID, OutBuffer);
	WriteBufferByte(8, Byte(Active), OutBuffer);
	SendBuffer(AChara.ClientInfo, OutBuffer, PacketDB.GetLength($0196,AChara.ClientVersion));
end;{SendStatusIcon}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendUpdatedLook																								 PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Updates the look of a character.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//		[2008/09/27] RaX - Created.
//------------------------------------------------------------------------------
procedure SendUpdatedLook(
	const AChara			: TCharacter;
	const CharacterID	: LongWord;
	const AType				: TLookTypes;
	const Value1			: Word;
	const Value2			: Word
);
var
	OutBuffer : TBuffer;
begin
	//$1d7 charID Type classID
	WriteBufferWord(0, $01d7, OutBuffer);
	WriteBufferLongWord(2, CharacterID, OutBuffer);
	WriteBufferByte(6, Byte(AType), OutBuffer);
	WriteBufferWord(7, Value1, OutBuffer);
	WriteBufferWord(9, Value2, OutBuffer);
	SendBuffer(AChara.ClientInfo, OutBuffer, PacketDB.GetLength($01d7,AChara.ClientVersion));
end;{SendUpdatedLook}
//------------------------------------------------------------------------------
end{ZoneSend}.
