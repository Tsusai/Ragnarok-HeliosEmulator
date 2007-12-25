//------------------------------------------------------------------------------
//ZoneRecv                                                                 UNIT
//------------------------------------------------------------------------------
//  What it does -
//      Receives packets sent by users on the zone server, does whatever they
//    tell us to do =) Contains all routines related to doing as such.
//
//  Changes -
//	[2007/01/18] RaX - Created Header;
//	[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
//	[2007/10/25] Aeomin - major clean up #1
//
//------------------------------------------------------------------------------
unit ZoneRecv;


interface


uses
	{RTL/VCL}
	//none
	{Project}
	Character,
	PacketTypes,
	{Third Party}
	IdContext
	;


	//----------------------------------------------------------------------
	//NoCommand                                                    PROCEDURE
	//----------------------------------------------------------------------
	Procedure NoCommand(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	); overload;
	procedure NoCommand(
		var
			AChara : TCharacter;
		const
			AvoidSelf:boolean = False
	); overload;
	//----------------------------------------------------------------------


	//----------------------------------------------------------------------
	// General (Procedure required game to run, or anything is not belong to detail category)
	//----------------------------------------------------------------------
	procedure GetNameAndID(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure MapConnect(
		const Version : Integer;
		var AClient : TIdContext;
		const Buffer  : TBuffer;
		const ReadPts : TReadPts
		);

	Procedure ShowMap(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure RecvTick(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);
	//----------------------------------------------------------------------


	//----------------------------------------------------------------------
	// Player's Input (Chat, command etc)
	//----------------------------------------------------------------------
	procedure AreaChat(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure GMBroadcast(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure GMMapMove(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure SlashWho(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure Whisper(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);
	//----------------------------------------------------------------------


	//----------------------------------------------------------------------
	// Player's Action (What player wanted to do..)
	//----------------------------------------------------------------------
	procedure CharaRotation(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure ActionRequest(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure CancelAttack(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure CharacterWalkRequest(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure NPCClick(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure NPCMenu(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure NPCNext(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure QuitGame(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure ReturnToCharacterSelect(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure StatUP(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure RequestToAddFriend(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure RemoveFriendFromList(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);

	procedure RequestToAddFriendResponse(
		var AChara  : TCharacter;
		const InBuffer : TBuffer;
		const ReadPts : TReadPts
	);
	//----------------------------------------------------------------------

	//----------------------------------------------------------------------
	// Packets receive from inter server ( NOT PLAYER )
	//----------------------------------------------------------------------
	procedure RecvGMCommandFromInter(
		const InBuffer : TBuffer
	);

	procedure RecvGMCommandResultFromInter(
		const InBuffer : TBuffer
	);

	procedure RecvMapWarpRequest(
			InBuffer : TBuffer
	);

	procedure RecvRedirectWhisper(
			InBuffer : TBuffer
	);

	procedure RecvWarpRequestReplyFromInter(
		const InBuffer : TBuffer
	);

	procedure RecvWhisperReply(
			InBuffer : TBuffer
	);

	procedure RecvAddFriendRequest(
			InBuffer : TBuffer
	);

	procedure RecvAddFriendRequestReply(
			InBuffer : TBuffer
	);
	//----------------------------------------------------------------------

implementation


uses
	{RTL/VCL}
	Types,
	WinLinux,
	Classes,
	SysUtils,
	Math,
	{Project}
	Account,
	Being,
	BufferIO,
	GameConstants,
	GMCommands,
	LuaNPCCore,
	Main,
	Map,
	MapTypes,
	MovementEvent,
	NPC,
	TCPServerRoutines,
	ZoneSend,
	ZoneServer,
	ZoneCharaCommunication,
	ZoneInterCommunication,
	Database,
	AddFriendEvent,
	AreaLoopEvents
	{3rd Party}
	//none
	;


//------------------------------------------------------------------------------
//NoCommand                                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      This is a dummy command for processes that either don't do anything or
//    don't do anything yet.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
procedure NoCommand(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
begin
	//Dummy Command for processes that don't have one.
end;{NoCommand}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//NoCommand                                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      This is a dummy command for processes that either don't do anything or
//    don't do anything yet.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
procedure NoCommand(
	var
		AChara    : TCharacter;
	const
		AvoidSelf : Boolean = False
);
begin
	//Dummy Command for processes that don't have one.
end;{NoCommand}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetNameAndID                                                         PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      This gets a character's name and id from the charalist if it's there,
//    else, it looks through mobs and npcs.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
procedure GetNameAndID(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
var
	ID : LongWord;
	idx : integer;
	RecvCharacter : TCharacter;
	RecvNPC : TNPC;
begin
	ID := BufferReadLongWord(ReadPts[0], InBuffer);
	idx := MainProc.ZoneServer.CharacterList.IndexOfAID(ID);
	if idx > -1 then
	begin
		RecvCharacter := MainProc.ZoneServer.CharacterList.Items[idx];
		{guild check}
		{guild version packet}
		{else}
		ZoneSendObjectNameAndIDBasic(
			AChara,
			RecvCharacter.ID,
			RecvCharacter.Name
		);
	end else
	begin
		//NPC and Mob shit here
		idx := MainProc.ZoneServer.NPCList.IndexOf(ID);
		if idx > -1 then
		begin
			RecvNPC := MainProc.ZoneServer.NPCList.Objects[idx] as TNPC;
			ZoneSendObjectNameAndIDBasic(
				AChara,
				RecvNPC.ID,
				RecvNPC.Name
			);
		end;
	end;
end;{GetNameAndID}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//MapConnect                                                           PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      This is executed when a character requests to conenct ot the map server.
//    It first checks to see if a character should be able to connect to the
//    zone (to stop hacking attempts) and then it links the chosen character to
//    the connection.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//    April 10th, 2007 - Aeomin - Added SendZoneCharaLogon.
//    April 12th, 2007 - Aeomin - Append check to disallow 0 of LoginKey
//    May 25th, 2007 - Tsusai - Removed Mapload. Wrong spot for it.  Added
//		the zone increase here.
//------------------------------------------------------------------------------
procedure MapConnect(
	const Version : Integer;
	var AClient : TIdContext;
	const Buffer  : TBuffer;
	const ReadPts : TReadPts
);
var
	AccountID   : LongWord;
	CharacterID : LongWord;
	ValidateID1 : LongWord;
	//ClientTick  : LongWord;
	Gender      : Byte;
	AnAccount   : TAccount;
	ACharacter  : TCharacter;
begin
	AccountID      := BufferReadLongWord(ReadPts[0], Buffer);
	CharacterID    := BufferReadLongWord(ReadPts[1], Buffer);
	ValidateID1    := BufferReadLongWord(ReadPts[2], Buffer);
	{ClientTick     := }BufferReadLongWord(ReadPts[3], Buffer);
	Gender         := BufferReadByte    (ReadPts[4], Buffer);
	TThreadLink(AClient.Data).DatabaseLink.CommonData.Connect;
	try
			AnAccount  := TThreadLink(AClient.Data).DatabaseLink.CommonData.GetAccount(AccountID);
	finally
			TThreadLink(AClient.Data).DatabaseLink.CommonData.Disconnect;
	end;

	//use client local game database
	TThreadLink(AClient.Data).DatabaseLink.GameData.Connect;
	try
			ACharacter := TThreadLink(AClient.Data).DatabaseLink.GameData.GetChara(CharacterID);
	finally
			TThreadLink(AClient.Data).DatabaseLink.GameData.Disconnect;
	end;

	if Assigned(AnAccount) and Assigned(ACharacter) then
	begin
		if (AnAccount.LoginKey[1] = ValidateID1) and
			 (AnAccount.LoginKey[1] > 0) and
			(AnAccount.GenderNum = Gender) then
		begin
			// Duplicate session safe check!
			if MainProc.ZoneServer.CharacterList.IndexOf(ACharacter.CID) > -1 then
			begin
				ACharacter.ClientInfo.Connection.Disconnect;
			end else
			begin
				TClientLink(AClient.Data).AccountLink := AnAccount;
				TClientLink(AClient.Data).CharacterLink := ACharacter;

				ACharacter.ClientVersion := Version;
				ACharacter.Online  := 1;
				MainProc.ZoneServer.CharacterList.Add(ACharacter);

				ACharacter.ZoneStatus := isOnline;
				SendZoneCharaLogon(MainProc.ZoneServer.ToCharaTCPClient, ACharacter);

				SendPadding(ACharacter.ClientInfo);

				ZoneSendMapConnectReply(ACharacter);
				SendZoneCharaIncrease(MainProc.ZoneServer.ToCharaTCPClient,MainProc.ZoneServer);

				//Friendslist (No longer placeholder XD)
				ACharacter.SendFriendList;
			end;

		end else
		begin
			ZoneSendMapConnectDeny(AClient);
		end;
	end;
end;{MapConnect}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ShowMap                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      This is executed on a character showing the map, it sends them all the
//    information they need, such as their skill list, stats, friends list,
//    guild, etc. Anything that would be used by the character.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//    March 30th, 2007 - Aeomin - Move SendZoneCharaIncrease to here.
//    May 25th, 2007 - Tsusai - Moved MapLoad here, removed Zone increase.
//	  May 28th, 2007 - Tsusai - Removed MapPointer.
//------------------------------------------------------------------------------
procedure ShowMap(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
var
	OutBuffer   : TBuffer;
	AMap        : TMap;
	MapIndex    : Integer;
	NPCIndex    : Integer;
	AnNPC       : TNPC;
begin
	AChara.EventList.Clear;
	//Load map cells if they are not already loaded
	MapIndex := MainProc.ZoneServer.MapList.IndexOf(AChara.Map);
	if MapIndex > -1 then
	begin
		if MainProc.ZoneServer.MapList[MapIndex].State = UNLOADED then
		begin
			MainProc.ZoneServer.MapList[MapIndex].Load;
		end;
		AMap := MainProc.ZoneServer.MapList[MapIndex];
		AChara.MapInfo := AMap;
		//Enable all npcs on this map.
		for NPCIndex := 0 to MainProc.ZoneServer.NPCList.Count -1 do
		begin
			AnNPC := TNPC(MainProc.ZoneServer.NPCList.Objects[NPCIndex]);
			if AnNPC.Map = MainProc.ZoneServer.MapList[MapIndex].Name then
			begin
				AnNPC.MapInfo := MainProc.ZoneServer.MapList[MapIndex];
				MainProc.ZoneServer.MapList[MapIndex].Cell[AnNPC.Position.X][AnNPC.Position.Y].Beings.AddObject(AnNPC.ID, AnNPC);
				AnNPC.Enabled := true;
			end;
		end;

		AChara.OnTouchIDs.Clear;

		//Update character options
		//Clear all vending/trading/etc id storages.
		//Clear some possible events from the event list.


		if (AChara.HP = 0) then
		begin
			if ((AChara.JID = JOB_NOVICE) or
				(AChara.JID = HJOB_HIGH_NOVICE) or
				(AChara.JID = HJOB_BABY) {OR
			 AChara can fully recover (Osiris card?) }) then
			begin
				AChara.CalcMaxHP;
				AChara.HP := AChara.MaxHP;
				AChara.SP := AChara.MaxSP;
			end else begin
				AChara.HP := 1;
				AChara.SP := 1;
			end;
		end;

		//skilllist placeholder
		WriteBufferWord(0, $010F, OutBuffer);
		WriteBufferWord(2, 4, OutBuffer);
		SendBuffer(AChara.ClientInfo, OutBuffer, 4);

		AChara.SendCharacterStats;

		//Inventory Placeholder
		WriteBufferWord(0, $00a3, OutBuffer);
		WriteBufferWord(2, 4, OutBuffer);
		SendBuffer(AChara.ClientInfo, OutBuffer, 4);
		//Equip Placeholder
		WriteBufferWord(0, $00a4, OutBuffer);
		WriteBufferWord(2, 4, OutBuffer);
		SendBuffer(AChara.ClientInfo, OutBuffer, 4);
		//Arrow Placeholder
		WriteBufferWord(0, $013c, OutBuffer);
		WriteBufferWord(2, 0, OutBuffer);
		SendBuffer(AChara.ClientInfo, OutBuffer, GetPacketLength($013c,AChara.ClientVersion));

		//Weather updates
		//Various other tweaks
		AChara.ShowTeleportIn;
		AChara.AreaLoop(ShowInitialAction);

		//Quick change to prevent characters from being added in bad places.
		if (AChara.Position.X < AMap.Size.X) AND
			 (AChara.Position.Y < AMap.Size.Y) AND
			 (AChara.Position.X >= 0) AND
			 (AChara.Position.X >= 0) then
		begin
			AMap.Cell[AChara.Position.X][AChara.Position.Y].Beings.AddObject(AChara.ID,AChara);
		end;
	end;
end;{ShowMap}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvTick                                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      This sends a simple ping command basically to the client on receiving a
//    "tick" from them. It ensures that the client is still connected to the
//    game server.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
procedure RecvTick(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
begin
	ZoneSendTickToClient(AChara);
end;{RecvTick}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//AreaChat                                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Receive and Send local speech
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    March 18th, 2007 - Aeomin - Created Header
//	[2007/08/09] Aeomin - change + 3 to + 4, otherwise theres extra space
//------------------------------------------------------------------------------
procedure AreaChat(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
var
	ChatLength     : Word;
	Chat           : String;
	TempChat       : String;
	CommandID      : Word;
begin
	ChatLength	:= BufferReadWord(ReadPts[0], InBuffer)-4;
	Chat		:= BufferReadString(ReadPts[1], ChatLength, InBuffer);

	//First, we remove the character's name and the colon after it from the
	//chat string.
	TempChat := Copy(Chat, Length(AChara.Name) + 4, Length(Chat));
	//We then check if it is a command.
	if MainProc.ZoneServer.Commands.IsCommand(TempChat) then
	begin
		CommandID := MainProc.ZoneServer.Commands.GetCommandID(
									MainProc.ZoneServer.Commands.GetCommandName(TempChat)
								 );
		//if it is a command, we check the account's access level to see if it is
		//able to use the gm command.
		if TClientLink(AChara.ClientInfo.Data).AccountLink.Level >= MainProc.ZoneServer.Commands.GetCommandGMLevel(CommandID) then
		begin
			ZoneSendGMCommandtoInter(MainProc.ZoneServer.ToInterTCPClient, AChara.ID, AChara.CID, TempChat);
		end else
		begin
			SendAreaChat(Chat, ChatLength, AChara);
		end;
	end else
	begin
		SendAreaChat(Chat, ChatLength, AChara);
	end;
end;{AreaChat}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMBroadcast                                                          PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Convert /b , /nb command (apparently, /bb works too)
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/08/09] Aeomin - Created.
//------------------------------------------------------------------------------
procedure GMBroadcast(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
var
	Size     : Word;
	Announce : String;
	CommandID: Integer;
	RequiredGMLevel : Byte;
begin
	CommandID := MainProc.ZoneServer.Commands.GetCommandID('BroadCastN');
	RequiredGMLevel := MainProc.ZoneServer.Commands.GetCommandGMLevel(CommandID);
	if TClientLink(AChara.ClientInfo.Data).AccountLink.Level >= RequiredGMLevel then
	begin
		Size     := BufferReadWord(2, InBuffer);
		Announce := BufferReadString(4, Size - 4, InBuffer);
		//Convert XD
		ZoneSendGMCommandtoInter(MainProc.ZoneServer.ToInterTCPClient, AChara.ID, AChara.CID, '#BroadCastN ' + Announce);
	end;
end;{GMBroadcast}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GMMapMove                                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Convert /mm /mapmove command
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/08/12] Aeomin - Created.
//------------------------------------------------------------------------------
procedure GMMapMove(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
var
	MapName : String;
	X       : Word;
	Y       : Word;
	CommandID: Integer;
	RequiredGMLevel : Byte;
begin
	CommandID := MainProc.ZoneServer.Commands.GetCommandID('Warp');
	RequiredGMLevel := MainProc.ZoneServer.Commands.GetCommandGMLevel(CommandID);
	if TClientLink(AChara.ClientInfo.Data).AccountLink.Level >= RequiredGMLevel then
	begin
		//get the request
		MapName := BufferReadString(ReadPts[0], ReadPts[1] - ReadPts[0], InBuffer);
		X       := BufferReadWord(ReadPts[1], InBuffer);
		Y       := BufferReadWord(ReadPts[2], InBuffer);
		ZoneSendGMCommandtoInter(MainProc.ZoneServer.ToInterTCPClient, AChara.ID, AChara.CID, '#Warp "' + MapName +'",' + IntToStr(X) + ',' + IntToStr(Y));
	end;
end;{GMMapMove}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SlashWho                                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Check how many player(s) were online.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    April 5th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure SlashWho(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
begin
	ZoneSendConnectionsCount(AChara.ClientInfo);
end;{SlashWho}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Whisper                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Receive Whisper request from client.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    May 3rd, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure Whisper(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
var
	Len : Word;
	TargetName : String;
	Msg : String;
begin
	Len := BufferReadWord(ReadPts[0], InBuffer);
	TargetName := BufferReadString(ReadPts[1], 24, InBuffer);
	Msg := BufferReadString(ReadPts[2], Len - 28, InBuffer);
	SendWhisperToInter(MainProc.ZoneServer.ToInterTCPClient, AChara, TargetName, Msg);
end;{Whisper}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//CharaRotation                                                        PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Rotate Character, check,  set and send to other characters.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    March 20th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure CharaRotation(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
begin
	AChara.HeadDirection := BufferReadWord(ReadPts[0], InBuffer);
	AChara.Direction     := BufferReadByte(ReadPts[1], InBuffer);
	AChara.UpdateDirection;
end;{CharaRotation}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ActionRequest                                                       PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      This sends a client an action command, actions can be anything from
//			sitting to critical hits. SEE GAMECONSTANTS.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    December 24th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure ActionRequest(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
var
	ActionType	: Byte;
	TargetID		: LongWord;
begin
	TargetID := BufferReadLongWord(ReadPts[0], InBuffer);
	ActionType := BufferReadByte(ReadPts[1], InBuffer);

	case ActionType of

		0,7	://Hit target one time (0) and hit continuous (7)
			begin
				//Temporary
				AChara.CharaState := charaAttacking;
				AChara.TargetID := TargetID;
				AChara.AreaLoop(ShowAction, FALSE, TRUE, TRUE, ACTION_DO_ATTACK);
				AChara.CharaState := charaStanding;
				AChara.TargetID := 0;
			end;

		2	://Sit
			begin
				//TODO -- basic skill checks here
				AChara.CharaState := charaSitting;
			end;

		3	://Stand
			begin
				AChara.CharaState := charaStanding;
			end;
	end;
end;{ActionRequest}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//CancelAttack                                                      PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      ?
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    December 24th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure CancelAttack(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
begin

end;{CancelAttack}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//CharaWalkRequest                                                    PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Processes a Character's request to walk to a certain point.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    February 27th, 2007 - RaX - Created Header;
//    May 28th, 2007 - Tsusai - Changed Rebruary to February, and added conditionals
//     for walking
//------------------------------------------------------------------------------
procedure CharacterWalkRequest(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
var
	MoveEvent : TMovementEvent;
	DestPoint : TPoint;
	spd       : LongWord;
	Index     : Integer;
begin
	DestPoint := BufferReadOnePoint(ReadPts[0], InBuffer);
	if (AChara.ScriptStatus = SCRIPT_NOTRUNNING) and
	((AChara.CharaState = charaStanding) or (AChara.CharaState = charaWalking)) then
	begin

		if AChara.MapInfo.GetPath(AChara.Position, DestPoint, AChara.Path) then
		begin
			with AChara do
			begin

				//Remove previous movement events from the event list
				for Index := AChara.EventList.Count -1 downto 0 do
				begin
					if AChara.EventList.Items[Index] is TMovementEvent then
					begin
						AChara.EventList.Delete(Index);
					end;
				end;

				AChara.CharaState := charaStanding;
				AChara.PathIndex := 0;

				//Setup first speed
				//Check to see if we're moving diagonally, if we are, we adjust the speed
				//accordingly.
				if (Direction in Diagonals) then
				begin
					spd := Speed * 7 DIV 5;
				end else begin
					spd := Speed;
				end;

				AChara.MoveTick := GetTick + spd DIV 2;//changed to div2 to offset
				//annoying difference between server and client.

				MoveEvent := TMovementEvent.Create(MoveTick,AChara);
				AChara.EventList.Add(MoveEvent);

				ZoneSendWalkReply(AChara,DestPoint);
				ShowBeingWalking;
			end;
		end;
	end;
end;{CharaWalkRequest}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//NPCClick                                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Runs a NPC script when someone clicks it (if conditions are ok)
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    [2007/05/27] Tsusai - Created
//------------------------------------------------------------------------------
procedure NPCClick(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
var
	NPCID : LongWord;
	ANPC : TNPC;
begin
	if (AChara.ScriptStatus = SCRIPT_NOTRUNNING) and
		(AChara.CharaState = charaStanding) then
	begin
		NPCID := BufferReadLongWord(ReadPts[0], InBuffer);
		//TEMPORARY
		ANPC := MainProc.ZoneServer.NPCList.Objects
		 [MainProc.ZoneServer.NPCList.IndexOf(NPCID)] as TNPC;
		if ANPC is TScriptNPC then
		begin
			AChara.ScriptID := ANPC.ID;
			TScriptNPC(ANPC).OnClick(AChara);
		end;
	end;
end;{NPCClick}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//NPCMenu                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Resumes a npc script with a menu selection
//      First menu choice returns a 1, 2nd returns 2, etc.  $ff = cancel
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    June 03rd, 2007 - Tsusai - Created
//------------------------------------------------------------------------------
procedure NPCMenu(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
var
	NPCID : LongWord;
	Choice : Byte;
begin
	NPCID := BufferReadLongWord(ReadPts[0], InBuffer);
	Choice := BufferReadByte(ReadPts[1], InBuffer);
	if (AChara.ScriptID = NPCID) and
		(AChara.ScriptStatus = SCRIPT_YIELD_MENU) then
	begin
		if Choice <> $FF then
		begin
			AChara.ScriptStatus := SCRIPT_RUNNING;
			ResumeLuaNPCScriptWithInteger(AChara,Choice);
		end else
		begin
			AChara.ScriptStatus := SCRIPT_NOTRUNNING;
		end;
	end;
end;{NPCMenu}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//NPCNext                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Resumes a npc script
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    May 25th, 2007 - Tsusai - Created
//------------------------------------------------------------------------------
procedure NPCNext(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
var
	NPCID : LongWord;
begin
	NPCID := BufferReadLongWord(ReadPts[0], InBuffer);
	if (AChara.ScriptID = NPCID) and
		(AChara.ScriptStatus = SCRIPT_YIELD_WAIT) then
	begin
		AChara.ScriptStatus := SCRIPT_RUNNING;
		ResumeLuaNPCScript(AChara);
	end;
end;{NPCNext}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//QuitGame                                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Fired when a player clicks Exit to windows button ingame.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    March 17th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
procedure QuitGame(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
begin
	//send client response.
	SendQuitGameResponse(AChara);
end;{QuitGame}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ReturnToCharacterSelect                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Fired when a player clicks the return to chara select button.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    March 17th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
procedure ReturnToCharacterSelect(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
var
	ActionByte : Byte;
	OutBuffer  : TBuffer;
begin
	FillChar(OutBuffer,3,0);
	ActionByte := BufferReadByte(ReadPts[0], InBuffer);
	case ActionByte of
	0:
		begin
			if AChara.CharaState <> charaDead then
				Exit;
			//Send Leave with '0' as byte modifier
			//Only runs when dead.
			//Return to save point, and load map
			AChara.Map := AChara.SaveMap;
			AChara.Position := AChara.SavePoint;
			//send map change packet stuff
		end;
	1:
		begin
			if not false {in combat} then
			begin
				//Send Leave 2
				SendCharacterSelectResponse(AChara);
			end;
		end;
	end;
end;{ReturnToCharacterSelect}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//StatUP                                                               PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Receive stat up request from client.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/08/20] - Aeomin - Created
//------------------------------------------------------------------------------
procedure StatUP(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
var
	StatType  : Word;
	LocalType : Byte;
	Amount    : Byte;
	Index     : Integer;
	OldAmount : Integer;
	Def       : Byte;
begin
	StatType   := BufferReadWord(ReadPts[0], InBuffer);
	Amount     := BufferReadByte(ReadPts[1], InBuffer);
	if Amount > 0 then
	begin
		//Gravity loves to play, so lets play then
		if (StatType >= $000d) and (StatType <= $0012) then
		begin
			LocalType := StatType - $000d;
		end else
		begin
			LocalType := 0;
		end;
		OldAmount := AChara.ParamBase[LocalType];
		//Loop, 1 stat a time
		for Index := 1 to Amount do
		begin
			if AChara.ParamUp[LocalType] <= AChara.StatusPts then
			begin
				if OldAmount < CHAR_STAT_MAX then
				begin
					AChara.StatusPts := AChara.StatusPts - AChara.ParamUp[LocalType];
					AChara.ParamBase[LocalType] := AChara.ParamBase[LocalType] + 1;
				end else
				begin
					//Too much already!
					Break;
				end;
			end else
			begin
				//Not enough status point
				Break;
			end;
		end;

		//How many stats added?
		Def := AChara.ParamBase[LocalType] - OldAmount;

		if Def = 0 then
		begin
			//Nothing has changed, assume failed
			SendStatUPResult(AChara, True, Def);
		end else
		begin
			SendStatUPResult(AChara, False, Def);
		end;
	end;
end;{StatUp}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RequestToAddFriend                                                   PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Request add friend.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    [2007/12/7] Aeomin - Created.
//------------------------------------------------------------------------------
procedure RequestToAddFriend(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
var
	TargetChar : String;  //The character name want to add to friend list
	TargetChara : TCharacter;
	ZoneID      : Integer;
	AFriendRequestEvent : TFriendRequestEvent;
	AlreadyFriend : Boolean;
begin
	TargetChar := BufferReadString(ReadPts[0], NAME_LENGTH, InBuffer);

	TargetChara := nil;
	AlreadyFriend := True;
	with TThreadLink(AChara.ClientInfo.Data).DatabaseLink.GameData do
	begin
		if Connect then
		try
			TargetChara := GetChara(TargetChar);
			if (TargetChara <> NIL) then
				AlreadyFriend := IsFriend(AChara.CID, TargetChara.ID, TargetChara.CID);
		finally
			Disconnect;
		end;
	end;

	if (TargetChara <> NIL) and not AlreadyFriend then
	begin
		ZoneID := -1;
		with TThreadLink(AChara.ClientInfo.Data).DatabaseLink.StaticData do
		begin
			if Connect then
			try
				ZoneID := GetMapZoneID(TargetChara.Map);
			finally
				Disconnect;
			end;
		end;

		if ZoneID > -1 then
		begin
			if AChara.Friends >= MAX_FRIENDS then
			begin
				// Too much friends
				SendAddFriendRequestReply(
						AChara.ClientInfo,
						TargetChara.ID,
						TargetChara.CID,
						TargetChara.Name,
						2
				);

			end else
			begin
				AFriendRequestEvent := TFriendRequestEvent.Create(GetTick + 10000, AChara);
				AFriendRequestEvent.PendingFriend := TargetChara.CID;
				AChara.EventList.Add(AFriendRequestEvent);
				// Just let inter server handle it ^_^
				ZoneSendAddFriendRequest(
						MainProc.ZoneServer.ToInterTCPClient,
						AChara.ID,
						AChara.CID,
						AChara.Name,
						TargetChara.CID,
						ZoneID
				);
			end;
		end;
		TargetChara.Free;
	end;
end;{RequestToAddFriend}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RemoveFriendFromList                                                 PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Delete a friend from friend list.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    [2007/12/5] Aeomin - Created.
//------------------------------------------------------------------------------
procedure RemoveFriendFromList(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
var
	AccountID  : LongWord;
	CharID     : LongWord;
begin
	AccountID   := BufferReadLongWord(ReadPts[0], InBuffer);
	CharID      := BufferReadLongWord(ReadPts[1], InBuffer);

	TThreadLink(AChara.ClientInfo.Data).DatabaseLink.GameData.Connect;
	try
			if TThreadLink(AChara.ClientInfo.Data).DatabaseLink.GameData.DeleteFriend(AChara.CID, AccountID, CharID) then
			begin
			SendDeleteFriend(AChara.ClientInfo, AccountID, CharID);
			Dec(AChara.Friends);
			end;
	finally
			TThreadLink(AChara.ClientInfo.Data).DatabaseLink.GameData.Disconnect;
	end;
end;{RemoveFriendFromList}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RequestToAddFriendResponse                                           PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Receive player's reply of add firend request
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    [2007/12/07] Aeomin - Created.
//------------------------------------------------------------------------------
procedure RequestToAddFriendResponse(
	var AChara  : TCharacter;
	const InBuffer : TBuffer;
	const ReadPts : TReadPts
);
var
	AccID  : LongWord;
	CharID : LongWord;
	Reply  : Byte;
begin
	AccID   := BufferReadLongWord(ReadPts[0], InBuffer);
	CharID  := BufferReadLongWord(ReadPts[1], InBuffer);
	Reply   := BufferReadByte(ReadPts[2], InBuffer);

	// Nop... no way...
	if (AChara.ID <> AccID) and (AChara.CID <> CharID) then
	begin
		if Reply = 0 then
		begin
			// Denied, just forward..
			ZoneSendAddFriendReply(
				MainProc.ZoneServer.ToInterTCPClient,
				CharID,
				AChara.ID,
				AChara.CID,
				AChara.Name,
				1
			);
		end else
		begin
			// Accepted, tell request char to add data in database
			ZoneSendAddFriendReply(
				MainProc.ZoneServer.ToInterTCPClient,
				CharID,
				AChara.ID,
				AChara.CID,
				AChara.Name,
				0
			);
		end;
	end;
end;{RequestToAddFriendResponse}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvRedirectWhisper                                                  PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Redirect whisper message from Inter to client
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    May 3rd, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure RecvRedirectWhisper(
	InBuffer : TBuffer
);
var
	Size : Word;
	ZoneID : LongWord;
	FromID : LongWord;
	ToID   : LongWord;
	FromName : String;
	Whisper : String;
	Idx : Integer;
	Chara : TCharacter;
begin
	Size := BufferReadWord(2, InBuffer);
	ZoneID := BufferReadLongWord(4, InBuffer);
	FromID := BufferReadLongWord(8, InBuffer);
	ToID := BufferReadLongWord(12, InBuffer);
	FromName := BufferReadString(16, 24, InBuffer);
	Whisper := BufferReadString(40, Size - 40, InBuffer);
	Idx := MainProc.ZoneServer.CharacterList.IndexOf(ToID);
	if Idx > -1 then
	begin
		Chara := MainProc.ZoneServer.CharacterList.Items[idx];
		SendWhisper(FromName, Whisper, Chara.ClientInfo);

		if not ((ZoneID = 0) and (FromID = 0)) then
		begin
			SendWhisperReplyToInter(MainProc.ZoneServer.ToInterTCPClient, ZoneID, FromID, WHISPER_SUCCESS);
		end;
		{TODO: Check if ignored}
	end else
	begin
		SendWhisperReplyToInter(MainProc.ZoneServer.ToInterTCPClient, ZoneID, FromID, WHISPER_FAILED);
	end;
end;{RecvRedirectWhisper}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvWarpRequestReplyFromInter                                        PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Gets the warp request reply from the inter server and tells the client
//		to warp.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    April 29th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure RecvWarpRequestReplyFromInter(
	const InBuffer : TBuffer
);
var
	CharacterID     : LongWord;
	MapName         : String;
	MapNameLength   : Word;
	IP              : LongWord;
	Port            : Word;
	X, Y            : Word;
	ACharacter      : TCharacter;
	OutBuffer       : TBuffer;

begin
	CharacterID 	:= BufferReadLongWord(4, InBuffer);
	IP						:= BufferReadLongWord(8, InBuffer);
	Port					:= BufferReadWord(12, InBuffer);
	X							:= BufferReadWord(14, InBuffer);
	Y							:= BufferReadWord(16, InBuffer);
	MapNameLength := BufferReadWord(18, InBuffer);
	MapName				:= BufferReadString(20, MapNameLength, InBuffer);
	ACharacter		:= MainProc.ZoneServer.CharacterList.Items[MainProc.ZoneServer.CharacterList.IndexOf(CharacterID)];
	with ACharacter do
	begin
		MapInfo.Cell[ACharacter.Position.X, Position.Y].Beings.Delete(
		MapInfo.Cell[Position.X, Position.Y].Beings.IndexOfObject(ACharacter));
	end;
	ACharacter.Map := MapName;
	ACharacter.Position := Point(X,Y);

	WriteBufferWord(0, $0092, OutBuffer);
	WriteBufferString(2, MapName+'.rsw', 16, OutBuffer);
	WriteBufferWord(18, X, OutBuffer);
	WriteBufferWord(20, Y, OutBuffer);
	WriteBufferLongWord(22, IP, OutBuffer);
	WriteBufferWord(26, Port, Outbuffer);
	SendBuffer(ACharacter.ClientInfo, OutBuffer, 28);
end;{RecvWarpRequestReplyFromInter}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvWhisperReply                                                     PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Redirect whisper result from Inter to client
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    May 3rd, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure RecvWhisperReply(
	InBuffer : TBuffer
);
var
	CharacterID : LongWord;
	Code : Byte;
	idx : Integer;
	Chara : TCharacter;
begin
	CharacterID := BufferReadLongWord(2, InBuffer);
	Code := BufferReadByte(6, InBuffer);
	Idx := MainProc.ZoneServer.CharacterList.IndexOf(CharacterID);
	if Idx > -1 then
	begin
		Chara := MainProc.ZoneServer.CharacterList.Items[idx];
		SendWhisperReply(Chara, Code);
	end;
end;{RecvWhisperReply}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvGMCommandFromInter                                               PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Gets the gm command information from the buffer.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    March 21st, 2007 - RaX - Created.
//	[2007/08/09] Aeomin - "Upgraded" Almost everything..
//------------------------------------------------------------------------------
procedure RecvGMCommandFromInter(
	const InBuffer : TBuffer
);
var
	CommandID	: Word;
	GMID		: LongWord;
	CharacterID	: LongWord;
	ZoneID		: LongWord;
	TargetCID	: LongWord;
	ArgCount	: Word;
	Arguments	: array of String;
	ArgumentLen	: Integer;
	Index		: Integer;
	BufferIndex	: Integer;
	Error		: TStringList;
	FromChar	: String;
	TargetChar	: TCharacter;
	idxY		: SmallInt;
	idxX		: SmallInt;
	Map		: TMap;
begin
	CommandID 	:= BufferReadWord(4,InBuffer);
	GMID		:= BufferReadLongWord(6, InBuffer);
	CharacterID	:= BufferReadLongWord(10, InBuffer);
	ZoneID		:= BufferReadLongWord(14, InBuffer);
	TargetCID	:= BufferReadLongWord(22, InBuffer);
	ArgCount	:= BufferReadWord(26, InBuffer);

	BufferIndex := 28;

	//We need extra for store syntax help message
	SetLength(Arguments, ArgCount + 1);
	for Index := 0 to ArgCount - 1 do
	begin
		ArgumentLen := BufferReadWord(BufferIndex, InBuffer);
		inc(BufferIndex, 2);
		Arguments[Index] := BufferReadString(BufferIndex,ArgumentLen,InBuffer);
		inc(BufferIndex, ArgumentLen);
	end;
	//Since array is 0 based, this would be perfect index
	Arguments[ArgCount] := MainProc.ZoneServer.Commands.GetSyntax(CommandID);

	MainProc.ZoneServer.ZoneLocalDatabase.GameData.Connect;
	FromChar := MainProc.ZoneServer.ZoneLocalDatabase.GameData.GetCharaName(CharacterID);
	MainProc.ZoneServer.ZoneLocalDatabase.GameData.Disconnect;

	Error := TStringList.Create;

	case MainProc.ZoneServer.Commands.GetCommandType(CommandID) of
		//Whole zone server, no player involved
		TYPE_BROADCAST: begin
			//Server only, no player involved
			MainProc.ZoneServer.Commands.Commands[CommandID](Arguments, FromChar, nil, Error);
		end;

		//The Orignal GM
		TYPE_RETURNBACK: begin
			//Recycle
			Index := MainProc.ZoneServer.CharacterList.IndexOf(CharacterID);
			if Index > -1 then
			begin
				TargetChar := MainProc.ZoneServer.CharacterList.Items[Index];
				MainProc.ZoneServer.Commands.Commands[CommandID](Arguments, FromChar, TargetChar, Error);
			end;
		end;

		//All players
		TYPE_ALLPLAYERS: begin
			for Index := MainProc.ZoneServer.CharacterList.Count -1 downto 0 do
			begin
				TargetChar := MainProc.ZoneServer.CharacterList.Items[Index];
				MainProc.ZoneServer.Commands.Commands[CommandID](Arguments, FromChar, TargetChar, Error);
			end;
		end;

		//Specific Character
		TYPE_TARGETCHAR: begin
			Index := MainProc.ZoneServer.CharacterList.IndexOf(TargetCID);
			if Index > -1 then
			begin
				TargetChar := MainProc.ZoneServer.CharacterList.Items[Index];
				MainProc.ZoneServer.Commands.Commands[CommandID](Arguments, FromChar, TargetChar, Error);
			end else
			begin
				Error.Add('Character ' + Arguments[0] + ' not found!');
			end;
		end;

		//All players in Specific map
		TYPE_TARGETMAP: begin
			//Arguments[0] should be map name
			Index := MainProc.ZoneServer.MapList.IndexOf(Arguments[0]);
			if Index > -1 then
			begin
				Map := MainProc.ZoneServer.MapList.Items[Index];
				//Every player will be executed!
				//more checking should done in actual gm command code
				for idxY := Map.Size.Y - 1 downto 0 do
				begin
					for idxX := Map.Size.X - 1 downto 0 do
					begin
						for Index := Map.Cell[idxX, idxY].Beings.Count - 1 downto 0 do
						begin
							if not (Map.Cell[idxX,idxY].Beings.Objects[Index] is TCharacter) then
							begin
								Continue;
							end;
							TargetChar := Map.Cell[idxX,idxY].Beings.Objects[Index] as TCharacter;
							MainProc.ZoneServer.Commands.Commands[CommandID](Arguments, FromChar, TargetChar, Error);
						end;
					end;
				end;
			end else
			begin
				Error.Add('Map ' + Arguments[0] + ' not found!');
			end;
		end;
	end;

	if Error.Count > 0 then
	begin
		ZoneSendGMCommandResultToInter(GMID, CharacterID, ZoneID, Error);
	end;

	Error.Free;
end;{RecvGMCommandFromInter}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvGMCommandResultFromInter                                         PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Gets the gm command result information from the buffer.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    March 21st, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure RecvGMCommandResultFromInter(
	const InBuffer : TBuffer
);
var
	CharacterID : LongWord;
	ACharacter	: TCharacter;
	Index : Integer;
	ErrCount    : Word;
	BufferIndex : Integer;
	ErrLen      : Word;
	Error       : TStringList;
	LoopTrials  : Byte;
begin
	CharacterID := BufferReadLongWord(4, InBuffer);
	ErrCount   := BufferReadWord(8, InBuffer);
	if (ErrCount > 0) then
	begin
		Error := TStringList.Create;
		BufferIndex := 10;

		for Index := 0 to ErrCount - 1 do
		begin
			ErrLen := BufferReadWord(BufferIndex, InBuffer);
			inc(BufferIndex, 2);
			Error.Add(BufferReadString(BufferIndex,ErrLen,InBuffer));
			inc(BufferIndex, ErrLen);
		end;

		LoopTrials := 0;
		While LoopTrials < 10 do
		begin
			Index := MainProc.ZoneServer.CharacterList.IndexOf(CharacterID);
			if Index > -1 then
			begin
				ACharacter	:= MainProc.ZoneServer.CharacterList[Index];
				for Index := 0 to Error.Count - 1 do
				begin
					ZoneSendCharacterMessage(ACharacter, Error[Index]);
				end;
				Break;
			end else
			begin
				Inc(LoopTrials);
				Sleep(1000);
			end;
		end;
		Error.Free;
	end;
end;{RecvGMCommandFromInter}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvMapWarpRequest                                                   PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Receive Map warp request from Inter server.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/08/13] - Aeomin - Created
//------------------------------------------------------------------------------
procedure RecvMapWarpRequest(
	InBuffer : TBuffer
);
var
	CharID        : LongWord;
	ZoneID        : LongWord;
	MapNameLen    : Byte;
	MapName       : String;
	Map           : TMap;
	Index         : Integer;
	APoint        : TPoint;
begin
	CharID     := BufferReadLongWord(4, InBuffer);
	ZoneID     := BufferReadLongWord(8, InBuffer);
	APoint.X   := BufferReadWord(12, InBuffer);
	APoint.Y   := BufferReadWord(14, InBuffer);
	MapNameLen := BufferReadByte(16, InBuffer);
	MapName    := BufferReadString(17, MapNameLen, InBuffer);

	Index := MainProc.ZoneServer.MapList.IndexOf(MapName);
	if Index > -1 then
	begin
		Map := MainProc.ZoneServer.MapList.Items[Index];

		if Map.SafeLoad then
		begin

			if (APoint.X > Map.Size.X -1) or (APoint.Y > Map.Size.Y -1)
			or (APoint.X < 0) or (APoint.Y < 0) or Map.IsBlocked(APoint) then
			begin
				APoint := Map.RandomCell;
			end;

			ZoneSendMapWarpResultToInter(MainProc.ZoneServer.ToInterTCPClient, CharID, ZoneID, MapName, APoint);
		end;
	end;
end;{RecvMapWarpRequest}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvAddFriendRequest                                                 PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Receive add friend request from inter server
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/12/07] - Aeomin - Created
//------------------------------------------------------------------------------
procedure RecvAddFriendRequest(
	InBuffer : TBuffer
);
var
	ReqAID, ReqID  : LongWord;
	ReqName        : String;
	TargetChar     : LongWord;

	Index  : Integer;
	Chara  : TCharacter;
begin
	ReqAID      := BufferReadLongWord(2, InBuffer);
	ReqID       := BufferReadLongWord(6, InBuffer);
	TargetChar  := BufferReadLongWord(10, InBuffer);
	ReqName     := BufferReadString(14, NAME_LENGTH, InBuffer);

	Index := MainProc.ZoneServer.CharacterList.IndexOf(TargetChar);
	if Index > -1 then
	begin
		Chara := MainProc.ZoneServer.CharacterList[Index] as TCharacter;
		if Chara.Friends >= MAX_FRIENDS then
		begin
			// to much..
			ZoneSendAddFriendReply(
				MainProc.ZoneServer.ToInterTCPClient,
				ReqID,
				Chara.ID,
				Chara.CID,
				Chara.Name,
				3
			);
		end else
		begin
			SendAddFriendRequest(Chara.ClientInfo, ReqAID, ReqID, ReqName);
		end;
	end;
end;{RecvAddFriendRequest}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvAddFriendRequestReply                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Receive reply from inter server
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/12/08] - Aeomin - Created
//------------------------------------------------------------------------------
procedure RecvAddFriendRequestReply(
	InBuffer : TBuffer
);
var
	OrigID    : LongWord;
	AccID     : LongWord;
	CharID    : LongWord;
	CharName  : String;
	Reply     : Byte;

	Index  : Integer;
	Chara  : TCharacter;
	Send : Boolean;

	FriendRequestEvent : TFriendRequestEvent;
begin
	OrigID   := BufferReadLongWord(2, InBuffer);
	AccID    := BufferReadLongWord(6, InBuffer);
	CharID   := BufferReadLongWord(10, InBuffer);
	Reply    := BufferReadByte(14, InBuffer);
	CharName := BufferReadString(15, NAME_LENGTH, InBuffer);
	Send := True;
	Index := MainProc.ZoneServer.CharacterList.IndexOf(OrigID);
	if Index > -1 then
	begin
		Chara := MainProc.ZoneServer.CharacterList[Index] as TCharacter;
		if Reply = 0 then
		begin
			if Chara.EventList.Count > 0 then
			begin
				for Index := Chara.EventList.Count -1 downto 0 do
				begin
					if Chara.EventList.Items[Index] is TFriendRequestEvent then
					begin
						FriendRequestEvent := Chara.EventList.Items[Index] as TFriendRequestEvent;
						if FriendRequestEvent.PendingFriend = CharID then
						begin
							Chara.EventList.Delete(Index);
							//Accept
							with TThreadLink(Chara.ClientInfo.Data).DatabaseLink.GameData do
							begin
								Connect;
								try
									//Add target first
									if not IsFriend(OrigID, AccID, CharID) then
										AddFriend(OrigID, AccID, CharID, CharName);
									//Add back
									if not IsFriend(CharID, Chara.ID, Chara.CID) then
										AddFriend(CharID, Chara.ID, Chara.CID, Chara.Name);

									// Use 255 here!
									ZoneSendAddFriendReply(
										MainProc.ZoneServer.ToInterTCPClient,
										CharID,
										Chara.ID,
										Chara.CID,
										Chara.Name,
										255
									);
									Break;
								finally
									Disconnect;
								end;
							end;
						end else
							Send := False;
					end else
						Send := False;
				end;
			end else
				Send := False;
		end;

		// Why? to make sure not trigger again -.-
		if Reply = 255 then
			Reply := 0;
		if Reply = 0 then
			Inc(Chara.Friends);
		if Send then
			SendAddFriendRequestReply(Chara.ClientInfo, AccID, CharID, CharName, Reply);
	end;
end;{RecvAddFriendRequestReply}
//------------------------------------------------------------------------------
end{ZoneRecv}.
