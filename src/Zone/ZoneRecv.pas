//------------------------------------------------------------------------------
//ZoneRecv                                                                 UNIT
//------------------------------------------------------------------------------
//  What it does -
//      Receives packets sent by users on the zone server, does whatever they
//    tell us to do =) Contains all routines related to doing as such.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//		[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
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


//------------------------------------------------------------------------------
//NoCommand                                                           PROCEDURE
//------------------------------------------------------------------------------
	procedure NoCommand(
			AChara : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
		); overload;

//------------------------------------------------------------------------------
//NoCommand                                                           PROCEDURE
//------------------------------------------------------------------------------
	procedure NoCommand(
		const
			AChara : TCharacter;
		const
			AvoidSelf:boolean = False
		); overload;

//------------------------------------------------------------------------------
//MapConnect                                                          PROCEDURE
//------------------------------------------------------------------------------
	procedure MapConnect(
		Version : Integer;
		AClient : TIdContext;
		Buffer  : TBuffer;
		const
			ReadPts : TReadPts
		);

//------------------------------------------------------------------------------
//ShowMap                                                             PROCEDURE
//------------------------------------------------------------------------------
	Procedure ShowMap(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
		);

//------------------------------------------------------------------------------
//RecvTick                                                            PROCEDURE
//------------------------------------------------------------------------------
	procedure RecvTick(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);

//------------------------------------------------------------------------------
//CharacterWalkRequest                                                PROCEDURE
//------------------------------------------------------------------------------
	procedure CharacterWalkRequest(
			AChara : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);

//------------------------------------------------------------------------------
//GetNameAndID                                                        PROCEDURE
//------------------------------------------------------------------------------
	Procedure GetNameAndID(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);
//------------------------------------------------------------------------------

	procedure ReturnToCharacterSelect(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);

	procedure QuitGame(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);
	procedure AreaChat(
		ACharacter  : TCharacter;
		InBuffer		: TBuffer;
	const
		ReadPts : TReadPts
	);
	procedure RecvGMCommandFromInter(
		InBuffer : TBuffer
	);
	procedure RecvGMCommandResultFromInter(
			InBuffer : TBuffer
	);
	procedure CharaRotation(
			ACharacter  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);
	procedure SlashWho(
			ACharacter  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);


implementation


uses
	{RTL/VCL}
	Types,
	WinLinux,
	{Project}
	Account,
	Being,
	BufferIO,
	GameConstants,
	GMCommands,
	Main,
	Map,
	MapTypes,
	MovementEvent,
	TCPServerRoutines,
	ZoneSend,
	ZoneServer,
	ZoneCharaCommunication
	{3rd Party}
	//none
	;


//------------------------------------------------------------------------------
//NoCommand                                                           PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      This is a dummy command for processes that either don't do anything or
//    don't do anything yet.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
	Procedure NoCommand(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);
	Begin
		//Dummy Command for processes that don't have one.
	End;//NoCommand
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//NoCommand                                                           PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      This is a dummy command for processes that either don't do anything or
//    don't do anything yet.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
	Procedure NoCommand(
		const
			AChara    : TCharacter;
		const
			AvoidSelf : Boolean = False
	);
	Begin
		//Dummy Command for processes that don't have one.
	End;//NoCommand
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//MapConnect                                                          PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      This is executed when a character requests to conenct ot the map server.
//    It first checks to see if a character should be able to connect to the
//    zone (to stop hacking attempts) and then it links the chosen character to
//    the connection.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
	procedure MapConnect(
			Version : Integer;
			AClient : TIdContext;
			Buffer  : TBuffer;
		const
			ReadPts : TReadPts
	);
	var
		AccountID   : LongWord;
		CharacterID : LongWord;
		ValidateID1 : LongWord;
		//ClientTick  : LongWord;
		Gender      : Byte;
		AnAccount   : TAccount;
		ACharacter  : TCharacter;
		OutBuffer   : Tbuffer; //temp
		MapIndex    : Integer;
		
	begin
		AccountID      := BufferReadLongWord(ReadPts[0], Buffer);
		CharacterID    := BufferReadLongWord(ReadPts[1], Buffer);
		ValidateID1    := BufferReadLongWord(ReadPts[2], Buffer);
		{ClientTick     := }BufferReadLongWord(ReadPts[3], Buffer);
		Gender         := BufferReadByte    (ReadPts[4], Buffer);
		TThreadLink(AClient.Data).DatabaseLink.CommonData.Connect;
		AnAccount  := TThreadLink(AClient.Data).DatabaseLink.CommonData.GetAccount(AccountID);
		TThreadLink(AClient.Data).DatabaseLink.CommonData.Disconnect;

		//use client local game database
		TThreadLink(AClient.Data).DatabaseLink.GameData.Connect;
		ACharacter := TThreadLink(AClient.Data).DatabaseLink.GameData.GetChara(CharacterID,true);
		TThreadLink(AClient.Data).DatabaseLink.GameData.Disconnect;
		
		if Assigned(AnAccount) and Assigned(ACharacter) then
		begin
			if (AnAccount.LoginKey[1] = ValidateID1) and
				(AnAccount.GenderNum = Gender) then
			begin
				TClientLink(AClient.Data).AccountLink := AnAccount;
				TClientLink(AClient.Data).CharacterLink := ACharacter;
				ACharacter.ClientVersion := Version;
				ACharacter.Online  := 1;
				MainProc.ZoneServer.CharacterList.Add(ACharacter);

				//Load map cells if they are not already loaded
				MapIndex := MainProc.ZoneServer.MapList.IndexOf(ACharacter.Map);
				if MapIndex > -1 then
				begin
					if MainProc.ZoneServer.MapList[MapIndex].State = UNLOADED then
					begin
						MainProc.ZoneServer.MapList[MapIndex].Load;
					end;
				end;

				SendPadding(ACharacter.ClientInfo);

				ZoneSendMapConnectReply(ACharacter);

				//Friendslist placeholder
				WriteBufferWord(0, $0201, OutBuffer);
				WriteBufferWord(2, 4, OutBuffer);
				SendBuffer(ACharacter.ClientInfo, OutBuffer, 4);

			end else
			begin
				ZoneSendMapConnectDeny(AClient);
			end;
		end;
	end;//MapConnect
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ShowMap                                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      This is executed on a character showing the map, it sends them all the
//    information they need, such as their skill list, stats, friends list,
//    guild, etc. Anything that would be used by the character.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//    March 30th, 2007 - Aeomin - Move SendZoneCharaIncrease to here.
//------------------------------------------------------------------------------
	Procedure ShowMap(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);
	var
		OutBuffer : TBuffer;
		AMap : TMap;
		MapIndex : Integer;
	Begin
		SendZoneCharaIncrease(MainProc.ZoneServer.ToCharaTCPClient,MainProc.ZoneServer);
		MapIndex := MainProc.ZoneServer.MapList.IndexOf(AChara.Map);
		if MapIndex > -1 then
		begin
			if MainProc.ZoneServer.MapList[MapIndex].State = UNLOADED then
			begin
				MainProc.ZoneServer.MapList[MapIndex].Load;
			end;
		end;
		AMap := MainProc.ZoneServer.MapList[MapIndex];
		AChara.MapInfo := AMap;


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
		AMap.Cell[AChara.Position.X][AChara.Position.Y].Beings.AddObject(AChara.ID,AChara);
	end;//ShowMap
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvTick                                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      This sends a simple ping command basically to the client on receiving a
//    "tick" from them. It ensures that the client is still connected to the
//    game server.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
	procedure RecvTick(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);
	Begin
		ZoneSendTickToClient(AChara);
	end;//RecvTick
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//CharaWalkRequest                                                    PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Processes a Character's request to walk to a certain point.
//
//  Changes -
//    Rebruary 27th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
	procedure CharacterWalkRequest(
			AChara		: TCharacter;
			InBuffer	: TBuffer;
		const
			ReadPts		: TReadPts
	);
	var
		MoveEvent : TMovementEvent;
		DestPoint : TPoint;
		spd 			: LongWord;
		Index			: Integer;
	begin
		DestPoint := BufferReadOnePoint(ReadPts[0], InBuffer);
		if true {Various checks (not sitting)} then
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
//GetNameAndID                                                        PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      This gets a character's name and id from the charalist if it's there,
//    else, it looks through mobs and npcs.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
	Procedure GetNameAndID(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);
	var
		ID : LongWord;
		idx : integer;
		RecvCharacter : TCharacter;
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
		end;


	end;//GetNameAndID
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ReturnToCharacterSelect                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Fired when a player clicks the return to chara select button.
//
//  Changes -
//    March 17th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
	procedure ReturnToCharacterSelect(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
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
				if AChara.CharaState <> charaDead then Exit;
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
//QuitGame							                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Fired when a player clicks Exit to windows button ingame.
//
//  Changes -
//    March 17th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
procedure QuitGame(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);
begin
	//send client response.
	SendQuitGameResponse(AChara);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//AreaChat							       PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Receive and Send local speech
//
//  Changes -
//    March 18th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure AreaChat(
			ACharacter  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);
var
	ChatLength	: Word;
	Chat				: String;
	TempChat		: String;
  CommandID   : Word;
begin
		ChatLength	:= BufferReadWord(ReadPts[0], InBuffer)-4;
		Chat				:= BufferReadString(ReadPts[1], ChatLength, InBuffer);

		//First, we remove the character's name and the colon after it from the
		//chat string.
		TempChat := Copy(Chat, Length(ACharacter.Name) + 3, Length(Chat));
		//We then check if it is a command.
		if MainProc.ZoneServer.Commands.IsCommand(TempChat) then
		begin
      CommandID := MainProc.ZoneServer.Commands.GetCommandID(
                    MainProc.ZoneServer.Commands.GetCommandName(TempChat)
									 );
			//if it is a command, we check the account's access level to see if it is
			//able to use the gm command.
			if TClientLink(ACharacter.ClientInfo.Data).AccountLink.Level >= MainProc.ZoneServer.Commands.GetCommandGMLevel(CommandID) then
			begin
				ZoneSendGMCommandtoInter(ACharacter, TempChat);
			end else
			begin
				SendAreaChat(Chat, ChatLength, ACharacter);
			end;
		end else
		begin
			SendAreaChat(Chat, ChatLength, ACharacter);
		end;
end;{AreaChat}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//CharaRotation							       PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Rotate Character, check,  set and send to other characters.
//
//  Changes -
//    March 20th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure CharaRotation(
			ACharacter  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);

begin
	ACharacter.HeadDirection := BufferReadWord(ReadPts[0], InBuffer);
	ACharacter.Direction:=BufferReadByte(ReadPts[1], InBuffer);
	ACharacter.UpdateDirection;
end;{CharaRotation}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//SlashWho							       PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Check how many player(s) were online.
//
//  Changes -
//    April 5th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure SlashWho(
			ACharacter  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);
begin
	ZoneSendConnectionsCount(ACharacter.ClientInfo);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvGMCommandFromInter																							PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Gets the gm command information from the buffer.
//
//  Changes -
//    March 21st, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure RecvGMCommandFromInter(
			InBuffer : TBuffer
	);
var
	CommandID		: Word;
	GMID				: LongWord;
	CharacterID	: LongWord;
	ArgCount		: Word;
	Arguments		: array of String;
	ArgumentLen	: Integer;
	Index				: Integer;
	BufferIndex	: Integer;
	Error				: String;
	Success			: Boolean;

begin
	CommandID 	:= BufferReadWord(4,InBuffer);
	GMID				:= BufferReadLongWord(6, InBuffer);
	CharacterID	:= BufferReadLongWord(10, InBuffer);
	ArgCount		:= BufferReadWord(14, InBuffer);

	BufferIndex := 16;

	SetLength(Arguments, ArgCount);
	for Index := 0 to ArgCount - 1 do
	begin
		ArgumentLen := BufferReadWord(BufferIndex, InBuffer);
		inc(BufferIndex, 2);
		Arguments[Index] := BufferReadString(BufferIndex,ArgumentLen,InBuffer);
		inc(BufferIndex, ArgumentLen);
	end;

	Success := MainProc.ZoneServer.Commands.Commands[CommandID](Arguments, Error);
	ZoneSendGMCommandResultToInter(GMID,CharacterID, Success, Error);
end;{RecvGMCommandFromInter}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvGMCommandResultFromInter						PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Gets the gm command result information from the buffer.
//
//  Changes -
//    March 21st, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure RecvGMCommandResultFromInter(
			InBuffer : TBuffer
	);
var
	CharacterID : LongWord;
	ErrorLength	: LongWord;
	Error				: String;
	ACharacter	: TCharacter;

begin
	CharacterID := BufferReadLongWord(10, InBuffer);
	ErrorLength := BufferReadLongWord(14, InBuffer);
	Error				:= BufferReadString(18, ErrorLength, InBuffer);
	ACharacter	:= MainProc.ZoneServer.CharacterList[
										MainProc.ZoneServer.CharacterList.IndexOf(CharacterID)
									];
	ZoneSendCharacterMessage(ACharacter, Error);
end;{RecvGMCommandFromInter}
//------------------------------------------------------------------------------
end.
