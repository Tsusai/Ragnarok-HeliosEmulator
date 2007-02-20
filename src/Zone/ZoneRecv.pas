//------------------------------------------------------------------------------
//ZoneRecv                                                                 UNIT
//------------------------------------------------------------------------------
//  What it does -
//      Receives packets sent by users on the zone server, does whatever they
//    tell us to do =) Contains all routines related to doing as such.
//
//  Changes -
//    January 18th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
unit ZoneRecv;

interface
uses
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

implementation
uses
	Math,
	SysUtils,
	Types,
	Account,
	BufferIO,
	Main,
	GameConstants,
	Globals,
	MapTypes,
	Map,
	TCPServerRoutines,
	ZoneSend,
	ZoneServer;

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

		AnAccount  := ADatabase.CommonData.GetAccount(AccountID);
		//use global game database
		ACharacter := ADatabase.GameData.GetChara(CharacterID,true);

		if Assigned(AnAccount) and Assigned(ACharacter) then
		begin
			if (AnAccount.LoginKey[1] = ValidateID1) and
				(AnAccount.GenderNum = Gender) then
			begin
				TThreadLink(AClient.Data).CharacterLink := ACharacter;
				ACharacter.ClientVersion := Version;
				ACharacter.Account := AnAccount;
				ACharacter.Online  := 1;
				ACharacter.ClientInfo := AClient;
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
				SendBuffer(ACharacter.ClientInfo, OutBuffer, GetPacketLength($0201,Version));

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
		idx1 : integer;
		idx2 : integer;
		idx3 : integer;
		AnObject : TObject;
	Begin
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

		AMap.Cell[AChara.Point.X][AChara.Point.Y].Beings.AddObject(AChara.CID,AChara);

		for idx1 := Min(0,AChara.Point.Y-15) to Max(AChara.Point.Y+15,AMap.Size.Y) do
		begin
			for idx2 := Min(0,AChara.Point.X-15) to Max(AChara.Point.X+15,AMap.Size.X) do
			begin
				for idx3 := AMap.Cell[idx1][idx2].Beings.Count -1 downto 0 do
				begin
					AnObject := AMap.Cell[idx1][idx2].Beings.Objects[idx3];
					if AnObject is TCharacter then
					begin
						if AChara <> AnObject then
						begin
							//Send AChara to the other character
							//Send Character to AChara
						end;
					end;
				end;
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
		SendBuffer(AChara.ClientInfo, OutBuffer, GetPacketLength($010F,AChara.ClientVersion));

		AChara.SendCharacterStats;

		//Inventory Placeholder
		WriteBufferWord(0, $00a3, OutBuffer);
		WriteBufferWord(2, 4, OutBuffer);
		SendBuffer(AChara.ClientInfo, OutBuffer, GetPacketLength($00a3,AChara.ClientVersion));
		//Equip Placeholder
		WriteBufferWord(0, $00a4, OutBuffer);
		WriteBufferWord(2, 4, OutBuffer);
		SendBuffer(AChara.ClientInfo, OutBuffer, GetPacketLength($00a4,AChara.ClientVersion));
		//Arrow Placeholder
		WriteBufferWord(0, $013c, OutBuffer);
		WriteBufferWord(2, 0, OutBuffer);
		SendBuffer(AChara.ClientInfo, OutBuffer, GetPacketLength($013c,AChara.ClientVersion));

		//Weather updates
		//Various other tweaks

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


	procedure CharacterWalkRequest(
			AChara : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);
	var
		DestPoint : TPoint;
	begin
		DestPoint := BufferReadOnePoint(ReadPts[0], InBuffer);
		
		if true {Various checks (not sitting)} then
		begin
			MainProc.Console.WriteLn(Format('X%d Y%d recieved',[DestPoint.X,DestPoint.Y]));
			{if AChara.MapInfo.GetPath(AChara.Point,DestPoint,) then
			begin
			end;}
		end;
	end;


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

end.
