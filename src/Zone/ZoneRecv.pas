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
	Account,
	BufferIO,
	Console,
	GameConstants,
	Globals,
  MapTypes,
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
		AccountID   : Cardinal;
		CharacterID : Cardinal;
		ValidateID1 : Cardinal;
		//ClientTick  : Cardinal;
		Gender      : Byte;
		AnAccount   : TAccount;
		ACharacter  : TCharacter;
		OutBuffer   : Tbuffer; //temp
    MapIndex    : Integer;
	begin
		AccountID      := BufferReadCardinal(ReadPts[0], Buffer);
		CharacterID    := BufferReadCardinal(ReadPts[1], Buffer);
		ValidateID1    := BufferReadCardinal(ReadPts[2], Buffer);
		{ClientTick     := }BufferReadCardinal(ReadPts[3], Buffer);
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
//------------------------------------------------------------------------------
	Procedure ShowMap(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);
	var
		OutBuffer : TBuffer;
	Begin

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
		SendBuffer(AChara.ClientInfo, OutBuffer, 4);

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
		ZoneSendTickToClient(AChara.ClientInfo);
	end;//RecvTick
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
		ID : Cardinal;
		idx : integer;
		RecvCharacter : TCharacter;
	begin
		ID := BufferReadCardinal(ReadPts[0], InBuffer);
		idx := CharacterList.IndexOfAID(ID);
		if idx > -1 then
		begin
			RecvCharacter := CharacterList.Items[idx];
			{guild check}
			{guild version packet}
			{else}
			ZoneSendObjectNameAndIDBasic(
				AChara.ClientInfo,
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
