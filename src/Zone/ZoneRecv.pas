unit ZoneRecv;

interface
uses
	Character,
	PacketTypes,
	{Third Party}
	IdContext
	;

	procedure NoCommand(
			AChara : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
		); overload;

	procedure NoCommand(
		const
			AChara : TCharacter;
		const
			AvoidSelf:boolean = False
		); overload;

	procedure MapConnect(
		Version : Integer;
		AClient : TIdContext;
		Buffer  : TBuffer;
		const
			ReadPts : TReadPts
		);

	Procedure ShowMap(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
		);

	procedure RecvTick(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);

	Procedure GetNameAndID(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);

implementation
uses
	Account,
	BufferIO,
	Console,
	GameConstants,
	Globals,
	ZoneSend,
  ZoneServer;

	Procedure NoCommand(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);
	Begin
		//Dummy Command for processes that don't have one.
	End;

	Procedure NoCommand(
		const
			AChara    : TCharacter;
		const
			AvoidSelf : Boolean = False
	);
	Begin
		//Dummy Command for processes that don't have one.
	End;


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
		ClientTick  : Cardinal;
		Gender      : Byte;
		AnAccount   : TAccount;
		ACharacter  : TCharacter;
		OutBuffer   : Tbuffer; //temp
	begin
		AccountID      := BufferReadCardinal(ReadPts[0], Buffer);
		CharacterID    := BufferReadCardinal(ReadPts[1], Buffer);
		ValidateID1    := BufferReadCardinal(ReadPts[2], Buffer);
		ClientTick     := BufferReadCardinal(ReadPts[3], Buffer);
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
	end;

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

	end;

	procedure RecvTick(
			AChara  : TCharacter;
			InBuffer : TBuffer;
		const
			ReadPts : TReadPts
	);
	Begin
		ZoneSendTickToClient(AChara.ClientInfo);
	end;

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


	end;

end.
