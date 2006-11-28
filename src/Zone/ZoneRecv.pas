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
			RecvBuffer : TBuffer;
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
			RecvBuffer : TBuffer;
		const
			ReadPts : TReadPts
		);

implementation
uses
	Account,
	Database,
	Socket,
	ZoneSend;

	Procedure NoCommand(
			AChara  : TCharacter;
			RecvBuffer : TBuffer;
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
		ValidateIDs : array[1..2] of Cardinal;
		Gender      : Byte;
		ADatabase   : TDatabase;
		AnAccount   : TAccount;
		ACharacter  : TCharacter;
	begin
		AccountID      := BufferReadCardinal(ReadPts[0], Buffer);
		CharacterID    := BufferReadCardinal(ReadPts[1], Buffer);
		ValidateIDs[1] := BufferReadCardinal(ReadPts[2], Buffer);
		ValidateIDs[2] := BufferReadCardinal(ReadPts[3], Buffer);
		Gender         := BufferReadByte    (ReadPts[4], Buffer);

		ADatabase  := TDatabase.Create();
		AnAccount  := ADatabase.AnInterface.GetAccount(AccountID);
		// reinitiallize and connect to the game database
		ADatabase  := TDatabase.Create(true);
		ACharacter := ADatabase.AnInterface.GetChara(CharacterID);

		if Assigned(AnAccount) and Assigned(ACharacter) then
		begin
			if (AnAccount.LoginKey[1] = ValidateIDs[1]) and
				(AnAccount.LoginKey[2] = ValidateIDs[2]) and
				(AnAccount.GenderNum = Gender) then
			begin
				TThreadLink(AClient.Data).CharacterLink := ACharacter;
				ACharacter.ClientVersion := Version;
				ACharacter.Account := AnAccount;
				ACharacter.Online  := 1;
				ACharacter.ClientInfo := AClient;

				SendPadding(ACharacter.ClientInfo);

				ZoneSendMapConnectReply(ACharacter);

				//ADD FRIENDSLIST HERE
			end else
			begin
      	ZoneSendMapConnectDeny(AClient);
			end;
		end;
	end;

	Procedure ShowMap(
			AChara  : TCharacter;
			RecvBuffer : TBuffer;
		const
			ReadPts : TReadPts
		);
	Begin
	end;

end.