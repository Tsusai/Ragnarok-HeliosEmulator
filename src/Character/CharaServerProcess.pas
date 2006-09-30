(*------------------------------------------------------------------------------
CharaServerProcess
RaX/Tsusai 2006

Description:
 The Character Server.  Handles all client communication here.
------------------------------------------------------------------------------*)
unit CharaServerProcess;

interface
uses
	//3rd Party
	IdTCPServer;

	procedure ParseCharaServ(AThread : TIdPeerThread);

implementation
	uses
		//IDE
		SysUtils,
		//Helios
		Character,
    Database,
		Socket,
		Account,
		PacketTypes,
		GameGlobals,
		Globals,
		//3rd
		List32;


const
	INVALIDNAME = 0;
	INVALIDMISC = 2;
	DELETEBADCHAR = 0;
	DELETEBADEMAIL = 1;
	

(*------------------------------------------------------------------------------
SendCharas

Verifies the new connection by checking the account ID which is recieved and
 the two random keys generated during login.

Upon validation, check database for any/all created characters.

[2006/07/06] Tsusai - Started work on changing dummy procedure to real procedure
------------------------------------------------------------------------------*)
procedure SendCharas(AThread : TIdPeerThread; var ABuffer : TBuffer);
var
	AccountID   : Cardinal;
	Success     : boolean;
	AnAccount   : TAccount;
	ReplyBuffer : TBuffer;
	ACharacter  : TCharacter;
	idx         : byte;
	Count       : byte;
	PacketSize  : Word;
	CID         : Cardinal;
	Ver         : byte;
	CharaList   : TIntList32;
  ADatabase   : TDatabase;
begin
	Count := 0;
	Ver := 24;
  ADatabase := CreateDatabase;
	Success := false;
	AccountID := BufferReadCardinal(2, ABuffer);
	SQLQueryResult :=
		SQLConnection.query(
			Format('SELECT userid FROM login WHERE account_id = %d;',
			[AccountID]),true,Success);
	if Success then begin
		if SQLqueryResult.RowsCount > 0 then
		begin
			AnAccount := ADatabase.GetAccount(SQLQueryResult.FieldValue(0));
			if Assigned(AnAccount) then
			begin
				if AnAccount.ID = AccountID then
				begin
					if (AnAccount.LoginKey[1] = BufferReadCardinal(6,ABuffer)) and
						(AnAccount.LoginKey[2] = BufferReadCardinal(10,ABuffer)) then
					begin
						//LINK the account to the client connection for the other procedures
						TThreadLink(AThread.Data).AccountLink := AnAccount;
						SendPadding(AThread); //Legacy padding
						SQLQueryResult := SQLConnection.query(
							Format('SELECT char_id FROM `char` WHERE account_id = %d and char_num < 9;',
								[AccountID]),true,Success);
						if Success then
						begin
							if SQLQueryResult.RowsCount > 0 then
							begin
								CharaList := TIntList32.Create;
								//Save all characterIDs
								for idx := 1 to SQLQueryResult.RowsCount do
								begin
									CharaList.Add(StrToInt(SQLQueryResult.FieldValue(0)));
									SQLQueryResult.Next;
								end;

								for idx := 0 to CharaList.Count - 1 do
								begin
									CID := CharaList.Integers[idx];
									//THE GETCHARACTERS IS SCREWING UP THE QUERY RESULT
									//STORE ALL FIELDS FIRST WITH TINTLIST32
									ACharacter := GetCharacters(CID);
									SQLQueryResult.Next;
									if not (ACharacter = nil) then
									begin
										with ACharacter do
										begin
											FillChar(ReplyBuffer[Ver+(Count*106)], 106, 0);
											WriteBufferCardinal(Ver+(Count*106)+  0, CID,ReplyBuffer);
											WriteBufferCardinal(Ver+(Count*106)+  4, BaseEXP,ReplyBuffer);
											WriteBufferCardinal(Ver+(Count*106)+  8, Zeny,ReplyBuffer);
											WriteBufferCardinal(Ver+(Count*106)+ 12, JobEXP,ReplyBuffer);
											WriteBufferCardinal(Ver+(Count*106)+ 16, JobLV,ReplyBuffer);
											WriteBufferCardinal(Ver+(Count*106)+ 20, 0,ReplyBuffer);
											WriteBufferCardinal(Ver+(Count*106)+ 24, 0,ReplyBuffer);
											WriteBufferCardinal(Ver+(Count*106)+ 28, Option,ReplyBuffer);
											WriteBufferCardinal(Ver+(Count*106)+ 32, Karma,ReplyBuffer);
											WriteBufferCardinal(Ver+(Count*106)+ 36, Manner,ReplyBuffer);

											WriteBufferWord(Ver+(Count*106)+ 40, StatusPts,ReplyBuffer);
											WriteBufferWord(Ver+(Count*106)+ 42, HP,ReplyBuffer);
											WriteBufferWord(Ver+(Count*106)+ 44, MAXHP,ReplyBuffer);
											WriteBufferWord(Ver+(Count*106)+ 46, SP,ReplyBuffer);
											WriteBufferWord(Ver+(Count*106)+ 48, MAXSP,ReplyBuffer);
											WriteBufferWord(Ver+(Count*106)+ 50, Speed,ReplyBuffer);
											WriteBufferWord(Ver+(Count*106)+ 52, JID,ReplyBuffer);
											WriteBufferWord(Ver+(Count*106)+ 54, Hair,ReplyBuffer);
											WriteBufferWord(Ver+(Count*106)+ 56, Weapon,ReplyBuffer);
											WriteBufferWord(Ver+(Count*106)+ 58, BaseLV,ReplyBuffer);
											WriteBufferWord(Ver+(Count*106)+ 60, SkillPts,ReplyBuffer);
											WriteBufferWord(Ver+(Count*106)+ 62, HeadBottom,ReplyBuffer); //Head3
											WriteBufferWord(Ver+(Count*106)+ 64, Shield,ReplyBuffer);
											WriteBufferWord(Ver+(Count*106)+ 66, HeadTop,ReplyBuffer); //head1
											WriteBufferWord(Ver+(Count*106)+ 68, HeadMid,ReplyBuffer); //head2
											WriteBufferWord(Ver+(Count*106)+ 70, HairColor,ReplyBuffer);
											WriteBufferWord(Ver+(Count*106)+ 72, ClothesColor,ReplyBuffer);

											WriteBufferString(Ver+(Count*106)+ 74, Name, 24,ReplyBuffer);

											WriteBufferByte(Ver+(Count*106)+98,  ParamBase[STR],ReplyBuffer);
											WriteBufferByte(Ver+(Count*106)+99,  ParamBase[AGI],ReplyBuffer);
											WriteBufferByte(Ver+(Count*106)+100, ParamBase[VIT],ReplyBuffer);
											WriteBufferByte(Ver+(Count*106)+101, ParamBase[INT],ReplyBuffer);
											WriteBufferByte(Ver+(Count*106)+102, ParamBase[DEX],ReplyBuffer);
											WriteBufferByte(Ver+(Count*106)+103, ParamBase[LUK],ReplyBuffer);

											WriteBufferByte(Ver+(Count*106)+104, CharaNum,ReplyBuffer);
											WriteBufferByte(Ver+(Count*106)+105, 0,ReplyBuffer);
											Inc(Count);
										end;
									end;
									{ TODO -oTsusai -cCharacterServer :
									Add loop code for passing characters to client }
								end;
								CharaList.Free;
							end;
							//size is (24 + (character count * 106))
							PacketSize := (Ver + (Count * 106));
							WriteBufferWord(0,$006b,ReplyBuffer); //header
							WriteBufferWord(2,PacketSize,ReplyBuffer);
							SendBuffer(AThread,ReplyBuffer,PacketSize);
						end;
					end;
				end;
			end;
		end;
	end;
  FreeAndNil(ADatabase);
end; (* proc SendCharas
------------------------------------------------------------------------------*)

// SendCharaToMap - RaX - Stubbed for later use.
procedure SendCharaToMap();
begin

end;

(*------------------------------------------------------------------------------
CreateChara

Is called after creating a character in the client.
Creates and saves the character object

[2006/07/06] Tsusai - Started work on changing dummy procedure to real procedure
------------------------------------------------------------------------------*)
procedure CreateChara(AThread : TIdPeerThread; var ABuffer : TBuffer);
var
	CharaName  : string;
	StatPoints : array [0..5] of byte;
	HairStyle  : byte;
	HairColor  : byte;
	SlotNum    : byte;
	ACharacter : TCharacter;
	Account    : TAccount;
	Success    : boolean;
	ReplyBuffer: TBuffer;
	idx        : byte;
	TotalStatPt: byte;

	procedure CreateCharaError(const Error : byte);
	var
	ReplyBuf : TBuffer;
	begin
		WriteBufferWord(0, $006e, ReplyBuf);
		WriteBufferByte(2, Error, ReplyBuf);
		SendBuffer(AThread,ReplyBuf,3);
	end;

begin
	Account := TThreadLink(AThread.Data).AccountLink;
	CharaName := BufferReadString(2,24,ABuffer);
	SlotNum   := BufferReadByte(32,ABuffer);
	HairColor := BufferReadByte(33,ABuffer);
	HairStyle := BufferReadByte(35,ABuffer);

	TotalStatPt := 0;
	//Name Check
	SQLQueryResult :=
		SQLConnection.query(
		Format('SELECT name FROM `char` WHERE name = "%s"',[CharaName]),true,Success);
	if SQLQueryResult.RowsCount = 0 then
	begin
		//Stat Point check
		for idx := 0 to 5 do begin
			StatPoints[idx] := BufferReadByte(idx+26,ABuffer);
			if (StatPoints[idx] < 1) or (StatPoints[idx] > 9) then
			begin
				CreateCharaError(INVALIDMISC);
				exit;
			end else
			begin
				Inc(TotalStatPt,StatPoints[idx]);
			end;
		end;
		//Too many statpoints
		if TotalStatPt <> 30 then begin
			CreateCharaError(INVALIDMISC);
			exit;
		end;
		SQLQueryResult :=
			SQLConnection.query(
			Format('SELECT * FROM `char` WHERE char_num = %d',[SlotNum]),true,Success);
		if SQLQueryResult.RowsCount > 0 then
		begin
			CreateCharaError(INVALIDMISC);
			exit;
		end;
		//Validated...Procede with creation
		ACharacter := TCharacter.Create;
		//Set a record in SQL for our new character
		if ACharacter.CreateInSQL(Account.ID,CharaName) then
		begin
			ACharacter.Name := CharaName;
			ACharacter.Hair := HairStyle;
			ACharacter.HairColor := HairColor;
			ACharacter.CharaNum := SlotNum;
			ACharacter.ParamBase[STR] := StatPoints[0];
			ACharacter.ParamBase[AGI] := StatPoints[1];
			ACharacter.ParamBase[VIT] := StatPoints[2];
			ACharacter.ParamBase[INT] := StatPoints[3];
			ACharacter.ParamBase[DEX] := StatPoints[4];
			ACharacter.ParamBase[LUK] := StatPoints[5];
			//INSERT ANY OTHER CREATION CHANGES HERE!
			ACharacter.SaveToSQL;
			with ACharacter do begin
				WriteBufferWord(0, $006d,ReplyBuffer);
				WriteBufferCardinal(2+  0, CID,ReplyBuffer);
				WriteBufferCardinal(2+  4, BaseEXP,ReplyBuffer);
				WriteBufferCardinal(2+  8, Zeny,ReplyBuffer);
				WriteBufferCardinal(2+ 12, JobEXP,ReplyBuffer);
				WriteBufferCardinal(2+ 16, JobLV,ReplyBuffer);
				WriteBufferCardinal(2+ 20, 0,ReplyBuffer);
				WriteBufferCardinal(2+ 24, 0,ReplyBuffer);
				WriteBufferCardinal(2+ 28, Option,ReplyBuffer);
				WriteBufferCardinal(2+ 32, Karma,ReplyBuffer);
				WriteBufferCardinal(2+ 36, Manner,ReplyBuffer);
				WriteBufferWord(2+ 40, StatusPts,ReplyBuffer);
				WriteBufferWord(2+ 42, HP,ReplyBuffer);
				WriteBufferWord(2+ 44, MAXHP,ReplyBuffer);
				WriteBufferWord(2+ 46, SP,ReplyBuffer);
				WriteBufferWord(2+ 48, MAXSP,ReplyBuffer);
				WriteBufferWord(2+ 50, Speed,ReplyBuffer);
				WriteBufferWord(2+ 52, JID,ReplyBuffer);
				WriteBufferWord(2+ 54, Hair,ReplyBuffer);
				WriteBufferWord(2+ 56, Weapon,ReplyBuffer);
				WriteBufferWord(2+ 58, BaseLV,ReplyBuffer);
				WriteBufferWord(2+ 60, SkillPts,ReplyBuffer);
				WriteBufferWord(2+ 62, HeadBottom,ReplyBuffer);
				WriteBufferWord(2+ 64, Shield,ReplyBuffer);
				WriteBufferWord(2+ 66, HeadTop,ReplyBuffer);
				WriteBufferWord(2+ 68, HeadMid,ReplyBuffer);
				WriteBufferWord(2+ 70, HairColor,ReplyBuffer);
				WriteBufferWord(2+ 72, ClothesColor,ReplyBuffer);
				WriteBufferString(2+ 74, Name, 24,ReplyBuffer);
				for idx := STR to LUK do
				begin
					WriteBufferByte(2+98+idx, ParamBase[idx],ReplyBuffer);
				end;
				WriteBufferByte(2+104, CharaNum,ReplyBuffer);
				WriteBufferByte(2+105, 0,ReplyBuffer);
			end;
			AThread.Connection.WriteBuffer(ReplyBuffer, 108);
		end;
	end else
	begin
		CreateCharaError(INVALIDNAME);
	end;
end;

procedure DeleteChara(AThread : TIdPeerThread; var ABuffer : Tbuffer);
var
	CharacterID : Cardinal;
	EmailOrID   : string;
	AnAccount   : TAccount;
	ACharacter  : TCharacter;
	ReplyBuffer : TBuffer;

	procedure DeleteCharaError(const Error : byte);
	begin
		WriteBufferWord(0, $0070, ReplyBuffer);
		WriteBufferByte(2, Error, ReplyBuffer);
		AThread.Connection.WriteBuffer(ReplyBuffer,3);
	end;

begin
	CharacterID := BufferReadCardinal(2,ABuffer);
	EmailOrID := BufferReadString(6,40,ABuffer);
	AnAccount := TThreadLink(AThread.Data).AccountLink;
	ACharacter := GetCharacters(CharacterID);
	if (AnAccount.EMail = EmailOrID) and (ACharacter.Account = AnAccount) then
	begin
		if Assigned(ACharacter) then
		begin
			if CharacterList.IndexOf(CharacterID) > -1 then
			begin
				if TCharacter(CharacterList.IndexOfObject(CharacterID)).CID = ACharacter.CID then
				begin
					if ACharacter.RemoveFromSQL then
					begin
						CharacterList.Delete(CharacterList.IndexOf(CharacterID));
						AnAccount.CharaID[ACharacter.CharaNum] := 0;
						WriteBufferWord(0, $006f, ReplyBuffer);
						AThread.Connection.WriteBuffer(ReplyBuffer, 2);
					end else DeleteCharaError(DELETEBADCHAR);
				end else DeleteCharaError(DELETEBADCHAR);
			end else DeleteCharaError(DELETEBADCHAR);
		end else DeleteCharaError(DELETEBADCHAR);
	end else DeleteCharaError(DELETEBADEMAIL);
end;

(*------------------------------------------------------------------------------
ParseCharaServ

Root procedure to handling client connections to the Character Server.
 Incoming connections do not have a valid TThreadLink.AccountLink, so
 we check for that, and then assign as needed.  Keeps the various checks to a
 minimum.

[2006/07/06] Tsusai - Added TThreadLink check for security
------------------------------------------------------------------------------*)
{ TODO -oTsusai -cCharacterServer :
	Check against Prometheus Indy CharaServ prototype.  Must validate usage of
	ReadFromStack and the looping of the parser }
procedure ParseCharaServ(AThread : TIdPeerThread);
var
	PacketLength  : Integer;
	ABuffer       : TBuffer;
	PacketID      : Word;
	Link          : TThreadLink;
begin
	if AThread.Connection.Connected then
	begin
		AThread.Connection.ReadFromStack(false,-1,false);
		if AThread.Connection.InputBuffer.Size >= 2 then
		begin
			PacketLength := AThread.Connection.InputBuffer.Size;
			AThread.Connection.ReadBuffer(ABuffer,PacketLength);
			PacketID := BufferReadWord(0, ABuffer);
			if (AThread.Data = nil) or not (AThread.Data is TThreadLink) then
			begin
				//Thread Data should have a TThreadLink object...if not, make one
				Link := TThreadLink.Create;
				AThread.Data := Link;
			end;
			//First time connection from login needs to do 0x0065.  No exceptions.
			if TThreadLink(AThread.Data).AccountLink = nil then
			begin
				if PacketID = $0065 then
				begin
					//Verify login and send characters
					SendCharas(AThread,ABuffer);
				end;
			end else
			begin
				case PacketID of
				$0066: // Character Selected -- Refer Client to Map Server
					begin
						SendCharaToMap();
					end;
				$0067: // Create New Character
					begin
						CreateChara(AThread,ABuffer);
					end;
				$0068: // Request to Delete Character
					begin
						DeleteChara(AThread,ABuffer);
					end;
				end;
			end;
		end;
	end;
end; (*Proc ParseCharaServ
------------------------------------------------------------------------------*)

end.




