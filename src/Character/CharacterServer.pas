//------------------------------------------------------------------------------
//CharacterServer			                                                UNIT
//------------------------------------------------------------------------------
//	What it does-
//      The Character Server Class.
//    An object type that contains all information about a character server
//    This unit is to help for future design of multi server communication IF
//    the user were to want to do so.  Else, it would act as an all-in-one.
//	  Only one type in this unit for the time being.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit CharacterServer;

interface
uses
  IdTCPServer,
  IdContext,
	CommClient,
	SysUtils,
  PacketTypes,
  Database;
type
//------------------------------------------------------------------------------
//TCharacterServer                                                        CLASS
//------------------------------------------------------------------------------
	TCharacterServer = class
  protected
  //
	private
		fWANPort           : Word;
    TCPServer       : TIdTCPServer;
		CharaToLoginClient : TInterClient;

    ACommonDatabase : TDatabase;
    AGameDatabase   : TDatabase;

    Procedure OnException(AConnection: TIdContext;
      AException: Exception);

		procedure LoginClientOnConnect(Sender : TObject);
		procedure LoginClientRead(AClient : TInterClient);

    procedure ParseCharaServ(AClient : TIdContext);
    procedure SendCharas(AClient : TIdContext; var ABuffer : TBuffer);
    procedure SendCharaToMap();
    procedure CreateChara(AClient : TIdContext; var ABuffer : TBuffer);
    procedure DeleteChara(AClient : TIdContext; var ABuffer : Tbuffer);

    Procedure SetPort(Value : Word);
    Function GetStarted() : Boolean;
	public
		ServerName    : String;
		OnlineUsers   : Word;
		WANIP : string;
		LANIP : string;
		property WANPort : Word read fWANPort write SetPort;
    property Started : Boolean read GetStarted;
		Constructor Create();
    Destructor  Destroy();Override;
    Procedure   Start();
    Procedure   Stop();
	end;
//------------------------------------------------------------------------------


implementation
uses
	//Helios
	CharaLoginPackets,
	BufferIO,
	Character,
	Account,
	GameConstants,
	Globals,
	TCPServerRoutines,
	//3rd
	StrUtils,
	CharaList,
	Console;

const
	INVALIDNAME = 0;
	INVALIDMISC = 2;
	DELETEBADCHAR = 0;
	DELETEBADEMAIL = 1;

//------------------------------------------------------------------------------
//Create  ()                                                        CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our character server
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Constructor TCharacterServer.Create;
begin
  ACommonDatabase := TDatabase.Create(FALSE);
  AGameDatabase   := TDatabase.Create(TRUE);

  TCPServer := TIdTCPServer.Create;
	TCPServer.OnExecute   := ParseCharaServ;
	TCPServer.OnException := OnException;

	CharaToLoginClient := TInterClient.Create;

	CharaToLoginClient.OnConnected := LoginClientOnConnect;
	CharaToLoginClient.OnRecieve := LoginClientRead;

end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy()                                                        DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys our character server
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Destructor TCharacterServer.Destroy;
begin
  ACommonDatabase.Free;
  AGameDatabase.Free;

  TCPServer.Free;
	CharaToLoginClient.Free;
end;{Destroy}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//OnException                                                             EVENT
//------------------------------------------------------------------------------
//	What it does-
//			Handles Socket exceptions gracefully by outputting the exception message
//    and then disconnecting the client.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacterServer.OnException(AConnection: TIdContext;
	AException: Exception);
begin
	if AnsiContainsStr(AException.Message, IntToStr(10053)) or
		AnsiContainsStr(AException.Message, IntToStr(10054))
	then begin
		AConnection.Connection.Disconnect;
	end;
end;{OnException}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Start()                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		 Enables the character server to accept incoming connections
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TCharacterServer.Start();
begin
	TCPServer.DefaultPort := ServerConfig.CharaPort;
	ActivateServer('Character',TCPServer);
	WANIP := ServerConfig.CharaWANIP;
	LANIP := ServerConfig.CharaLANIP;
	CharaToLoginClient.Host := ServerConfig.LoginComIP;
	CharaToLoginClient.Port := ServerConfig.LoginComPort;
	ActivateClient(CharaToLoginClient);
end;{Start}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Stop()                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		 Stops the character server from accepting incoming connections
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TCharacterServer.Stop();
begin
	DeActivateServer('Character',TCPServer);
	DeActivateClient(CharaToLoginClient);
end;{Start}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoginClientOnConnect()                                                 EVENT
//------------------------------------------------------------------------------
//	What it does-
//		 Executed on connection to the login server.
//
//	Changes -
//		January 4th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacterServer.LoginClientOnConnect(Sender : TObject);
begin
	ValidateWithLoginServer(CharaToLoginClient,Self);
end;{LoginClientOnConnect}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoginClientRead()                                                   PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		 Reads information sent from the login server.
//
//	Changes -
//		January 3rd, 2006 - Tsusai - Added console messages.
//		January 4th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacterServer.LoginClientRead(AClient : TInterClient);
var
	ABuffer : TBuffer;
	PacketID : Word;
	Response : Byte;
begin
	RecvBuffer(AClient,ABuffer,2);
	PacketID := BufferReadWord(0,ABuffer);
	case PacketID of
	$2001:
		begin
			RecvBuffer(AClient,ABuffer[2],GetPacketLength($2001)-2);
			Response := BufferReadByte(2,ABuffer);
			if Boolean(Response) then
			begin
				MainProc.Console('Character Server: Verified with Login Server, '+
					'sending details.');
				SendWANIP(CharaToLoginClient,Self);
				SendLANIP(CharaToLoginClient,Self);
				SendOnlineUsers(CharaToLoginClient,Self);
			end else
			begin
				MainProc.Console('Character Server: Failed to verify with Login Server.');
				MainProc.Console('Character Server: Stopping...');
				Stop;
			end;
		end;
	end;
end;{LoginClientRead}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendCharas			                                                    PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      Verifies the new connection by checking the account ID which is recieved
//    and the two random keys generated during login. Upon validation, check
//    database for any/all created characters.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//    July 6th, 2006 - Tsusai - Started work on changing dummy procedure to real
//      procedure.
//		January 3rd, 2006 - Tsusai - Added console messages.
//
//------------------------------------------------------------------------------
procedure TCharacterServer.SendCharas(AClient : TIdContext; var ABuffer : TBuffer);
var
	AccountID   : Cardinal;
	AnAccount   : TAccount;
	ReplyBuffer : TBuffer;
	ACharacter  : TCharacter;
	Index       : integer;
	Count       : Byte;
	PacketSize  : Word;
	Ver         : Byte;
  ACharaList  : TCharacterList;
begin
	Count     := 0;
	Ver       := 24;
	AccountID := BufferReadCardinal(2, ABuffer);
	AnAccount := ACommonDatabase.AnInterface.GetAccount(AccountID);

	if Assigned(AnAccount) then
	begin
		if AnAccount.ID = AccountID then
		begin
			if  (AnAccount.LoginKey[1] = BufferReadCardinal(6,  ABuffer)) and
				(AnAccount.LoginKey[2] = BufferReadCardinal(10, ABuffer)) then
			begin
				//LINK the account to the client connection for the other procedures
				TThreadLink(AClient.Data).AccountLink := AnAccount;
				SendPadding(AClient); //Legacy padding

				ACharaList := AGameDatabase.AnInterface.GetAccountCharas(AccountID);
				for Index := 0 to ACharaList.Count-1 do
				begin
					ACharacter := ACharaList.Items[Index];
          ACharacter.Account := AnAccount;
          AnAccount.CharaID[ACharacter.CharaNum] := ACharacter.CID;
					if not (ACharacter = NIL) then
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
				end;
        //size is (24 + (character count * 106))
				PacketSize := (Ver + (Count * 106));
				WriteBufferWord(0,$006b,ReplyBuffer); //header
				WriteBufferWord(2,PacketSize,ReplyBuffer);
				SendBuffer(AClient,ReplyBuffer,PacketSize);

				ACharaList.Free;
			end else
			begin
				WriteBufferWord(0, $0081, ReplyBuffer);
				WriteBufferByte(2, 01, ReplyBuffer);
				SendBuffer(AClient,ReplyBuffer,GetPacketLength($0081));

				MainProc.Console('Character Server: Connecting RO client from '+
					AClient.Connection.Socket.Binding.PeerIP + ' did not pass key validation.');
			end;
		end;
	end;
end; {SendCharas}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendCharaToMap	                                                    PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      Refers a character to whatever map server is handling the map that the
//    character is on.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header. - Stubbed for later use.
//
//------------------------------------------------------------------------------
procedure TCharacterServer.SendCharaToMap();
begin

end;{SendToMap}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//CreateChara			                                                    PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      Is called after creating a character in the client.
//    Creates and saves the character object
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//    July      6th, 2006 - Tsusai - Started work on changing dummy procedure to
//      real procedure.
//
//------------------------------------------------------------------------------
procedure TCharacterServer.CreateChara(
	AClient : TIdContext;
	var ABuffer : TBuffer
);
var
	CharaName  : string;
	StatPoints : array [0..5] of byte;
	HairStyle  : byte;
	HairColor  : byte;
	SlotNum    : byte;
	ACharacter : TCharacter;
	Account    : TAccount;
	ReplyBuffer: TBuffer;
	idx        : byte;
	TotalStatPt: byte;
	Validated  : Boolean;

	procedure CreateCharaError(const Error : byte);
	var
	ReplyBuf : TBuffer;
	begin
		WriteBufferWord(0, $006e, ReplyBuf);
		WriteBufferByte(2, Error, ReplyBuf);
		SendBuffer(AClient,ReplyBuf,GetPacketLength($006e));
	end;

begin
	Validated   := TRUE; //Assume passes all checks.

	Account     := TThreadLink(AClient.Data).AccountLink;
	CharaName   := BufferReadString(2,24,ABuffer);
	SlotNum     := BufferReadByte(32,ABuffer);
	HairColor   := BufferReadByte(33,ABuffer);
	HairStyle   := BufferReadByte(35,ABuffer);

	TotalStatPt := 0;

	//Name Check.
	if NOT AGameDatabase.AnInterface.CharaExists(CharaName) then
	begin
		//Stat Point check.
		for idx := 0 to 5 do begin
			StatPoints[idx] := BufferReadByte(idx+26,ABuffer);
			if (StatPoints[idx] < 1) or (StatPoints[idx] > 9) then
			begin
				CreateCharaError(INVALIDMISC);
				Validated := FALSE;
			end else
			begin
				Inc(TotalStatPt,StatPoints[idx]);
			end;
		end;

		//Stat Point check.
		if TotalStatPt <> 30 then begin
			CreateCharaError(INVALIDMISC);
			Validated := FALSE;
		end;

		//Slot Check.
		if AGameDatabase.AnInterface.CharaExists(Account.ID, SlotNum) then
		begin
			CreateCharaError(INVALIDMISC);
			Validated := FALSE;
		end;

		//Did we pass all the checks?
		if Validated then
		begin
			//Validated...Procede with creation
			ACharacter := TCharacter.Create;
			//Set a record in Database for our new character
			if AGameDatabase.AnInterface.CreateChara(
				ACharacter,Account.ID,CharaName) then
			begin
				ACharacter.Name           := CharaName;
				ACharacter.CharaNum       := SlotNum;
        ACharacter.Account        := Account;
        if ACharacter.CharaNum < 9 then
			  begin
				  //If its active, then attach to the player.
				  ACharacter.Account.CharaID[ACharacter.CharaNum] := ACharacter.CID;
			  end;
        ACharacter.BaseLV         := 1;
        ACharacter.JobLV          := 1;
        ACharacter.JID            := 0;
        ACharacter.Speed          := 50;
        ACharacter.Zeny           := 0;
				ACharacter.ParamBase[STR] := StatPoints[0];
				ACharacter.ParamBase[AGI] := StatPoints[1];
				ACharacter.ParamBase[VIT] := StatPoints[2];
				ACharacter.ParamBase[INT] := StatPoints[3];
				ACharacter.ParamBase[DEX] := StatPoints[4];
				ACharacter.ParamBase[LUK] := StatPoints[5];
        ACharacter.MaxHP          := 0;//need to calculate HP/SP values here.
        ACharacter.MaxSP          := 0;
        ACharacter.HP             := 0;
        ACharacter.SP             := 0;
        ACharacter.StatusPts      := 0;
        ACharacter.SkillPts       := 0;
        ACharacter.Option         := 0;
        ACharacter.Karma          := 0;
        ACharacter.Manner         := 0;
        ACharacter.PartyID        := 0;
        ACharacter.GuildID        := 0;
        ACharacter.PetID          := 0;
        ACharacter.Hair           := HairStyle;
        ACharacter.HairColor      := HairColor;
        ACharacter.ClothesColor   := 0;
        ACharacter.Weapon         := 0;
        ACharacter.Shield         := 0;
        ACharacter.HeadTop        := 0;
        ACharacter.HeadMid        := 0;
        ACharacter.HeadBottom     := 0;
        ACharacter.Map            := 'prontera';
        //ACharacter.Point.X        := 156;
        //ACharacter.Point.Y        := 180;
        ACharacter.SaveMap        := 'prontera';
        //ACharacter.SavePoint.X    := 156;
        //ACharacter.SavePoint.Y    := 180;
        ACharacter.PartnerID      := 0;
        ACharacter.ParentID1      := 0;
        ACharacter.ParentID2      := 0;
        ACharacter.BabyID         := 0;
        ACharacter.Online         := 0;
        ACharacter.HomunID        := 0;

				//INSERT ANY OTHER CREATION CHANGES HERE!
				AGameDatabase.AnInterface.SaveChara(ACharacter);
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
				SendBuffer(AClient,ReplyBuffer,GetPacketLength($006d));
			end;
		end;
	end else
	begin
		CreateCharaError(INVALIDNAME);
	end;
end;{CreateChara}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//DeleteChara                                                      PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//        Delete's a character after client request and criteria are met
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacterServer.DeleteChara(
	AClient : TIdContext;
	var ABuffer : Tbuffer
);
var
	CharacterID : Cardinal;
	EmailOrID   : string;
	AnAccount   : TAccount;
	ACharacter  : TCharacter;
	ReplyBuffer : TBuffer;
  CharacterIndex : Integer;

	procedure DeleteCharaError(const Error : byte);
	begin
		WriteBufferWord(0, $0070, ReplyBuffer);
		WriteBufferByte(2, Error, ReplyBuffer);
		SendBuffer(AClient,ReplyBuffer,GetPacketLength($0070));
	end;

begin
	CharacterID := BufferReadCardinal(2,ABuffer);
	EmailOrID := BufferReadString(6,40,ABuffer);
	AnAccount := TThreadLink(AClient.Data).AccountLink;
	ACharacter := AGameDatabase.AnInterface.GetChara(CharacterID);

  if Assigned(ACharacter) then
  begin
    if ACharacter.Account = AnAccount then
    begin
      if AnAccount.EMail = EmailOrID then
	    begin
        CharacterIndex := CharacterList.IndexOf(CharacterID);
			  if CharacterIndex > -1 then
			  begin
				  if CharacterList.Items[CharacterIndex].CID = ACharacter.CID then
				  begin
					  if AGameDatabase.AnInterface.DeleteChara(ACharacter) then
					  begin
						  WriteBufferWord(0, $006f, ReplyBuffer);
						  SendBuffer(AClient,ReplyBuffer, GetPacketLength($006f));
					  end else DeleteCharaError(DELETEBADCHAR);
				  end else DeleteCharaError(DELETEBADCHAR);
			  end else DeleteCharaError(DELETEBADCHAR);
      end else DeleteCharaError(DELETEBADEMAIL);
    end else DeleteCharaError(DELETEBADCHAR);
  end else DeleteCharaError(DELETEBADCHAR);
end;{DeleteChara}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ParseCharaServ                                                      PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//        Root procedure to handling client connections to the Character Server.
//    Incoming connections do not have a valid TThreadLink.AccountLink, so
//    we check for that, and then assign as needed.  Keeps the various checks to
//    a minimum.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacterServer.ParseCharaServ(AClient : TIdContext);
var
	ABuffer       : TBuffer;
	PacketID      : Word;
begin
	if AClient.Connection.Connected then
	begin
		RecvBuffer(AClient,ABuffer,2);
			PacketID := BufferReadWord(0, ABuffer);
			if (AClient.Data = nil) or not (AClient.Data is TThreadLink) then
			begin
				//Thread Data should have a TThreadLink object...if not, make one
				AClient.Data := TThreadlink.Create;
			end;
			//First time connection from login needs to do 0x0065.  No exceptions.
			if TThreadLink(AClient.Data).AccountLink = nil then
			begin
				if PacketID = $0065 then
				begin
					//Verify login and send characters
				RecvBuffer(AClient,ABuffer[2],GetPacketLength($0065)-2);
					SendCharas(AClient,ABuffer);
				end;
			end else
			begin
				case PacketID of
				$0066: // Character Selected -- Refer Client to Map Server
					begin
						RecvBuffer(AClient,ABuffer[2],GetPacketLength($0066)-2);
						SendCharaToMap();
					end;
				$0067: // Create New Character
					begin
					RecvBuffer(AClient,ABuffer[2],GetPacketLength($0067)-2);
						CreateChara(AClient,ABuffer);
					end;
				$0068: // Request to Delete Character
					begin
					RecvBuffer(AClient,ABuffer[2],GetPacketLength($0068)-2);
						DeleteChara(AClient,ABuffer);
					end;
				end;
			end;
		end;
end; {ParseCharaServ}
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
//SetPort                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the internal fPort variable to the value specified. Also sets the
//    TCPServer's port.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TCharacterServer.SetPort(Value : Word);
begin
  fWANPort := Value;
  TCPServer.DefaultPort := Value;
end;{SetPort}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetStarted                                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Checks to see if the internal TCP server is active, if it is it returns
//    true.
//
//	Changes -
//		January 4th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Function TCharacterServer.GetStarted() : Boolean;
begin
  Result := TCPServer.Active;
end;{SetPort}
//------------------------------------------------------------------------------
end.
