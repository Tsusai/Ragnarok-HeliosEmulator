//------------------------------------------------------------------------------
//CharacterServer			                                    UNIT
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
	Character,
	IdTCPServer,
	IdContext,
	CommClient,
	List32,
	SysUtils,
	PacketTypes,
	CharaOptions;

type
//------------------------------------------------------------------------------
//TCharacterServer                                                        CLASS
//------------------------------------------------------------------------------
	TCharacterServer = class
	protected
	//
	private
		fWANPort           : Word;

		fZoneServerList : TIntList32;

		TCPServer       : TIdTCPServer;
		CharaToLoginClient : TInterClient;

		Procedure OnDisconnect(AConnection: TIdContext);
		Procedure OnException(AConnection: TIdContext;
			AException: Exception);

		procedure LoginClientOnConnect(Sender : TObject);
		procedure LoginClientRead(AClient : TInterClient);

		procedure ParseCharaServ(AClient : TIdContext);

		procedure VerifyZoneServer(
			AClient : TIdContext;
			InBuffer : TBuffer
		);

		function WriteCharacterDataToBuffer(
			ACharacter : TCharacter;
			var ReplyBuffer : TBuffer;
			const Offset : Integer
		) : Byte;

		procedure SendCharas(AClient : TIdContext; var ABuffer : TBuffer);
		procedure SendCharaToMap(AClient : TIdContext; var ABuffer : TBuffer);
		procedure CreateChara(AClient : TIdContext; var ABuffer : TBuffer);
		procedure DeleteChara(AClient : TIdContext; var ABuffer : Tbuffer);

		Procedure SetPort(Value : Word);
		Function GetStarted() : Boolean;
		Procedure LoadOptions;

	public
		ServerName    : String;
		OnlineUsers   : Word;
		WANIP : string;
		LANIP : string;

		Options : TCharaOptions;

		property WANPort : Word read fWANPort write SetPort;
		property Started : Boolean read GetStarted;
		Constructor Create();
		Destructor  Destroy();Override;
		Procedure   Start();
		Procedure   Stop();
    Procedure ConnectToLogin();
	end;
//------------------------------------------------------------------------------


implementation
uses
	//Helios
	CharaList,
	CharaLoginCommunication,
	ZoneCharaCommunication,
	BufferIO,
	Account,
	GameConstants,
	Globals,
	TCPServerRoutines,
	ZoneServerInfo,
	//3rd
  Types,
	StrUtils,
	Main;

const
	INVALIDNAME = 0;
	INVALIDMISC = 2;
	DELETEBADCHAR = 1;
	DELETEBADEMAIL = 0;

//------------------------------------------------------------------------------
//Create  ()                                                        CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our character server
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//		January 14th, 2007 - Tsusai - Updated CharaClient create, and initialized
//			the fZoneServerList
//
//------------------------------------------------------------------------------
Constructor TCharacterServer.Create;
begin
	TCPServer := TIdTCPServer.Create;
	TCPServer.OnExecute    := ParseCharaServ;
	TCPServer.OnException  := OnException;
	TCPServer.OnDisconnect := OnDisconnect;

	CharaToLoginClient := TInterClient.Create('Character','Login');

	CharaToLoginClient.OnConnected := LoginClientOnConnect;
	CharaToLoginClient.OnRecieve := LoginClientRead;

	fZoneServerList := TIntList32.Create;
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
//		January 14th, 2007 - Tsusai - Frees fZoneServerList
//
//------------------------------------------------------------------------------
Destructor TCharacterServer.Destroy;
begin
	TCPServer.Free;
	CharaToLoginClient.Free;

	fZoneServerList.Free;
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
  if NOT Started then
	begin
    LoadOptions;

    ServerName := Options.ServerName;
	  WANPort := Options.Port;

	  ActivateServer('Character',TCPServer);
	  WANIP := Options.WANIP;
	  LANIP := Options.LANIP;
  end else
  begin
		Console.Message('Cannot Start():: Character Server is already running!', 'Character Server', MS_ALERT);
	end;
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
var
  Index : Integer;
begin
  if Started then
  begin
	  DeActivateServer('Character',TCPServer);
	  DeActivateClient(CharaToLoginClient);

		//Free up our existing server info objects
    for Index := 0 to fZoneServerList.Count - 1 do
		begin
      TZoneServerInfo(fZoneServerList[Index]).Free;
      fZoneServerList.Delete(Index);
    end;

    Options.Save;
    Options.Free;
  end else
	begin
		Console.Message('Cannot Stop():: Character Server is not running', 'Character Server', MS_ALERT);
  end;
end;{Start}
//------------------------------------------------------------------------------


//Tsusai
//Write information about the character into the buffer
//Takes into account the ini flag to use clients from Dec 2006 and newer
function TCharacterServer.WriteCharacterDataToBuffer(
	ACharacter : TCharacter;
	var ReplyBuffer : TBuffer;
	const Offset : Integer
) : Byte;
begin
	if Options.Use108LengthForReply then
	begin
		Result := 108;
		FillChar(ReplyBuffer[Offset],Offset + Result,0);
		WriteBufferWord(Offset + 104, ACharacter.CharaNum,ReplyBuffer);
		WriteBufferWord(Offset + 106, 0,ReplyBuffer); //eA : Rename bit (?)
	end else
	begin
		Result := 106;
		FillChar(ReplyBuffer[Offset],Offset + Result,0);
		WriteBufferByte(Offset + 104, ACharacter.CharaNum,ReplyBuffer);
		WriteBufferByte(Offset + 106, 0,ReplyBuffer);
	end;
	with ACharacter do begin
		WriteBufferLongWord(Offset +  0, CID,ReplyBuffer);
		WriteBufferLongWord(Offset +  4, BaseEXP,ReplyBuffer);
		WriteBufferLongWord(Offset +  8, Zeny,ReplyBuffer);
		WriteBufferLongWord(Offset + 12, JobEXP,ReplyBuffer);
		WriteBufferLongWord(Offset + 16, JobLV,ReplyBuffer);
		WriteBufferLongWord(Offset + 20, 0,ReplyBuffer);
		WriteBufferLongWord(Offset + 24, 0,ReplyBuffer);
		WriteBufferLongWord(Offset + 28, Option,ReplyBuffer);
		WriteBufferLongWord(Offset + 32, Karma,ReplyBuffer);
		WriteBufferLongWord(Offset + 36, Manner,ReplyBuffer);
		WriteBufferWord(Offset + 40, StatusPts,ReplyBuffer);
		WriteBufferWord(Offset + 42, HP,ReplyBuffer);
		WriteBufferWord(Offset + 44, MAXHP,ReplyBuffer);
		WriteBufferWord(Offset + 46, SP,ReplyBuffer);
		WriteBufferWord(Offset + 48, MAXSP,ReplyBuffer);
		WriteBufferWord(Offset + 50, Speed,ReplyBuffer);
		WriteBufferWord(Offset + 52, JID,ReplyBuffer);
		WriteBufferWord(Offset + 54, Hair,ReplyBuffer);
		WriteBufferWord(Offset + 56, RightHand,ReplyBuffer);
		WriteBufferWord(Offset + 58, BaseLV,ReplyBuffer);
		WriteBufferWord(Offset + 60, SkillPts,ReplyBuffer);
		WriteBufferWord(Offset + 62, HeadBottom,ReplyBuffer);
		WriteBufferWord(Offset + 64, LeftHand,ReplyBuffer);
		WriteBufferWord(Offset + 66, HeadTop,ReplyBuffer);
		WriteBufferWord(Offset + 68, HeadMid,ReplyBuffer);
		WriteBufferWord(Offset + 70, HairColor,ReplyBuffer);
		WriteBufferWord(Offset + 72, ClothesColor,ReplyBuffer);
		WriteBufferString(Offset + 74, Name, 24,ReplyBuffer);

		WriteBufferByte(Offset + 98,  ParamBase[STR],ReplyBuffer);
		WriteBufferByte(Offset + 99,  ParamBase[AGI],ReplyBuffer);
		WriteBufferByte(Offset + 100, ParamBase[VIT],ReplyBuffer);
		WriteBufferByte(Offset + 101, ParamBase[INT],ReplyBuffer);
		WriteBufferByte(Offset + 102, ParamBase[DEX],ReplyBuffer);
		WriteBufferByte(Offset + 103, ParamBase[LUK],ReplyBuffer);
	end;
end;


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
//		January 3rd, 2007 - Tsusai - Added console messages.
//		January 4th, 2007 - RaX - Created Header.
//		January 14th, 2007 - Tsusai - Updated procedure calls.
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
				Console.Message('Verified with Login Server, '+
					'sending details.', 'Character Server', MS_NOTICE);
				SendCharaWANIPToLogin(CharaToLoginClient,Self);
				SendCharaLANIPToLogin(CharaToLoginClient,Self);
				SendCharaOnlineUsersToLogin(CharaToLoginClient,Self);
			end else
			begin
				Console.Message('Failed to verify with Login Server. Invalid Security Key', 'Character Server', MS_ERROR);
				Console.Message('Stopping...', 'Character Server', MS_NOTICE);
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
//      Verifies the new connection by checking the account ID which is received
//    and the two random keys generated during login. Upon validation, check
//    database for any/all created characters. If found any, send to client.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//		July 6th, 2006 - Tsusai - Started work on changing dummy procedure to real
//      	procedure.
//		January 3rd, 2007 - Tsusai - Added console messages.
//		January 20th, 2007 - Tsusai - Wrapped the console messages, now using
//			IdContext.Binding shortcut
//		March 12th, 2007 - Aeomin - Modificaion Header.
//
//------------------------------------------------------------------------------
procedure TCharacterServer.SendCharas(AClient : TIdContext; var ABuffer : TBuffer);
var
	AccountID   : LongWord;
	AnAccount   : TAccount;
	ReplyBuffer : TBuffer;
	ACharacter  : TCharacter;
	Index       : integer;
	Count       : Byte;
	PacketSize  : Word;
	Ver         : Byte;
	ACharaList  : TCharacterList;
	BaseIndex   : Integer;
	CharacterDataSize : integer;

begin
	Count     := 0;
	Ver       := 24;

	if Options.Use108LengthForReply then
	begin
		CharacterDataSize := 108;
	end else
	begin
		CharacterDataSize := 106;
	end;

	AccountID := BufferReadLongWord(2, ABuffer);
	AnAccount := ADatabase.CommonData.GetAccount(AccountID);

	if Assigned(AnAccount) then
	begin
		if AnAccount.ID = AccountID then
		begin
			if  (AnAccount.LoginKey[1] = BufferReadLongWord(6,  ABuffer)) and
				(AnAccount.LoginKey[2] = BufferReadLongWord(10, ABuffer)) then
			begin
				//LINK the account to the client connection for the other procedures
				if Assigned(TThreadLink(AClient.Data).AccountLink) then
				begin
					TThreadLink(AClient.Data).AccountLink.Free;
				end;

				TThreadLink(AClient.Data).AccountLink := AnAccount;
				SendPadding(AClient); //Legacy padding

				ACharaList := ADatabase.GameData.GetAccountCharas(AccountID);
				for Index := ACharaList.Count-1 downto 0 do
				begin
					ACharacter := ACharaList.Items[Index];
					AnAccount.CharaID[ACharacter.CharaNum] := ACharacter.CID;
					with ACharacter do
					begin
						BaseIndex := Ver+(Count*CharacterDataSize);
						WriteCharacterDataToBuffer(ACharacter,ReplyBuffer,BaseIndex);
						Inc(Count);
					end;
					ACharacter.Free;
					ACharaList.Delete(Index);
				end;
				ACharaList.Free;
				//size is (24 + (character count * Character data size))
				PacketSize := (Ver + (Count * CharacterDataSize));
				WriteBufferWord(0,$006b,ReplyBuffer); //header
				WriteBufferWord(2,PacketSize,ReplyBuffer);
				SendBuffer(AClient,ReplyBuffer,PacketSize);
			end else
			begin
				WriteBufferWord(0, $0081, ReplyBuffer);
				WriteBufferByte(2, 01, ReplyBuffer);
				SendBuffer(AClient,ReplyBuffer,GetPacketLength($0081));

				Console.Message(
					'Connecting RO client from '+
					AClient.Binding.PeerIP +
					' did not pass key validation.', 'Character Server', MS_WARNING
				);
			end;
		end;
	end;

	//make sure that if something goes horribly wrong we free the account anyways.
	if TThreadLink(AClient.Data).AccountLink <> AnAccount then
	begin
		AnAccount.Free;
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
//		January 20th, 2007 - Tsusai - Now using IdContext.Binding shortcut,
//			Removed extra comma inthe ZserverAddress procedure (Kylix caught this)
//
//------------------------------------------------------------------------------
procedure TCharacterServer.SendCharaToMap(
	AClient : TIdContext;
	var ABuffer : TBuffer
);
var
	AnAccount : TAccount;
	CharaIdx : byte;
	ACharacter : TCharacter;
	OutBuffer : TBuffer;
	ZServerInfo : TZoneServerInfo;
	ZoneID : byte;
	idx : integer;

begin
	if not (AClient.Data is TThreadLink) then exit;
	AnAccount := TThreadLink(AClient.Data).AccountLink;

	//Tsusai: Possible Check for online characters here...but
	//they should be terminated when logging in.

	CharaIdx := BufferReadByte(2, ABuffer);
	if AnAccount.CharaID[CharaIdx] <> 0 then
	begin
		ACharacter := ADatabase.GameData.GetChara(AnAccount.CharaID[CharaIdx],true);
		//ACharacter.ClientVersion := -1; //Need to either save, or make sure its cleared
																			//later on

		if ADatabase.StaticData.GetMapCannotSave(ACharacter.Map) then
		begin
			ACharacter.Map := ACharacter.SaveMap;
			ACharacter.Position := ACharacter.SavePoint;
		end;

		ADatabase.GameData.SaveChara(ACharacter);
		//get zone ID for the map.
		ZoneID := ADatabase.StaticData.GetMapZoneID(ACharacter.Map);
		//get the zone info from that

		idx := fZoneServerList.IndexOf(ZoneID);
		if idx > -1 then
		begin
			ZServerInfo := TZoneServerInfo(fZoneServerList.Objects[idx]);

			WriteBufferWord(0, $0071, OutBuffer);
			WriteBufferLongWord(2, ACharacter.CID, OutBuffer);
			WriteBufferString(6, ACharacter.Map + '.rsw', 16, OutBuffer);
			WriteBufferLongWord(22,
				ZServerInfo.Address(AClient.Binding.PeerIP
				),
				OutBuffer
			);
			WriteBufferWord(26, ZServerInfo.Port, OutBuffer);
			SendBuffer(AClient, OutBuffer, GetPacketLength($0071));
		end else
		begin
			//Server offline error goes here
			WriteBufferWord(0, $0081, Outbuffer);
			WriteBufferByte(2, 03, OutBuffer);
			SendBuffer(AClient, OutBuffer, GetPacketLength($0081));
		end;
		ACharacter.Free;
	end;

end;{SendCharaToMap}
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
//		January 12th, 2007 - Tsusai - Fixed Stat point display.
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
	Size : integer;

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
	if NOT ADatabase.GameData.CharaExists(CharaName) then
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
		if ADatabase.GameData.CharaExists(Account.ID, SlotNum) then
		begin
			CreateCharaError(INVALIDMISC);
			Validated := FALSE;
		end;

		//Did we pass all the checks?
		if Validated then
		begin
			//Validated...Procede with creation
			//Set a record in Database for our new character
			if ADatabase.GameData.CreateChara(
				ACharacter,Account.ID,CharaName,SlotNum) then
			begin
				//All other info is already saved
				ACharacter.BaseLV         := 1;
				ACharacter.JobLV          := 1;
				ACharacter.JID            := 0;
				ACharacter.Zeny           := Options.DefaultZeny;
				ACharacter.ParamBase[STR] := StatPoints[0];
				ACharacter.ParamBase[AGI] := StatPoints[1];
				ACharacter.ParamBase[VIT] := StatPoints[2];
				ACharacter.ParamBase[INT] := StatPoints[3];
				ACharacter.ParamBase[DEX] := StatPoints[4];
				ACharacter.ParamBase[LUK] := StatPoints[5];
				ACharacter.CalcMaxHP;
				ACharacter.CalcMaxSP;
				ACharacter.CalcSpeed;
				ACharacter.CalcMaxWeight;
				ACharacter.HP             := ACharacter.MaxHP;
				ACharacter.SP             := ACharacter.MaxSP;
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

				ACharacter.RightHand      := Options.DefaultRightHand;
				ACharacter.LeftHand       := Options.DefaultLeftHand;
				ACharacter.Armor          := Options.DefaultArmor;
				ACharacter.Garment        := Options.DefaultGarment;
				ACharacter.Shoes          := Options.DefaultShoes;
				ACharacter.Accessory1     := Options.DefaultAccessory1;
				ACharacter.Accessory2     := Options.DefaultAccessory2;
				ACharacter.HeadTop        := Options.DefaultHeadTop;
				ACharacter.HeadMid        := Options.DefaultHeadMid;
				ACharacter.HeadBottom     := Options.DefaultHeadLow;
				ACharacter.Map            := Options.DefaultMap;
				ACharacter.Position       := Point(Options.DefaultPoint.X,Options.DefaultPoint.Y);
				ACharacter.SaveMap        := Options.DefaultMap;
				ACharacter.SavePoint      := Point(Options.DefaultPoint.X,Options.DefaultPoint.Y);
				ACharacter.PartnerID      := 0;
				ACharacter.ParentID1      := 0;
				ACharacter.ParentID2      := 0;
				ACharacter.BabyID         := 0;
				ACharacter.Online         := 0;
				ACharacter.HomunID        := 0;

				//INSERT ANY OTHER CREATION CHANGES HERE!
				ADatabase.GameData.SaveChara(ACharacter);
				Account.CharaID[ACharacter.CharaNum] := ACharacter.CID;
				WriteBufferWord(0, $006d,ReplyBuffer);
				Size := WriteCharacterDataToBuffer(ACharacter,ReplyBuffer,2);
				SendBuffer(AClient,ReplyBuffer,Size+2);
				ACharacter.Free;
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
	CharacterID : LongWord;
	EmailOrID   : string;
	AnAccount   : TAccount;
	ACharacter  : TCharacter;
	ReplyBuffer : TBuffer;

	procedure DeleteCharaError(const Error : byte);
	begin
		WriteBufferWord(0, $0070, ReplyBuffer);
		WriteBufferByte(2, Error, ReplyBuffer);
		SendBuffer(AClient,ReplyBuffer,GetPacketLength($0070));
	end;

begin
	CharacterID := BufferReadLongWord(2,ABuffer);
	EmailOrID := BufferReadString(6,40,ABuffer);
	AnAccount := TThreadLink(AClient.Data).AccountLink;
	ACharacter := ADatabase.GameData.GetChara(CharacterID,true);
	if Assigned(ACharacter) then
	begin
		if AnAccount.EMail = EmailOrID then
		begin
			if ADatabase.GameData.DeleteChara(ACharacter) then
			begin
				WriteBufferWord(0, $006f, ReplyBuffer);
				SendBuffer(AClient,ReplyBuffer, GetPacketLength($006f));
			end else DeleteCharaError(DELETEBADCHAR);
		end else DeleteCharaError(DELETEBADEMAIL);
	end else DeleteCharaError(DELETEBADCHAR);
	ACharacter.Free;
end;{DeleteChara}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//VerifyZoneServer                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Verify request connection from zone server
//
//	Changes -
//		January 14th, 2007 - Tsusai -  Added
//		January 20th, 2007 - Tsusai - Wrapped the console messages, now using
//			IdContext.Binding shortcut
//		March 12th, 2007 - Aeomin - Fix the header.
//
//------------------------------------------------------------------------------
procedure TCharacterServer.VerifyZoneServer(
	AClient : TIdContext;
	InBuffer : TBuffer
);
var
	Password : string;
	Validated : byte;
	ZServerInfo : TZoneServerInfo;
	ID : LongWord;
begin
	Validated := 0; //Assume true
	Console.Message(
		'Reading Zone Server connection from ' +
		AClient.Binding.PeerIP, 'Character Server', MS_NOTICE
	);
	ID := BufferReadLongWord(2,InBuffer);
	Password := BufferReadMD5(8,InBuffer);

	if (fZoneServerList.IndexOf(ID) > -1) then
	begin
		Console.Message('Zone Server failed verification. ID already in use.', 'Character Server', MS_WARNING);
		Validated := 1;
	end;

	if (Password <> GetMD5(Options.Key)) then
	begin
		Console.Message('Zone Server failed verification. Invalid Security Key.', 'Character Server', MS_WARNING);
		Validated := 2;
	end;

	if Validated = 0 then
	begin
		Console.Message('Zone Server connection validated.','Character Server', MS_INFO);

		ZServerInfo :=  TZoneServerInfo.Create;
		ZServerInfo.ZoneID := ID;
		ZServerInfo.Port := BufferReadWord(6,InBuffer);
		AClient.Data := TZoneServerLink.Create;
		TZoneServerLink(AClient.Data).Info := ZServerInfo;
		fZoneServerList.AddObject(ZServerInfo.ZoneID,ZServerInfo);
	end;
	SendValidateFlagToZone(AClient,Validated);
end;
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
//		January 14th, 2007 - Tsusai - Added Zone server packet parsing.
//
//------------------------------------------------------------------------------
procedure TCharacterServer.ParseCharaServ(AClient : TIdContext);
var
	ABuffer       : TBuffer;
	PacketID      : Word;
	Size          : Word;
begin
	RecvBuffer(AClient,ABuffer,2);
	PacketID := BufferReadWord(0, ABuffer);
	//Check to see if it is an already connected Client
	if (AClient.Data is TThreadLink) then
	begin
		case PacketID of
		$0066: // Character Selected -- Refer Client to Map Server
			begin
				RecvBuffer(AClient,ABuffer[2],GetPacketLength($0066)-2);
				SendCharaToMap(AClient,ABuffer);
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
	end else
	begin
		case PacketID of
		$0065: // RO Client request to connect and get characters
			begin
				//Thread Data should have a TThreadLink object...if not, make one
				AClient.Data := TThreadlink.Create;
				//Verify login and send characters
				RecvBuffer(AClient,ABuffer[2],GetPacketLength($0065)-2);
				SendCharas(AClient,ABuffer);
			end;
		$2100: // Zone Server Connection request
			begin
				RecvBuffer(AClient,ABuffer[2],GetPacketLength($2100)-2);
				VerifyZoneServer(AClient,ABuffer);
			end;
		$2102: // Zone Server sending new WAN location details
			begin
				if AClient.Data is TZoneServerLink then
				begin
					RecvBuffer(AClient,ABuffer[2],2);
					Size := BufferReadWord(2,ABuffer);
					RecvBuffer(AClient,ABuffer[4],Size-4);
					TZoneServerLink(AClient.Data).Info.WAN := BufferReadString(4,Size-4,ABuffer);
					Console.Message('Received updated Zone Server WANIP.', 'Character Server', MS_NOTICE);
				end;
			end;
		$2103: // Zone Server sending new LAN location details
			begin
				if AClient.Data is TZoneServerLink then
				begin
					RecvBuffer(AClient,ABuffer[2],2);
					Size := BufferReadWord(2,ABuffer);
					RecvBuffer(AClient,ABuffer[4],Size-4);
					TZoneServerLink(AClient.Data).Info.LAN := BufferReadString(4,Size-4,ABuffer);
					Console.Message('Received updated Zone Server LANIP.', 'Character Server', MS_NOTICE);
				end;
			end;
		$2104: // Zone Server sending new Online User count
			begin
				if AClient.Data is TZoneServerLink then
				begin
					RecvBuffer(AClient,ABuffer[2],GetPacketLength($2104)-2);
					//TZoneServerLink(AClient.Data).Info.OnlineUsers := BufferReadWord(2,ABuffer);
					Console.Message('Received updated Zone Server Online Users.', 'Character Server', MS_NOTICE);
				end;
			end;
		else
			begin
				Console.Message('Unknown Character Server Packet : ' + IntToHex(PacketID,4), 'Character Server', MS_WARNING);
			end;
		end;
	end;
end; {ParseCharaServ}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ConnectToLogin                                                      PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Connects to the login server.
//
//	Changes -
//		January 25th, 2007 - RaX - Moved from Start().
//
//------------------------------------------------------------------------------
Procedure TCharacterServer.ConnectToLogin;
begin
  CharaToLoginClient.Host := Options.LoginIP;
  CharaToLoginClient.Port := Options.LoginPort;
  ActivateClient(CharaToLoginClient);
end;//ConnectToLogin
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadOptions                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Creates and Loads the inifile.
//
//	Changes -
//		January 4th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TCharacterServer.LoadOptions;
begin
	Options    := TCharaOptions.Create(MainProc.Options.ConfigDirectory+'/Character.ini');

	Options.Load;
end;{LoadOptions}
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


//------------------------------------------------------------------------------
//OnDisconnect()                                                         EVENT
//------------------------------------------------------------------------------
//	What it does-
//		  Executes when a client disconnects from the character server.
//		Removes disconnected zone servers from the list.
//
//	Changes -
//		January 10th, 2007 - Tsusai - Created Header.
//		March 12th, 2007 - Aeomin - Fix header (should be character
//						server not login server)
//
//------------------------------------------------------------------------------
Procedure TCharacterServer.OnDisconnect(AConnection: TIdContext);
var
	idx : integer;
	AZoneServInfo : TZoneServerInfo;
begin
	if AConnection.Data is TZoneServerLink then
	begin
		AZoneServInfo := TZoneServerLink(AConnection.Data).Info;
		idx := fZoneServerList.IndexOfObject(AZoneServInfo);
		if not (idx = -1) then
		begin
			fZoneServerList.Delete(idx);
			AZoneServInfo.Free;
		end;
	end else
	begin
		if Assigned(TThreadLink(AConnection.Data).AccountLink) then
		begin
			TThreadLink(AConnection.Data).AccountLink.Free;
    end;
  end;
end;{OnDisconnect}
//------------------------------------------------------------------------------

end.
