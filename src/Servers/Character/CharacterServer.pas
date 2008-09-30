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
//		June 28th, 2008 - Tsusai - Updated GetPacketLength to PacketDB.GetLength
//			in various calls
//
//------------------------------------------------------------------------------
unit CharacterServer;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
uses
	Character,
	IdContext,
	CommClient,
	List32,
	SysUtils,
	PacketTypes,
	CharaOptions,
	Server;

type
//------------------------------------------------------------------------------
//TCharacterServer                                                        CLASS
//------------------------------------------------------------------------------
	TCharacterServer = class(TServer)
	protected
		fZoneServerList : TIntList32;
		fAccountList    : TIntList32;

		CharaToLoginClient : TInterClient;
		Procedure OnConnect(AConnection: TIdContext);override;
		Procedure OnExecute(AConnection: TIdContext);override;
		Procedure OnDisconnect(AConnection: TIdContext);override;
		Procedure OnException(AConnection: TIdContext;
			AException: Exception);override;

		procedure LoginClientOnConnect(Sender : TObject);
		procedure LoginClientKickAccount(
		AClient : TInterClient;
		var InBuffer : TBuffer
		);
		procedure LoginClientRead(AClient : TInterClient);

		procedure VerifyZoneServer(
			AClient : TIdContext;
			InBuffer : TBuffer
		);

		procedure UpdateToAccountList(
			AClient : TIdContext;
			InBuffer : TBuffer
		);

		procedure RemoveFromAccountList(
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
		procedure RenameChara(AClient : TIdContext; var ABuffer : Tbuffer);

		Procedure LoadOptions;

	public
		ServerName    : String;

		Options : TCharaOptions;

		Constructor Create();
		Destructor  Destroy();Override;
		Procedure   Start();override;
		Procedure   Stop();override;
		Procedure ConnectToLogin();
		function GetOnlineUserCount : Word;
		procedure UpdateOnlineCountToZone;
	end;
//------------------------------------------------------------------------------


implementation
uses
	Math,
	//Helios
	CharaList,
	CharAccountInfo,
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
	inherited;

	CharaToLoginClient := TInterClient.Create('Character','Login', true, MainProc.Options.ReconnectDelay);

	CharaToLoginClient.OnConnected := LoginClientOnConnect;
	CharaToLoginClient.OnReceive := LoginClientRead;

	fZoneServerList := TIntList32.Create;
	fAccountList := TIntList32.Create;
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
	CharaToLoginClient.Free;

	fZoneServerList.Free;
	fAccountList.Free;
	inherited;
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
		Port := Options.Port;

		ActivateServer('Character',TCPServer, Options.IndySchedulerType, Options.IndyThreadPoolSize);
		WANIP := Options.WANIP;
		LANIP := Options.LANIP;
		inherited;
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
		inherited;
		DeActivateServer('Character',TCPServer);
		DeActivateClient(CharaToLoginClient);

		//Free up our existing server info objects
		for Index := fZoneServerList.Count - 1 downto 0 do
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
	Result := 108;
	FillChar(ReplyBuffer[Offset],Offset + Result,0);
	WriteBufferWord(Offset + 104, ACharacter.CharaNum,ReplyBuffer);
	WriteBufferWord(Offset + 106, 1,ReplyBuffer); //eA : Rename bit (?)
	with ACharacter do begin
		WriteBufferLongWord(Offset +  0, ID,ReplyBuffer);
		WriteBufferLongWord(Offset +  4, BaseEXP,ReplyBuffer);
		WriteBufferLongWord(Offset +  8, Zeny,ReplyBuffer);
		WriteBufferLongWord(Offset + 12, JobEXP,ReplyBuffer);
		WriteBufferLongWord(Offset + 16, JobLV,ReplyBuffer);
		WriteBufferLongWord(Offset + 20, 0,ReplyBuffer);
		WriteBufferLongWord(Offset + 24, 0,ReplyBuffer);
		WriteBufferLongWord(Offset + 28, Option,ReplyBuffer);
		WriteBufferLongWord(Offset + 32, Karma,ReplyBuffer);
		WriteBufferLongWord(Offset + 36, Manner,ReplyBuffer);
		WriteBufferWord(Offset + 40, EnsureRange(StatusPts, 0, High(SmallInt)),ReplyBuffer);
		WriteBufferWord(Offset + 42, EnsureRange(HP, 0, High(SmallInt)),ReplyBuffer);
		WriteBufferWord(Offset + 44, EnsureRange(MAXHP, 0, High(SmallInt)),ReplyBuffer);
		WriteBufferWord(Offset + 46, EnsureRange(SP, 0, High(SmallInt)),ReplyBuffer);
		WriteBufferWord(Offset + 48, EnsureRange(MAXSP, 0, High(SmallInt)),ReplyBuffer);
		WriteBufferWord(Offset + 50, Speed,ReplyBuffer);
		WriteBufferWord(Offset + 52, JID,ReplyBuffer);
		WriteBufferWord(Offset + 54, Hair,ReplyBuffer);
		WriteBufferWord(Offset + 56, RightHand,ReplyBuffer);
		WriteBufferWord(Offset + 58, BaseLV,ReplyBuffer);
		WriteBufferWord(Offset + 60, EnsureRange(SkillPts, 0, High(Word)),ReplyBuffer);
		WriteBufferWord(Offset + 62, HeadBottom,ReplyBuffer);
		WriteBufferWord(Offset + 64, LeftHand,ReplyBuffer);
		WriteBufferWord(Offset + 66, HeadTop,ReplyBuffer);
		WriteBufferWord(Offset + 68, HeadMid,ReplyBuffer);
		WriteBufferWord(Offset + 70, HairColor,ReplyBuffer);
		WriteBufferWord(Offset + 72, ClothesColor,ReplyBuffer);
		WriteBufferString(Offset + 74, Name, 24,ReplyBuffer);

		WriteBufferByte(Offset + 98,  EnsureRange(ParamBase[STR], 0, High(Byte)),ReplyBuffer);
		WriteBufferByte(Offset + 99,  EnsureRange(ParamBase[AGI], 0, High(Byte)),ReplyBuffer);
		WriteBufferByte(Offset + 100, EnsureRange(ParamBase[VIT], 0, High(Byte)),ReplyBuffer);
		WriteBufferByte(Offset + 101, EnsureRange(ParamBase[INT], 0, High(Byte)),ReplyBuffer);
		WriteBufferByte(Offset + 102, EnsureRange(ParamBase[DEX], 0, High(Byte)),ReplyBuffer);
		WriteBufferByte(Offset + 103, EnsureRange(ParamBase[LUK], 0, High(Byte)),ReplyBuffer);
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


procedure TCharacterServer.LoginClientKickAccount(
	AClient : TInterClient;
	var InBuffer : TBuffer
	);
var
	AccountID : LongWord;
	Idx       : Integer;
	ZoneIdx   : Integer;
	AccountInfo : TCharAccountInfo;
	OutBuffer : TBuffer;
begin
	AccountID := BufferReadLongWord(2, InBuffer);
	Idx := fAccountList.IndexOf(AccountID);
	if Idx > -1 then
	begin
		AccountInfo := fAccountList.Objects[Idx] as TCharAccountInfo;

		if AccountInfo.InGame then
		begin
			//If we can't find the zone, just forget about it
			//Most cause will be connection between zone and char server is dropped.
			ZoneIdx := fZoneServerList.IndexOf(AccountInfo.ZoneServerID);
			if ZoneIdx > -1 then
			begin
				SendKickAccountToZone(TZoneServerInfo(fZoneServerList.Objects[idx]).Connection, AccountInfo.CharacterID);
			end;
		end else begin
			//Show "Someone has already logged in with this ID" ?
			if Options.ShowFriendlyMessageOnDupLogin then
			begin
			FillChar(OutBuffer, PacketDB.GetLength($0081), 0);
			WriteBufferWord(0, $0081, OutBuffer);
			WriteBufferByte(2, 2, OutBuffer);
			SendBuffer(AccountInfo.ClientInfo, OutBuffer, PacketDB.GetLength($0081));
			end;
			AccountInfo.ClientInfo.Connection.Disconnect;
		end;
	end;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//OnConnect											                                          EVENT
//------------------------------------------------------------------------------
//	What it does-
//			Nothing
//
//	Changes -
//
//------------------------------------------------------------------------------
procedure TCharacterServer.OnConnect(AConnection: TIdContext);
begin

end;{OnConnect}
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
//		April 10th, 2007 - Aeomin - Updated to support Server ID.
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
			RecvBuffer(AClient,ABuffer[2],PacketDB.GetLength($2001)-2);
			Response := BufferReadByte(2,ABuffer);
			if Response = 0 then
			begin
				Console.Message('Verified with Login Server, '+
					'sending details.', 'Character Server', MS_NOTICE);
				SendCharaWANIPToLogin(CharaToLoginClient,Self);
				SendCharaLANIPToLogin(CharaToLoginClient,Self);
				SendCharaOnlineUsersToLogin(CharaToLoginClient,Self);
			end else
			begin
				case Response of
				1 : Console.Message('Failed to verify with Login Server. ID already in use.', 'Character Server', MS_WARNING);
				2 : Console.Message('Failed to verify with Login Server. Invalid security key.', 'Character Server', MS_WARNING);
				end;
				Console.Message('Stopping...', 'Zone Server', MS_NOTICE);
				Stop;
			end;
		end;
	$2007:
		begin
			RecvBuffer(AClient,ABuffer[2],PacketDB.GetLength($2007)-2);
			LoginClientKickAccount(AClient, ABuffer);
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
//		March 12th, 2007 - Aeomin - Modified Header.
//		April 12th, 2007 - Aeomin - Append check to disallow 0 of LoginKey
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
	Idx         : Integer;
	AccountInfo : TCharAccountInfo;
begin
	Count     := 0;
	Ver       := 24;

	CharacterDataSize := 108;

	AccountID := BufferReadLongWord(2, ABuffer);
	AnAccount := TAccount.Create(AClient);
	AnAccount.ID := AccountID;
	TThreadLink(AClient.Data).DatabaseLink.Account.Load(AnAccount);

	if AnAccount.ID = AccountID then
	begin
		if  (AnAccount.LoginKey[1] = BufferReadLongWord(6,  ABuffer)) and (AnAccount.LoginKey[1] > 0) and
			(AnAccount.LoginKey[2] = BufferReadLongWord(10, ABuffer)) and (AnAccount.LoginKey[2] > 0) then
		begin
			//LINK the account to the client connection for the other procedures
			TClientLink(AClient.Data).AccountLink := AnAccount;
			SendPadding(AClient); //Legacy padding
			ACharaList := TCharacterList.Create(TRUE);
			TThreadLink(AClient.Data).DatabaseLink.Character.LoadByAccount(ACharaList, AnAccount);

			Idx := fAccountList.IndexOf(AnAccount.ID);
			if Idx > -1 then
			begin
				AccountInfo := fAccountList.Objects[idx] as TCharAccountInfo;
				//Make sure no dup login via skip server.
				if (not AccountInfo.Transfering)or(AccountInfo.InGame) then
				begin   //Reject!
					WriteBufferWord(0, $006a, ReplyBuffer);
					WriteBufferByte(2, 03, ReplyBuffer);
					SendBuffer(AClient,ReplyBuffer,PacketDB.GetLength($006a));
				end else
				begin
					AccountInfo.ClientInfo := AClient;
					TClientLink(AClient.Data).AccountInfo := AccountInfo;
				end;
			end else
			begin
				AccountInfo := TCharAccountInfo.Create(AnAccount.ID);
				AccountInfo.ClientInfo := AClient;
				TClientLink(AClient.Data).AccountInfo := AccountInfo;
				AccountInfo.Transfering := True;
				fAccountList.AddObject(AnAccount.ID, AccountInfo);
				SendAccountLogon(CharaToLoginClient, AccountInfo, Self);
			end;

			//Ah..lets make sure again
			if (AccountInfo.Transfering)or(not AccountInfo.InGame) then
			begin
				for Index := ACharaList.Count-1 downto 0 do
				begin
					ACharacter := ACharaList.Items[Index];
					AnAccount.CharaID[ACharacter.CharaNum] := ACharacter.ID;
					with ACharacter do
					begin
						BaseIndex := Ver+(Count*CharacterDataSize);
						WriteCharacterDataToBuffer(ACharacter,ReplyBuffer,BaseIndex);
						Inc(Count);
					end;
					ACharaList.Delete(Index);
				end;
				//size is (24 + (character count * Character data size))
				PacketSize := (Ver + (Count * CharacterDataSize));
				WriteBufferWord(0,$006b,ReplyBuffer); //header
				WriteBufferWord(2,PacketSize,ReplyBuffer);
				SendBuffer(AClient,ReplyBuffer,PacketSize);
			end;
			ACharaList.Free;
			AccountInfo.Transfering := False;
		end else
		begin
			WriteBufferWord(0, $0081, ReplyBuffer);
			WriteBufferByte(2, 01, ReplyBuffer);
			SendBuffer(AClient,ReplyBuffer,PacketDB.GetLength($0081));

			Console.Message(
				'Connecting RO client from '+
				AClient.Binding.PeerIP +
				' did not pass key validation.', 'Character Server', MS_WARNING
			);
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
//		January 20th, 2007 - Tsusai - Now using IdContext.Binding shortcut,
//			Removed extra comma inthe ZserverAddress procedure (Kylix caught this)
// 		March 30th, 2007 - Aeomin - Changed server down Error Code from 3 to 1
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
	ZoneID : Integer;
	idx : integer;

begin
	if not (AClient.Data is TThreadLink) then exit;
	AnAccount := TClientLink(AClient.Data).AccountLink;

	//Tsusai: Possible Check for online characters here...but
	//they should be terminated when logging in.

	CharaIdx := BufferReadByte(2, ABuffer);
	if AnAccount.CharaID[CharaIdx] <> 0 then
	begin
			ACharacter := TCharacter.Create(AClient);
			ACharacter.ID := AnAccount.CharaID[CharaIdx];
			TThreadLink(AClient.Data).DatabaseLink.Character.Load(ACharacter);
			//ACharacter.ClientVersion := -1; //Need to either save, or make sure its cleared
																			//later on

			if TThreadLink(AClient.Data).DatabaseLink.Map.CantSave(ACharacter.Map) then
			begin
				ACharacter.Map := ACharacter.SaveMap;
				ACharacter.Position := ACharacter.SavePoint;
			end;

			//get zone ID for the map.
			ZoneID := TThreadLink(AClient.Data).DatabaseLink.Map.GetZoneID(ACharacter.Map);
			//get the zone info from that

			idx := fZoneServerList.IndexOf(ZoneID);
			if idx > -1 then
			begin
				TThreadLink(AClient.Data).DatabaseLink.Character.Save(ACharacter);


				TClientLink(AClient.Data).AccountInfo.CharacterID := ACharacter.ID;
				TClientLink(AClient.Data).Transfering := True;
				ZServerInfo := TZoneServerInfo(fZoneServerList.Objects[idx]);

				WriteBufferWord(0, $0071, OutBuffer);
				WriteBufferLongWord(2, ACharacter.ID, OutBuffer);
				WriteBufferString(6, ACharacter.Map + '.rsw', 16, OutBuffer);
				WriteBufferLongWord(22,
					ZServerInfo.Address(AClient.Binding.PeerIP
					),
					OutBuffer
				);
				WriteBufferWord(26, ZServerInfo.Port, OutBuffer);
				SendBuffer(AClient, OutBuffer, PacketDB.GetLength($0071));
			end else
			begin
				//Server offline error goes here
				WriteBufferWord(0, $0081, Outbuffer);
				WriteBufferByte(2, 01, OutBuffer);
				SendBuffer(AClient, OutBuffer, PacketDB.GetLength($0081));
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
//    September 29th 2008 - Tsusai - Corrected InventoryID spelling error.
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
		SendBuffer(AClient,ReplyBuf,PacketDB.GetLength($006e));
	end;

begin
	Validated   := TRUE; //Assume passes all checks.

	Account     := TClientLink(AClient.Data).AccountLink;
	CharaName   := BufferReadString(2,24,ABuffer);
	SlotNum     := BufferReadByte(32,ABuffer);
	HairColor   := BufferReadByte(33,ABuffer);
	HairStyle   := BufferReadByte(35,ABuffer);

	TotalStatPt := 0;
	//Name Check.
	ACharacter := TCharacter.Create(AClient);
	try
		ACharacter.Name := CharaName;
		if NOT TThreadLink(AClient.Data).DatabaseLink.Character.Exists(ACharacter) then
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
			ACharacter.CharaNum := SlotNum;
			if TThreadLink(AClient.Data).DatabaseLink.Character.Exists(ACharacter) then
			begin
				CreateCharaError(INVALIDMISC);
				Validated := FALSE;
			end;

			//Did we pass all the checks?
			if Validated then
			begin
				//Validated...Procede with creation
				//Set a record in Database for our new character
				ACharacter.AccountID := Account.ID;
				ACharacter.Inventory.InventoryID := TThreadLink(AClient.Data).DatabaseLink.Character.CreateInventory;
				TThreadLink(AClient.Data).DatabaseLink.Character.New(ACharacter);
				TThreadLink(AClient.Data).DatabaseLink.Character.Load(ACharacter);
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
				ACharacter.CalcASpeed;
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
				ACharacter.Hair           := EnsureRange(HairStyle,0,Options.MaxHairStyle);
				ACharacter.HairColor      := EnsureRange(HairColor,0,Options.MaxHairColor);
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
				TThreadLink(AClient.Data).DatabaseLink.Character.Save(ACharacter);
				Account.CharaID[ACharacter.CharaNum] := ACharacter.ID;
				WriteBufferWord(0, $006d,ReplyBuffer);
				Size := WriteCharacterDataToBuffer(ACharacter,ReplyBuffer,2);
				SendBuffer(AClient,ReplyBuffer,Size+2);
			end;
		end else
		begin
			CreateCharaError(INVALIDNAME);
		end;
  finally
		ACharacter.Free;
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
		SendBuffer(AClient,ReplyBuffer,PacketDB.GetLength($0070));
	end;

begin
	CharacterID := BufferReadLongWord(2,ABuffer);
	EmailOrID := BufferReadString(6,40,ABuffer);
	AnAccount := TClientLink(AClient.Data).AccountLink;
	ACharacter := TCharacter.Create(AClient);
	try
		ACharacter.ID := CharacterID;
		TThreadLink(AClient.Data).DatabaseLink.Character.Load(ACharacter);
		if AnAccount.EMail = EmailOrID then
		begin
			TThreadLink(AClient.Data).DatabaseLink.Character.Delete(ACharacter);
			WriteBufferWord(0, $006f, ReplyBuffer);
			SendBuffer(AClient,ReplyBuffer, PacketDB.GetLength($006f));
		end else
			DeleteCharaError(DELETEBADEMAIL);
	finally
		ACharacter.Free;
	end;
end;{DeleteChara}
//------------------------------------------------------------------------------

procedure TCharacterServer.RenameChara(AClient : TIdContext; var ABuffer : Tbuffer);
var
//	AccountID : LongWord;
//	CharacterID : LongWord;
//	NewName : String;
	ReplyBuffer : TBuffer;
begin
	//No rename support yet XD
	WriteBufferWord(0, $028e, ReplyBuffer);
	WriteBufferWord(2, 0, ReplyBuffer);
	SendBuffer(AClient,ReplyBuffer, 4);
//	AccountID :=  BufferReadLongWord(2,ABuffer);
//	CharacterID := BufferReadLongWord(6,ABuffer);
//	NewName := BufferReadString(10,24,ABuffer);

//	TThreadLink(AClient.Data).DatabaseLink.Character.Rename(
//		AccountID,
//		CharacterID,
//		NewName
//	);
end;


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
		ZServerInfo.Connection := AClient;
		AClient.Data := TZoneServerLink.Create(AClient);
		TZoneServerLink(AClient.Data).DatabaseLink := Database;
		TZoneServerLink(AClient.Data).Info := ZServerInfo;
		fZoneServerList.AddObject(ZServerInfo.ZoneID,ZServerInfo);
	end;
	SendValidateFlagToZone(AClient,Validated);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//UpdateToAccountList                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Update the account data in list (ex, switching zone)
//
//	Changes -
//		April 12th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
procedure TCharacterServer.UpdateToAccountList(
	AClient : TIdContext;
	InBuffer : TBuffer
);
var
	AccountID: LongWord;
	Idx	: Integer;
	AccountInfo : TCharAccountInfo;
begin
	AccountID := BufferReadLongWord(2, InBuffer);
	Idx := fAccountList.IndexOf(AccountID);
	//If already exist, just update it.
	//Should be exist, since data is create upon connect to Char Server
	if Idx > -1 then
	begin
		AccountInfo := fAccountList.Objects[Idx] as TCharAccountInfo;

		AccountInfo.ZoneServerID := BufferReadWord(6, InBuffer);
		AccountInfo.InGame := True;
		AccountInfo.Transfering := False;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RemoveFromAccountList                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Remove/Reset the account data in list (ex, Exit game/Return to character select)
//
//	Changes -
//		April 12th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
procedure TCharacterServer.RemoveFromAccountList(
	AClient : TIdContext;
	InBuffer : TBuffer
);
var
	AccountID: LongWord;
	Idx	: Integer;
	AccountInfo : TCharAccountInfo;
	Action  : Byte;
begin
	AccountID := BufferReadLongWord(2, InBuffer);
	Idx := fAccountList.IndexOf(AccountID);
	//Should be exist, since data is create upon connect to Char Server
	if Idx > -1 then
	begin
		AccountInfo := fAccountList.Objects[Idx] as TCharAccountInfo;

		Action := BufferReadByte(6, InBuffer);
		// 0 = Delete it and forward to Account Server..
		// 1 = Keep data (Returning to Character Server..)
		if Action = 0 then
		begin
			if AccountInfo.InGame then
			begin
				fAccountList.Delete(Idx);
				SendAccountLogOut(CharaToLoginClient, AccountInfo, Self);
			end;
		end else
		begin
			AccountInfo.Transfering := True;
			AccountInfo.InGame := False;
		end;
	end;
end;


//------------------------------------------------------------------------------
//OnExecute                                                      PROCEDURE
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
//		March 30th, 2007 - Tsusai - Added packets 0x2105 & 0x2106
//		March 30th, 2007 - Aeomin - Change from TThreadLink to TClientLink
//
//------------------------------------------------------------------------------
procedure TCharacterServer.OnExecute(AConnection : TIdContext);
var
	ABuffer       : TBuffer;
	PacketID      : Word;
	Size          : Word;
begin
	RecvBuffer(AConnection,ABuffer,2);
	PacketID := BufferReadWord(0, ABuffer);
	//Check to see if it is an already connected Client
	if (AConnection.Data is TClientLink) then
	begin
		if not TThreadLink(AConnection.Data).CharRename then
		begin
			case PacketID of
			$0066: // Character Selected -- Refer Client to Map Server
				begin
					RecvBuffer(AConnection,ABuffer[2],PacketDB.GetLength($0066)-2);
					SendCharaToMap(AConnection,ABuffer);
				end;
			$0067: // Create New Character
				begin
					RecvBuffer(AConnection,ABuffer[2],PacketDB.GetLength($0067)-2);
					CreateChara(AConnection,ABuffer);
				end;
			$0068: // Request to Delete Character
				begin
					RecvBuffer(AConnection,ABuffer[2],PacketDB.GetLength($0068)-2);
					DeleteChara(AConnection,ABuffer);
				end;
			$0187: //Client keep alive
				begin
					RecvBuffer(AConnection,ABuffer[2],PacketDB.GetLength($0187)-2);
				end;
			$028d: //Enter Character Rename mode
				begin
					{RecvBuffer(AConnection,ABuffer[2],2);
					TThreadLink(AConnection.Data).CharRename := True;}
				end;
			$028f:  // ??
				begin
					//<Char ID>
					RecvBuffer(AConnection,ABuffer[2],4);
				end
			else
				begin
					Size := PacketDB.GetLength(PacketID);
					Console.Message('Unknown Character Server Packet : ' + IntToHex(PacketID,4), 'Character Server', MS_WARNING);
					if (Size-2 > 0) then
					begin
						Console.Message(IntToStr(Size-2) + ' additional bytes were truncated','Character Server', MS_WARNING);
						RecvBuffer(AConnection,ABuffer[2],PacketDB.GetLength(PacketID)-2);
					end;
				end;
			end;
		end else
		begin
			//Char rename mode
			RecvBuffer(AConnection,ABuffer[2],2);
			RenameChara(AConnection,ABuffer);
		end;
	end else
	begin
		case PacketID of
		$0065: // RO Client request to connect and get characters
			begin
				//Thread Data should have a TThreadLink object...if not, make one
				AConnection.Data := TClientlink.Create(AConnection);
				TClientLink(AConnection.Data).DatabaseLink := Database;
				//Verify login and send characters
				RecvBuffer(AConnection,ABuffer[2],PacketDB.GetLength($0065)-2);
				SendCharas(AConnection,ABuffer);
			end;
		$2100: // Zone Server Connection request
			begin
				RecvBuffer(AConnection,ABuffer[2],PacketDB.GetLength($2100)-2);
				VerifyZoneServer(AConnection,ABuffer);
			end;
		$2102: // Zone Server sending new WAN location details
			begin
				if AConnection.Data is TZoneServerLink then
				begin
					RecvBuffer(AConnection,ABuffer[2],2);
					Size := BufferReadWord(2,ABuffer);
					RecvBuffer(AConnection,ABuffer[4],Size-4);
					TZoneServerLink(AConnection.Data).Info.WAN := BufferReadString(4,Size-4,ABuffer);
					Console.Message('Received updated Zone Server WANIP.', 'Character Server', MS_NOTICE);
				end;
			end;
		$2103: // Zone Server sending new LAN location details
			begin
				if AConnection.Data is TZoneServerLink then
				begin
					RecvBuffer(AConnection,ABuffer[2],2);
					Size := BufferReadWord(2,ABuffer);
					RecvBuffer(AConnection,ABuffer[4],Size-4);
					TZoneServerLink(AConnection.Data).Info.LAN := BufferReadString(4,Size-4,ABuffer);
					Console.Message('Received updated Zone Server LANIP.', 'Character Server', MS_NOTICE);
				end;
			end;
		$2104: // Zone Server sending new Online User count (relink while running)
			begin
				if AConnection.Data is TZoneServerLink then
				begin
					RecvBuffer(AConnection,ABuffer[2],PacketDB.GetLength($2104)-2);
					TZoneServerLink(AConnection.Data).Info.OnlineUsers := BufferReadWord(2,ABuffer);
					if CharaToLoginClient.Connected then
					begin
						SendCharaOnlineUsersToLogin(CharaToLoginClient,Self);
					end;
					UpdateOnlineCountToZone;
					Console.Message('Received updated Zone Server Online Users.', 'Character Server', MS_DEBUG);
				end;
			end;
		$2105: // Zone Server sending decrease of online users by 1
			begin
				if AConnection.Data is TZoneServerLink then
				begin
					TZoneServerLink(AConnection.Data).Info.OnlineUsers :=
					Min(TZoneServerLink(AConnection.Data).Info.OnlineUsers +1, High(Word));
					SendCharaOnlineUsersToLogin(CharaToLoginClient,Self);
					UpdateOnlineCountToZone;
					Console.Message('Received updated Zone Server Online Users (+1).', 'Character Server', MS_DEBUG);
				end;
			end;
		$2106: // Zone Server sending decrease of online users by 1
			begin
				if AConnection.Data is TZoneServerLink then
				begin
					TZoneServerLink(AConnection.Data).Info.OnlineUsers :=
					Max(TZoneServerLink(AConnection.Data).Info.OnlineUsers -1, 0);
					SendCharaOnlineUsersToLogin(CharaToLoginClient,Self);
					UpdateOnlineCountToZone;
					Console.Message('Received updated Zone Server Online Users (-1).', 'Character Server', MS_DEBUG);
				end;
			end;
		$2108: //Zone send update an account in fAccountList
			begin
				if AConnection.Data is TZoneServerLink then
				begin
					RecvBuffer(AConnection,ABuffer[2],PacketDB.GetLength($2108)-2);
					UpdateToAccountList(AConnection,ABuffer);
				end;
		end;
		$2109: //Zone send remove an account in fAccountList
			begin
				if AConnection.Data is TZoneServerLink then
				begin
					RecvBuffer(AConnection,ABuffer[2],PacketDB.GetLength($2109)-2);
					RemoveFromAccountList(AConnection,ABuffer);
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
	Options.Save;
end;{LoadOptions}
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
//		April 10th, 2007 - Aeomin - Added
//
//------------------------------------------------------------------------------
Procedure TCharacterServer.OnDisconnect(AConnection: TIdContext);
var
	idx : integer;
	AZoneServInfo : TZoneServerInfo;
	AccountInfo : TCharAccountInfo;
begin
	if AConnection.Data is TZoneServerLink then
	begin
		AZoneServInfo := TZoneServerLink(AConnection.Data).Info;
		idx := fZoneServerList.IndexOfObject(AZoneServInfo);
		if not (idx = -1) then
		begin
			fZoneServerList.Delete(idx);
		end;
	end else
	if AConnection.Data is TClientLink then
	begin
		Idx := fAccountList.IndexOf(TClientLink(AConnection.Data).AccountLink.ID);
		if (Idx > -1) and (not TClientLink(AConnection.Data).Transfering) then
		begin
			AccountInfo := fAccountList.Objects[Idx] as TCharAccountInfo;
			SendAccountLogOut(CharaToLoginClient, AccountInfo, Self);
			fAccountList.Delete(Idx);
		end;
	end;
	if Assigned(AConnection.Data) then
	begin
		AConnection.Data.Free;
		AConnection.Data:=nil;
	end;
end;{OnDisconnect}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetOnlineUserCount()                                                 Procedure
//------------------------------------------------------------------------------
//	What it does-
//		  Count total online players
//
//	Changes -
//		March 31th, 2007 - Aeomin - Added header.
//
//------------------------------------------------------------------------------
function TCharacterServer.GetOnlineUserCount : Word;
var
	Index : integer;
begin
	Result := 0;
	for Index := fZoneServerList.Count - 1 downto 0 do
	begin
		Inc(Result,TZoneServerInfo(fZoneServerList.Objects[Index]).OnlineUsers);
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//UpdateOnlineCountToZone()                                            Procedure
//------------------------------------------------------------------------------
//	What it does-
//		  Send new online count to every zone server
//
//	Changes -
//		April 5th, 2007 - Aeomin - Added header.
//
//------------------------------------------------------------------------------
procedure TCharacterServer.UpdateOnlineCountToZone;
var
	Count : Word;
	Index : Integer;
begin
	Count := GetOnlineUserCount;
	for Index := fZoneServerList.Count - 1 downto 0 do
	begin
		SendOnlineCountToZone(TZoneServerInfo(fZoneServerList.Objects[Index]).Connection, Count);
	end;
end;
//------------------------------------------------------------------------------

end.
