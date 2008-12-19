//------------------------------------------------------------------------------
//ZoneServer			                                                        UNIT
//------------------------------------------------------------------------------
//	What it does-
//      The Zone Server.
//    Contains the brunt of the zone and packet processes.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//		[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
//		[2007/04/23] Tsusai - Changed lua filename
//		June 28th, 2008 - Tsusai - Updated to use TPacketDB for packet usage.
//
//
//------------------------------------------------------------------------------
unit ZoneServer;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	CommClient,
	Classes,
	SysUtils,
	{Project}
	Character,
	BeingEventThread,
	GroundItemRemovalEventThread,
	BeingList,
	GMCommands,
	LuaTypes,
	MapList,
	PacketTypes,
	Server,
	ZoneOptions,
	{3rd Party}
	IdContext,
	List32
	;


type
	TZoneServer = class(TServer)
	protected

		Procedure OnExecute(AConnection: TIdContext);override;
		Procedure OnConnect(AConnection: TIdContext);override;
		Procedure OnDisconnect(AConnection: TIdContext);override;
		Procedure OnException(AConnection: TIdContext;
			AException: Exception);override;

		Procedure CharaClientOnConnect(Sender : TObject);
		Procedure CharaClientRead(AClient : TInterClient);

		Procedure InterClientOnConnect(Sender : TObject);
		Procedure InterClientRead(AClient : TInterClient);

		Procedure ProcessZonePacket(AClient : TIdContext);

		Procedure LoadOptions;
		Procedure LoadMaps;

	public
		ToCharaTCPClient : TInterClient;
		ToInterTCPClient : TInterClient;

		Commands			: TGMCommands;

		OnlineUsers   : Word;
		TotalOnlinePlayers: Word;

		Options       : TZoneOptions;

		MapList       : TMapList;
		InstanceMapList  : TMapList;
		CharacterList : TBeingList;
		NPCList       : TIntList32;
		InstanceCache  : TStringList;
		MobList       : TBeingList;

		CharacterEventThread : TBeingEventThread;
		GroundItemEventThread : TGroundItemEventThread;
		MobEventThread : TBeingEventThread;

		GroundItemList : TThreadList;

		NPCLua : TLua;
		ItemLua : TLua;

		Constructor Create;
		Destructor  Destroy;Override;

		Procedure   Start();override;
		Procedure   Stop;override;

		Procedure   ConnectToCharacter;
		Procedure   ConnectToInter;

	end;


implementation


uses
	{RTL/VCL}
	StrUtils,
	{Project}
	GameObject,
	Being,
	BufferIO,
	Globals,
	LuaCoreRoutines,
	LuaItemCommands,
	LuaNPCCommands,
	Main,
	Map,
	Packets,
	TCPServerRoutines,
	ZoneCharaCommunication,
	ZoneInterCommunication,
	ZoneRecv,
	ZoneSend
	{3rd Party}
	//none
	;


//------------------------------------------------------------------------------
//Create  ()                                                        CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our character server
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//		January 14th, 2007 - Tsusai - Chara Client is now setup
//
//------------------------------------------------------------------------------
Constructor TZoneServer.Create;
begin
	inherited;
	OnlineUsers := 0;

	Commands := TGMCommands.Create;

	MapList := TMapList.Create(TRUE);
	InstanceMapList := TMapList.Create(TRUE);
	CharacterList := TBeingList.Create(TRUE);
	NPCList := TIntList32.Create;
	InstanceCache  := TStringList.Create;
	MobList := TBeingList.Create(TRUE);

	ToCharaTCPClient := TInterClient.Create('Zone','Character', true, MainProc.Options.ReconnectDelay);
	ToInterTCPClient := TInterClient.Create('Zone','Inter', true, MainProc.Options.ReconnectDelay);

	ToCharaTCPClient.OnConnected := CharaClientOnConnect;
	ToCharaTCPClient.OnReceive   := CharaClientRead;

	ToInterTCPClient.OnConnected := InterClientOnConnect;
	ToInterTCPClient.OnReceive   := InterClientRead;

	CharacterEventThread := TBeingEventThread.Create(CharacterList,'CharacterEventThread');
	MobEventThread := TBeingEventThread.Create(MobList, 'MobEventThread');

	GroundItemList := TThreadList.Create;
	GroundItemEventThread := TGroundItemEventThread.Create(GroundItemList);
end;{Create}
//------------------------------------------------------------------------------


(*- Destructor ----------------------------------------------------------------*
TZoneServer.Destroy
--------------------------------------------------------------------------------
Overview:
--
	Cleans up lists and objects owned by the Zone Server instance.


--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2006/09/19] RaX - Created Header.
[2007/06/02] CR - Improved description.  Bugfix: Memory leak plugged -- NPCList
	objects are now freed before freeing the list itself.  Did the research, and
	RaX's lists for MapList and CharacterList are self-cleaning (Good job there!).
	Added comments to point out which are potentially unsafe Free calls.
*-----------------------------------------------------------------------------*)
Destructor TZoneServer.Destroy;
Var
	Index : Integer;
Begin
	{[2007/06/02] CR - MapList and CharacterList are both self-cleaning. }
	MapList.Free;
	InstanceMapList.Free;
	CharacterList.Free;
	MobList.Free;

	if InstanceCache.Count > 0 then
	begin
		for Index := 0 to InstanceCache.Count - 1 do
		begin
			InstanceCache.Objects[Index].Free;
		end;
	end;
	InstanceCache.Free;

	{[2007/06/02] CR - Tsusai was right, we did need to free up this list - the
	Zone owns the NPCs on it's maps. }
	if NPCList.Count > 0 then
	begin
		for Index := NPCList.Count -1 downto 0 do
		begin
			if Assigned(NPCList.Objects[Index]) then
			begin
				NPCList.Objects[Index].Free;
			end;
		end;
	end;
	NPCList.Free;

	//--
	{[2007/06/02] CR - These are still a it suspect for freeing up their sub-
	objects properly... }
	MobEventThread.Free;
	CharacterEventThread.Free;
	GroundItemEventThread.Free;
	GroundItemList.Free;

	ToCharaTCPClient.Free;
	ToInterTCPClient.Free;
	//--

	{[2007/06/02] CR - Safe. }
	Commands.Free;
	inherited;
End; (* Dest TZoneServer.Destroy
*-----------------------------------------------------------------------------*)


//------------------------------------------------------------------------------
//OnExecute()                                          EVENT
//------------------------------------------------------------------------------
//	What it does-
//			An event which fires when the server is started. It allows the server
//    to accept incoming client connections.
//
//	Changes -
//		November 11th, 2006 - Tsusai - Created.
//
//------------------------------------------------------------------------------
procedure TZoneServer.OnExecute(AConnection: TIdContext);
begin
	ProcessZonePacket(AConnection);
end;{TMainProc.ZoneServerExecute}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//OnConnect()                                                             EVENT
//------------------------------------------------------------------------------
//	What it does-
//			An event which fires when a user connects to the Zone Server.
//
//	Changes -
//		November 11th, 2006 - Tsusai - Created.
//
//------------------------------------------------------------------------------
procedure TZoneServer.OnConnect(AConnection: TIdContext);
begin
	AConnection.Data := TClientLink.Create(AConnection);
	TClientLink(AConnection.Data).DatabaseLink := Database;
end;{OnConnect}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//OnDisconnect()                                                             EVENT
//------------------------------------------------------------------------------
//	What it does-
//			An event which fires when a user disconnects from the zone server.
//    Cleans up disconnected user's data.
//
//	Changes -
//		January 25th, 2007 - RaX - Created.
//		March 28th, 2007 - Aeomin - Add a check see if server socket to void AV.
//		March 30th, 2007 - Aeomin - Change from TThreadLink to TClientLink
//
//------------------------------------------------------------------------------
procedure TZoneServer.OnDisconnect(AConnection: TIdContext);
var
	CharacterIndex : Integer;
	ACharacter : TCharacter;

begin
	if AConnection.Data is TClientLink then
	begin
		ACharacter := TClientLink(AConnection.Data).CharacterLink;

		ACharacter.ZoneStatus := isOffline;

		ZoneSendPlayerOnlineStatus(
					ToInterTCPClient,
					ACharacter.AccountID,
					ACharacter.ID,
					1 //0 = online; 1=offline
				);

		TThreadLink(AConnection.Data).DatabaseLink.Character.Save(ACharacter);

		SendZoneCharaLogOut(ToCharaTCPClient, ACharacter, Byte(ACharacter.DcAndKeepData));


		if Started and (ACharacter.MapInfo <> nil) then
		begin
			ACharacter.RemoveFromMap;
			ACharacter.ShowTeleportOut;
		end;

		if ACharacter.CharaState = charaDead then
		begin
			ACharacter.Map := ACharacter.SaveMap;
			ACharacter.Position := ACharacter.SavePoint;
		end;

		CharacterIndex := CharacterList.IndexOf(ACharacter.ID);
		if CharacterIndex > -1 then
		begin
			CharacterList.Delete(CharacterIndex);
		end;
		SendZoneCharaDecrease(ToCharaTCPClient,Self);
	end;
	AConnection.Data.Free;
	AConnection.Data:=nil;
end;{OnDisconnect}
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
procedure TZoneServer.OnException(AConnection: TIdContext;
	AException: Exception);
begin
	if ContainsStr(AException.Message, '10053') or
		ContainsStr(AException.Message, '10054')
	then begin
		AConnection.Connection.Disconnect;
	end;
end;{OnException}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Start()                                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		 Enables the zone server to accept incoming connections
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//		January 14th, 2007 - Tsusai - Chara client settings in place
//
//------------------------------------------------------------------------------
Procedure TZoneServer.Start();
begin
	if NOT Started then
	begin
	inherited;
		//Load our Zone.ini
		LoadOptions;

		//Initialize ips and port.
		WANIP := Options.WANIP;
		LANIP := Options.LANIP;
		Port := Options.Port;

		//Activate server and clients.
		ActivateServer('Zone',TCPServer, Options.IndySchedulerType, Options.IndyThreadPoolSize);

		if Database.GameConnection.Connected then
		begin
			//Load Maps
			LoadMaps;
		end else
		begin
			Console.Message('Could not start, helios could not connect to the database. Helios is now shutting down.', 'Zone Server', MS_ERROR);
			Sleep(3000);
			MainProc.Run := false;
		end;
		//Initiate NPC Lua
		InitLuaState(NPCLua);
		LoadNPCCommands(NPCLua);
		//Intitiate Item Lua
		InitLuaState(ItemLua);
		LoadItemCommands(ItemLua);
		//Run the Core script
		LoadAndRunLuaScript(NPCLua, MainProc.Options.ScriptDirectory + LUA_NPC_CORE_FILE);
		LoadAndRunLuaScript(NPCLua, MainProc.Options.ScriptDirectory + LUA_ITEM_CORE_FILE);

		CharacterEventThread.Start;
		GroundItemEventThread.Start;
		MobEventThread.Start;
	end else
	begin
		Console.Message('Cannot Start():: Zone Server already running!', 'Zone Server', MS_ALERT);
	end;
end;{Start}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Stop()                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		 Stops the zone server from accepting incoming connections
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TZoneServer.Stop();
begin
	if Started then
	begin

		if Options.KickOnShutdown then
		begin
			KickAll;
		end;

		CharacterEventThread.Terminate;
		while NOT CharacterEventThread.Terminated do
		begin
			Sleep(1);
		end;
		GroundItemEventThread.Terminate;
		while NOT GroundItemEventThread.Terminated do
		begin
			Sleep(1);
		end;
		MobEventThread.Terminate;
		while NOT MobEventThread.Terminated do
		begin
			Sleep(1);
		end;


		//deactivate server and clients.
		DeActivateServer('Zone', TCPServer);
		DeActivateClient(ToCharaTCPClient);
		DeActivateClient(ToInterTCPClient);

		//Clear Lists
		CharacterList.Clear;
		MapList.Clear;

		//Kill ALL LUAS by killing the root lua.
		TerminateLua(NPCLua);
		TerminateLua(ItemLua);

		//Save and free options, options must be free'd here to force a reload after
		//start.
		Options.Save;
		Options.Free;
		inherited;
	end else
	begin
		Console.Message('Cannot Stop():: Zone Server is not running!', 'Zone Server', MS_ALERT);
	end;
end;{Stop}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ProcessZonePacket                                                   PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      Packet Parser for the zone server.
//
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//		January 20th, 2007 - Tsusai - Wrapped the console messages, now using
//			IdContext.Binding shortcut
//		June 08th, 2008 - Tsusai - Upgraded to parse the packets that have been
//			moved to TObjectList form, than arrays.

//    Old Comments Follow...
//    [2005/07/11] CR - Added Comment Header, Reindented, using newer syntax for
//	    MapConnect that is MUCH less verbose.
//    [2006/03/11] Tsusai - Removed packet id -> hex string conversions.  CIdx, and
//     packetid now words.
//    [2006/08/12] Tsusai - Updated for Indy.
//    [2006/09/26] Tsusai - Imported to Helios
//    [2006/09/28] RaX - Re-formatted, re-indented, renamed - PIdx to PacketIndex,
//    	CIdx to ClientBaseIndex to signify our supported client database or
//      packet_db.
//    [2007/05/21] Tsusai - Changed AChara to be a pointer OF TCharacter.  This
//      way I can get the original character data and pass it down for use with
//      lua.
//------------------------------------------------------------------------------
Procedure TZoneServer.ProcessZonePacket(AClient : TIdContext);
Var
	Lth             : Integer;
	AChara          : ^TCharacter;
	Version         : Word; //Index of the packet in the packet(allowed client)
				//database (client-base).
	PacketID        : Word; //The ID of a packet in said database.
	ABuffer         : TBuffer;
	PacketInfo      : TPackets;
	Size            : Word;
Begin
	AClient.Connection.IOHandler.CheckForDataOnSource();
	while AClient.Connection.IOHandler.InputBuffer.Size >= 2 do
	begin
		Lth := AClient.Connection.IOHandler.InputBuffer.Size;
		//Only accepting 2 bytes first because for some of the incoming packets, they
		//are broken up according to WPE and other packet sniffers (Mapconnect is an
		//exception).  Once we know what version and what ID, we can read the packets
		//we need.  If we ask for more than is on the buffer, Indy waits for it.
		//That process should not bog the server down as it should be skipping and
		//going to the next client enqueued.
		RecvBuffer(AClient,ABuffer[0], 2);
		PacketID := BufferReadWord(0, ABuffer);
		{if (TClientLink(AClient.Data).EncKey1 > 0)AND(TClientLink(AClient.Data).EncKey2 > 0) then
		begin
			PacketID := TClientLink(AClient.Data).DecryptMessageID(PacketID);
		end;}
		
		AChara := @TClientLink(AClient.Data).CharacterLink;

		if NOT Assigned(AChara^) then
		begin
			PacketDB.GetMapConnectPacket(PacketID, Lth, Version, PacketInfo);
			if Assigned(PacketInfo) then
			begin
				//Use found data packet
				RecvBuffer(AClient, ABuffer[2], (Lth - 2)); //Get the rest of the packet info
				MapConnect(Version,
					AClient,
					ABuffer,
					PacketInfo.ReadPoints
				);
				PacketInfo.Free;
			end else
			begin
				//They can't get in, get their shit off buffer, then DC
				RecvBuffer(AClient, ABuffer[2], (Lth - 2));
				Console.Message(
					'Someone with the IP '+
					AClient.Binding.PeerIP +
					' attempted to send a packet '+
					IntToHex(packetID, 4) +
					' with a length of ' + IntToStr(Lth), 'Zone Server', MS_WARNING
				);
				Console.WriteLn(
					'Reason for this response: Unsupported ' +
					'client or a bot attempted to connect.'
				);
				AClient.Connection.Disconnect;
			end;

		end else begin
			//Already authenticated, parse the packet
			PacketDB.GetPacketInfo(PacketID,AChara^.ClientVersion,PacketInfo);
			if Assigned(PacketInfo) then
			begin
				if PacketInfo.PLength <> -1 then
				begin
					RecvBuffer(AClient,ABuffer[2], PacketInfo.PLength - 2);
				end else
				begin
					//Usually our string messages, where the 2nd location is the
					//size holder of the string
					RecvBuffer(AClient,ABuffer[2], 2);
					Size := BufferReadWord(2, ABuffer);
					RecvBuffer(AClient,ABuffer[4], Size - 4);
				end;

				//Execute the packet procedure, only one is runned because the other
				//runs a dummy procedure
				PacketInfo.ExecCommand(AChara^,ABuffer,PacketInfo.ReadPoints);
				PacketInfo.ExecAvoidSelfCommand(AChara^);
			end;
			PacketInfo.Free;
		end;
	end;
	//Sleep to free up the processor.
	Sleep(Options.ZoneTick);
End;{ProcessZonePacket}
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
Procedure TZoneServer.LoadOptions;
begin
	Options := TZoneOptions.Create(MainProc.Options.ConfigDirectory+'/Zone.ini');
	Options.Load;
	Options.Save;
end;{LoadOptions}
//------------------------------------------------------------------------------


(*- Procedure -----------------------------------------------------------------*
TZoneServer.LoadMaps
--------------------------------------------------------------------------------
Overview:
--
	Loads all maps into the maplist.

--
Pre:
	TODO
Post:
	TODO

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/01/25] RaX - Created Header.
[2007/06/01] CR - Altered Comment Header.  Major Memory leak fixed. (649 maps as
	of this writing, so 649 dangling strings freed).  We now free MapNames on
	routine completion, since this routine owns that object, it MUST clean it up.
*-----------------------------------------------------------------------------*)
Procedure TZoneServer.LoadMaps;
Const
	FSTR_MAPFILE = '%s/%s.pms';
Var
	Index       : Integer;
	MapFileName : String;
	AMap				: TMap;
	Pass				: Boolean;
Begin
	Console.WriteLn('      - Loading Maps...');
	Database.Map.LoadList(MapList, MainProc.ZoneServer.Options.ID);

	Console.WriteLn('        : '+ IntToStr(MapList.Count)+ ' Found!');

	for Index := MapList.Count - 1 downto 0 do
	begin
		AMap := MapList[Index];
		MapFileName :=
			Format(
				FSTR_MAPFILE,
				[MainProc.Options.MapDirectory, AMap.Name]
			);

		Pass := false;
		if FileExists(MapFileName) then
		begin
			if AMap.LoadFromFile(MapFileName) then
			begin
				if NOT Options.DynamicMapLoading then
				begin
					AMap.Load;
				end;
				
				Pass := true;
			end else
			begin
				Console.WriteLn('      - Map ' + MapFileName + ' seems to be corrupt!');
			end;
		end else
		begin
			Console.WriteLn('      - Map ' + MapFileName + ' does not exist!');
		end;

		if NOT Pass then
		begin
			MapList.Delete(Index);
		end;
	end;
	Console.WriteLn('      - Maps Loaded!');
End; (* Proc TZoneServer.LoadMaps
*-----------------------------------------------------------------------------*)


//------------------------------------------------------------------------------
//CharaClientOnConnect                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Validates with the character server once it connects.
//
//	Changes -
//		January 17th, 2007 - RaX - Created Header.
//		January 14th, 2007 - Tsusai - Added
//
//------------------------------------------------------------------------------
procedure TZoneServer.CharaClientOnConnect(Sender : TObject);
begin
	ValidateWithCharaServer(ToCharaTCPClient,Self);
end;//CharaClientOnConnect
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//CharaClientRead                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Executes on receiving information back from a character server.
//
//	Changes -
//		January 17th, 2007 - RaX - Created Header.
//		January 14th, 2007 - Tsusai - Added
//		April 5th, 2007 - Aeomin - Add support of $2107 to set total online count
//
//------------------------------------------------------------------------------
procedure TZoneServer.CharaClientRead(AClient : TInterClient);
var
	ABuffer : TBuffer;
	PacketID : Word;
	Response : Byte;
begin
	RecvBuffer(AClient,ABuffer,2);
	PacketID := BufferReadWord(0,ABuffer);
	case PacketID of
	$2101:
		begin
			RecvBuffer(AClient,ABuffer[2],PacketDB.GetLength($2101)-2);
			Response := BufferReadByte(2,ABuffer);

			//If validated.
			if Response = 0 then
			begin
				Console.Message('Verified with Character Server, '+
					'sending details.', 'Zone Server', MS_NOTICE);
				SendZoneWANIPToChara(ToCharaTCPClient,Self);
				SendZoneLANIPToChara(ToCharaTCPClient,Self);
				SendZoneOnlineUsersToChara(ToCharaTCPClient,Self);
			end else
			begin
				case Response of
				1 : Console.Message('Failed to verify with Character Server. ID already in use.', 'Zone Server', MS_WARNING);
				2 : Console.Message('Failed to verify with Character Server. Invalid security key.', 'Zone Server', MS_WARNING);
				end;
				Console.Message('Stopping...', 'Zone Server', MS_NOTICE);
				Stop;
			end;
		end;
	$2107:
		begin
			RecvBuffer(AClient,ABuffer[2],PacketDB.GetLength($2107)-2);
			TotalOnlinePlayers := BufferReadWord(2,ABuffer);
		end;
	$2110:
		begin
			RecvBuffer(AClient,ABuffer[2],PacketDB.GetLength($2110)-2);
			DuplicateSessionKick(ABuffer);
		end;
	end;
end;{CharaClientRead}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ConnectToCharacter                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Connects to the character server
//
//	Changes -
//		January 25th, 2007 - RaX - Moved from Start().
//
//------------------------------------------------------------------------------
Procedure TZoneServer.ConnectToCharacter;
begin
	//initialize commclient ips and ports.
	ToCharaTCPClient.Host := Options.CharaIP;
	ToCharaTCPClient.Port := Options.CharaPort;

	ActivateClient(ToCharaTCPClient);
end;//ConnectToCharacter
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//InterClientOnConnect                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Validates with the inter once it connects.
//
//	Changes -
//		March 18th, 2007 - RaX - Created
//
//------------------------------------------------------------------------------
procedure TZoneServer.InterClientOnConnect(Sender : TObject);
begin
	ValidateWithInterServer(ToInterTCPClient,Self);
end;//InterClientOnConnect
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//CharaClientRead                                                      PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Executes on receiving information back from an inter server.
//
//	Changes -
//		March 18th, 2007 - RaX - Created
//
//------------------------------------------------------------------------------
procedure TZoneServer.InterClientRead(AClient : TInterClient);
var
	ABuffer : TBuffer;
	PacketID : Word;
	Response : Byte;
	Size		 : Word;
begin
	RecvBuffer(AClient,ABuffer,2);
	PacketID := BufferReadWord(0,ABuffer);
	case PacketID of
	$2201:
		begin
			RecvBuffer(AClient,ABuffer[2],PacketDB.GetLength($2101)-2);
			Response := BufferReadByte(2,ABuffer);

			//If validated.
			if Response = 0 then
			begin
				Console.Message('Verified with Inter Server, '+
					'sending details.', 'Zone Server', MS_NOTICE);
				SendZoneWANIPToInter(ToInterTCPClient,Self);
				SendZoneLANIPToInter(ToInterTCPClient,Self);
				SendZoneOnlineUsersToInter(ToInterTCPClient,Self);
				ZoneSendAllowInstanceFlag(ToInterTCPClient,Options.AllowInstance);
				ZoneSendInstanceList(ToInterTCPClient);
			end else
			begin
				case Response of
				1 : Console.Message('Failed to verify with Inter Server. ID already in use.', 'Zone Server', MS_WARNING);
				2 : Console.Message('Failed to verify with Inter Server. Invalid security key.', 'Zone Server', MS_WARNING);
				end;
				Console.Message('Stopping...', 'Zone Server', MS_NOTICE);
				Stop;
			end;
		end;

	$2206://Get a gm command from the inter server
		begin
			RecvBuffer(AClient,ABuffer[2],2);
			Size := BufferReadWord(2,ABuffer);
			RecvBuffer(AClient,ABuffer[4],Size-4);
			RecvGMCommandFromInter(ABuffer);
		end;

	$2207://Get a gm command reply from the inter server
		begin
			RecvBuffer(AClient,ABuffer[2],2);
			Size := BufferReadWord(2,ABuffer);
			RecvBuffer(AClient,ABuffer[4],Size-4);
			RecvGMCommandResultFromInter(ABuffer);
		end;

	$2209://Get warp request reply from inter
		begin
			RecvBuffer(AClient,ABuffer[2],2);
			Size := BufferReadWord(2,ABuffer);
			RecvBuffer(AClient,ABuffer[4],Size-4);
			RecvWarpRequestReplyFromInter(ABuffer);
		end;
	$2210:
		begin
			RecvBuffer(AClient,ABuffer[2],2);
			Size := BufferReadWord(2,ABuffer);
			RecvBuffer(AClient,ABuffer[4],Size-4);
			RecvRedirectWhisper(ABuffer);
		end;
	$2212:
		begin
			RecvBuffer(AClient,ABuffer[2],PacketDB.GetLength($2212)-2);
			RecvWhisperReply(ABuffer);
		end;
	$2216:
		begin
			RecvBuffer(AClient,ABuffer[2],PacketDB.GetLength($2216)-2);
			RecvAddFriendRequest(ABuffer);
		end;
	$2217:
		begin
			RecvBuffer(AClient,ABuffer[2],PacketDB.GetLength($2217)-2);
			RecvAddFriendRequestReply(ABuffer);
		end;
	$2219:
		begin
			RecvBuffer(AClient,ABuffer[2],PacketDB.GetLength($2219)-2);
			RecvFriendOnlineStatus(ABuffer);
		end;
	$2221:
		begin
			RecvBuffer(AClient,ABuffer[2],PacketDB.GetLength($2221)-2);
			RecvFriendStatus(ABuffer);
		end;
	$2223:
		begin
			RecvBuffer(AClient,ABuffer[2],PacketDB.GetLength($2223)-2);
			RecvMailNotify(ABuffer);
		end;
	$2225:
		begin
			RecvBuffer(AClient,ABuffer[2],2);
			Size := BufferReadWord(2,ABuffer);
			RecvBuffer(AClient,ABuffer[4],Size-4);
			RecvCreateInstanceResult(ABuffer);
		end;
	$2227:
		begin
			RecvBuffer(AClient,ABuffer[2],2);
			Size := BufferReadWord(2,ABuffer);
			RecvBuffer(AClient,ABuffer[4],Size-4);
			RecvCreateInstance(ABuffer);
		end;
	end;
end;{InterClientRead}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ConnectToInter                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Connects to the inter server
//
//	Changes -
//		March 18th, 2007 - RaX - Created
//
//------------------------------------------------------------------------------
Procedure TZoneServer.ConnectToInter;
begin
	//initialize commclient ips and ports.
	ToInterTCPClient.Host := Options.InterIP;
	ToInterTCPClient.Port := Options.InterPort;

	ActivateClient(ToInterTCPClient);
end;//ConnectToInter
//------------------------------------------------------------------------------


end.
