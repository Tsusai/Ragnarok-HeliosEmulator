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
//
//------------------------------------------------------------------------------
unit ZoneServer;


interface


uses
	{RTL/VCL}
	CommClient,
	SysUtils,
	{Project}
	Character,
	CharacterEventThread,
	CharaList,
	Database,
	GMCommands,
	LuaCoreRoutines,
	MapList,
	PacketTypes,
	ZoneOptions,
	{3rd Party}
	IdTCPServer,
	IdContext,
	List32
	;


type
	TZoneServer = class
	protected
	//
	private

		fPort            : Word;

		TCPServer        : TIdTCPServer;

		Procedure OnExecute(AConnection: TIdContext);
		Procedure OnConnect(AConnection: TIdContext);
		Procedure OnDisconnect(AConnection: TIdContext);
		Procedure OnException(AConnection: TIdContext;
			AException: Exception);

		Procedure CharaClientOnConnect(Sender : TObject);
		Procedure CharaClientRead(AClient : TInterClient);

		Procedure InterClientOnConnect(Sender : TObject);
		Procedure InterClientRead(AClient : TInterClient);

		Procedure ProcessZonePacket(AClient : TIdContext);
		Function  SearchPacketListing(
			var
				AChara   : TCharacter;
			var
				AClient  : TIdContext;
			var
				InBuffer :  TBuffer;
			const
				Version  : Word;
			const
				Packet   : Word
			) : Boolean;

		Procedure SetPort(Value : Word);
		Function GetStarted : Boolean;

		Procedure LoadOptions;
		Procedure LoadMaps;

	public
		WANIP : string;
		LANIP : string;

		ToCharaTCPClient : TInterClient;
		ToInterTCPClient : TInterClient;

		Commands			: TGMCommands;

		OnlineUsers   : Word;
		TotalOnlinePlayers: Word;

		Options       : TZoneOptions;

		MapList       : TMapList;
		CharacterList : TCharacterList;
		NPCList       : TIntList32;

		CharacterEventThread : TCharacterEventThread;

		ZoneLocalDatabase : TDatabase;

		NPCLua : TLua;

		property Started : Boolean read GetStarted;
		property Port : Word read fPort write SetPort;

		Constructor Create;
		Destructor  Destroy;Override;

		Procedure   Start(Reload : Boolean = FALSE);
		Procedure   Stop;

		Procedure   ConnectToCharacter;
		Procedure   ConnectToInter;

	end;


implementation


uses
	{RTL/VCL}
	Classes,
	StrUtils,
	{Project}
	Being,
	BufferIO,
	Globals,
	LuaNPCCommands,
	Main,
	Map,
	PacketDB,
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
	OnlineUsers := 0;

	Commands := TGMCommands.Create;

	MapList := TMapList.Create(TRUE);
	CharacterList := TCharacterList.Create(TRUE);
	NPCList := TIntList32.Create;

	TCPServer := TIdTCPServer.Create;
	ToCharaTCPClient := TInterClient.Create('Zone','Character');
	ToInterTCPClient := TInterClient.Create('Zone','Inter');

	ToCharaTCPClient.OnConnected := CharaClientOnConnect;
	ToCharaTCPClient.OnRecieve   := CharaClientRead;

	ToInterTCPClient.OnConnected := InterClientOnConnect;
	ToInterTCPClient.OnRecieve   := InterClientRead;

	TCPServer.OnConnect   := OnConnect;
	TCPServer.OnDisconnect:= OnDisconnect;
	TCPServer.OnExecute   := OnExecute;
	TCPServer.OnException := OnException;

	CharacterEventThread := TCharacterEventThread.Create(CharacterList);
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
	Idx : Integer;
Begin
	{[2007/06/02] CR - MapList and CharacterList are both self-cleaning. }
	MapList.Free;
	CharacterList.Free;

	{[2007/06/02] CR - Tsusai was right, we did need to free up this list - the
	Zone owns the NPCs on it's maps. }
	for Idx := NPCList.Count -1 downto 0 do
	begin
		if Assigned(NPCList.Objects[Idx]) then
		begin
			NPCList.Objects[Idx].Free;
		end;
	end;
	NPCList.Free;

	//--
	{[2007/06/02] CR - These are still a it suspect for freeing up their sub-
	objects properly... }
	CharacterEventThread.Free;

	TCPServer.Free;
	ToCharaTCPClient.Free;
	ToInterTCPClient.Free;
	//--

	{[2007/06/02] CR - Safe. }
	Commands.Free;
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
	AMap : TMap;

begin
	if AConnection.Data is TClientLink then
	begin
		ACharacter := TClientLink(AConnection.Data).CharacterLink;

		TThreadLink(AConnection.Data).DatabaseLink.GameData.Connect;
    try
		  TThreadLink(AConnection.Data).DatabaseLink.GameData.SaveChara(ACharacter);
    finally
		  TThreadLink(AConnection.Data).DatabaseLink.GameData.Disconnect;
    end;

		SendZoneCharaLogOut(ToCharaTCPClient, ACharacter, Byte(ACharacter.DcAndKeepData));
		if Started and (ACharacter.MapInfo <> nil) then
		begin
			AMap := ACharacter.MapInfo;
			CharacterIndex := AMap.Cell[ACharacter.Position.X][ACharacter.Position.Y].Beings.IndexOfObject(ACharacter);
			if CharacterIndex > -1 then
			begin
				AMap.Cell[ACharacter.Position.X][ACharacter.Position.Y].Beings.Delete(CharacterIndex);
				ACharacter.ShowTeleportOut;
			end;
		end;
		ACharacter.ZoneStatus := isOffline;
		CharacterIndex := CharacterList.IndexOf(ACharacter.CID);
		if CharacterIndex > -1 then
		begin
			CharacterList.Delete(CharacterIndex);
		end;
		SendZoneCharaDecrease(ToCharaTCPClient,Self);
	end;

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
	if AnsiContainsStr(AException.Message, IntToStr(10053)) or
		AnsiContainsStr(AException.Message, IntToStr(10054))
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
Procedure TZoneServer.Start(Reload : Boolean = FALSE);
begin
  if NOT Started then
	begin
		//Load our Zone.ini
		LoadOptions;

		//Initialize ips and port.
		WANIP := Options.WANIP;
		LANIP := Options.LANIP;
		Port := Options.Port;

		ZoneLocalDatabase := TDatabase.Create(NIL);

		//Activate server and clients.
		ActivateServer('Zone',TCPServer, Options.IndySchedulerType, Options.IndyThreadPoolSize);

		//Load Maps
		LoadMaps;

		//Initiate NPC Lua
		InitLuaState(NPCLua);
		LoadNPCCommands(NPCLua);
		//Run the Core script
		RunLuaScript(NPCLua, MainProc.Options.ScriptDirectory + '/Core/Core.lua');

		CharacterEventThread.Start;
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
		

		//deactivate server and clients.
		DeActivateServer('Zone', TCPServer);
		DeActivateClient(ToCharaTCPClient);
		DeActivateClient(ToInterTCPClient);

		//Clear Lists
		CharacterList.Clear;
		MapList.Clear;

		//Kill ALL LUAS by killing the root lua.
		TerminateLua(NPCLua);

		ZoneLocalDatabase.Free;

		//Save and free options, options must be free'd here to force a reload after
		//start.
		Options.Save;
		Options.Free;
	end else
	begin
		Console.Message('Cannot Stop():: Zone Server is not running!', 'Zone Server', MS_ALERT);
	end;
end;{Start}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SearchPacketListing                                                  FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//      This is only executed to find information about a packet for ingame/zone
//    packets.
//
//  Notes -
//    TChara - required

//    Socket - we are passing the buffer over so we can make sure we got the
//      right length of the packet

//    Ver - The version of packets we are going to look at.  This is called
//      twice, first if we need to look through it with our new client packet,
//      and if we can't find it, we need to search through the 0 version aka
//      oldschool aka 0628sakexe based.

//    packet - The integer packet id.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//   	Old Comments Follow...
//    [2006/03/11] Tsusai - Packet is now a word type, not a 0xHexString
//    [2006/08/12] Tsusai - Updated for Indy.
//    [2006/09/26] Tsusai - Imported to Helios
//    [2006/09/28] RaX - Variables re-cased, moved "begins" to next line.
//    [2006/10/04] Tsusai - Updated ExecCommand call
//------------------------------------------------------------------------------
function TZoneServer.SearchPacketListing(

	var   AChara    : TCharacter;
	var   AClient   : TIdContext;
	var   InBuffer  : TBuffer;

	const Version   : Word;
	const Packet    : Word

) :Boolean;
var
	Index : Integer;
	Size  : Word;
begin
	Result := False;

	for Index := 0 to Length(CodeBase[Version].Packet) - 1 do
	begin

		with CodeBase[Version].Packet[Index] do
		begin

			if (ID = Packet) then
			begin
				//Ok so we found a matching packet ID, now to read our right length
				if PLength <> -1 then
				begin
					RecvBuffer(AClient,InBuffer[2], PLength - 2);
				end else
				begin
					//Usually our string messages, where the 2nd location is the
					//size holder of the string
					RecvBuffer(AClient,InBuffer[2], 2);
					Size := BufferReadWord(2, InBuffer);
					RecvBuffer(AClient,InBuffer[4], Size - 4);
				end;

				//Execute the packet procedure, only one is runned because the other
				//runs a dummy procedure
				ExecCommand(AChara,InBuffer,ReadPoints);
				ExecAvoidSelfCommand(AChara);
				Result := True;
			end;
		end;
		if Result then break;//Breaks loop if packet found.
	end;
end;{SearchPacketListing}
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
	ClientBaseIndex : Word; //Index of the packet in the packet(allowed client)
				//database (client-base).
	PacketID        : Word; //The ID of a packet in said database.
	PacketIndex     : Integer;
	Found           : Boolean;
	ABuffer         : TBuffer;
Begin
	AClient.Connection.IOHandler.CheckForDataOnSource();
	while AClient.Connection.IOHandler.InputBuffer.Size >= 2 do
	begin
		Lth := AClient.Connection.IOHandler.InputBuffer.Size;
		RecvBuffer(AClient,ABuffer[0], 2);
		PacketID := BufferReadWord(0, ABuffer);
		AChara := @TClientLink(AClient.Data).CharacterLink;

		Found := False;
		if NOT Assigned(AChara^) then
		begin
			for ClientBaseIndex := (Length(CodeBase) -1) downto 0 do
			begin //Go through all the client-bases w/ packets

				for PacketIndex := 0 to Length(CodeBase[ClientBaseIndex].Packet) - 1 do
				begin //Search for the packet from this client-base

					if (CodeBase[ClientBaseIndex].Packet[PacketIndex].ID = PacketID) then
					begin
						if (CodeBase[ClientBaseIndex].Packet[PacketIndex].PLength = Lth) then
						begin
							if (CodeBase[ClientBaseIndex].Packet[PacketIndex].Command = 'wanttoconnection') then
							begin
								RecvBuffer(AClient, ABuffer[2], (Lth - 2)); //Get the rest of the packet info
								MapConnect(ClientBaseIndex,
									AClient,
									ABuffer,
									CodeBase[ClientBaseIndex].Packet[PacketIndex].ReadPoints
								);
								Found := True;
								Break;
							end;
						end;
					end;
				end;

				if Found then
				begin
					Break;
				end;
			end;

			if NOT Found then
			begin
				//They can't get in, so prevent the rest of their useless packets from parsing
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
			if AChara.ClientVersion <> 0 then
			begin
				if not SearchPacketListing(AChara^, AClient, ABuffer, AChara.ClientVersion, PacketID) then
				begin //look for
					if NOT SearchPacketListing(AChara^, AClient, ABuffer, 0, PacketID) then
					begin
						Exit; //try the older
					end;
				end;
			end
			else if NOT SearchPacketListing(AChara^, AClient, ABuffer, 0, PacketID) then
			begin //since it's older, use that
				Exit;
			end;
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
	AMap        : TMap;
	MapNames    : TStringList;
	MapFileName : String;
Begin
	Console.WriteLn('      - Loading Maps...');

	{[2007/06/01] CR - Get List of Map Names from DB. }
	with ZoneLocalDatabase.StaticData do
	begin
		Connect;
		MapNames := GetMapsForZone(Options.ID);
		Disconnect;
	end;

	try
		Console.WriteLn('        : '+ IntToStr(MapNames.Count)+ ' Found!');

		for Index := 0 to (MapNames.Count - 1) do
		begin
			MapFileName := Format(
				FSTR_MAPFILE,
				[MainProc.Options.MapDirectory, MapNames[Index]]
			);
			if FileExists(MapFileName) then
			begin
				AMap := TMap.Create;
				if AMap.LoadFromFile(MapFileName) then
				begin
					AMap.Name := MapNames[Index];
					MapList.Add(AMap);
				end;
			end else begin
				Console.WriteLn('      - Map ' + MapFileName + ' does not exist!');
			end;
		end;
	finally
		MapNames.Free;
	end;
	Console.WriteLn('      - Maps Loaded!');
End; (* Proc TZoneServer.LoadMaps
*-----------------------------------------------------------------------------*)


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
Procedure TZoneServer.SetPort(Value : Word);
begin
	fPort := Value;
	TCPServer.DefaultPort := Value;
end;//SetPort
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
Function TZoneServer.GetStarted() : Boolean;
begin
	Result := TCPServer.Active;
end;{SetPort}
//------------------------------------------------------------------------------


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
			RecvBuffer(AClient,ABuffer[2],GetPacketLength($2101)-2);
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
			RecvBuffer(AClient,ABuffer[2],GetPacketLength($2107)-2);
			TotalOnlinePlayers := BufferReadWord(2,ABuffer);
		end;
	$2110:
		begin
			RecvBuffer(AClient,ABuffer[2],GetPacketLength($2110)-2);
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
			RecvBuffer(AClient,ABuffer[2],GetPacketLength($2101)-2);
			Response := BufferReadByte(2,ABuffer);

			//If validated.
			if Response = 0 then
			begin
				Console.Message('Verified with Inter Server, '+
					'sending details.', 'Zone Server', MS_NOTICE);
				SendZoneWANIPToInter(ToInterTCPClient,Self);
				SendZoneLANIPToInter(ToInterTCPClient,Self);
				SendZoneOnlineUsersToInter(ToInterTCPClient,Self);
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
			RecvBuffer(AClient,ABuffer[2],GetPacketLength($2212)-2);
			RecvWhisperReply(ABuffer);
		end;
	$2213:
		begin
			RecvBuffer(AClient,ABuffer[2],2);
			Size := BufferReadWord(2,ABuffer);
			RecvBuffer(AClient,ABuffer[4],Size-4);
			RecvMapWarpRequest(ABuffer);
		end;
	$2216:
		begin
			RecvBuffer(AClient,ABuffer[2],GetPacketLength($2216)-2);
			RecvAddFriendRequest(ABuffer);
		end;
	$2217:
		begin
			RecvBuffer(AClient,ABuffer[2],GetPacketLength($2217)-2);
			RecvAddFriendRequestReply(ABuffer);
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
