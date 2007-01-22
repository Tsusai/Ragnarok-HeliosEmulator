//------------------------------------------------------------------------------
//ZoneServer			                                                        UNIT
//------------------------------------------------------------------------------
//	What it does-
//      The Zone Server.
//    Contains the brunt of the zone and packet processes.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit ZoneServer;
interface

uses
	IdTCPServer,
	CommClient,
	IdContext,
	SysUtils,
	PacketTypes,
	Character,
	ZoneOptions,
  Classes,
  MapList;

type
	TZoneServer = class
	protected
	//
	private

		fPort            : Word;

		TCPServer        : TIdTCPServer;
		ToCharaTCPClient : TInterClient;
		ToInterTCPClient : TInterClient;

		Procedure OnExecute(AConnection: TIdContext);
		Procedure OnConnect(AConnection: TIdContext);
		Procedure OnException(AConnection: TIdContext;
			AException: Exception);

		Procedure CharaClientOnConnect(Sender : TObject);
		Procedure CharaClientRead(AClient : TInterClient);

		Procedure ProcessZonePacket(AClient : TIdContext);
		Function  SearchPacketListing(var AChara : TCharacter; var AClient : TIdContext;
																	var InBuffer :  TBuffer; const Version   : Word;
																	const Packet    : Word  ) :Boolean;

		Procedure SetPort(Value : Word);
		Function GetStarted() : Boolean;

		Procedure LoadOptions;
	public
		WANIP : string;
		LANIP : string;

		OnlineUsers   : Word;

		Options       : TZoneOptions;

    MapList       : TMapList;

		property Started : Boolean read GetStarted;
		property Port : Word read fPort write SetPort;

		Constructor Create();
		Destructor  Destroy();Override;
		Procedure   Start(Reload : Boolean = FALSE);
		Procedure   Stop();
	end;
implementation

uses
	//Helios
	BufferIO,
	WinLinux,
	Console,
	PacketDB,
	Globals,
  Map,
	TCPServerRoutines,
	ZoneCharaPackets,
	ZoneRecv,
	//3rd
	StrUtils;

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
  MapList := TMapList.Create(TRUE);

	TCPServer := TIdTCPServer.Create;
	ToCharaTCPClient := TInterClient.Create('Zone','Character');
	ToInterTCPClient := TInterClient.Create('Zone','Inter');

	ToCharaTCPClient.OnConnected := CharaClientOnConnect;
	ToCharaTCPClient.OnRecieve   := CharaClientRead;

	TCPServer.OnConnect   := OnConnect;
	TCPServer.OnExecute   := OnExecute;
	TCPServer.OnException := OnException;

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
Destructor TZoneServer.Destroy;
begin
	TCPServer.Free;
	ToCharaTCPClient.Free;
	ToInterTCPClient.Free;
end;{Destroy}
//------------------------------------------------------------------------------


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
	AConnection.Data := TThreadLink.Create;
end;{OnConnect}
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

    //initialize commclient ips and ports.
	  ToCharaTCPClient.Host := Options.CharaIP;
	  ToCharaTCPClient.Port := Options.CharaPort;

    //Activate server and clients.
	  ActivateServer('Zone',TCPServer);
	  ActivateClient(ToCharaTCPClient);
	  //ActivateClient(ToInterTCPClient);
  end else
  begin
    MainProc.Console('Zone Server : Cannot Start():: Zone Server already running!');
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
    //deactivate server and clients.
	  DeActivateServer('Zone', TCPServer);
	  DeActivateClient(ToCharaTCPClient);
	  //DeActivateClient(ToInterTCPClient);

    //Clear MapList
    MapList.Clear;

    //Save and free options, options must be free'd here to force a reload after
    //start.
    Options.Save;
	  Options.Free;
  end else
  begin
    MainProc.Console('Zone Server : Cannot Stop():: Zone Server is not running!');
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
//------------------------------------------------------------------------------
Procedure TZoneServer.ProcessZonePacket(AClient : TIdContext);
Var
	Lth             : Integer;
	AChara          : TCharacter;
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
		AChara := TThreadLink(AClient.Data).CharacterLink;

		Found := False;
		if NOT Assigned(AChara) then
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
				MainProc.Console(
					'Someone with the IP '+
					AClient.Binding.PeerIP +
					' attempted to send a packet '+
					IntToHex(packetID, 4) +
					' with a length of ' + IntToStr(Lth)
				);
				MainProc.Console(
					'Reason for this response: Unsupported ' +
					'client or a bot attempted to connect.'
				);
				AClient.Connection.Disconnect;
			end;

		end else begin
			if AChara.ClientVersion <> 0 then
			begin
				if not SearchPacketListing(AChara, AClient, ABuffer, AChara.ClientVersion, PacketID) then
				begin //look for
					if NOT SearchPacketListing(AChara, AClient, ABuffer, 0, PacketID) then
					begin
						Exit; //try the older
					end;
				end;
			end
			else if NOT SearchPacketListing(AChara, AClient, ABuffer, 0, PacketID) then
			begin //since it's older, use that
				Exit;
			end;
		end;
	end;
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
	Options    := TZoneOptions.Create('./Zone.ini');
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
				MainProc.Console('Zone Server: Verified with Character Server, '+
					'sending details.');
				SendZoneWANIPToChara(ToCharaTCPClient,Self);
				SendZoneLANIPToChara(ToCharaTCPClient,Self);
				SendZoneOnlineUsersToChara(ToCharaTCPClient,Self);
			end else
			begin
				case Response of
				1 : MainProc.Console('Zone Server: Failed to verify with Character Server. ID already in use.');
				2 : MainProc.Console('Zone Server: Failed to verify with Character Server. Invalid security key.');
				end;
				MainProc.Console('Zone Server: Stopping...');
				Stop;
			end;
		end;
	end;
end;{CharaClientRead}
//------------------------------------------------------------------------------
end.
