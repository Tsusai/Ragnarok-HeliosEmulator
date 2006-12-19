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
  IdTCPClient,
  IdContext,
  PacketTypes,
  Character;

type
	TZoneServer = class
  protected
  //
	private
		fIP              : String;
    fPort            : Word;

    TCPServer        : TIdTCPServer;
    ToCharaTCPClient : TIdTCPClient;
    ToInterTCPClient : TIdTCPClient;

    Procedure OnExecute(AConnection: TIdContext);
    Procedure OnConnect(AConnection: TIdContext);

    procedure ProcessZonePacket(AClient : TIdContext);
    function  SearchPacketListing(var AChara : TCharacter; var AClient : TIdContext;
	                                var InBuffer :  TBuffer; const Version   : Word;
                                  const Packet    : Word  ) :Boolean;

		Procedure SetIPCardinal(Value : string);
    Procedure SetPort(Value : Word);

  public
		IPCardinal    : Cardinal;
		ServerName    : String;
		OnlineUsers   : Word;

		property IP   : string read fIP write SetIPCardinal;
    property Port : Word read fPort write SetPort;

    Constructor Create();
    Destructor  Destroy();Override;
    Procedure   Start();
    Procedure   Stop();
  end;
implementation

uses
	//Helios
	WinLinux,
  Console,
  CharaList,
  Database,
  DatabaseTXT,
  Socket,
  Account,
  GameConstants,
  Globals,
  TCPServerRoutines,
  ZoneRecv,
  //3rd
  SysUtils,
  List32;

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
Constructor TZoneServer.Create;
begin
  TCPServer := TIdTCPServer.Create;
  ToCharaTCPClient := TIdTCPClient.Create;
  ToInterTCPClient := TIdTCPClient.Create;

  TCPServer.OnConnect   := OnConnect;
  TCPServer.OnExecute   := OnExecute;
	TCPServer.OnException := MainProc.ServerException;

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
//Start()                                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		 Enables the zone server to accept incoming connections
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TZoneServer.Start();
begin
  TCPServer.DefaultPort := ServerConfig.ZonePort;
  ActivateServer('Zone',TCPServer);
  ActivateClient(ToCharaTCPClient);
  ActivateClient(ToInterTCPClient);
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
  DeActivateServer(TCPServer);
  DeActivateClient(ToCharaTCPClient);
  DeActivateClient(ToInterTCPClient);
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
//    Old Comments Follow...
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
	while AClient.Connection.IOHandler.InputBuffer.Size >= 2 do
	begin
		Lth := AClient.Connection.IOHandler.InputBuffer.Size;
		RecvBuffer(AClient,ABuffer[0], 2);
		PacketID := BufferReadWord(0, ABuffer);
		AChara := TThreadLink(AClient.Data).CharacterLink;

		{if (AChara <> nil) and (Option_Packet_Out) then
		begin
			Console(Format('3:%.8d CMD %.4x', [AChara.ID, PacketID]));
		end;}

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
				MainProc.Console('Someone with the IP '+
					AClient.Connection.Socket.Binding.PeerIP +
					' attempted to send a packet '+ IntToHex(packetID, 4) +' with a length of ' + IntToStr(Lth));
				MainProc.Console('Reason for this response: Unsupported client or a bot attempted to connect.');
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
//SetIPCardinal   			                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      The Ragnarok client does not connect to a server using the plain x.x.x.x
//    IP string format.  It uses a cardinal form.  Making the IP a property, we
//    are able to call a function to go ahead and set the Cardinal form at any
//    time.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TZoneServer.SetIPCardinal(Value : string);
begin
	fIP         := GetIPStringFromHostname(Value);
	IPCardinal  := GetCardinalFromIPString(fIP);
end; //proc SetIPCardinal
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
end.
