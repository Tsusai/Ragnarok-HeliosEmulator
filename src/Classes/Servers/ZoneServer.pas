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
  PacketTypes;

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

    //procedure ProcessZonePacket(AClient : TIdContext);

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
  Character,
  CharaList,
  Database,
  Socket,
  Account,
  GameConstants,
  Globals,
  TCPServerRoutines,
  //3rd
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
	//ProcessZonePacket(AConnection);
end;{TMainProc.ZoneServerExecute}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//OnConnect()                                           EVENT
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
//Start()                                                          PROCEDURE
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
