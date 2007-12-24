//------------------------------------------------------------------------------
//InterServer                                                               UNIT
//------------------------------------------------------------------------------
//	What it does-
//      The Inter Server.
//    Contains the brunt of the Inter and packet processes.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//		[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
//
//------------------------------------------------------------------------------
unit InterServer;


interface


uses
	{RTL/VCL}
	Classes,
	SysUtils,
	{Project}
	GMCommands,
	InterOptions,
	PacketTypes,
	ZoneServerInfo,
	{3rd Party}
	IdContext,
	IdTCPServer,
	List32
	;


type
(*= CLASS =====================================================================*
TInterServer

*------------------------------------------------------------------------------*
Overview:
*------------------------------------------------------------------------------*

	Acts as the Inter-server that message passes and brokers between Zone(Game) servers.
	It handles anything that communicates between zones, IE:: Whisper chat, guild
	chat, party chat, GM Commands, etc.

[2007/05/19] CR - TODO: Someone with more knowledge of the overall workings
	should mention which tasks the InterServer is charged with.  What specific
	things is the InterServer responsible for?
[2007/05/21] RaX - Added small description addition.

*------------------------------------------------------------------------------*
Revisions:
*------------------------------------------------------------------------------*
(Format: [yyyy/mm/dd] <Author> - <Description of Change>)
[2007/05/19] CR - Added class header.  Removed private section - all private
	fields and methods are now protected.  Added properties ClientList,
	ZoneServerInfo, and ZoneServerLink to encapsulate the lookup and casting
	dirty work to shield it from other TInterServer routines.  Lends clarity to
	code that used to do the casting work in-situ.
*=============================================================================*)
	TInterServer = class(TObject)
	protected
		fIP              : String;
		fPort            : Word;
		fZoneServerList	 : TIntList32;

		TCPServer        : TIdTCPServer;

		Procedure OnDisconnect(AConnection: TIdContext);
		Procedure OnException(AConnection: TIdContext;
			AException: Exception);
		procedure OnConnect(AConnection: TIdContext);
		//procedure ProcessInterPacket(AClient : TIdContext);

		Procedure SetIPLongWord(Value : string);

		Procedure SetPort(
			const
				Value : Word
			);

		Function  GetClientList(
			const
				Index : Integer
			) : TIdContext;

		Function  GetStarted : Boolean;

		Function  GetZoneServerInfo(
			const
				Index : Integer
			) : TZoneServerInfo;

		Function  GetZoneServerLink(
			const
				Index : Integer
			) : TZoneServerLink;


		Procedure LoadOptions;

		procedure VerifyZoneServer(
			AClient		: TIdContext;
			InBuffer	: TBuffer
		);

		procedure ParseInterServ(AClient : TIdContext);

	public
		IPLongWord     : LongWord;
		ServerName    : String;
		Commands				 : TGMCommands;
		fClientList			 : TList;
		Options : TInterOptions;

		Constructor Create;
		Destructor  Destroy;Override;
		Procedure   Start;
		Procedure   Stop;

		property ZoneServerList : TIntList32 read fZoneServerList;
		property IP   : String
			read  fIP
			write SetIPLongWord;

		property ClientList[const Index : Integer] : TIdContext
			read  GetClientList;

		property Port : Word
			read  fPort
			write SetPort;

		property Started : Boolean
			read  GetStarted;

		property ZoneServerInfo[const Index : Integer] : TZoneServerInfo
			read  GetZoneServerInfo;

		{[2007/05/19] CR - N.B.: This is the link held in the fClientList NOT the
		objects held in fZoneServerList! }
		property ZoneServerLink[const Index : Integer] : TZoneServerLink
			read  GetZoneServerLink;

	End;(* TInterServer
*== CLASS ====================================================================*)


implementation


uses
	{RTL/VCL}
	StrUtils,
	{Project}
	//none
	BufferIO,
	Globals,
	Main,
	TCPServerRoutines,
	ZoneInterCommunication,
	InterRecv
	{3rd Party}
	//none
	;


//------------------------------------------------------------------------------
//Create  ()                                                        CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our inter server
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Constructor TInterServer.Create;
begin
	Inherited;
	Commands := TGMCommands.Create;
	TCPServer := TIdTCPServer.Create;
	TCPServer.OnExecute   := ParseInterServ;
	TCPServer.OnConnect		:= OnConnect;
	TCPServer.OnException := OnException;
	TCPServer.OnDisconnect:= OnDisconnect;

	fZoneServerList := TIntList32.Create;
	fClientList := TList.Create;
end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy()                                                        DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys our inter server
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Destructor TInterServer.Destroy;
begin
	TCPServer.Free;
	fZoneServerList.Free;
	fClientList.Free;
	Commands.Free;
	Inherited;
end;{Destroy}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//OnConnect                                                              EVENT
//------------------------------------------------------------------------------
//	What it does-
//			An event which fires when a zone server connects to the inter.
//
//	Changes -
//		March 18th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TInterServer.OnConnect(AConnection: TIdContext);
begin
	fClientList.Add(AConnection);
end;{OnDisconnect}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//OnDisconnect()                                                              EVENT
//------------------------------------------------------------------------------
//	What it does-
//			An event which fires when a zone server disconnects from the inter.
//
//	Changes -
//		November 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TInterServer.OnDisconnect(AConnection: TIdContext);
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
		end;
	end;
	fClientList.Delete(fClientList.IndexOf(AConnection));
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
procedure TInterServer.OnException(AConnection: TIdContext;
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
//		 Enables the Inter server to accept incoming connections
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TInterServer.Start();
begin
	if NOT Started then
	begin
		LoadOptions;

		Port := Options.Port;
		ActivateServer('Inter',TCPServer, Options.IndySchedulerType, Options.IndyThreadPoolSize);
	end else
	begin
		Console.Message('Cannot Start():: Inter Server already running!', 'Inter Server', MS_ALERT);
	end;
end;{Start}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Stop()                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		 Stops the Inter server from accepting incoming connections
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TInterServer.Stop();
begin
	if Started then
	begin
		DeActivateServer('Inter',TCPServer);
		Options.Save;
		Options.Free;
	end else
	begin
		Console.Message('Cannot Start():: Inter Server not running.', 'Inter Server', MS_ALERT);
	end;
end;{Start}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ParseInterServ                                                      PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//        Root procedure to handling client connections to the Inter Server.
//
//	Changes -
//			March 18th, 2007 - RaX - Created.
//			April 26th, 2007 - Aeomin - added packet 0x2208 support
//
//------------------------------------------------------------------------------
procedure TInterServer.ParseInterServ(AClient : TIdContext);
var
	ABuffer       : TBuffer;
	PacketID      : Word;
	Size          : Word;
begin
	RecvBuffer(AClient,ABuffer,2);
	PacketID := BufferReadWord(0, ABuffer);

	case PacketID of
	$2200: // Zone Server Connection request
		begin
			RecvBuffer(AClient,ABuffer[2],GetPacketLength($2200)-2);
			VerifyZoneServer(AClient,ABuffer);
		end;
	$2202: // Zone Server sending new WAN location details
		begin
			if AClient.Data is TZoneServerLink then
			begin
				RecvBuffer(AClient,ABuffer[2],2);
				Size := BufferReadWord(2,ABuffer);
				RecvBuffer(AClient,ABuffer[4],Size-4);
				TZoneServerLink(AClient.Data).Info.WAN := BufferReadString(4,Size-4,ABuffer);
				Console.Message('Received updated Zone Server WANIP.', 'Inter Server', MS_NOTICE);
			end;
		end;
	$2203: // Zone Server sending new LAN location details
		begin
			if AClient.Data is TZoneServerLink then
			begin
				RecvBuffer(AClient,ABuffer[2],2);
				Size := BufferReadWord(2,ABuffer);
				RecvBuffer(AClient,ABuffer[4],Size-4);
				TZoneServerLink(AClient.Data).Info.LAN := BufferReadString(4,Size-4,ABuffer);
				Console.Message('Received updated Zone Server LANIP.', 'Inter Server', MS_NOTICE);
			end;
		end;
	$2204: // Zone Server sending new Online User count
		begin
			if AClient.Data is TZoneServerLink then
			begin
				RecvBuffer(AClient,ABuffer[2],GetPacketLength($2204)-2);
				//TZoneServerLink(AClient.Data).Info.OnlineUsers := BufferReadWord(2,ABuffer);
				Console.Message('Received updated Zone Server Online Users.', 'Inter Server', MS_NOTICE);
			end;
		end;
	$2205: // Zone server sending GM command to be sent to other servers + self
		begin
			if AClient.Data is TZoneServerLink then
			begin
				RecvBuffer(AClient,ABuffer[2],2);
				Size := BufferReadWord(2,ABuffer);
				RecvBuffer(AClient,ABuffer[4],Size-4);
				RecvGMCommand(AClient, ABuffer);
			end;
		end;
	$2207: // Zone server sending GM command result
		begin
			if AClient.Data is TZoneServerLink then
			begin
				RecvBuffer(AClient,ABuffer[2],2);
				Size := BufferReadWord(2,ABuffer);
				RecvBuffer(AClient,ABuffer[4],Size-4);
				RecvGMCommandReply(AClient, ABuffer);
			end;
		end;
	$2208: // Zone server sending Warp Request
		begin
			if AClient.Data is TZoneServerLink then
			begin
				RecvBuffer(AClient,ABuffer[2],2);
				Size := BufferReadWord(2,ABuffer);
				RecvBuffer(AClient,ABuffer[4],Size-4);
				RecvZoneWarpRequest(AClient, ABuffer);
			end;
		end;
	$2210: //Zone Server send Private Message
		begin
			if AClient.Data is TZoneServerLink then
			begin
				RecvBuffer(AClient,ABuffer[2],2);
				Size := BufferReadWord(2,ABuffer);
				RecvBuffer(AClient,ABuffer[4],Size-4);
				RecvWhisper(AClient, ABuffer);
			end;
		end;
	$2211:
		begin
			if AClient.Data is TZoneServerLink then
			begin
				RecvBuffer(AClient,ABuffer[2],GetPacketLength($2211)-2);
				RecvWhisperReply(AClient, ABuffer);
			end;
		end;
	$2213:
		begin
			if AClient.Data is TZoneServerLink then
			begin
				RecvBuffer(AClient,ABuffer[2],2);
				Size := BufferReadWord(2,ABuffer);
				RecvBuffer(AClient,ABuffer[4],Size-4);
				RecvZoneMapWarpRequest(AClient, ABuffer);
			end;
		end;
	$2214:
		begin
			if AClient.Data is TZoneServerLink then
			begin
				RecvBuffer(AClient,ABuffer[2],2);
				Size := BufferReadWord(2,ABuffer);
				RecvBuffer(AClient,ABuffer[4],Size-4);
				RecvZoneMapWarpResult(AClient, ABuffer);
			end;
		end;
	$2215:
		begin
			if AClient.Data is TZoneServerLink then
			begin
				RecvBuffer(AClient,ABuffer[2],GetPacketLength($2215)-2);
				RecvZoneRequestFriend(AClient, ABuffer);
			end;
		end;
	$2217:
		begin
			if AClient.Data is TZoneServerLink then
			begin
				RecvBuffer(AClient,ABuffer[2],GetPacketLength($2217)-2);
				RecvZoneRequestFriendReply(AClient, ABuffer);
			end;
		end;
	else
		begin
			Console.Message('Unknown Inter Server Packet : ' + IntToHex(PacketID,4), 'Inter Server', MS_WARNING);
		end;
	end;
end; {ParseCharaInterServ}
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
Procedure TInterServer.LoadOptions;
begin
	Options    := TInterOptions.Create(MainProc.Options.ConfigDirectory+'/Inter.ini');
	Options.Load;
end;{LoadOptions}
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
//		March 21st, 2007 - RaX - Copied here from Character server
//
//------------------------------------------------------------------------------
procedure TInterServer.VerifyZoneServer(
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
		AClient.Binding.PeerIP, 'Inter Server', MS_NOTICE
	);
	ID := BufferReadLongWord(2,InBuffer);
	Password := BufferReadMD5(8,InBuffer);

	if (fZoneServerList.IndexOf(ID) > -1) then
	begin
		Console.Message('Zone Server failed verification. ID already in use.', 'Inter Server', MS_WARNING);
		Validated := 1;
	end;

	if (Password <> GetMD5(Options.Key)) then
	begin
		Console.Message('Zone Server failed verification. Invalid Security Key.', 'Inter Server', MS_WARNING);
		Validated := 2;
	end;

	if Validated = 0 then
	begin
		Console.Message('Zone Server connection validated.','Inter Server', MS_INFO);

		ZServerInfo :=  TZoneServerInfo.Create;
		ZServerInfo.Connection := AClient;
		ZServerInfo.ZoneID := ID;
		ZServerInfo.Port := BufferReadWord(6,InBuffer);
		AClient.Data := TZoneServerLink.Create(AClient);
		TZoneServerLink(AClient.Data).Info := ZServerInfo;
		fZoneServerList.AddObject(ZServerInfo.ZoneID,ZServerInfo);
	end;
	SendValidateFlagToZone(AClient,Validated);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetIPLongWord   			                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      The Ragnarok client does not connect to a server using the plain x.x.x.x
//    IP string format.  It uses a LongWord form.  Making the IP a property, we
//    are able to call a function to go ahead and set the LongWord form at any
//    time.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TInterServer.SetIPLongWord(Value : string);
begin
	//fIP         := GetIPStringFromHostname(Value);
	//IPLongWord  := GetLongWordFromIPString(fIP);
end; //proc SetIPLongWord
//------------------------------------------------------------------------------


(*- Procedure -----------------------------------------------------------------*
TInterServer.SetPort
--------------------------------------------------------------------------------
Overview:
--

	Sets the internal fPort variable to the value specified. Also sets the
TCPServer's port.

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/12/17] RaX - Created Header.
[2007/05/19] CR - Altered Comment Header.  Changed behavior so that fPort and
	TCPServer.DefaultPort are only altered IFF Value is different from fPort.
	Value parameter made constant.
*-----------------------------------------------------------------------------*)
Procedure TInterServer.SetPort(
	const
		Value : Word
	);
Begin
	if (Value <> fPort) then
	begin
		fPort := Value;
		TCPServer.DefaultPort := Value;
	end;
End; (* Proc TInterServer.SetPort
*-----------------------------------------------------------------------------*)


(*- Function ------------------------------------------------------------------*
TInterServer.GetClientList
--------------------------------------------------------------------------------
Overview:
--
	Does the dirty work casting the TList entry to TIdContext.

	Returns the given TIdContext stored at Index in fClientList

--
Pre:
	Index must be within range 0..Count-1
Post:
	TODO

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/05/19] CR - Created routine.  Property read handler for ClientList.
*-----------------------------------------------------------------------------*)
Function TInterServer.GetClientList(
	const
		Index : Integer
	): TIdContext;
Begin
	//Pre
	Assert(
		(Index >= 0) AND (Index < fClientList.Count),
		'Index is out of bounds.'
	);
	//--

	Result := TIdContext(fClientList[Index]);
End;(* Func TInterServer.GetClientList
*-----------------------------------------------------------------------------*)


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
Function TInterServer.GetStarted : Boolean;
Begin
	Result := TCPServer.Active;
End;{SetPort}
//------------------------------------------------------------------------------


(*- Function ------------------------------------------------------------------*
TInterServer.GetZoneServerInfo
--------------------------------------------------------------------------------
Overview:
--
	Does the dirty work of casting the object held in the TZoneServerList

--
Pre:
	Index must be in range of the fZoneServerList count.
Post:
	TODO

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/05/19] CR - Created routine.  Property read handler for ZoneServerInfo.
*-----------------------------------------------------------------------------*)
Function TInterServer.GetZoneServerInfo(
	const
		Index : Integer
	) : TZoneServerInfo;
Begin
	//Pre
	Assert(
		(Index >= 0) AND (Index < fZoneServerList.Count),
		'ZoneServerInfo Index out of bounds.'
	);
	//--

	Result := TZoneServerInfo(fZoneServerList.Objects[Index]);
End;(* Func TInterServer.GetZoneServerInfo
*-----------------------------------------------------------------------------*)


(*- Function ------------------------------------------------------------------*
TInterServer.GetZoneServerLink
--------------------------------------------------------------------------------
Overview:
--
	Does the dirty work of casting for our Client List entries, so that our
calling code for ZoneServerLink[] is cleaner, and that both are cleaner than
all the casting to dig and get this object reference from the fClientList.

Returns the given TZoneServerLink associated with fClientLink at Index.

--
Pre:
	Index must be in range of 0..fClientLink.Count-1
Post:
	TODO

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/05/19] CR - Created routine.  Property Read handler for ZoneServerLink.
*-----------------------------------------------------------------------------*)
Function TInterServer.GetZoneServerLink(
	const
		Index : Integer
	) : TZoneServerLink;
Begin
	//Pre
	Assert(
		(Index >= 0) AND (Index < fClientList.Count),
		'ClientList Index out of bounds.'
	);
	//--

	Result := TZoneServerLink(TIdContext(fClientList[Index]).Data);
End; (* Func TInterServer.GetZoneServerLink
*-----------------------------------------------------------------------------*)

end.
