//------------------------------------------------------------------------------
//InterServer			                                                        UNIT
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
	{3rd Party}
	IdContext,
	IdTCPServer,
	List32
	;


type
	TInterServer = class
	protected
	//
	private
		fIP              : String;
		fPort            : Word;

		fZoneServerList	 : TIntList32;
		fClientList			 : TList;

		Commands				 : TGMCommands;
		TCPServer        : TIdTCPServer;

		Procedure OnDisconnect(AConnection: TIdContext);
		Procedure OnException(AConnection: TIdContext;
			AException: Exception);
		procedure OnConnect(AConnection: TIdContext);
		//procedure ProcessInterPacket(AClient : TIdContext);

		Procedure SetIPLongWord(Value : string);
    Procedure SetPort(Value : Word);
		Function GetStarted() : Boolean;

		Procedure LoadOptions;

		procedure VerifyZoneServer(
			AClient		: TIdContext;
			InBuffer	: TBuffer
		);

		procedure RecvGMCommand(
			AClient : TIdContext;
			InBuffer : TBuffer
		);
		procedure InterSendGMCommandToZones(
			AClient : TIdContext;
			GMID : LongWord;
			CharacterID : LongWord;
			CommandSeparator : TStringList
		);
		procedure RecvGMCommandReply(AClient : TIdContext; InBuffer : TBuffer);
		procedure ParseInterServ(AClient : TIdContext);

	public
		IPLongWord     : LongWord;
		ServerName    : String;

		Options : TInterOptions;

		property IP   : string read fIP write SetIPLongWord;
		property Port : Word read fPort write SetPort;
		property Started : Boolean read GetStarted;

		Constructor Create();
		Destructor  Destroy();Override;
		Procedure   Start();
		Procedure   Stop();
	end;


implementation


uses
	{RTL/VCL}
	StrUtils,
	{Project}
	//none
	BufferIO,
	Character,
	Globals,
	Main,
	TCPServerRoutines,
	ZoneInterCommunication,
	ZoneServerInfo
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
    ActivateServer('Inter',TCPServer);
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
//RecvGMCommand   			                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      Gets a gm command from an authenticated zone server
//
//	Changes -
//		March 19th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TInterServer.RecvGMCommand(AClient : TIdContext; InBuffer : TBuffer);
//See Notes/GM Command Packets for explanation.
var
	GMID						: LongWord;
	CharaID					: LongWord;
	CommandLength		: LongWord;
	CommandString		: String;
	CommandSeparator: TStringList;

begin
	//See Notes/GMCommand Packets.txt
	GMID						:= BufferReadLongWord(4, InBuffer);
	CharaID					:= BufferReadLongWord(8, InBuffer);
	CommandLength		:= BufferReadWord(12,InBuffer);
	CommandString		:= BufferReadString(14, CommandLength, InBuffer);

	CommandSeparator := TStringList.Create;
	CommandSeparator.Delimiter := ',';
	CommandSeparator.DelimitedText := CommandString;

	//after getting the command information, we get ready to send it to the other
	//zones.
	InterSendGMCommandToZones(AClient, GMID, CharaID, CommandSeparator);

	CommandSeparator.Free;
end; //RecvGMCommand
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvGMCommand   			                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      Gets a gm command from an authenticated zone server
//
//	Changes -
//		March 19th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TInterServer.RecvGMCommandReply(AClient : TIdContext; InBuffer : TBuffer);
//See Notes/GM Command Packets for explanation.
var
	ErrorLength		: LongWord;
	CharacterID		: LongWord;
	ACharacter		: TCharacter;
	ZoneID				: LongWord;
	Index					: Integer;
	ListClient		: TIdContext;
	Size					: LongWord;

begin
	//See Notes/GMCommand Packets.txt
	ErrorLength		:= BufferReadLongWord(12,InBuffer);
	if ErrorLength > 0 then
	begin
		Size := BufferReadLongWord(2, InBuffer);
		CharacterID := BufferReadLongWord(10, InBuffer);

		TThreadLink(AClient.Data).DatabaseLink.GameData.Connect;
		ACharacter := TThreadLink(AClient.Data).DatabaseLink.GameData.GetChara(CharacterID);
		TThreadLink(AClient.Data).DatabaseLink.GameData.Disconnect;

		TThreadLink(AClient.Data).DatabaseLink.StaticData.Connect;
		ZoneID := TThreadLink(AClient.Data).DatabaseLink.StaticData.GetMapZoneID(ACharacter.Map);
		TThreadLink(AClient.Data).DatabaseLink.StaticData.Disconnect;

		for Index := 0 to fClientList.Count - 1 do
		begin
			if assigned(TZoneServerLink(TIdContext(fClientList[Index]).Data)) then
			begin
				if TZoneServerLink(TIdContext(fClientList[Index]).Data).Info.ZoneID = ZoneID then
				begin
					ListClient := TIdContext(fClientList[Index]);
					SendBuffer(ListClient, InBuffer, Size);
					Break;
				end;
			end;
		end;
	end;
end; //RecvGMCommand
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//InterSendGMCommandToZones                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      Sends the gm command to all connected zone servers.
//
//	Changes -
//		March 19th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TInterServer.InterSendGMCommandToZones(AClient : TIdContext; GMID : LongWord; CharacterID : LongWord; CommandSeparator : TStringList);
var
	Index			: Integer;
	CommandID	: Integer;
	ABuffer		: TBuffer;
	Size			: Cardinal;
	BufferIndex: Integer;
	ListClient : TIdContext;
begin
  //Get the name do the command, remove the first character which is #
	CommandSeparator[0] := Commands.GetCommandName(CommandSeparator[0]);
  //Get the command ID to which the Command name is associated.
	CommandID := Commands.GetCommandID(CommandSeparator[0]);
  //If the command is a real gm command.
	if CommandID >= 0 then
	begin
    //start writing packet 2206 (see Notes/GM Command Packets.txt)
		WriteBufferWord(0, $2206,ABuffer);
		WriteBufferWord(4, CommandID, ABuffer);
		WriteBufferLongWord(6, GMID, ABuffer);
		WriteBufferLongWord(10, CharacterID, ABuffer);
		WriteBufferWord(14, CommandSeparator.Count - 1, ABuffer);
		BufferIndex := 16;//Where we are currently in the buffer.
    //We then write the Command's arguments to the buffer.
		for Index := 1 to CommandSeparator.Count - 1 do
		begin
			WriteBufferWord(BufferIndex, Length(CommandSeparator[Index]), ABuffer);
			inc(BufferIndex, 2);
			WriteBufferString(BufferIndex, CommandSeparator[Index], Length(CommandSeparator[Index]), ABuffer);
			inc(BufferIndex, Length(CommandSeparator[Index]));
		end;
    //Then, after we're done building the buffer, we send it to all connected
    //zones. - incomplete
		Size := BufferIndex+1;
		WriteBufferWord(2, Size, ABuffer);
		for Index := 0 to fClientList.Count - 1 do
		begin
			if assigned(TZoneServerLink(TIdContext(fClientList[Index]).Data)) then
			begin
				ListClient := fClientList[Index];
				SendBuffer(ListClient, ABuffer, Size);
			end;
		end;
	end;
end; //RecvGMCommand
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
Procedure TInterServer.SetPort(Value : Word);
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
Function TInterServer.GetStarted() : Boolean;
begin
  Result := TCPServer.Active;
end;{SetPort}
//------------------------------------------------------------------------------
end.
