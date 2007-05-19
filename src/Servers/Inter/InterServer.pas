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

	Acts as the Inter-server that message passes and brokers between the Login,
Chara, and Zone(Game) servers.

[2007/05/19] CR - TODO: Someone with more knowledge of the overall workings
	should mention which tasks the InterServer is charged with.  What specific
	things is the InterServer responsible for?

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
		fClientList			 : TList;

		Commands				 : TGMCommands;
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

		procedure RecvGMCommand(
			AClient : TIdContext;
			InBuffer : TBuffer
		);

		Procedure RecvZoneWarpRequest(
			AClient : TIdContext;
			ABuffer : TBuffer
		);

		Procedure InterSendWarpReplyToZone(
			AClient				: TIdContext;
			CharacterID		: LongWord;
			ReturnIPCard	: LongWord;
			ReturnPort		: Word;
			MapName				: String;
			X							: Word;
			Y							: Word
		);

		procedure InterSendGMCommandToZones(
			AClient : TIdContext;
			GMID : LongWord;
			CharacterID : LongWord;
			CommandSeparator : TStringList
		);
		procedure RecvWhisper(
			AClient : TIdContext;
			InBuffer : TBuffer
		);
		procedure RecvWhisperReply(
			AClient : TIdContext;
			InBuffer : TBuffer
		);
		procedure RecvGMCommandReply(AClient : TIdContext; InBuffer : TBuffer);
		procedure ParseInterServ(AClient : TIdContext);

	public
		IPLongWord     : LongWord;
		ServerName    : String;

		Options : TInterOptions;

		Constructor Create;
		Destructor  Destroy;Override;
		Procedure   Start;
		Procedure   Stop;


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
	Character,
	Globals,
	Main,
	TCPServerRoutines,
	ZoneInterCommunication,
	WinLinux
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


(*- Procedure -----------------------------------------------------------------*
TInterServer.RecvGMCommandReply
--------------------------------------------------------------------------------
Overview:
--
	Gets a gm command from an authenticated zone server

--
Pre:
	TODO
Post:
	TODO

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/03/19] RaX - Created Header.
[2007/05/19] CR - Altered header, indent and style changes.  Use of new
	ClientList and ZoneServerLink property and a with clause to make code cleaner
	to read.
*-----------------------------------------------------------------------------*)
Procedure TInterServer.RecvGMCommandReply(
		AClient : TIdContext;
		InBuffer : TBuffer
	);
//See Notes/GM Command Packets for explanation.
Var
	ErrorLength : LongWord;
	CharacterID : LongWord;
	ACharacter  : TCharacter;
	ZoneID      : LongWord;
	ZoneLink    : TZoneServerLink;
	Index       : Integer;
	ListClient  : TIdContext;
	Size        : LongWord;

Begin
	//See Notes/GMCommand Packets.txt
	ErrorLength := BufferReadLongWord(12, InBuffer);
	if (ErrorLength > 0) then
	begin
		Size := BufferReadLongWord(2, InBuffer);
		CharacterID := BufferReadLongWord(10, InBuffer);

		with TThreadLink(AClient.Data).DatabaseLink do
		begin
			GameData.Connect;
			ACharacter := GameData.GetChara(CharacterID);
			GameData.Disconnect;

			StaticData.Connect;
			ZoneID := StaticData.GetMapZoneID(ACharacter.Map);
			StaticData.Disconnect;
		end;

		for Index := 0 to (fClientList.Count - 1) do
		begin
			ZoneLink := ZoneServerLink[Index];
			if (ZoneLink <> NIL) AND
				(ZoneLink.Info.ZoneID = ZoneID) then
			begin
				ListClient := ClientList[Index];
				SendBuffer(ListClient, InBuffer, Size);
				Break;
			end;
		end;//for

	end;
End; (* Func TInterServer.RecvGMCommandReply
*-----------------------------------------------------------------------------*)


(*- Procedure -----------------------------------------------------------------*
TInterServer.InterSendGMCommandToZones
--------------------------------------------------------------------------------
Overview:
--
	Sends the GM command to all connected zone servers.

{[2007/05/19] CR - TODO:
	(open to anyone more knowledgable of this routine than I am!)
	- Parameters are not yet explicitly var/const/in/out }

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/03/19] RaX - Created Header.
[2007/05/19] CR - Altered header, indent and style changes.  Extracted a local
	procedure to construct the packet ($2206), and slight optimizations made.
	Used ClientList and ZoneServerLink properties for clarity.
*-----------------------------------------------------------------------------*)
Procedure TInterServer.InterSendGMCommandToZones(
		AClient          : TIdContext;
		GMID             : LongWord;
		CharacterID      : LongWord;
		CommandSeparator : TStringList
	);
Var
	ABuffer     : TBuffer;
	CommandID   : Integer;
	Size        : Cardinal;

	(*- Local Procedure .................*
	WritePacket2206
	--
	[2007/05/19] CR - Extracted from main body.  Speedup: Using a local variable
		to store the CommandSeparator length to avoid 2 extra function calls.
	*...................................*)
	procedure WritePacket2206;
	var
		BufferIndex : Integer;
		CSLength    : Integer;
		Index       : Integer;
	begin
		WriteBufferWord(0, $2206,ABuffer);
		WriteBufferWord(4, CommandID, ABuffer);
		WriteBufferLongWord(6, GMID, ABuffer);
		WriteBufferLongWord(10, CharacterID, ABuffer);
		WriteBufferWord(14, CommandSeparator.Count - 1, ABuffer);
		BufferIndex := 16;//Where we are currently in the buffer.

		//We then write the Command's arguments to the buffer.
		for Index := 1 to CommandSeparator.Count - 1 do
		begin
			CSLength := Length(CommandSeparator[Index]);

			WriteBufferWord(BufferIndex, CSLength, ABuffer);
			Inc(BufferIndex, 2);

			WriteBufferString(
				BufferIndex,
				CommandSeparator[Index],
				CSLength,
				ABuffer
			);
			Inc(BufferIndex, CSLength);
		end;

		Size := BufferIndex + 1;
		WriteBufferWord(2, Size, ABuffer);
	end;(* WritePacket2206
	*...................................*)

Var
	Index      : Integer;
	ListClient : TIdContext;
Begin
	//Get the name of the command, remove the first character which is #
	CommandSeparator[0] := Commands.GetCommandName(CommandSeparator[0]);
	//Get the command ID to which the Command name is associated.
	CommandID := Commands.GetCommandID(CommandSeparator[0]);

	//Test if valid gm command.
	if (CommandID >= 0) then
	begin
		//start writing packet 2206 (see Notes/GM Command Packets.txt)
		WritePacket2206;

		//Then, after we're done building the buffer, we send it to all connected
		//zones. - incomplete
		{[2007/05/19] CR - Talking with RaX on IRC, simple commands work, but
		this is still incomplete -- more complex GM commands aren't working or
		are not yet verified. }

		for Index := 0 to (fClientList.Count - 1) do
		begin
			if Assigned(ZoneServerLink[Index]) then
			begin
				ListClient := ClientList[Index];
				SendBuffer(ListClient, ABuffer, Size);
			end;
		end;
	end;
End; (* Proc TInterServer.InterSendGMCommandToZones
*-----------------------------------------------------------------------------*)


(*- Procedure -----------------------------------------------------------------*
TInterServer.RecvWhisper
--------------------------------------------------------------------------------
Overview:
--
	Receives a whisper message from zone server and finds proper zone to direct
to.  If proper zone not found, an error is sent back.

--
Pre:
	TODO
Post:
	TODO

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/05/03] Aeomin - Created Header.
[2007/05/19] CR - Altered header, indent and style changes.  Use of with, and
	ZoneServerLink to clarify code.
*-----------------------------------------------------------------------------*)
Procedure TInterServer.RecvWhisper(
		AClient : TIdContext;
		InBuffer : TBuffer
	);
Var
	Size       : Word;
	FromID     : LongWord;
	FromName   : String;
	TargetName : String;
	Whisper    : String;
	Chara      : TCharacter;
	ZoneID     : LongWord;
	Index      : Integer;

	SentWhisper : Boolean;
Begin
	Size       := BufferReadWord(2, InBuffer);
	FromID     := BufferReadLongWord(4, InBuffer);
	FromName   := BufferReadString(8, 24, InBuffer);
	TargetName := BufferReadString(32, 24, InBuffer);
	Whisper    := BufferReadString(56, (Size - 56), InBuffer);

	with TZoneServerLink(AClient.Data).DatabaseLink.GameData do
	begin
		Connect;
		Chara := GetChara(TargetName);
		Disconnect;
	end;

	if (Chara <> NIL) AND (Chara.Map <> '') then
	begin
		with TZoneServerLink(AClient.Data).DatabaseLink.StaticData do
		begin
			Connect;
			ZoneID := GetMapZoneID(Chara.Map);
			Disconnect;
		end;

		Index := (fClientList.Count -1);
		SentWhisper := False;

		if (Index > -1) then
		begin
			repeat
				if Assigned(ZoneServerLink[Index]) AND
					(ZoneServerLink[Index].Info.ZoneID = ZoneID) then
				begin
					RedirectWhisperToZone(
						ClientList[Index],
						TZoneServerLink(AClient.Data).Info.ZoneID,
						FromID,
						Chara.CID,
						FromName,
						Whisper
					);
					SentWhisper := True;
				end;

				Dec(Index);
			until (SentWhisper) OR (Index = -1);
		end;


		//This will fire if can't find any..WHY THE LIST DONT HAVE INDEXOF?
		if NOT SentWhisper then
		begin
			//Well..if zone is disconnected..then just say not online
			SendWhisperReplyToZone(AClient, FromID, 1);
			{[2007/05/19] CR - What does the "1" signify? Shouldn't this be a
			descriptive constant? }
		end;
	end else
	begin
		SendWhisperReplyToZone(AClient, FromID, 1);  //Target is not online
	end;
End; (* Func TInterServer.RecvWhisper
*-----------------------------------------------------------------------------*)


(*- Procedure -----------------------------------------------------------------*
TInterServer.RecvWhisperReply
--------------------------------------------------------------------------------
Overview:
--
	Receives whisper result from zone, usually is 1 (which is character not
online).

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/05/03] Aeomin - Created Header.
[2007/05/19] CR - Alterd Comment Header, indent and style changes.  Used
	ZoneServerLink and ClientLink properties to make code cleaner to read.
*-----------------------------------------------------------------------------*)
Procedure TInterServer.RecvWhisperReply(
		AClient  : TIdContext;
		InBuffer : TBuffer
	);
Var
	ZoneID : LongWord;
	CharID : Integer;
	Flag   : Byte;
	Index  : Integer;
Begin
	ZoneID := BufferReadLongWord(2, InBuffer);
	CharID := BufferReadLongWord(6, InBuffer);
	Flag   := BufferReadByte(10, InBuffer);

	for Index := (fClientList.Count - 1) downto 0 do
	begin
		if Assigned(ZoneServerLink[Index]) AND
			(ZoneServerLink[Index].Info.ZoneID = ZoneID) then
		begin
			SendWhisperReplyToZone(ClientList[Index], CharID, Flag);
			Break;
		end;
	end;
End; (* Proc TInterServer.RecvWhisperReply
*-----------------------------------------------------------------------------*)


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


(*- Procedure -----------------------------------------------------------------*
TInterServer.RecvZoneWarpRequest
--------------------------------------------------------------------------------
Overview:
--
	Receives a zone's warp request, figures out what zone the map is on,
and replies.


--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/04/27] RaX - Created.
[2007/05/19] CR - Altered Comment Header.  Indent and style changes, with clause
	added to declutter a database value read.
*-----------------------------------------------------------------------------*)
Procedure TInterServer.RecvZoneWarpRequest(
		AClient : TIdContext;
		ABuffer : TBuffer
	);
Var
	CharacterID		: LongWord;
	X             : Word;
	Y             : Word;
	MapNameSize   : Word;
	MapName       : String;
	ClientIPSize  : Word;
	ClientIP      : String;
	ZoneID        : Integer;
	ZServerInfo   : TZoneServerInfo;
	ReturnIPCard  : LongWord;
Begin
	CharacterID := BufferReadLongWord(4, ABuffer);
	X						:= BufferReadWord(8, ABuffer);
	Y						:= BufferReadWord(10, ABuffer);
	MapNameSize := BufferReadWord(12, ABuffer);
	MapName			:= BufferReadString(14, MapNameSize, ABuffer);

	ClientIPSize := BufferReadWord(14+MapNameSize, ABuffer);
	ClientIP		 := BufferReadString(16+MapNameSize, ClientIPSize, ABuffer);

	with TThreadLink(AClient.Data).DatabaseLink.StaticData do
	begin
		Connect;
		ZoneID := GetMapZoneID(MapName);
		Disconnect;
	end;

	//Why warp to a unknown zone..or reply to it.  Kill it here. They can walk fine
	if (ZoneID <> -1) then
	begin
		ZServerInfo		:= Self.ZoneServerInfo[fZoneServerList.IndexOf(ZoneID)];
		ReturnIPCard	:= ZServerInfo.Address(ClientIP);
		InterSendWarpReplyToZone(
			AClient,
			CharacterID,
			ReturnIPCard,
			ZServerInfo.Port,
			MapName,
			X,
			Y
		);
	end;
End; (* Proc TInterServer.RecvZoneWarpRequest
*-----------------------------------------------------------------------------*)

//------------------------------------------------------------------------------
//InterSendWarpReplyToZone                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Tells the zone server to approve the character's warp.
//
//	Changes -
//		April 26th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TInterServer.InterSendWarpReplyToZone(
	AClient				: TIdContext;
	CharacterID		: LongWord;
	ReturnIPCard	: LongWord;
	ReturnPort		: Word;
	MapName				: String;
	X							: Word;
	Y							: Word
);
var
	Size					: Word;
	MapNameSize		: Word;
	ABuffer				: TBuffer;

begin
	MapNameSize := Length(MapName);
	Size := MapNameSize + 20;
	//<id>,<size>,<charaid>,<ip>,<port>,<x>,<y>,<mapnamesize><mapname>
	WriteBufferWord(0, $2209, ABuffer);
	WriteBufferWord(2, Size, ABuffer);
	WriteBufferLongWord(4, CharacterID, ABuffer);
	WriteBufferLongWord(8, ReturnIPCard, ABuffer);
	WriteBufferWord(12, ReturnPort, ABuffer);
	WriteBufferWord(14, X, ABuffer);
	WriteBufferWord(16, Y, ABuffer);
	WriteBufferword(18, MapNameSize, ABuffer);
	WriteBufferString(20, MapName, MapNameSize, ABuffer);
	SendBuffer(AClient, ABuffer, Size);
end;
//------------------------------------------------------------------------------


end.
