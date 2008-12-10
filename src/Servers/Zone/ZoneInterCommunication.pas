//------------------------------------------------------------------------------
//ZoneInterCommunication	                                            UNIT
//------------------------------------------------------------------------------
//  What it does -
//      Houses our Zone<->Inter server communication routines. These are
//    used in ZoneServer.pas and also in InterServer.pas.
//
//  Changes -
//    March 18th, 2007 - RaX - Created.
//	[2007/10/25] Aeomin - Major clean up #1
//	[2007/10/25] Aeomin - Major clean up #1b
//		June 28th, 2008 - Tsusai - Updated GetPacketLength to PacketDB.GetLength
//			in various calls
//------------------------------------------------------------------------------
unit ZoneInterCommunication;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
uses
	Classes,
	ZoneServer,
	IdContext,
	Character,
	CommClient;

const
	WHISPER_SUCCESS  = 0;
	WHISPER_FAILED   = 1;
	WHISPER_BLOCKED  = 2;

	//----------------------------------------------------------------------
	// Server Validation
	//----------------------------------------------------------------------
	procedure ValidateWithInterServer(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);

	procedure SendValidateFlagToZone(
		AClient : TIdContext;
		Validated : byte
	);
	//----------------------------------------------------------------------


	//----------------------------------------------------------------------
	// Update IP Address
	//----------------------------------------------------------------------
	procedure SendZoneWANIPToInter(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	procedure SendZoneLANIPToInter(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	//----------------------------------------------------------------------


	//----------------------------------------------------------------------
	//  Update Online player count
	//----------------------------------------------------------------------
	procedure SendZoneOnlineUsersToInter(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	//----------------------------------------------------------------------


	//----------------------------------------------------------------------
	//  Whisper handle
	//----------------------------------------------------------------------
	procedure SendWhisperToInter(
		AClient : TInterClient;
		const AChara: TCharacter;
		const TargetName, Whisper: String
	);
	procedure SendWhisperReplyToZone(
		AClient : TIdContext;
		CharID:LongWord;
		Code : byte
	);
	procedure RedirectWhisperToZone(
		AClient : TIdContext;
		const ZoneID,FromID,CharID:LongWord;
		const FromName,Whisper: String
	);
	procedure SendWhisperReplyToInter(
		AClient : TInterClient;
		const ZoneID, CharID:LongWord;
		Code : byte
	);
	//----------------------------------------------------------------------


	//----------------------------------------------------------------------
	// GM Command Handle
	//----------------------------------------------------------------------
	procedure ZoneSendGMCommandtoInter(
		AClient : TInterClient;
		const AID, CharID:LongWord;
		const Command : string
	);
	procedure ZoneSendGMCommandResultToInter(
		const AccountID : LongWord;
		const CharacterID : LongWord;
		const ZoneID : LongWord;
		const Error : TStringList
	);
	//----------------------------------------------------------------------


	//----------------------------------------------------------------------
	// Friend List
	//----------------------------------------------------------------------
	procedure ZoneSendAddFriendRequest(
		AClient : TInterClient;
		const ReqAID,ReqID : LongWord;
		const ReqChar      : String;
		const TargetChar   : LongWord;
		const ZoneID       : LongWord
	);
	procedure ZoneSendAddFriendReply(
		AClient : TInterClient;
		const OrigID : LongWord;
		const AccID  : LongWord;
		const CharID : LongWord;
		const CharName : String;
		const Reply  : Byte
	);
	procedure ZoneSendPlayerOnlineStatus(
		AClient : TInterClient;
		const AID : LongWord;
		const CID : LongWord;
		const Offline : Byte
	);
	procedure ZoneSendPlayerOnlineReply(
		AClient : TInterClient;
		const AID      : LongWord;
		const CID      : LongWord;
		const TargetID : LongWord;
		const Offline  : Byte;
		const ZoneID   : LongWord
	);
	//----------------------------------------------------------------------


	//----------------------------------------------------------------------
	// MailBox
	//----------------------------------------------------------------------
	procedure ZoneSendNotifyMail(
		AClient : TInterClient;
		const MailID : LongWord
	);
	//----------------------------------------------------------------------


	//----------------------------------------------------------------------
	// Instance Map
	//----------------------------------------------------------------------
	procedure ZoneSendCreateInstanceMapRequest(
		AClient : TInterClient;
		const Identifier : String;
		const MapName : String;
		const CharID : LongWord;
		const ScriptID : LongWord
	);
	procedure ZoneSendAllowInstanceFlag(
		AClient : TInterClient;
		const Flag : Boolean
	);
	procedure ZoneSendCreatedInstance(
		AClient : TInterClient;
		const ZoneID : LongWord;
		const CharID : LongWord;
		const ScriptID : LongWord;
		const MapName : String
	);
	procedure ZoneSendInstanceList(
		AClient : TInterClient
	);
	//----------------------------------------------------------------------

implementation
uses
	Main,
	BufferIO,
	Globals,
	PacketTypes,
	TCPServerRoutines,
	GameConstants,
	SysUtils;

//------------------------------------------------------------------------------
//ValidateWithInterServer                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the Inter server key, zone id, and zoneport to the inter
//    server for validation.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    March 18th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure ValidateWithInterServer(
	AClient : TInterClient;
	ZoneServer : TZoneServer
	);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $2200, OutBuffer);
	WriteBufferLongWord(2, ZoneServer.Options.ID, OutBuffer);
	WriteBufferWord(6, ZoneServer.Port, OutBuffer);
	WriteBufferMD5String(8, GetMD5(ZoneServer.Options.InterKey), OutBuffer);
	SendBuffer(AClient,OutBuffer,PacketDB.GetLength($2200));
end;{ValidateWithInterServer}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendValidateFlagToZone                                               PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      After validation, if a zone server checks out it is sent this flag back
//    which contains a simple TRUE/FALSE flag that lets a zone know that it
//    passed or not.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    March 18th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure SendValidateFlagToZone(
	AClient : TIdContext;
	Validated : byte
	);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0, $2201, OutBuffer);
	WriteBufferByte(2, Validated, OutBuffer);
	SendBuffer(AClient,OutBuffer,PacketDB.GetLength($2201));
end;{SendValidateFlagToZone}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendZoneWANIPToChara                                                 PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the zone's WANIP to the inter server.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    March 18th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure SendZoneWANIPToInter(
	AClient : TInterClient;
	ZoneServer : TZoneServer
	);
var
	OutBuffer : TBuffer;
	Size : integer;
begin
	Size := Length(ZoneServer.Options.WANIP);
	WriteBufferWord(0,$2202,OutBuffer);
	WriteBufferWord(2,Size+4,OutBuffer);
	WriteBufferString(4,ZoneServer.Options.WANIP,Size,OutBuffer);
	SendBuffer(AClient,OutBuffer,Size+4);
end;{SendZoneWANIPToInter}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendZoneLANIPToInter                                                 PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the zone's LANIP to the inter server.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    March 18th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure SendZoneLANIPToInter(
	AClient : TInterClient;
	ZoneServer : TZoneServer
	);
var
	OutBuffer : TBuffer;
	Size : integer;
begin
	Size := Length(ZoneServer.Options.LANIP);
	WriteBufferWord(0,$2203,OutBuffer);
	WriteBufferWord(2,Size+4,OutBuffer);
	WriteBufferString(4,ZoneServer.Options.LANIP,Size,OutBuffer);
	SendBuffer(AClient,OutBuffer,Size+4);
end;{SendZoneLANIPToInter}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendZoneOnlineUsersToInter                                           PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Updates the inter server's internal list of online inters.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    March 18th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure SendZoneOnlineUsersToInter(
	AClient : TInterClient;
	ZoneServer : TZoneServer
	);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0,$2204,OutBuffer);
	WriteBufferWord(2,ZoneServer.OnlineUsers,OutBuffer);
	SendBuffer(AClient,OutBuffer,PacketDB.GetLength($2204));
end;{SendZoneOnlineUsersToInter}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendWhisperToInter                                                   PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Send Whisper to Inter server
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    May 3rd, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure SendWhisperToInter(
	AClient : TInterClient;
	const AChara: TCharacter;
	const TargetName,Whisper: String
	);
var
	OutBuffer : TBuffer;
	Size : Integer;
begin
	//Strlen is used here to count..since length count 2 byte character as one (UNICODE)
	Size := StrLen(PChar(Whisper));
	WriteBufferWord(0,$2210,OutBuffer);
	WriteBufferWord(2,Size + 56,OutBuffer);
	WriteBufferLongWord(4,AChara.ID,OutBuffer);
	WriteBufferString(8,AChara.Name, 24, OutBuffer);
	WriteBufferString(32,TargetName, 24, OutBuffer);
	WriteBufferString(56,Whisper,Size,OutBuffer);
	SendBuffer(AClient,OutBuffer,Size + 56);
end;{SendWhisperToInter}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendWhisperReplyToZone                                               PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Send Zone of whisper reply from Inter
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    May 3rd, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure SendWhisperReplyToZone(
	AClient : TIdContext;
	CharID:LongWord;
	Code : byte
	);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0,$2212,OutBuffer);
	WriteBufferLongWord(2,CharID,OutBuffer);
	WriteBufferByte(6, Code, OutBuffer);
	SendBuffer(AClient, OutBuffer, PacketDB.GetLength($2212));
end;{SendWhisperReplyToZone}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RedirectWhisperToZone                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Inter redirect whisper message to proper Zone
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    May 3rd, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure RedirectWhisperToZone(
	AClient : TIdContext;
	const ZoneID,FromID,CharID:LongWord;
	const FromName,Whisper: String
	);
var
	OutBuffer : TBuffer;
	Size : Integer;
begin
	Size := StrLen(PChar(Whisper));
	WriteBufferWord(0,$2210,OutBuffer);
	WriteBufferWord(2,Size + 40,OutBuffer);
	WriteBufferLongWord(4,ZoneID,OutBuffer);
	WriteBufferLongWord(8,FromID,OutBuffer);
	WriteBufferLongWord(12,CharID,OutBuffer);
	WriteBufferString(16,FromName,24,OutBuffer);
	WriteBufferString(40,Whisper,Size,OutBuffer);
	SendBuffer(AClient, OutBuffer, Size + 40);
end;{RedirectWhisperToZone}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendWhisperReplyToInter                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Zone send whisper status reply to Inter
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    May 3rd, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure SendWhisperReplyToInter(
	AClient : TInterClient;
	const ZoneID, CharID:LongWord;
	Code : byte
	);
var
	OutBuffer : TBuffer;
begin
	WriteBufferWord(0,$2211,OutBuffer);
	WriteBufferLongWord(2,ZoneID,OutBuffer);
	WriteBufferLongWord(6,CharID,OutBuffer);
	WriteBufferByte(10, Code, OutBuffer);
	SendBuffer(AClient, OutBuffer, PacketDB.GetLength($2211));
end;{SendWhisperReplyToInter}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendGMCommandtoInter                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//	Sends the received GM command to the inter server.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//[2007/03/19] RaX - Created Header;
//[2007/05/01] Tsusai - Added const to the parameters
//[2007/06/01] CR - Altered Comment Header, minor formatting/bracketing changes,
//	use of try-finally to ensure no leaks if errors occur before our local
//	Stringlist is freed.
//[2007/7/23] Aeomin - Moved from ZoneSend.pas
//------------------------------------------------------------------------------
procedure ZoneSendGMCommandtoInter(
	AClient : TInterClient;
	const AID, CharID:LongWord;
	const Command : string
	);
var
	TotalLength : Integer;
	OutBuffer   : TBuffer;
begin
	TotalLength := 19 + StrLen(PChar(Command));
	WriteBufferWord(0, $2205, OutBuffer);
	WriteBufferWord(2, TotalLength, OutBuffer);

	WriteBufferLongWord(4, AID, OutBuffer);
	WriteBufferLongWord(8, CharID, OutBuffer);
	WriteBufferWord(12, Length(Command), OutBuffer);
	WriteBufferString(14, Command, Length(Command), OutBuffer);
	SendBuffer(AClient, OutBuffer, TotalLength);
end;{ZoneSendGMCommandtoInter}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendGMCommandResultToInter                                       PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the received gm command to the inter server.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    March 19th, 2007 - RaX - Created Header;
//		May 1st, 2007 - Tsusai - Added const to the parameters
//------------------------------------------------------------------------------
procedure ZoneSendGMCommandResultToInter(
	const AccountID : LongWord;
	const CharacterID : LongWord;
	const ZoneID : LongWord;
	const Error : TStringList
	);
var
	ReplyBuffer : TBuffer;
	BufferIndex : Integer;
	CSLength    : Integer;
	Index       : Integer;
begin
	WriteBufferWord(0, $2207, ReplyBuffer);
	WriteBufferLongWord(4, AccountID, ReplyBuffer);
	WriteBufferLongWord(8, CharacterID, ReplyBuffer);
	WriteBufferLongWord(12, ZoneID, ReplyBuffer);
	WriteBufferWord(16, Error.Count, ReplyBuffer);
	BufferIndex := 18;
	for Index := 0 to Error.Count - 1 do
	begin
		CSLength := Length(Error[Index]);
		WriteBufferWord(BufferIndex, CSLength, ReplyBuffer);
		Inc(BufferIndex, 2);
		WriteBufferString(
			BufferIndex,
			Error[Index],
			CSLength,
			ReplyBuffer
		);
	Inc(BufferIndex, CSLength);
	end;
	WriteBufferWord(2, BufferIndex + 1, ReplyBuffer);
	SendBuffer(MainProc.ZoneServer.ToInterTCPClient,ReplyBuffer,BufferIndex + 1);
end;{ZoneSendGMCommandResultToInter}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendAddFriendRequest                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//	Let inter server to request add friend.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/12/07] Aeomin - Created.
//------------------------------------------------------------------------------
procedure ZoneSendAddFriendRequest(
	AClient : TInterClient;
	const ReqAID,ReqID : LongWord;
	const ReqChar      : String;
	const TargetChar   : LongWord;
	const ZoneID       : LongWord
);
var
	OutBuffer   : TBuffer;
begin
	WriteBufferWord(0, $2215, OutBuffer);
	WriteBufferLongWord(2, ReqAID, OutBuffer);
	WriteBufferLongWord(6, ReqID, OutBuffer);
	WriteBufferLongWord(10, TargetChar, OutBuffer);
	WriteBufferLongWord(14, ZoneID, OutBuffer);
	WriteBufferString(18, ReqChar, NAME_LENGTH, OutBuffer);
	SendBuffer(AClient, OutBuffer, PacketDB.GetLength($2215));
end;{ZoneSendAddFriendRequest}
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
//ZoneSendAddFriendReply                                               PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//	We got reply now, let's send back to inter
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/12/08] Aeomin - Created.
//------------------------------------------------------------------------------
procedure ZoneSendAddFriendReply(
	AClient : TInterClient;
	const OrigID : LongWord;
	const AccID  : LongWord;
	const CharID : LongWord;
	const CharName : String;
	const Reply  : Byte
);
var
	OutBuffer   : TBuffer;
begin
	WriteBufferWord(0, $2217, OutBuffer);
	WriteBufferLongWord(2, OrigID, OutBuffer);
	WriteBufferLongWord(6, AccID, OutBuffer);
	WriteBufferLongWord(10, CharID, OutBuffer);
	WriteBufferByte(14, Reply, OutBuffer);
	WriteBufferString(15, CharName, NAME_LENGTH, OutBuffer);
	SendBuffer(AClient, OutBuffer, PacketDB.GetLength($2217));
end;{ZoneSendAddFriendReply}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendPlayerOnlineStatus                                           PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      This player is online/offline tell inter server do something about it
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/12/??] - Aeomin - Created
//------------------------------------------------------------------------------
procedure ZoneSendPlayerOnlineStatus(
	AClient : TInterClient;
	const AID : LongWord;
	const CID : LongWord;
	const Offline : Byte
);
var
	OutBuffer   : TBuffer;
begin
	WriteBufferWord(0, $2218, OutBuffer);
	WriteBufferLongWord(2, AID, OutBuffer);
	WriteBufferLongWord(6, CID, OutBuffer);
	WriteBufferByte(10, Offline, OutBuffer);
	SendBuffer(AClient, OutBuffer, PacketDB.GetLength($2218));
end;{ZoneSendPlayerOnlineStatus}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendPlayerOnlineReply                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Tell origin players about target char's status via inter server
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/12/??] - Aeomin - Created
//------------------------------------------------------------------------------
procedure ZoneSendPlayerOnlineReply(
	AClient : TInterClient;
	const AID      : LongWord;
	const CID      : LongWord;
	const TargetID : LongWord;
	const Offline  : Byte;
	const ZoneID   : LongWord
);
var
	OutBuffer   : TBuffer;
begin
	WriteBufferWord(0, $2220, OutBuffer);
	WriteBufferLongWord(2, AID, OutBuffer);
	WriteBufferLongWord(6, CID, OutBuffer);
	WriteBufferLongWord(10, TargetID, OutBuffer);
	WriteBufferByte(14, Offline, OutBuffer);
	WriteBufferLongWord(15, ZoneID, OutBuffer);
	SendBuffer(AClient, OutBuffer, PacketDB.GetLength($2220));
end;{ZoneSendPlayerOnlineReply}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendNotifyMail                                                   PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//	Tell inter to notify player about new mail (if can)
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2008/08/11] - Aeomin - Created
//------------------------------------------------------------------------------
procedure ZoneSendNotifyMail(
	AClient : TInterClient;
	const MailID : LongWord
);
var
	OutBuffer   : TBuffer;
begin
	WriteBufferWord(0, $2222, OutBuffer);
	WriteBufferLongWord(2, MailID, OutBuffer);
	SendBuffer(AClient, OutBuffer, PacketDB.GetLength($2222));
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendCreateInstanceMapRequest                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Tell inter server to create an instance map
//
//	Changes -
//		[2008/12/26] - Aeomin - Created
//------------------------------------------------------------------------------
procedure ZoneSendCreateInstanceMapRequest(
	AClient : TInterClient;
	const Identifier : String;
	const MapName : String;
	const CharID : LongWord;
	const ScriptID : LongWord
);
var
	OutBuffer   : TBuffer;
	Len : Byte;
begin
	Len := Length(Identifier);
	if Len > 0 then
	begin
		WriteBufferWord(0, $2224, OutBuffer);
		WriteBufferWord(2, Len + 28, OutBuffer);
		WriteBufferString(4, MapName, 16, OutBuffer);
		WriteBufferLongWord(20, CharID, OutBuffer);
		WriteBufferLongWord(24, ScriptID, OutBuffer);
		WriteBufferString(28, Identifier, Len, OutBuffer);
		SendBuffer(AClient, OutBuffer, Len + 28);
	end;
end;{ZoneSendCreateInstanceMapRequest}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendAllowInstanceFlag                                            PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Tell inter server about this zone server allow instance or not
//
//	Changes -
//		[2008/12/06] - Aeomin - Created
//------------------------------------------------------------------------------
procedure ZoneSendAllowInstanceFlag(
	AClient : TInterClient;
	const Flag : Boolean
);
var
	OutBuffer   : TBuffer;
begin
	WriteBufferWord(0, $2226, OutBuffer);
	WriteBufferByte(2, Byte(Flag), OutBuffer);
	SendBuffer(AClient, OutBuffer, PacketDB.GetLength($2226));
end;{ZoneSendAllowInstanceFlag}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendCreatedInstance                                              PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Created, let's tell Inter server
//
//	Changes -
//		[2008/12/07] - Aeomin - Created
//------------------------------------------------------------------------------
procedure ZoneSendCreatedInstance(
	AClient : TInterClient;
	const ZoneID : LongWord;
	const CharID : LongWord;
	const ScriptID : LongWord;
	const MapName : String
);
var
	OutBuffer   : TBuffer;
	Len : Byte;
begin
	Len := Length(MapName);
	if Len > 0 then
	begin
		WriteBufferWord(0, $2228, OutBuffer);
		WriteBufferWord(2, Len + 16, OutBuffer);
		WriteBufferLongWord(4, ZoneID, OutBuffer);
		WriteBufferLongWord(8, CharID, OutBuffer);
		WriteBufferLongWord(12, ScriptID, OutBuffer);
		WriteBufferString(16, MapName, Len, OutBuffer);
		SendBuffer(AClient, OutBuffer, Len + 16);
	end;
end;{ZoneSendCreatedInstance}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ZoneSendInstanceList                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Only send after connect to Inter Server.
//	This is used when in situation connection dropped and reconnect.
//
//	Changes -
//		[2008/12/09] - Aeomin - Created
//------------------------------------------------------------------------------
procedure ZoneSendInstanceList(
	AClient : TInterClient
);
var
	OutBuffer   : TBuffer;
	Index : Integer;
	Count : Byte;
	OffSet:Word;
	Len : Word;
	StrLen:Byte;
	Name : String;
begin
	Count := 0;
	Len := 0;
	OffSet := 5;
	with MainProc.ZoneServer do
	begin
		if InstanceMapList.Count > 0 then
		begin
			for Index := 0 to InstanceMapList.Count - 1 do
			begin

				Name  := InstanceMapList.Items[Index].Name;
				StrLen := Length(Name);

				WriteBufferByte(OffSet, StrLen, OutBuffer);
				WriteBufferString(OffSet+1, Name, Len, OutBuffer);
				Inc(OffSet,Len+1);
				Inc(Len,Len+1);

				if (Count > 100) OR (Index = (InstanceMapList.Count - 1)) then
				begin
					WriteBufferWord(0, $2229, OutBuffer);
					WriteBufferWord(2, Len + 5, OutBuffer);
					WriteBufferByte(4, Count, OutBuffer);
					SendBuffer(AClient, OutBuffer, Len + 5);
					OffSet := 5;
					Len := 0;
				end;
				//Send 100 of them at a time.
				if Count > 100 then
					Count := 0;

				Inc(Count);
			end;
		end;
	end;
end;{ZoneSendInstanceList}
//------------------------------------------------------------------------------
end{ZoneInterCommunication}.
