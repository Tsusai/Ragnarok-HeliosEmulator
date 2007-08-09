//------------------------------------------------------------------------------
//ZoneInterCommunication	                                                 UNIT
//------------------------------------------------------------------------------
//  What it does -
//      Houses our Zone<->Inter server communication routines. These are
//    used in ZoneServer.pas and also in InterServer.pas.
//
//  Changes -
//    March 18th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
unit ZoneInterCommunication;

interface
uses
	ZoneServer,
	CommClient,
	IdContext,
	Character;

const
	WHISPER_SUCCESS  = 0;
	WHISPER_FAILED   = 1;
	WHISPER_BLOCKED  = 2;
	
	procedure ValidateWithInterServer(
		AClient : TInterClient;
		ZoneServer : TZoneServer
	);
	procedure SendValidateFlagToZone(AClient : TIdContext; Validated : byte);
	procedure SendZoneWANIPToInter(AClient : TInterClient; ZoneServer : TZoneServer);
	procedure SendZoneLANIPToInter(AClient : TInterClient; ZoneServer : TZoneServer);
	procedure SendZoneOnlineUsersToInter(AClient : TInterClient; ZoneServer : TZoneServer);
	procedure SendWhisperToInter(AClient : TInterClient; const AChara: TCharacter;const TargetName, Whisper: String);
	procedure SendWhisperReplyToZone(AClient : TIdContext; CharID:LongWord; Code : byte);
	procedure RedirectWhisperToZone(AClient : TIdContext;const ZoneID,FromID,CharID:LongWord;const FromName,Whisper: String);
	procedure SendWhisperReplyToInter(AClient : TInterClient;const ZoneID, CharID:LongWord; Code : byte);
	procedure ZoneSendGMCommandtoInter(AClient : TInterClient;const AID, CharID:LongWord;const Command : string);
implementation
uses
	BufferIO,
	Globals,
	PacketTypes,
	TCPServerRoutines,
	SysUtils;

//------------------------------------------------------------------------------
//ValidateWithInterServer                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the Inter server key, zone id, and zoneport to the inter
//    server for validation.
//
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
		SendBuffer(AClient,OutBuffer,GetPacketLength($2200));
	end;//ValidateWithInterServer
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendValidateFlagToZone                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      After validation, if a zone server checks out it is sent this flag back
//    which contains a simple TRUE/FALSE flag that lets a zone know that it
//    passed or not.
//
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
		SendBuffer(AClient,OutBuffer,GetPacketLength($2201));
	end;//SendValidateFlagToZone
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendZoneWANIPToChara                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the zone's WANIP to the inter server.
//
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
	end;//SendZoneWANIPToInter
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendZoneLANIPToInter                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sends the zone's LANIP to the inter server.
//
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
	end;//SendZoneLANIPToInter
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendZoneOnlineUsersToInter                                          PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Updates the inter server's internal list of online inters.
//
//  Changes -
//    March 18th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
	procedure SendZoneOnlineUsersToInter(AClient : TInterClient; ZoneServer : TZoneServer);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0,$2204,OutBuffer);
		WriteBufferWord(2,ZoneServer.OnlineUsers,OutBuffer);
		SendBuffer(AClient,OutBuffer,GetPacketLength($2204));
	end;//SendZoneOnlineUsersToInter
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendWhisperToInter                                                   PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Send Whisper to Inter server
//
//  Changes -
//    May 3rd, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
	procedure SendWhisperToInter(AClient : TInterClient; const AChara: TCharacter;const TargetName,Whisper: String);
	var
		OutBuffer : TBuffer;
		Size : Integer;
	begin
		//Strlen is used here to count..since length count 2 byte character as one (UNICODE)
		Size := StrLen(PChar(Whisper));
		WriteBufferWord(0,$2210,OutBuffer);
		WriteBufferWord(2,Size + 56,OutBuffer);
		WriteBufferLongWord(4,AChara.CID,OutBuffer);
		WriteBufferString(8,AChara.Name, 24, OutBuffer);
		WriteBufferString(32,TargetName, 24, OutBuffer);
		WriteBufferString(56,Whisper,Size,OutBuffer);
		SendBuffer(AClient,OutBuffer,Size + 56);
	end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendWhisperReplyToZone                                               PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Send Zone of whisper reply from Inter
//
//  Changes -
//    May 3rd, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
	procedure SendWhisperReplyToZone(AClient : TIdContext; CharID:LongWord; Code : byte);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0,$2212,OutBuffer);
		WriteBufferLongWord(2,CharID,OutBuffer);
		WriteBufferByte(6, Code, OutBuffer);
		SendBuffer(AClient, OutBuffer, GetPacketLength($2212));
	end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RedirectWhisperToZone                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Inter redirect whisper message to proper Zone
//
//  Changes -
//    May 3rd, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
	procedure RedirectWhisperToZone(AClient : TIdContext;const ZoneID,FromID,CharID:LongWord;const FromName,Whisper: String);
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
	end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendWhisperReplyToInter                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Zone send whisper status reply to Inter
//
//  Changes -
//    May 3rd, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
	procedure SendWhisperReplyToInter(AClient : TInterClient;const ZoneID, CharID:LongWord; Code : byte);
	var
		OutBuffer : TBuffer;
	begin
		WriteBufferWord(0,$2211,OutBuffer);
		WriteBufferLongWord(2,ZoneID,OutBuffer);
		WriteBufferLongWord(6,CharID,OutBuffer);
		WriteBufferByte(10, Code, OutBuffer);
		SendBuffer(AClient, OutBuffer, GetPacketLength($2211));
	end;
//------------------------------------------------------------------------------


(*- Procedure -----------------------------------------------------------------*
ZoneSendGMCommandtoInter
--------------------------------------------------------------------------------
Overview:
--
	Sends the received GM command to the inter server.


--
Pre:
	TODO
Post:
	TODO

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/03/19] RaX - Created Header;
[2007/05/01] Tsusai - Added const to the parameters
[2007/06/01] CR - Altered Comment Header, minor formatting/bracketing changes,
	use of try-finally to ensure no leaks if errors occur before our local
	Stringlist is freed.
[2007/7/23] Aeomin - Moved from ZoneSend.pas
*-----------------------------------------------------------------------------*)
procedure ZoneSendGMCommandtoInter(AClient : TInterClient;const AID, CharID:LongWord;const Command : string);
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
end; (* Proc ZoneSendGMCommandtoInter
*-----------------------------------------------------------------------------*)
end.
