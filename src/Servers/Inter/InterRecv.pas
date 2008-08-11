//------------------------------------------------------------------------------
//InterServer                                                               UNIT
//------------------------------------------------------------------------------
//	What it does-
//      Contains Recv routines. Anything that is received by the inter server.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//		[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
//
//------------------------------------------------------------------------------
unit InterRecv;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	//none
	{Project}
	PacketTypes,
	{3rd Party}
	IdContext
	;

Procedure RecvGMCommand(
	const
		AClient : TIdContext;
	const
		InBuffer : TBuffer
	);

Procedure RecvGMCommandReply(
		AClient : TIdContext;
		InBuffer : TBuffer
	);

Procedure RecvWhisper(
		AClient : TIdContext;
		InBuffer : TBuffer
	);

Procedure RecvWhisperReply(
		AClient  : TIdContext;
		InBuffer : TBuffer
	);

Procedure RecvZoneWarpRequest(
		AClient : TIdContext;
		ABuffer : TBuffer
	);

procedure RecvZoneRequestFriend(
		AClient : TIdContext;
		ABuffer : TBuffer
	);

procedure RecvZoneRequestFriendReply(
	AClient : TIdContext;
	ABuffer : TBuffer
);

procedure RecvZonePlayerOnlineStatus(
	AClient : TIdContext;
	ABuffer : TBuffer
);

procedure RecvZonePlayerOnlineReply(
	AClient : TIdContext;
	ABuffer : TBuffer
);

procedure RecvZoneNewMailNotify(
	AClient : TIdContext;
	ABuffer : TBuffer
);
implementation


uses
	{RTL/VCL}
	Classes,
	SysUtils,
	{Project}
	BufferIO,
	Character,
	Main,
	ZoneServerInfo,
	ZoneInterCommunication,
	InterSend,
	GameConstants,
	CharaList,
	MailBox
	{3rd Party}
	//none
	;

(*- Procedure -----------------------------------------------------------------*
RecvGMCommand
--------------------------------------------------------------------------------
Overview:
--
	Gets a GM command from an authenticated zone server

N.B: See Notes/GM Command Packets for explanation.

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/03/19] RaX - Created Header.
[2007/06/01] CR - Made all parameters constant.  Added try-finally construct
	around the use of the local TStringList, to safeguard against a memory leak if
	InterSendGMCommandToZones were to cause errors.
[2007/08/08] Aeomin - "upgraded" GM command parsing...
*-----------------------------------------------------------------------------*)
Procedure RecvGMCommand(
	const
		AClient : TIdContext;
	const
		InBuffer : TBuffer
	);
Var
	GMID             : LongWord;
	CharaID          : LongWord;
	CommandLength    : LongWord;
	CommandString    : String;
Begin
	//See Notes/GMCommand Packets.txt
	GMID          := BufferReadLongWord(4, InBuffer);
	CharaID       := BufferReadLongWord(8, InBuffer);
	CommandLength := BufferReadWord(12,InBuffer);
	CommandString := BufferReadString(14, CommandLength, InBuffer);

	InterParseGMCommand(AClient, GMID, CharaID, TZoneServerLink(AClient.Data).Info.ZoneID, CommandString);
End; (* Proc RecvGMCommand
*-----------------------------------------------------------------------------*)


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
[2007/12/23] Tsusai - Variable "Error" changed to "Errors", since there is a
	TIntList32.Error.
*-----------------------------------------------------------------------------*)
Procedure RecvGMCommandReply(
		AClient : TIdContext;
		InBuffer : TBuffer
	);
//See Notes/GM Command Packets for explanation.
Var
	CharacterID : LongWord;
	ZoneID      : LongWord;
	ZoneLink    : TZoneServerLink;
	Index       : Integer;
	ListClient  : TIdContext;
	ErrCount    : Word;
	BufferIndex : Integer;
	ErrLen      : Word;
	Errors       : TStringList;
Begin
	with MainProc.InterServer do
	begin
		ErrCount   := BufferReadWord(16, InBuffer);
		if (ErrCount > 0) then
		begin
			CharacterID := BufferReadLongWord(8, InBuffer);

			BufferIndex := 18;

			Errors := TStringList.Create;
			for Index := 0 to ErrCount - 1 do
			begin
				ErrLen := BufferReadWord(BufferIndex, InBuffer);
				inc(BufferIndex, 2);
				Errors.Add(BufferReadString(BufferIndex,ErrLen,InBuffer));
				inc(BufferIndex, ErrLen);
			end;

			ZoneID := BufferReadLongWord(12, InBuffer);
			for Index := 0 to (ClientList.Count - 1) do
			begin
				ZoneLink := ZoneServerLink[Index];
				if (ZoneLink <> NIL) AND
					(ZoneLink.Info.ZoneID = ZoneID) then
				begin
					ListClient := ClientList[Index];
					InterSendGMCommandReplyToZone(ListClient, CharacterID, Errors);
					Break;
				end;
			end;//for

			Errors.Free;
		end;
	end;
End; (* Func TInterServer.RecvGMCommandReply
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
Procedure RecvWhisper(
		AClient : TIdContext;
		InBuffer : TBuffer
	);
Var
	Size       : Word;
	FromID     : LongWord;
	FromName   : String;
	TargetName : String;
	Whisper    : String;
	ACharacter : TCharacter;
	ZoneID     : LongWord;
	Index      : Integer;
	SentWhisper : Boolean;
Begin
	Size       := BufferReadWord(2, InBuffer);
	FromID     := BufferReadLongWord(4, InBuffer);
	FromName   := BufferReadString(8, 24, InBuffer);
	TargetName := BufferReadString(32, 24, InBuffer);
	Whisper    := BufferReadString(56, (Size - 56), InBuffer);

	with TZoneServerLink(AClient.Data).DatabaseLink.Character do
	begin

		ACharacter := TCharacter.Create(Aclient);
		ACharacter.Name := TargetName;
		Load(ACharacter);

	end;

	with MainProc.InterServer do
	begin
		if (ACharacter.Map <> '') then
		begin
			with TZoneServerLink(AClient.Data).DatabaseLink.Map do
			begin
				ZoneID := GetZoneID(ACharacter.Map);
			end;
			Index := ZoneServerList.IndexOf(ZoneID);

			SentWhisper := False;

			if (Index > -1) then
			begin
				if Assigned(ZoneServerLink[Index]) AND
					(ZoneServerLink[Index].Info.ZoneID = ZoneID) then
				begin
					RedirectWhisperToZone(
						ZoneServerInfo[Index].Connection,
						TZoneServerLink(AClient.Data).Info.ZoneID,
						FromID,
						ACharacter.ID,
						FromName,
						Whisper
				);
					SentWhisper := True;
				end;
			end;


			//This will fire if can't find any..WHY THE LIST DONT HAVE INDEXOF?
			if NOT SentWhisper then
			begin
				//Well..if zone is disconnected..then just say not online
				SendWhisperReplyToZone(AClient, FromID, WHISPER_FAILED);
			end;
			aCharacter.Free;
		end else
		begin
			SendWhisperReplyToZone(AClient, FromID, WHISPER_FAILED);  //Target is not online
		end;
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
Procedure RecvWhisperReply(
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
	with MainProc.InterServer do
	begin
		for Index := (ClientList.Count - 1) downto 0 do
		begin
			if Assigned(ZoneServerLink[Index]) AND
				(ZoneServerLink[Index].Info.ZoneID = ZoneID) then
			begin
				SendWhisperReplyToZone(ClientList[Index], CharID, Flag);
				Break;
			end;
		end;
	end;
End; (* Proc TInterServer.RecvWhisperReply
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
Procedure RecvZoneWarpRequest(
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
	Zidx : integer;
Begin
	CharacterID := BufferReadLongWord(4, ABuffer);
	X						:= BufferReadWord(8, ABuffer);
	Y						:= BufferReadWord(10, ABuffer);
	MapNameSize := BufferReadWord(12, ABuffer);
	MapName			:= BufferReadString(14, MapNameSize, ABuffer);

	ClientIPSize := BufferReadWord(14+MapNameSize, ABuffer);
	ClientIP		 := BufferReadString(16+MapNameSize, ClientIPSize, ABuffer);

	with TThreadLink(AClient.Data).DatabaseLink.Map do
	begin
		ZoneID := GetZoneID(MapName);
	end;

	ZIdx := MainProc.InterServer.ZoneServerList.IndexOf(ZoneID);

	//Why warp to a unknown zone..or reply to it.  Kill it here. They can walk fine
	if Zidx > -1 then
	begin
		ZServerInfo		:= MainProc.InterServer.ZoneServerInfo[Zidx];
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
	end else
	begin
		RedirectWhisperToZone(
			AClient,
			0,0,CharacterID,
			'System',
			Format('The map %s is currently unavailable, please try again later.',[MapName])
		);
	end;
End; (* Proc TInterServer.RecvZoneWarpRequest
*-----------------------------------------------------------------------------*)


//------------------------------------------------------------------------------
//RecvZoneRequestFriend                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Got request from zone server
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    [2007/12/7] Aeomin - Created.
//------------------------------------------------------------------------------
procedure RecvZoneRequestFriend(
	AClient : TIdContext;
	ABuffer : TBuffer
);
var
	ReqAID,ReqID : LongWord;
	ReqChar    : String;
	TargetChar : LongWord;

	Index      : Integer;
	ZoneID     : LongWord;
begin
	// Get packet data.
	ReqAID    := BufferReadLongWord(2, ABuffer);
	ReqID     := BufferReadLongWord(6, ABuffer);
	TargetChar:= BufferReadLongWord(10, ABuffer);
	ZoneID    := BufferReadLongWord(14, ABuffer);
	ReqChar   := BufferReadString(18, NAME_LENGTH, ABuffer);

	with MainProc.InterServer do
	begin
		Index := ZoneServerList.IndexOf(ZoneID);


		if (Index > -1) then
		begin
			if Assigned(ZoneServerLink[Index]) AND
			(ZoneServerLink[Index].Info.ZoneID = ZoneID) then
			begin
				InterSendFriendRequest(
					ZoneServerInfo[Index].Connection,
					ReqAID,
					ReqID,
					ReqChar,
					TargetChar
				);
			end;
		end;
	end;
end;{RecvZoneRequestFriend}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvZoneRequestFriendReply                                           PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Got a reply, let's find the origin and push to there.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    [2007/12/08] Aeomin - Created.
//------------------------------------------------------------------------------
procedure RecvZoneRequestFriendReply(
	AClient : TIdContext;
	ABuffer : TBuffer
);
var
	OrigID    : LongWord;
	AccID     : LongWord;
	CharID    : LongWord;
	CharName  : String;
	Reply     : Byte;

	ACharacter: TCharacter;
	Index     : Integer;
	ZoneID    : LongWord;
begin
	OrigID   := BufferReadLongWord(2, ABuffer);
	AccID    := BufferReadLongWord(6, ABuffer);
	CharID   := BufferReadLongWord(10, ABuffer);
	Reply    := BufferReadByte(14, ABuffer);
	CharName := BufferReadString(15, NAME_LENGTH, ABuffer);


	with TZoneServerLink(AClient.Data).DatabaseLink.Character do
	begin
		ACharacter := TCharacter.Create(AClient);
		ACharacter.ID := OrigID;
		Load(ACharacter);
	end;

	with MainProc.InterServer do
	begin
		if (ACharacter.Map <> '') then
		begin
			with TZoneServerLink(AClient.Data).DatabaseLink.Map do
			begin
				ZoneID := GetZoneID(ACharacter.Map);
			end;
			Index := ZoneServerList.IndexOf(ZoneID);

			if (Index > -1) then
			begin
				if Assigned(ZoneServerLink[Index]) AND
					(ZoneServerLink[Index].Info.ZoneID = ZoneID) then
				begin
					InterSendFriendRequestReply(
						ZoneServerInfo[Index].Connection,
						OrigID,
						AccID,
						CharID,
						CharName,
						Reply
					);
				end;
			end;
			ACharacter.Free;
		end;
	end;
end;{RecvZoneRequestFriendReply}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvZonePlayerOnlineStatus                                           PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      A player is online/offline, now inter server need announce to friends,
// party member, guild member or whatever is that [Universal procedure]
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/12/??] - Aeomin - Created
//------------------------------------------------------------------------------
procedure RecvZonePlayerOnlineStatus(
	AClient : TIdContext;
	ABuffer : TBuffer
);
var
	AID		: LongWord;
	CID		: LongWord;
	Offline		: Byte;
	FriendList	: TCharacterList;
	ACharacter		: TCharacter;
	Index		: Byte;
	ZoneID		: Integer;
	ZoneIndex	: Integer;
begin
	ACharacter := TCharacter.Create(AClient);
	AID	:= BufferReadLongWord(2, ABuffer);
	CID	:= BufferReadLongWord(6, ABuffer);
	Offline	:= BufferReadByte(10, ABuffer);

	ACharacter.ID := CID;
	ACharacter.AccountID := AID;

	with TZoneServerLink(AClient.Data).DatabaseLink.Friend do
	begin
		FriendList := TCharacterList.Create(TRUE);
		LoadList(FriendList, ACharacter);
	end;
	ACharacter.Free;

	with MainProc.InterServer do
	if Assigned(FriendList) then
	begin
		if FriendList.Count > 0 then
		begin
			for Index := 0 to FriendList.Count - 1 do
			begin
				ACharacter := FriendList.Items[Index];

				// OK, we got all chars.
				// let's SPAM 'EM
				with TZoneServerLink(AClient.Data).DatabaseLink.Map do
				begin
						ZoneID := GetZoneID(ACharacter.Map);
				end;
				ZoneIndex := ZoneServerList.IndexOf(ZoneID);
				if ZoneIndex > -1 then
				begin
					InterSendFriendOnlineStatus(
						ZoneServerInfo[ZoneIndex].Connection,
						AID,
						CID,
						ACharacter.AccountID,
						ACharacter.ID,
						TZoneServerLink(AClient.Data).Info.ZoneID,
						Offline
					);
				end;
			end;
		end;
		FriendList.Clear;
		FriendList.Free;
	end;
end;{RecvZonePlayerOnlineStatus}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvZonePlayerOnlineReply                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//     Target zone replied something, time to send back to origin zone!
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/12/??] - Aeomin - Created
//------------------------------------------------------------------------------
procedure RecvZonePlayerOnlineReply(
	AClient : TIdContext;
	ABuffer : TBuffer
);
var
	AID     : LongWord;
	CID     : LongWord;
	TargetID: LongWord;
	Offline : Byte;
	ZoneID  : LongWord;
	Index   : Integer;
begin
	AID      := BufferReadLongWord(2, ABuffer);
	CID      := BufferReadLongWord(6, ABuffer);
	TargetID := BufferReadLongWord(10, ABuffer);
	OffLine  := BufferReadByte(14, ABuffer);
	ZoneID   := BufferReadLongWord(15, ABuffer);

	with MainProc.InterServer do
	begin
		Index := ZoneServerList.IndexOf(ZoneID);

		if (Index > -1) then
		begin
			InterSendFriendStatusReply(
				ZoneServerInfo[Index].Connection,
				AID,
				CID,
				TargetID,
				Offline
			);
		end;
	end;
end;{RecvZonePlayerOnlineReply}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvZoneNewMailNotify                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//	Load mail then notify the player (if online)
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2008/08/11] Aeomin - Created
//------------------------------------------------------------------------------
procedure RecvZoneNewMailNotify(
	AClient : TIdContext;
	ABuffer : TBuffer
);
var
	CharID : LongWord;
	MailID : LongWord;
	Mail : TMail;
	TargetChara : TCharacter;
	ZoneID : Integer;
	ZoneIndex : Integer;
begin
	MailID := BufferReadLongWord(2, ABuffer);
	Mail := TZoneServerLink(AClient.Data).DatabaseLink.Mail.Get(
			MailID
		);

	TargetChara := TCharacter.Create(nil);
	TargetChara.ID := Mail.ReceiverID;
	try
		TZoneServerLink(AClient.Data).DatabaseLink.Character.Load(
			TargetChara
		);

		ZoneID := TZoneServerLink(AClient.Data).DatabaseLink.Map.GetZoneID(TargetChara.Map);
		if ZoneID > -1 then
		begin
			ZoneIndex := MainProc.InterServer.ZoneServerList.IndexOf(ZoneID);
			if ZoneIndex > -1 then
			begin
				InterSendMailNotify(
					TZoneServerInfo(MainProc.InterServer.ZoneServerList.Objects[ZoneIndex]).Connection,
					Mail.ReceiverID,
					Mail.ID,
					Mail.SenderName,
					Mail.Title
				);
			end;
		end;
	finally
		Mail.Free;
		TargetChara.Free;
	end;
end;{RecvZoneNewMailNotify}
//------------------------------------------------------------------------------
end.
