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

procedure RecvZoneCreateInstanceMapRequest(
	AClient : TIdContext;
	ABuffer : TBuffer
);

procedure RecvSetAllowInstance(
	AClient : TIdContext;
	InBuffer : TBuffer
);

procedure RecvInstanceCreated(
	AClient : TIdContext;
	InBuffer : TBuffer
);

procedure RecvInstanceList(
	AClient : TIdContext;
	InBuffer : TBuffer
);

procedure RecvDeleteInstance(
	AClient : TIdContext;
	InBuffer : TBuffer
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
	BeingList,
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
	function GetZoneByMap:Boolean;
	begin
		with TThreadLink(AClient.Data).DatabaseLink.Map do
		begin
			ZoneID := GetZoneID(MapName);
			Result := True;
		end;
	end;
	function GetZoneByInstanceMap:Boolean;
	var
		Index : Integer;
	begin
		Result := False;
		Index := MainProc.InterServer.Instances.IndexOf(MapName);
		if (Index > -1) then
		begin
			ZoneID := TZoneServerLink(MainProc.InterServer.Instances.Objects[Index]).Info.ZoneID;
			Result := True;
		end;
	end;
Begin
	CharacterID	:= BufferReadLongWord(4, ABuffer);
	X		:= BufferReadWord(8, ABuffer);
	Y		:= BufferReadWord(10, ABuffer);
	MapNameSize	:= BufferReadWord(12, ABuffer);
	MapName		:= BufferReadString(14, MapNameSize, ABuffer);

	ClientIPSize	:= BufferReadWord(14+MapNameSize, ABuffer);
	ClientIP	:= BufferReadString(16+MapNameSize, ClientIPSize, ABuffer);

	if Pos('#', MapName) > 0 then
	begin
		if NOT GetZoneByInstanceMap then
  			GetZoneByMap;
	end else
	begin
		GetZoneByMap;
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
	FriendList	: TBeingList;
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
		FriendList := TBeingList.Create(TRUE);
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
				ACharacter := FriendList.Items[Index] as TCharacter;

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


//------------------------------------------------------------------------------
//RecvZoneCreateInstanceMapRequest                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Want create an instance map?
//	Changes -
//		[2008/12/06] Aeomin - Created
//------------------------------------------------------------------------------
procedure RecvZoneCreateInstanceMapRequest(
	AClient : TIdContext;
	ABuffer : TBuffer
);
var
	Size : Word;
	Identifier : String;
	MapName : String;
	CharID : LongWord;
	ScriptID : LongWord;
	FullName : String;

	Index : Integer;
	ZoneServerLink : TZoneServerLink;
begin
	Size       := BufferReadWord(2, ABuffer);
	MapName    := BufferReadString(4, 16, Abuffer);
	CharID     := BufferReadLongWord(20, ABuffer);
	ScriptID   := BufferReadLongWord(24, ABuffer);
	Identifier := BufferReadString(28, Size-28, Abuffer);

	FullName := Identifier + '#' + MapName;

	if MainProc.InterServer.InstanceZones.Count > 0 then
	begin
		Index := MainProc.InterServer.Instances.IndexOf(FullName);
		if Index > -1 then
		begin
			InterSendInstanceMapResult(
				AClient,
				CharID,
				ScriptID,
				1,
				FullName
			);
		end else
		begin
			//Randomly pick one
			Index := Random(MainProc.InterServer.InstanceZones.Count);
			ZoneServerLink := MainProc.InterServer.InstanceZones.Items[Index];
			//TELL 'EM TELL 'EM, "WE ARE ALIVE"
			InterSendCreateInstance(
				ZoneServerLink.Info.Connection,
				TZoneServerLink(AClient.Data).Info.ZoneID,
				CharID,
				ScriptID,
				Identifier,
				MapName
			);
		end;
	end else
	begin
		InterSendInstanceMapResult(
			AClient,
			CharID,
			ScriptID,
			1,
			FullName
		);
	end;
end;{RecvZoneCreateInstanceMapRequest}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvSetAllowInstance                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Is zone server allow instance?
//
//	Changes -
//		[2008/12/06] Aeomin - Created
//------------------------------------------------------------------------------
procedure RecvSetAllowInstance(
	AClient : TIdContext;
	InBuffer : TBuffer
);
var
	Flag : Boolean;
	Index : Integer;
begin
	Flag  := Boolean(BufferReadByte(2, InBuffer));
	if TZoneServerLink(AClient.Data).Info.AllowInstance and NOT Flag then
	begin
		//Clean up
		Index := MainProc.InterServer.InstanceZones.IndexOf(AClient.Data);
		if Index > -1 then
		begin
			MainProc.InterServer.InstanceZones.Delete(Index);
		end;

		Index := MainProc.InterServer.Instances.IndexOfObject(AClient.Data);
		while Index > -1 do
		begin
			MainProc.InterServer.Instances.Delete(Index);
			Index := MainProc.InterServer.Instances.IndexOfObject(AClient.Data);
		end;
	end else
	if NOT TZoneServerLink(AClient.Data).Info.AllowInstance AND Flag then
	begin
		//Add to record
		MainProc.InterServer.InstanceZones.Add(AClient.Data);
	end;
	TZoneServerLink(AClient.Data).Info.AllowInstance := Flag;
end;{RecvSetAllowInstance}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvInstanceCreated                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		So instance created eh? let's do something about it...
//
//	Changes -
//		[2008/12/07] Aeomin - Created
//------------------------------------------------------------------------------
procedure RecvInstanceCreated(
	AClient : TIdContext;
	InBuffer : TBuffer
);
var
	Size : Word;
	ZoneID : LongWord;
	CharID : LongWord;
	ScriptID : LongWord;
	FullName : String;

	Index : Integer;
begin
	Size       := BufferReadWord(2, InBuffer);
	ZoneID     := BufferReadLongWord(4, InBuffer);
	CharID     := BufferReadLongWord(8, InBuffer);
	ScriptID   := BufferReadLongWord(12, InBuffer);
	FullName := BufferReadString(16, Size - 16, InBuffer);

	with MainProc.InterServer do
	begin
		Index := ZoneServerList.IndexOf(ZoneID);
		if (Index > -1) then
		begin
			if Assigned(ZoneServerLink[Index]) AND
				(ZoneServerLink[Index].Info.ZoneID = ZoneID) then
			begin
				Instances.AddObject(FullName,AClient.Data);
				InterSendInstanceMapResult(
					ZoneServerInfo[Index].Connection,
					CharID,
					ScriptID,
					0,
					FullName
				);
			end;
		end;
	end;
end;{RecvInstanceCreated}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvInstanceList                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Instance list, this should NOT commonly occur!
//
//	Changes -
//		[2008/12/09] Aeomin - Created
//------------------------------------------------------------------------------
procedure RecvInstanceList(
	AClient : TIdContext;
	InBuffer : TBuffer
);
var
	Count : Byte;
	Index : Byte;
	OffSet : Word;
	NameLen : Byte;
	Name : String;
begin
	Count := BufferReadByte(4, InBuffer);
	OffSet := 5;
	for Index := 1 to Count do
	begin
		NameLen := BufferReadByte(OffSet, InBuffer);
		Name := BufferReadString(OffSet+1,NameLen,InBuffer);
		Inc(OffSet, NameLen + 1 );
		MainProc.InterServer.Instances.AddObject(Name,AClient.Data);
	end;
end;{RecvInstanceList}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RecvDeleteInstance                                                   PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Delete instance.
//
//	Changes -
//		[2009/04/17] Aeomin - Created
//------------------------------------------------------------------------------
procedure RecvDeleteInstance(
	AClient : TIdContext;
	InBuffer : TBuffer
);
var
	Index : Integer;
	NameLen : Byte;
	Name : String;
begin
	NameLen := BufferReadByte(4, InBuffer);
	Name := BufferReadString(5,NameLen,InBuffer);
	Index := MainProc.InterServer.Instances.IndexOf(Name);
	if Index > -1 then
	begin
		MainProc.InterServer.Instances.Delete(Index);
	end;
end;{RecvDeleteInstance}
//------------------------------------------------------------------------------
end.
