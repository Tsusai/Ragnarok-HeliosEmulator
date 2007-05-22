//------------------------------------------------------------------------------
//InterServer			                                                        UNIT
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


interface


uses
	{RTL/VCL}
	Classes,

	{Project}
	PacketTypes,

	{3rd Party}
	IdContext
	;

procedure RecvGMCommand(AClient : TIdContext; InBuffer : TBuffer);
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
implementation


uses
	{RTL/VCL}

	{Project}
	//none
	BufferIO,
	Character,

	Main,
	ZoneServerInfo,
	ZoneInterCommunication,
	InterSend
	{3rd Party}
	//none
	;

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
procedure RecvGMCommand(AClient : TIdContext; InBuffer : TBuffer);
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
Procedure RecvGMCommandReply(
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
	with MainProc.InterServer do
	begin
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
	with MainProc.InterServer do
	begin
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
		for Index := (fClientList.Count - 1) downto 0 do
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
		ZServerInfo		:= MainProc.InterServer.ZoneServerInfo[MainProc.InterServer.fZoneServerList.IndexOf(ZoneID)];
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

end.
