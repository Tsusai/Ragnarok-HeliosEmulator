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


implementation


uses
	{RTL/VCL}
	Classes,
	{Project}
	BufferIO,
	Character,
	Main,
	ZoneServerInfo,
	ZoneInterCommunication,
	InterSend,
	GMCommands,
	Account
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
	CommandSeparator : TStringList;
	CommandID        : Word;
	Index            : Integer;
	ListClient       : TIdContext;
	ACharacter       : TCharacter;
	ZoneID           : Integer;
	LoopIndex        : Integer;
	ZoneLink         : TZoneServerLink;
	AAccount         : TAccount;
Begin
	//See Notes/GMCommand Packets.txt
	GMID          := BufferReadLongWord(4, InBuffer);
	CharaID       := BufferReadLongWord(8, InBuffer);
	CommandLength := BufferReadWord(12,InBuffer);
	CommandString := BufferReadString(14, CommandLength, InBuffer);

	CommandID := MainProc.InterServer.Commands.GetCommandID(
								MainProc.InterServer.Commands.GetCommandName(CommandString)
								);

	CommandSeparator := TStringList.Create;
	try
		CommandSeparator.Delimiter := ',';
		CommandSeparator.DelimitedText := CommandString;
		//Get command type
		case MainProc.InterServer.Commands.GetCommandType(CommandID) of
			//Send to ALL zones!
			TYPE_ALLPLAYERS,
			TYPE_BROADCAST: begin
				//after getting the command information, we get ready to send it to the
				//other zones.
				for Index := 0 to (MainProc.InterServer.fClientList.Count - 1) do
				begin
					if Assigned(MainProc.InterServer.ZoneServerLink[Index]) then
					begin
						ListClient := MainProc.InterServer.ClientList[Index];
						InterSendGMCommandToZone(ListClient, GMID, CharaID, CommandSeparator);
					end;
				end;
			end;

			//Send back to orignal Zone
			TYPE_RETURNBACK: begin
				InterSendGMCommandToZone(AClient, GMID, CharaID, CommandSeparator);
			end;

			//We are controling other player
			TYPE_TARGETCHAR: begin
				with TZoneServerLink(AClient.Data).DatabaseLink do
				begin
					//At least 2 parameters required
					if CommandSeparator.Count >= 2 then
					begin
						GameData.Connect;
						ACharacter := GameData.LoadChara(CommandSeparator[1]);
						GameData.Disconnect;
						if ACharacter <> nil then
						begin
							StaticData.Connect;
							ZoneID := StaticData.GetMapZoneID(ACharacter.Map);
							Index := -1;
							for LoopIndex := (MainProc.InterServer.fClientList.Count - 1) downto 0 do
							begin
								ZoneLink := MainProc.InterServer.ZoneServerLink[LoopIndex];
								if (ZoneLink <> NIL) AND
								(ZoneLink.Info.ZoneID = ZoneID) then
								begin
									Index := LoopIndex;
									Break;
								end;
							end;
							StaticData.Disconnect;
							if Index > -1 then
							begin
								//Same thing, but extra 2 parameter to store target character
								InterSendGMCommandToZone(MainProc.InterServer.ClientList[Index], GMID, CharaID, CommandSeparator, ACharacter.ID, ACharacter.CID);
							end;
						end else
						begin
							InterSendGMCommandReplyToZone(AClient, CharaID, 'Character ''' + CommandSeparator[1] + ''' not found!');
						end;
					end;
				end;
			end;

			//Map based control, ex. Kill all players in prontera
			TYPE_TARGETMAP: begin
				with TZoneServerLink(AClient.Data).DatabaseLink do
				begin
					if CommandSeparator.Count >= 2 then
					begin
						StaticData.Connect;
						ZoneID := StaticData.GetMapZoneID(CommandSeparator[1]);
						if ZoneID > -1 then
						begin
							Index := -1;
							for LoopIndex := 0 to (MainProc.InterServer.fClientList.Count - 1) do
							begin
								ZoneLink := MainProc.InterServer.ZoneServerLink[LoopIndex];
								if (ZoneLink <> NIL) AND
								(ZoneLink.Info.ZoneID = ZoneID) then
								begin
									Index := LoopIndex;
									Break;
								end;
							end;
							StaticData.Disconnect;
							if Index > -1 then
							begin
								InterSendGMCommandToZone(MainProc.InterServer.ClientList[Index], GMID, CharaID, CommandSeparator);
							end;
						end else
						begin
							InterSendGMCommandReplyToZone(AClient, CharaID, 'Map name ''' + CommandSeparator[1] + ''' not found!');
						end;
					end;
				end;
			end;
		end;
	finally
		CommandSeparator.Free;
	end;
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
*-----------------------------------------------------------------------------*)
Procedure RecvGMCommandReply(
		AClient : TIdContext;
		InBuffer : TBuffer
	);
//See Notes/GM Command Packets for explanation.
Var
	ErrorLength : Word;
	CharacterID : LongWord;
	ACharacter  : TCharacter;
	ZoneID      : LongWord;
	ZoneLink    : TZoneServerLink;
	Index       : Integer;
	ListClient  : TIdContext;
	Size        : LongWord;
	Error       : String;
Begin
	with MainProc.InterServer do
	begin
		//See Notes/GMCommand Packets.txt
		ErrorLength := BufferReadWord(14, InBuffer);
		Error       := BufferReadString(16, ErrorLength, InBuffer);
		if (ErrorLength > 0) then
		begin
			Size := BufferReadLongWord(2, InBuffer);
			CharacterID := BufferReadLongWord(10, InBuffer);

			with TThreadLink(AClient.Data).DatabaseLink do
			begin
				GameData.Connect;
        try
				  ACharacter := GameData.GetChara(CharacterID);
        finally
				  GameData.Disconnect;
        end;

				StaticData.Connect;
        try
				  ZoneID := StaticData.GetMapZoneID(ACharacter.Map);
        finally
				  StaticData.Disconnect;
        end;
			end;

			for Index := 0 to (fClientList.Count - 1) do
			begin
				ZoneLink := ZoneServerLink[Index];
				if (ZoneLink <> NIL) AND
					(ZoneLink.Info.ZoneID = ZoneID) then
				begin
					ListClient := ClientList[Index];
					InterSendGMCommandReplyToZone(ListClient, CharacterID, Error);
//					SendBuffer(ListClient, InBuffer, Size);
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
	LoopIndex  : Integer;
	SentWhisper : Boolean;
	ZoneLink    : TZoneServerLink;
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
			Index := -1;
			for LoopIndex := 0 to (fClientList.Count - 1) do
			begin
				ZoneLink := ZoneServerLink[LoopIndex];
				if (ZoneLink <> NIL) AND
					(ZoneLink.Info.ZoneID = ZoneID) then
				begin
					Index := LoopIndex;
					Break;
				end;
			end;

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
				SendWhisperReplyToZone(AClient, FromID, WHISPER_FAILED);
			end;
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
