//------------------------------------------------------------------------------
//InterSend                                                                 UNIT
//------------------------------------------------------------------------------
//	What it does-
//      Contains any routines that involve sending information out from the
//		inter server.
//
//	Changes -
//		May 22nd, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit InterSend;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	Classes,
	{Project}
	{3rd Party}
	IdContext
	;

	Procedure InterSendWarpReplyToZone(
		AClient				: TIdContext;
		CharacterID		: LongWord;
		ReturnIPCard	: LongWord;
		ReturnPort		: Word;
		MapName				: String;
		X							: Word;
		Y							: Word
	);

	Procedure InterSendGMCommandToZone(
			AClient          : TIdContext;
		const
			GMID             : LongWord;
		const
			CharacterID      : LongWord;
		const
			ZoneID           : LongWord;
		const
			CommandSeparator : TStringList;
		const
			TargetAID: LongWord = 0;
		const
			TargetCID: LongWord = 0
	);

	procedure InterSendGMCommandReplyToZone(
			AClient          : TIdContext;
		const
			CID              : LongWord;
		const
			Error            : TStringList
	);

	procedure InterParseGMCommand(
		const
			AClient : TIdContext;
		const
			GMID             : LongWord;
		const
			CharaID          : LongWord;
		const
			AZoneID          : LongWord;
		const
			CommandString    : String
	);

	procedure InterSendFriendRequest(
			AClient : TIdContext;
			const ReqAID, ReqID  : LongWord;
			const ReqName  : String;
			const TargetChar : LongWord
		);

	procedure InterSendFriendRequestReply(
			AClient : TIdContext;
			const OrigID : LongWord;
			const AccID  : LongWord;
			const CharID : LongWord;
			const CharName : String;
			const Reply  : Byte
		);

	procedure InterSendFriendOnlineStatus(
			AClient : TIdContext;
			const AID : LongWord;
			const CID : LongWord;
			const TargetAID : LongWord;
			const TargetCID : LongWord;
			const ZoneID    : LongWord;
			const Offline	: Byte
		);

	procedure InterSendFriendStatusReply(
			AClient : TIdContext;
			const AID : LongWord;
			const CID : LongWord;
			const TargetID : LongWord;
			const Offline : Byte
		);
implementation


uses
	{RTL/VCL}
	SysUtils,
	{Project}
	//none
	PacketTypes,
	GMCommands,
	BufferIO,
	Main,
	Character,
	TCPServerRoutines,
	GameConstants
	{3rd Party}
	//none
	;


(*- Procedure -----------------------------------------------------------------*
InterSendGMCommandToZone
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
Procedure InterSendGMCommandToZone(
			AClient          : TIdContext;
		const
			GMID             : LongWord;
		const
			CharacterID      : LongWord;
		const
			ZoneID           : LongWord;
		const
			CommandSeparator : TStringList;
		const
			TargetAID: LongWord = 0;
		const
			TargetCID: LongWord = 0
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
		WriteBufferLongWord(14, ZoneID, ABuffer);
		WriteBufferLongWord(18, TargetAID, ABuffer);
		WriteBufferLongWord(22, TargetCID, ABuffer);
		WriteBufferWord(26, CommandSeparator.Count - 1, ABuffer);
		BufferIndex := 28;//Where we are currently in the buffer.

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

Begin
	with MainProc.InterServer do
	begin
		//Get the name of the command, remove the first character which is #
		//Get the command ID to which the Command name is associated.
		CommandID := Commands.GetCommandID(
						Commands.GetCommandName(CommandSeparator[0])
						);

		//Test if valid gm command.
		if (CommandID >= 0) then
		begin
			//start writing packet 2206 (see Notes/GM Command Packets.txt)
			WritePacket2206;
			SendBuffer(AClient, ABuffer, Size);
		end;
	end;
End; (* Proc TInterServer.InterSendGMCommandToZone
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
Procedure InterSendWarpReplyToZone(
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


//------------------------------------------------------------------------------
//InterSendGMCommandReplyToZone                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Send command result to client
//
//	Changes -
//		[2007/08/09] Aeomin - Create.
//------------------------------------------------------------------------------
procedure InterSendGMCommandReplyToZone(
		AClient          : TIdContext;
	const
		CID              : LongWord;
	const
		Error            : TStringList
);
var
	ABuffer     : TBuffer;
	BufferIndex : Integer;
	CSLength    : Integer;
	Index       : Integer;
begin
	if Error.Count > 0 then
	begin
		WriteBufferWord(0, $2207, ABuffer);
		WriteBufferLongWord(4, CID, ABuffer);
		WriteBufferWord(8, Error.Count, ABuffer);

		BufferIndex := 10;
		for Index := 0 to Error.Count - 1 do
		begin
			CSLength := Length(Error[Index]);
			WriteBufferWord(BufferIndex, CSLength, ABuffer);
			Inc(BufferIndex, 2);

			WriteBufferString(
				BufferIndex,
				Error[Index],
				CSLength,
				ABuffer
			);
			Inc(BufferIndex, CSLength);
		end;
		WriteBufferWord(2, BufferIndex + 1, ABuffer);
		SendBuffer(AClient, ABuffer, BufferIndex + 1);
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//InterParseGMCommand                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		parse then send command result to client (Moved from RecvGMCommand)
//	AZoneID should be the zone where commad result send to
//
//	Changes -
//		[2007/08/13] Aeomin - Create, Moved from RecvGMCommand.
//------------------------------------------------------------------------------
procedure InterParseGMCommand(
		const
			AClient : TIdContext;
		const
			GMID             : LongWord;
		const
			CharaID          : LongWord;
		const
			AZoneID          : LongWord;
		const
			CommandString    : String
	);
var
	CommandID        : Word;
	CommandSeparator : TStringList;
	Index            : Integer;
	ListClient       : TIdContext;
	ACharacter       : TCharacter;
	ZoneID           : Integer;
	LoopIndex        : Integer;
	ZoneLink         : TZoneServerLink;
	Position         : Integer;
	Error            : TStringList;
begin
	CommandID := MainProc.InterServer.Commands.GetCommandID(
								MainProc.InterServer.Commands.GetCommandName(CommandString)
								);

	CommandSeparator := TStringList.Create;
	Error := TStringList.Create;
	try
		//Let's flag it!
		case MainProc.InterServer.Commands.GetCommandFlag(CommandID) of
			GMFLAG_NORMAL: begin
				CommandSeparator.Delimiter := ',';
				CommandSeparator.DelimitedText := CommandString;
			end;
			//DelimitedText will break parameter even when no need to, so using this way
			GMFLAG_NOSPLIT: begin
				Position := Pos(' ', CommandString);
				if Position > 0 then
				begin
					CommandSeparator.Add(Copy(CommandString, 1, Position - 1));
					CommandSeparator.Add(Copy(CommandString, Position + 1, StrLen(PChar(CommandString)) - Cardinal(Position)));
				end else
				begin
					CommandSeparator.Add(CommandString);
				end;
			end;
		end;

		//Get command type
		case MainProc.InterServer.Commands.GetCommandType(CommandID) of
			//Send to ALL zones!
			TYPE_ALLPLAYERS,
			TYPE_BROADCAST: begin
				//after getting the command information, we get ready to send it to the
				//other zones.
				for Index := (MainProc.InterServer.ClientList.Count - 1) downto 0 do
				begin
					if Assigned(MainProc.InterServer.ZoneServerLink[Index]) then
					begin
						ListClient := MainProc.InterServer.ClientList[Index];
						InterSendGMCommandToZone(ListClient, GMID, CharaID, AZoneID, CommandSeparator);
					end;
				end;
			end;

			//Send back to orignal Zone
			TYPE_RETURNBACK: begin
				InterSendGMCommandToZone(AClient, GMID, CharaID, AZoneID, CommandSeparator);
			end;

			//We are controling other player
			TYPE_TARGETCHAR: begin
				with TZoneServerLink(AClient.Data).DatabaseLink do
				begin
					//At least 2 parameters required
					if CommandSeparator.Count >= 2 then
					begin
						ACharacter := GameData.LoadChara(AClient, CommandSeparator[1]);
						if ACharacter <> nil then
						begin
							ZoneID := StaticData.GetMapZoneID(ACharacter.Map);
							Index := -1;
							for LoopIndex := (MainProc.InterServer.ClientList.Count - 1) downto 0 do
							begin
								ZoneLink := MainProc.InterServer.ZoneServerLink[LoopIndex];
								if (ZoneLink <> NIL) AND
								(ZoneLink.Info.ZoneID = Cardinal(ZoneID)) then
								begin
									Index := LoopIndex;
									Break;
								end;
							end;
							if Index > -1 then
							begin
								//Same thing, but extra 2 parameter to store target character
								InterSendGMCommandToZone(MainProc.InterServer.ClientList[Index], GMID, CharaID, AZoneID, CommandSeparator, ACharacter.ID, ACharacter.CID);
							end;
						end else
						begin
							Error.Add('Character ''' + CommandSeparator[1] + ''' not found!');
							InterSendGMCommandReplyToZone(AClient, CharaID, Error);
						end;
					end else
					begin
						Error.Add('Syntax Help:');
						Error.Add(MainProc.InterServer.Commands.GetSyntax(CommandID));
						InterSendGMCommandReplyToZone(AClient, CharaID, Error);
					end;
				end;
			end;

			//Map based control, ex. Kill all players in prontera
			TYPE_TARGETMAP: begin
				with TZoneServerLink(AClient.Data).DatabaseLink do
				begin
					if CommandSeparator.Count >= 2 then
					begin
						ZoneID := StaticData.GetMapZoneID(CommandSeparator[1]);
						if ZoneID > -1 then
						begin
							Index := -1;
							for LoopIndex := 0 to (MainProc.InterServer.ClientList.Count - 1) do
							begin
								ZoneLink := MainProc.InterServer.ZoneServerLink[LoopIndex];
								if (ZoneLink <> NIL) AND
								(ZoneLink.Info.ZoneID = Cardinal(ZoneID)) then
								begin
									Index := LoopIndex;
									Break;
								end;
							end;
							if Index > -1 then
							begin
								InterSendGMCommandToZone(MainProc.InterServer.ClientList[Index], GMID, CharaID, AZoneID, CommandSeparator);
							end;
						end else
						begin
							Error.Add('Map name ''' + CommandSeparator[1] + ''' not found!');
							InterSendGMCommandReplyToZone(AClient, CharaID, Error);
						end;
					end else
					begin
						Error.Add('Syntax Help:');
						Error.Add(MainProc.InterServer.Commands.GetSyntax(CommandID));
						InterSendGMCommandReplyToZone(AClient, CharaID, Error);
					end;
				end;
			end;
		end;
	finally
		CommandSeparator.Free;
		Error.Free;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//InterSendFirendRequest                                               PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Send add friend request to target zone
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    [2007/12/7] Aeomin - Created.
//------------------------------------------------------------------------------
procedure InterSendFriendRequest(
	AClient : TIdContext;
	const ReqAID, ReqID  : LongWord;
	const ReqName  : String;
	const TargetChar : LongWord
);
var
	OutBuffer   : TBuffer;
begin
	WriteBufferWord(0, $2216, OutBuffer);
	WriteBufferLongWord(2, ReqAID, OutBuffer);
	WriteBufferLongWord(6, ReqID, OutBuffer);
	WriteBufferLongWord(10, TargetChar, OutBuffer);
	WriteBufferString(14, ReqName, NAME_LENGTH, OutBuffer);
	SendBuffer(AClient, OutBuffer, GetPacketLength($2216));
end;{InterSendFirendRequest}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//InterSendFirendRequest                                               PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Send reply to zone
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//    [2007/12/08] Aeomin - Created.
//------------------------------------------------------------------------------
procedure InterSendFriendRequestReply(
	AClient : TIdContext;
	const OrigID : LongWord;
	const AccID  : LongWord;
	const CharID : LongWord;
	const CharName : String;
	const Reply  : Byte
);
var
	OutBuffer   : TBuffer;
begin
	// YEs using same packet...
	WriteBufferWord(0, $2217, OutBuffer);
	WriteBufferLongWord(2, OrigID, OutBuffer);
	WriteBufferLongWord(6, AccID, OutBuffer);
	WriteBufferLongWord(10, CharID, OutBuffer);
	WriteBufferByte(14, Reply, OutBuffer);
	WriteBufferString(15, CharName, NAME_LENGTH, OutBuffer);
	SendBuffer(AClient, OutBuffer, GetPacketLength($2217));
end;{InterSendFriendRequestReply}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//InterSendFriendOnlineStatus                                          PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//     Send status update to target zone.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/12/??] - Aeomin - Created
//------------------------------------------------------------------------------
procedure InterSendFriendOnlineStatus(
	AClient : TIdContext;
	const AID : LongWord;
	const CID : LongWord;
	const TargetAID : LongWord;
	const TargetCID : LongWord;
	const ZoneID    : LongWord;
	const Offline	: Byte
);
var
	OutBuffer   : TBuffer;
begin
	WriteBufferWord(0, $2219, OutBuffer);
	WriteBufferLongWord(2,  AID, OutBuffer);
	WriteBufferLongWord(6,  CID, OutBuffer);
	WriteBufferLongWord(10, TargetAID, OutBuffer);
	WriteBufferLongWord(14, TargetCID, OutBuffer);
	WriteBufferLongWord(18, ZoneID, OutBuffer);
	WriteBufferByte(22, Offline, OutBuffer);
	SendBuffer(AClient, OutBuffer, GetPacketLength($2219));
end;{InterSendFriendOnlineStatus}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//InterSendFriendStatusReply                                           PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//     Send status back to origin zone.
//--
//   Pre:
//	TODO
//   Post:
//	TODO
//--
//  Changes -
//	[2007/12/??] - Aeomin - Created
//------------------------------------------------------------------------------
procedure InterSendFriendStatusReply(
	AClient : TIdContext;
	const AID : LongWord;
	const CID : LongWord;
	const TargetID : LongWord;
	const Offline : Byte
);
var
	OutBuffer   : TBuffer;
begin
	WriteBufferWord(0, $2221, OutBuffer);
	WriteBufferLongWord(2,  AID, OutBuffer);
	WriteBufferLongWord(6,  CID, OutBuffer);
	WriteBufferLongWord(10,  TargetID, OutBuffer);
	WriteBufferByte(14, Offline, OutBuffer);
	SendBuffer(AClient, OutBuffer, GetPacketLength($2221));
end;{InterSendFriendStatusReply}
//------------------------------------------------------------------------------
end.
