//------------------------------------------------------------------------------
//InterSend			                                                        UNIT
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
			Error            : String
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
	Main
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
		WriteBufferLongWord(14, TargetAID, ABuffer);
		WriteBufferLongWord(18, TargetCID, ABuffer);
		WriteBufferWord(22, CommandSeparator.Count - 1, ABuffer);
		BufferIndex := 24;//Where we are currently in the buffer.

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
		CommandSeparator[0] := Commands.GetCommandName(CommandSeparator[0]);
		//Get the command ID to which the Command name is associated.
		CommandID := Commands.GetCommandID(CommandSeparator[0]);

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
		Error            : String
);
var
	Size        : Word;
	ABuffer     : TBuffer;
begin
	Size := StrLen(PChar(Error));
	WriteBufferWord(0, $2207, ABuffer);
	WriteBufferWord(2, Size + 10, ABuffer);
	WriteBufferLongWord(4, CID, ABuffer);
	WriteBufferWord(8, Size, ABuffer);
	WriteBufferString(10, Error, Size, ABuffer);
	SendBuffer(AClient, ABuffer, Size + 10);
end;
//------------------------------------------------------------------------------
end.
