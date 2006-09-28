unit ZoneCore;

(*------------------------------------------------------------------------------
Description:
	Handles incoming packets for the zone.  Used from Prometheus under clause 10
	of GPL

	10. If you wish to incorporate parts of the Program into other free
programs whose distribution conditions are different, write to the author
to ask for permission.  For software which is copyrighted by the Free
Software Foundation, write to the Free Software Foundation; we sometimes
make exceptions for this.  Our decision will be guided by the two goals
of preserving the free status of all derivatives of our free software and
of promoting the sharing and reuse of software generally.

If you still aren't convince the legality of using this...I'm still allowed to
use it because it was written on paper verbatum before use, so I still own the
copyright, besides, no one sues the author for copyright infringing his own work

--
Revisions:
--
[2005/05/29] CR - Created Unit Header
[2005/10/18] CR - Changed SendBuf syntax in comments above for newer SendBuf
	method of TChara
[2006/01/12] Tsusai - Explaination wording fixed.
[2006/09/26] Tsusai - Imported to Helios
[yyyy/mm/dd] <Author> - <Desc of Changes>
------------------------------------------------------------------------------*)



interface
uses
	{Helios}
	{3rd Party}
	IdTCPServer
	;

	procedure ProcessZonePacket(AThread : TIdPeerThread);


implementation
uses
	{VCL/RTL}
	Classes,
	SysUtils,
	{Helios}
	Character,
	Console,
	DatabaseTXT,
	Globals,
	PacketTypes,
	Socket,
	ZoneRecv
	{3rd Party}
	//none
	;

(*------------------------------------------------------------------------------
SearchPacketListing
This is only executed to find information about a packet for ingame/zone packets.
TChara - required
Socket - we are passign the buffer over so we can make sure we got the right
				 length of the packet
Ver - The version of packets we are going to look at.  This is called twice, first
			if we need to look through it with our new client packet, and if we can't
			find it, we need to search through the 0 version aka oldschool aka 0628sakexe
			based.
packet - The integer packet id.

[2006/03/11] Tsusai - Packet is now a word type, not a 0xHexString
[2006/08/12] Tsusai - Updated for Indy.
[2006/09/26] Tsusai - Imported to Helios
[2006/09/28] RaX - Variables re-cased, moved "begins" to next line.
------------------------------------------------------------------------------*)
function SearchPacketListing(

	var   AChara    : TCharacter;
	var   AThread   : TIdPeerThread;
	var   InBuffer  : TBuffer;

	const Version   : Word;
	const Packet    : Word

) :Boolean;
var
	Index : Integer;
	Size  : Word;
begin
	Result := False;

	for Index := 0 to Length(CodeBase[Version].Packet) - 1 do
  begin

		with CodeBase[Version].Packet[Index] do
    begin

			if (ID = Packet) then
      begin

				//Ok so we found a matching packet ID, now to read our right length
				if PLength <> -1 then
        begin
					AThread.Connection.ReadBuffer(InBuffer[2], PLength - 2);
        end else
        begin
					//Usually our string messages, where the 2nd location is the
					//size holder of the string
					AThread.Connection.ReadBuffer(InBuffer[2], 2);
					Size := BufferReadWord(2, InBuffer);
					AThread.Connection.ReadBuffer(InBuffer[4], Size - 4);
				end;

				//Execute the packet procedure, only one is runned because the other
				//runs a dummy procedure
				ExecCommand(AChara,ReadPoints);
				ExecAvoidSelfCommand(AChara);
				Result := True;
			end;
		end;
		if Result then break;//Breaks loop if packet found.
	end;
end;


(*-----------------------------------------------------------------------------*
Proc ProcessZonePacket
--
Overview:
--
TODO: None

If our TChara is nil, we gotta find a version to load, else call
SearchPacketListing

Original //Comments by Tsusai

--
Pre: None
Post: None

--
Revisions:
--
[2005/07/11] CR - Added Comment Header, Reindented, using newer syntax for
	MapConnect that is MUCH less verbose.
[2006/03/11] Tsusai - Removed packet id -> hex string conversions.  CIdx, and
 packetid now words.
[2006/08/12] Tsusai - Updated for Indy.
[2006/09/26] Tsusai - Imported to Helios
[2006/09/28] RaX - Re-formatted, re-indented, renamed - PIdx to PacketIndex,
  CIdx to ClientBaseIndex to signify our supported client database or packet_db.
[yyyy/mm/dd] <Author> - <Comment>
*-----------------------------------------------------------------------------*)
Procedure ProcessZonePacket(AThread : TIdPeerThread);
Var
	Lth             : Integer;
	AChara          : TCharacter;
	ClientBaseIndex : Word; //Index of the packet in the packet(allowed client)
                          //database (client-base).
  PacketID        : Word; //The ID of a packet in said database.
	PacketIndex     : Integer;
	Found           : Boolean;
	ABuffer         : TBuffer;
Begin
	AThread.Connection.ReadFromStack(False, -1, False);
	while AThread.Connection.InputBuffer.Size >= 2 do
	begin
		Lth := AThread.Connection.InputBuffer.Size;
		AThread.Connection.ReadBuffer(ABuffer[0], 2);
		PacketID := BufferReadWord(0, ABuffer);
		AChara := TThreadLink(AThread.Data).CharacterLink;

		{if (AChara <> nil) and (Option_Packet_Out) then
		begin
			Console(Format('3:%.8d CMD %.4x', [AChara.ID, PacketID]));
		end;}

		Found := False;
		if NOT Assigned(AChara) then
		begin
			for ClientBaseIndex := (Length(CodeBase) -1) downto 0 do
			begin //Go through all the client-bases w/ packets

				for PacketIndex := 0 to Length(CodeBase[ClientBaseIndex].Packet) - 1 do
				begin //Search for the packet from this client-base

					if (CodeBase[ClientBaseIndex].Packet[PacketIndex].ID = PacketID) then
					begin
						if (CodeBase[ClientBaseIndex].Packet[PacketIndex].PLength = Lth) then
						begin
							if (CodeBase[ClientBaseIndex].Packet[PacketIndex].Command = 'wanttoconnection') then
							begin
								AThread.Connection.ReadBuffer(ABuffer[2], (Lth - 2)); //Get the rest of the packet info
								//MapConnect(CIdx, AThread, CodeBase[CIdx].Packet[PIdx].ReadPoints);
								Found := True;
								Break;
							end;
						end;
					end;
				end;

				if Found then
				begin
					Break;
				end;
			end;

			if NOT Found then
			begin
				//They can't get in, so prevent the rest of their useless packets from parsing
				MainProc.Console('Someone with the IP '+
					AThread.Connection.Socket.Binding.PeerIP +
					' attempted to send a packet '+ IntToHex(packetID, 4) +' with a length of ' + IntToStr(Lth));
				MainProc.Console('Reason for this response: Unsupported client or a bot attempted to connect.');
				AThread.Connection.Disconnect;
			end;

		end else begin
			if AChara.ClientVersion <> 0 then
			begin
				if not SearchPacketListing(AChara, AThread, ABuffer, AChara.ClientVersion, PacketID) then
				begin //look for
					if NOT SearchPacketListing(AChara, AThread, ABuffer, 0, PacketID) then
					begin
						Exit; //try the older
					end;
				end;
			end
			else if NOT SearchPacketListing(AChara, AThread, ABuffer, 0, PacketID) then
			begin //since it's older, use that
				Exit;
			end;
		end;
	end;
End; (* Proc ProcessZonePacket
*-----------------------------------------------------------------------------*)

end.
