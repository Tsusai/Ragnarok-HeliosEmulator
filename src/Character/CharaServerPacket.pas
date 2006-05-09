unit CharaServerPacket;

interface
uses
	IdTCPServer;

	procedure ParseCharaServ(AThread : TIdPeerThread);

implementation
	uses
		Socket,
		CharaServerTypes,
		PacketTypes,
		Console;

// SendCharas - RaX - Stubbed for later use.
procedure SendCharas();
begin

end;

// SendCharaToMap - RaX - Stubbed for later use.
procedure SendCharaToMap();
begin

end;

// CreateChara - RaX - Stubbed for later use.
procedure CreateChara();
begin

end;

// DeleteChara - RaX - Stubbed for later use.
procedure DeleteChara();
begin

end;

// ParseCharaServ - RaX - Parses packets to figure out what the client wants.
procedure ParseCharaServ(AThread : TIdPeerThread);
var
	PacketLength : Integer;
	ABuffer : TBuffer;
	PacketID : Word;
begin
	if AThread.Connection.Connected then
	begin
		PacketLength := AThread.Connection.ReadFromStack;
		if PacketLength >= 2 then
		begin
			AThread.Connection.ReadBuffer(ABuffer,PacketLength);
			PacketID := BufferReadWord(0, ABuffer);
			case PacketID of
			$0065: // Client connection - Return Characters
				begin
					SendCharas();
				end;
			$0066: // Character Selected -- Refer Client to Map Server
				begin
					SendCharaToMap();
				end;
			$0067: // Create New Character
				begin
					CreateChara();
				end;
			$0068: // Request to Delete Character
				begin
					DeleteChara();
				end;
			end;
		end;
	end;
end;
end.




