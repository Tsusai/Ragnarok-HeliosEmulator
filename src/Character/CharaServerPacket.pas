unit CharaServerPacket;

interface
uses
	IdTCPServer;

	procedure ParseCharaServ(AThread : TIdPeerThread);

implementation
	uses
		SysUtils,
		Socket,
		CharaServerTypes,
		AccountTypes,
		PacketTypes,
		Globals,
		Console;

// SendCharas - RaX - Stubbed for later use.
procedure SendCharas(AThread : TIdPeerThread; var ABuffer : TBuffer);
var
	AccountID : Cardinal;
	Success : boolean;
	AnAccount : TAccount;
begin
	Success := false;
	AccountID := Socket.BufferReadCardinal(2, ABuffer);
	SQLQueryResult := SQLConnection.query('SELECT * FROM login WHERE account_id = 100100;',true,Success);
	if Success then begin
		if SQLqueryResult.RowsCount > 0 then
		begin
			Console('Account found in chara server');
			AnAccount := TAccount.Create;
			AnAccount.ID := StrToInt(SQLQueryResult.FieldValue(0));
			AnAccount.Username := SQlQueryResult.FieldValue(1);
		end;
	end;
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
	Link : TThreadLink;
begin
	if AThread.Connection.Connected then
	begin
		PacketLength := AThread.Connection.ReadFromStack;
		if PacketLength >= 2 then
		begin
			AThread.Connection.ReadBuffer(ABuffer,PacketLength);
			PacketID := BufferReadWord(0, ABuffer);
			if AThread.Data = nil then
			begin
				//Thread Data should have a TThreadLink object...if not, make one
				Link := TThreadLink.Create;
				AThread.Data := Link;
			end;
			//First time connection from login needs to do 0x0064.  No exceptions.
			if TThreadLink(AThread.Data).AccountLink = nil then
			begin
				if PacketID = $0065 then
				begin
					//Verify login and send characters
					SendCharas(AThread,ABuffer);
				end;
			end else
			begin
				case PacketID of
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
end;
end.




