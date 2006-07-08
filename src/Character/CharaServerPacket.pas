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
		AccountDB,
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
	ReplyBuffer : TBuffer;
	idx : byte;
begin
	Success := false;
	AccountID := Socket.BufferReadCardinal(2, ABuffer);
	SQLQueryResult :=
		SQLConnection.query(
			Format('SELECT userid FROM login WHERE account_id = %d;',[AccountID]),true,Success);
	if Success then begin
		if SQLqueryResult.RowsCount > 0 then
		begin
			AnAccount := FindAccount(SQLQueryResult.FieldValue(0));
			if Assigned(AnAccount) then
			begin
				if AnAccount.ID = AccountID then
				begin
					if (AnAccount.LoginKey[1] = Socket.BufferReadCardinal(6,ABuffer)) and
						(AnAccount.LoginKey[2] = Socket.BufferReadCardinal(10,ABuffer)) then
					begin
						//Unknown why we need this spacer, but its required. Probably anti-hack
						Socket.WriteBufferCardinal(0,$00000000,ReplyBuffer);
						AThread.Connection.WriteBuffer(ReplyBuffer,4);

						SQLQueryResult := SQLConnection.query(
							Format('SELECT char_id FROM `char` WHERE account_id = %d;',[AccountID]),true,Success);
						if Success then
						begin
							for idx := 0 to SQLQueryResult.RowsCount do
							begin
								//Add loop code for sending/loading character information
								//If character doesn't exist, load ID and name from SQL
								//add to CharacterList
								//Rest of the character parameters will be loaded via
								//TCharacter.Property SQL calls
							end;
								Socket.WriteBufferWord(0,$006b,ReplyBuffer); //header
								Socket.WriteBufferWord(2,24,ReplyBuffer); //size (24 + count *106)
								AThread.Connection.WriteBuffer(ReplyBuffer,24);//size (24 + count *106)
						end;
					end;
				end;
			end;
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




