(*------------------------------------------------------------------------------
CharaServerProcess
RaX/Tsusai 2006

Description:
 The Character Server.  Handles all client communication here.
------------------------------------------------------------------------------*)
unit CharaServerProcess;

interface
uses
	//3rd Party
	IdTCPServer;

	procedure ParseCharaServ(AThread : TIdPeerThread);

implementation
	uses
		//IDE
		SysUtils,
		//Helios
		Socket,
		AccountDB,
		AccountTypes,
		PacketTypes,
		Globals;

(*------------------------------------------------------------------------------
SendCharas

Verifies the new connection by checking the account ID which is recieved and
 the two random keys generated during login.

Upon validation, check database for any/all created characters.

[2006/07/06] Tsusai - Started work on changing dummy procedure to real procedure
------------------------------------------------------------------------------*)
procedure SendCharas(AThread : TIdPeerThread; var ABuffer : TBuffer);
var
	AccountID : Cardinal;
	Success : boolean;
	AnAccount : TAccount;
	ReplyBuffer : TBuffer;
	idx : byte;
begin
	Success := false;
	AccountID := BufferReadCardinal(2, ABuffer);
	SQLQueryResult :=
		SQLConnection.query(
			Format('SELECT userid FROM login WHERE account_id = %d;',
			[AccountID]),true,Success);
	if Success then begin
		if SQLqueryResult.RowsCount > 0 then
		begin
			AnAccount := FindAccount(SQLQueryResult.FieldValue(0));
			if Assigned(AnAccount) then
			begin
				if AnAccount.ID = AccountID then
				begin
					if (AnAccount.LoginKey[1] = BufferReadCardinal(6,ABuffer)) and
						(AnAccount.LoginKey[2] = BufferReadCardinal(10,ABuffer)) then
					begin
						//LINK the account to the client connection for the other procedures
						TThreadLink(AThread.Data).AccountLink := AnAccount;
						SendPadding(AThread); //Legacy padding
						SQLQueryResult := SQLConnection.query(
							Format('SELECT char_id FROM `char` WHERE account_id = %d;',
								[AccountID]),true,Success);
						if Success then
						begin
							for idx := 0 to SQLQueryResult.RowsCount do
							begin
								{ TODO -oTsusai -cCharacterServer :
								Add loop code for passing characters to client }
							end;
								WriteBufferWord(0,$006b,ReplyBuffer); //header
								//size is (24 + (character count * 106))
								WriteBufferWord(2,24,ReplyBuffer);
								SendBuffer(AThread,ReplyBuffer,24);
						end;
					end;
				end;
			end;
		end;
	end;
end; (* proc SendCharas
------------------------------------------------------------------------------*)

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

(*------------------------------------------------------------------------------
ParseCharaServ

Root procedure to handling client connections to the Character Server.
 Incoming connections do not have a valid TThreadLink.AccountLink, so
 we check for that, and then assign as needed.  Keeps the various checks to a
 minimum.

[2006/07/06] Tsusai - Added TThreadLink check for security
------------------------------------------------------------------------------*)
{ TODO -oTsusai -cCharacterServer :
	Check against Prometheus Indy CharaServ prototype.  Must validate usage of
	ReadFromStack and the looping of the parser }
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
			//First time connection from login needs to do 0x0065.  No exceptions.
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
end; (*Proc ParseCharaServ
------------------------------------------------------------------------------*)

end.




