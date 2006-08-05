(*------------------------------------------------------------------------------
LoginProcesses
Tsusai 2006

Description:
 The login Server.  Handles all client communication here.
------------------------------------------------------------------------------*)
unit LoginProcesses;

interface
	uses
		IdTCPServer;

		procedure ParseLogin(AThread: TIdPeerThread);

implementation
	uses
		Account,
		AccountDB,
		Socket,
		PacketTypes,
		Globals,
		SysUtils,
		CharaServerTypes;

	const
		//ERROR REPLY CONSTANTS
		LOGIN_UNREGISTERED    = 0;
		LOGIN_INVALIDPASSWORD = 1;
		//LOGIN_SERVERFULL =  ;

(*------------------------------------------------------------------------------
SendLoginError

If some detail was wrong with login, this sends the reply with the constant Error
number (look at constants) to the client.
------------------------------------------------------------------------------*)
	procedure SendLoginError(const AThread: TIdPeerThread; const Error : byte);
	var
		Buffer : TBuffer;
	begin
		WriteBufferWord( 0, $006a, Buffer);
		WriteBufferWord( 2, Error, Buffer);
		SendBuffer(AThread, Buffer ,23);
	end; (* proc SendLoginError
------------------------------------------------------------------------------*)

(*------------------------------------------------------------------------------
SendCharacterServers

Upon successful authentication, this procedure sends the list of character
 servers to the client.
------------------------------------------------------------------------------*)
	procedure SendCharacterServers(AnAccount : TAccount; AThread: TIdPeerThread);
	var
		Buffer : TBuffer;
		Index : integer;
		Size : cardinal;
	begin
		//Packet Format...
		//R 0069 <len>.w <login ID1>.l <account ID>.l <login ID2>.l ?.32B <sex>.B
		//{<IP>.l <port>.w <server name>.20B <login users>.w <maintenance>.w <new>.w}.32B*
		Size := 2+2+4+4+4+32+1+(CharaServerList.Count * 32);
		WriteBufferWord(0,$0069,Buffer);
		WriteBufferWord(2,Size,Buffer);
		WriteBufferCardinal(4,AnAccount.LoginKey[1],Buffer);
		WriteBufferCardinal(8,AnAccount.ID,Buffer);
		WriteBufferCardinal(12,AnAccount.LoginKey[2],Buffer);
		WriteBufferCardinal(16, 0, Buffer);
		WriteBufferString(20, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz'#0, Now), 24, Buffer);
		WriteBufferWord(44, 0, Buffer);
		WriteBufferByte(46,AnAccount.GenderNum,Buffer);
		for Index := 0 to CharaServerList.Count - 1 do begin
			WriteBufferCardinal(47,TCharaServ(CharaServerList.Objects[Index]).IPCardinal,Buffer);
			WriteBufferWord(51,TCharaServ(CharaServerList.Objects[Index]).Port,Buffer);
			WriteBufferString(53+Index*32,TCharaServ(CharaServerList.Objects[Index]).ServerName,20,Buffer);
			WriteBufferWord(73+Index*32,TCharaServ(CharaServerList.Objects[Index]).OnlineUsers,Buffer);
			WriteBufferWord(75,0,Buffer);
			WriteBufferWord(77,0,Buffer);
		end;
    SendBuffer(AThread, Buffer ,Size);
	end; (* Proc SendCharacterServers
------------------------------------------------------------------------------*)

(*------------------------------------------------------------------------------
ParseLogin

Accepts incoming connections to the Login server and verifies the login data.
------------------------------------------------------------------------------*)
	procedure ParseLogin(AThread: TIdPeerThread);
	var
		PLength : Integer;
		Buffer : TBuffer;
		UserName : String;
		Password : String;
		AnAccount : TAccount;
		ID : Word;
	begin
		if AThread.Connection.Connected then
		begin
			PLength := AThread.Connection.ReadFromStack;
			if PLength >= 2 then
			begin
				AThread.Connection.ReadBuffer(Buffer,PLength);
				ID := BufferReadWord(0,Buffer);
				Case ID of
				$0064: //Basic login packet
					begin
						UserName := BufferReadString(6,24,Buffer);
						AnAccount := FindAccountByName(UserName);
						if Assigned(AnAccount) then begin
							Password := BufferReadString(30,24,Buffer);
							if AnAccount.Password = Password then
							begin
								AnAccount.LoginKey[1] := BufferReadCardinal(54,Buffer);
								AnAccount.LoginKey[2] := BufferReadCardinal(58,Buffer);
								AnAccount.LastIP := AThread.Connection.Socket.Binding.PeerIP;
								SendCharacterServers(AnAccount,AThread);
							end else begin
								SendLoginError(AThread,LOGIN_INVALIDPASSWORD);
							end;
						end else begin
							SendLoginError(AThread,LOGIN_UNREGISTERED);
						end;
					end;
				end;
			end;
		end;
	end;  (* Proc SendCharacterServers
------------------------------------------------------------------------------*)


end.
