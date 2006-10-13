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
    Database,
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
		Buffer  : TBuffer;
		Index   : integer;
		Size    : cardinal;
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

(*
*)

	procedure ValidateLogin(
		AThread: TIDPeerThread;
		RecvBuffer : TBuffer;
		const Username : String;
		const Password : String;
		MD5Key      : string = ''
	);
	var
		AnAccount : TAccount;
		ADatabase : TDatabase;
		AccountPassword : string;
	begin
		//New database system added 09/29/06 - RaX
		ADatabase := TDatabase.Create();
		AnAccount := ADatabase.AnInterface.GetAccount(UserName);
		FreeAndNil(ADatabase);
		if Assigned(AnAccount) then begin
			AccountPassword := AnAccount.Password;
			if not(MD5Key = '') then AccountPassword := GetMD5(MD5Key + AccountPassword);
			if AccountPassword = Password then
			begin
				AnAccount.LoginKey[1] := BufferReadCardinal(54,RecvBuffer);
				AnAccount.LoginKey[2] := BufferReadCardinal(58,RecvBuffer);
				AnAccount.LastIP := AThread.Connection.Socket.Binding.PeerIP;
				SendCharacterServers(AnAccount,AThread);
			end else begin
				SendLoginError(AThread,LOGIN_INVALIDPASSWORD);
			end;
		end else begin
			SendLoginError(AThread,LOGIN_UNREGISTERED);
		end;
	end;

	function ReadMD5Password(
		StartPos,
		PLength : word;
		Buffer : TBuffer
	) : string;
	var
		idx : integer;
	begin
		Result := '';
		//Read every byte, and convert that bite value into a hex string
		//Attach all hexstrings together to make the MD5 hash string.
		for idx := 0 to PLength-1 do
		begin
			Result := Result + IntToHex(BufferReadByte(StartPos+idx,Buffer),2);
		end;
	end;

(*------------------------------------------------------------------------------
ParseLogin

Accepts incoming connections to the Login server and verifies the login data.
------------------------------------------------------------------------------*)
	procedure ParseLogin(AThread: TIdPeerThread);
	var
		PLength   : Integer;
		Buffer    : TBuffer;
		UserName  : String;
		Password  : String;
		ID        : Word;

	begin
		if AThread.Connection.Connected then
		begin
			PLength := AThread.Connection.ReadFromStack(false,-1,false);
			if PLength >= 2 then
			begin
				//Get ID
				AThread.Connection.ReadBuffer(Buffer,2);
				ID := BufferReadWord(0,Buffer);
				Case ID of
				$0064: //Basic login
					begin
						//Read the rest of the packet
						AThread.Connection.ReadBuffer(Buffer[2],55-2);
						UserName := BufferReadString(6,24,Buffer);
						Password := BufferReadString(30,24,Buffer);
						ValidateLogin(AThread,Buffer,Username,Password);
					end;
				$01DB: //Client connected asking for md5 key to send a secure password
					begin
						WriteBufferWord(0,$01DC,Buffer);
						WriteBufferWord(2,Length('ILoveHelios')+4,Buffer);
						WriteBufferString(4,'ILoveHelios',Length('ILoveHelios'),Buffer);
						AThread.Connection.WriteBuffer(Buffer,Length('ILoveHelios')+4);
					end;
				$01DD: //Recieve secure login details
					begin
						AThread.Connection.ReadBuffer(Buffer[2],47-2);
						//AThread.Connection.InputBuffer.SaveToFile('c:\md5connection.txt');
						UserName := BufferReadString(6,24,Buffer);
						Password := ReadMD5Password(30,16,Buffer);
						Writeln('Recieved Md5 hash: ' +  Password);
						ValidateLogin(AThread,Buffer,Username,Password,'ILoveHelios');
					end;
				else
					begin
						WriteLn('Unknown Login Packet : ' + IntToHex(ID,4));
					end;
				end;
			end;
		end;
	end;  (* Proc SendCharacterServers
------------------------------------------------------------------------------*)


end.
