//------------------------------------------------------------------------------
//LoginProcesses    			                                                UNIT
//------------------------------------------------------------------------------
//	What it does-
//			The login Server.  Handles all client communication here.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit LoginProcesses;

interface
	uses
		IdContext;

		procedure ParseLogin(AClient: TIdContext);

implementation
	uses
		Account,
		Database,
		Socket,
		PacketTypes,
		Globals,
		SysUtils,
		Console,
		CharaServerTypes;

	const
		//ERROR REPLY CONSTANTS
		LOGIN_UNREGISTERED    = 0;
		LOGIN_INVALIDPASSWORD = 1;
		LOGIN_BANNED          = 4;
		LOGIN_TIMEUP          = 2;
		//LOGIN_SERVERFULL =  ;

//------------------------------------------------------------------------------
//SendLoginError   			                                              PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			If some detail was wrong with login, this sends the reply with the
//    constant Error number (look at constants) to the client.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure SendLoginError(const AClient: TIdContext; const Error : byte);
	var
		Buffer : TBuffer;
	begin
		WriteBufferWord( 0, $006a, Buffer);
		WriteBufferWord( 2, Error, Buffer);
		SendBuffer(AClient, Buffer ,23);
	end; // proc SendLoginError
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendCharacterServers                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Upon successful authentication, this procedure sends the list of
//    character servers to the client.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure SendCharacterServers(AnAccount : TAccount; AClient: TIdContext);
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
		SendBuffer(AClient, Buffer ,Size);

	end; // Proc SendCharacterServers
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ValidateLogin                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Checks a clients authentication information against the database. On
//    authentication success, it sends the client a list of character servers.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure ValidateLogin(
		AClient: TIdContext;
		RecvBuffer : TBuffer;
		const Username : String;
		const Password : String
	);
	var
		AnAccount : TAccount;
		AccountPassword : string;
		MD5Key    : string;
	begin
		//New database system added 09/29/06 - RaX
		AnAccount := MainProc.ACommonDatabase.AnInterface.GetAccount(UserName);
		if Assigned(AnAccount) then begin
			AccountPassword := AnAccount.Password;
			if (AClient.Data is TMD5String) then
			begin
				MD5Key := TMD5String(AClient.Data).Key;
			end;
			if not(MD5Key = '') then AccountPassword := GetMD5(MD5Key + AccountPassword);
			if AccountPassword = Password then
			begin
				if not AnAccount.IsBanned then
				begin
					if not AnAccount.GameTimeUp then
					begin
						AnAccount.LoginKey[1] := Random($7FFFFFFF) + 1;
						AnAccount.LoginKey[2] := Random($7FFFFFFF) + 1;
						AnAccount.LastIP := AClient.Connection.Socket.Binding.PeerIP;
						AnAccount.LastLoginTime := Now;
						Inc(AnAccount.LoginCount);
						MainProc.ACommonDatabase.AnInterface.SaveAccount(AnAccount);
						SendCharacterServers(AnAccount,AClient);
					end else begin
						SendLoginError(AClient,LOGIN_TIMEUP);
					end;
				end else begin
				SendLoginError(AClient,LOGIN_BANNED);
				end;
			end else begin
				SendLoginError(AClient,LOGIN_INVALIDPASSWORD);
			end;
		end else begin
			SendLoginError(AClient,LOGIN_UNREGISTERED);
		end;
	end;//ValidateLogin
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ReadMD5Password                                                     FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Get's an md5 password as a string.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
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
	end;//ReadMD5Password
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ParseLogin                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Accepts incoming connections to the Login server and verifies the login
//    data.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure ParseLogin(AClient: TIdContext);
	var
		Buffer    : TBuffer;
		UserName  : String;
		Password  : String;
		ID        : Word;
		MD5Len    : Integer;
		MD5Key    : TMD5String;

	begin
		//Get ID
		RecvBuffer(AClient,Buffer,2);
		ID := BufferReadWord(0,Buffer);
		Case ID of
		$0064: //Basic login
			begin
				//Read the rest of the packet
				RecvBuffer(AClient,Buffer[2],55-2);
				UserName := BufferReadString(6,24,Buffer);
				Password := BufferReadString(30,24,Buffer);
				ValidateLogin(AClient,Buffer,Username,Password);
			end;
		$01DB: //Client connected asking for md5 key to send a secure password
			begin
				MD5Key       := TMD5String.Create;
				MD5Key.Key   := MakeRNDString( Random(10)+1 );
				MD5Len       := Length(MD5Key.Key);
				AClient.Data := MD5Key;
				WriteBufferWord(0,$01DC,Buffer);
				WriteBufferWord(2,MD5Len+4,Buffer);
				WriteBufferString(4,MD5Key.Key,MD5Len,Buffer);
				SendBuffer(AClient,Buffer,MD5Len+4);
			end;
		$01DD: //Recieve secure login details
			begin
				RecvBuffer(AClient,Buffer[2],47-2);
				UserName := BufferReadString(6,24,Buffer);
				Password := ReadMD5Password(30,16,Buffer);
				ValidateLogin(AClient,Buffer,Username,Password);
			end;
		else
			begin
				MainProc.Console('Unknown Login Packet : ' + IntToHex(ID,4));
			end;
		end;
	end;  // Proc SendCharacterServers
//------------------------------------------------------------------------------


end.
