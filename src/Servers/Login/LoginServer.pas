//------------------------------------------------------------------------------
//LoginServer			                                                        UNIT
//------------------------------------------------------------------------------
//	What it does-
//      The Login Server Class.
//    An object type that contains all information about a login server
//    This unit is to help for future design of multi server communication IF
//    the user were to want to do so.  Else, it would act as an all-in-one.
//	  Only one type in this unit for the time being.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit LoginServer;

interface
uses
	IdTCPServer,
	IdContext,
	PacketTypes,
	Classes,
	SysUtils,
	Account,
	LoginOptions;

type
//------------------------------------------------------------------------------
//TLoginServer                                                            CLASS
//------------------------------------------------------------------------------
	TLoginServer = class
	protected
	//
	private
		fPort         : Word;
		TCPServer     : TIdTCPServer;
		fCharaServerList : TStringList;

		Procedure OnConnect(AConnection: TIdContext);
		Procedure OnDisconnect(AConnection: TIdContext);
		Procedure OnException(AConnection: TIdContext;
			AException: Exception);

		Procedure LoadOptions;

		Procedure ParseLogin(AClient: TIdContext);
		Procedure SendLoginError(var AClient: TIdContext; const Error : byte);
		Procedure SendCharacterServers(AnAccount : TAccount; AClient: TIdContext);
		Procedure ParseMF(AClient : TIdContext; var Username : string; Password : string);
		Procedure ValidateLogin(
			AClient: TIdContext;
			InBuffer : TBuffer;
			Username : String;
			Password : String
		);

		procedure VerifyCharaServer(
			AClient: TIdContext;
			InBuffer : TBuffer;
			Password : String
		);

		Procedure SetPort(Value : Word);
		Function GetStarted() : Boolean;

	public
		Options : TLoginOptions;

		Property Port : Word read fPort write SetPort;
		property Started : Boolean read GetStarted;

		Constructor Create();
		Destructor  Destroy();override;
		Procedure   Start();
		Procedure   Stop();
	end;
//------------------------------------------------------------------------------


implementation
uses
	//Helios
	CharaLoginCommunication,
	Globals,
	BufferIO,
	CharacterServerInfo,
	StrUtils,
	Main,
	TCPServerRoutines;

const
//ERROR REPLY CONSTANTS
	LOGIN_UNREGISTERED    = 0;
	LOGIN_INVALIDPASSWORD = 1;
	LOGIN_BANNED          = 4;
	LOGIN_TIMEUP          = 2;
	//LOGIN_SERVERFULL =  ;

//------------------------------------------------------------------------------
//Create()                                                          CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//		 Initializes our Login Server
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Constructor TLoginServer.Create();
begin
	Inherited;

	TCPServer := TIdTCPServer.Create;

	TCPServer.OnExecute   := ParseLogin;
	TCPServer.OnConnect   := OnConnect;
	TCPServer.OnDisconnect:= OnDisconnect;
	TCPServer.OnException := OnException;

	fCharaServerList      := TStringList.Create;
end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy()                                                          DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//		 Destroys our Login Server
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Destructor TLoginServer.Destroy();
begin
	TCPServer.Free;
	fCharaServerList.Free;
	Inherited;
end;{Destroy}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Start()                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		 Enables the login server to accept incoming connections
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TLoginServer.Start();
begin
  if NOT Started then
  begin
	  LoadOptions;

	  Port := Options.Port;
		ActivateServer('Login',TCPServer);
  end else
	begin
		Console.Message('Cannot start():: Login server is already running.', 'Login Server', MS_ALERT);
	end;
end;{Start}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Stop()                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		 Stops the login server from accepting incoming connections
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TLoginServer.Stop();
var
  Index : Integer;
begin
  if Started then
  begin
	  DeActivateServer('Login', TCPServer);

		//Free up our existing server info objects
		for Index := 0 to fCharaServerList.Count - 1 do
		begin
      fCharaServerList.Delete(Index);
    end;

    Options.Save;
    Options.Free;
  end else
  begin
		Console.Message('Cannot Stop():: Login server is not running.', 'Login Server', MS_ALERT);
  end;
end;{Start}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//OnConnect()                                          EVENT
//------------------------------------------------------------------------------
//	What it does-
//			Event which is fired when a user attempts to connect to the login
//    server. It writes information about the connection to the console.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TLoginServer.OnConnect(AConnection: TIdContext);
begin
	AConnection.Data := TThreadLink.Create(AConnection);
	//Console.Message('Connection from ' + AConnection.Connection.Socket.Binding.PeerIP. 'Login Server', MS_INFO);
end;{LoginServerConnect}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//OnDisconnect()                                                         EVENT
//------------------------------------------------------------------------------
//	What it does-
//		  Executes when a client disconnects from the login server. Removes
//    disconnected character servers from the list.
//
//	Changes -
//		January 4th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TLoginServer.OnDisconnect(AConnection: TIdContext);
var
	idx : integer;
	ACharaServInfo : TCharaServerInfo;
begin
	if AConnection.Data is TCharaServerLink then
	begin
		ACharaServInfo := TCharaServerLink(AConnection.Data).Info;
		idx := fCharaServerList.IndexOfObject(ACharaServInfo);
		if not (idx = -1) then
		begin
			fCharaServerList.Delete(idx);
		end;
	end;
end;{OnDisconnect}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//OnException                                                             EVENT
//------------------------------------------------------------------------------
//	What it does-
//			Handles Socket exceptions gracefully by outputting the exception message
//    and then disconnecting the client.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TLoginServer.OnException(AConnection: TIdContext;
	AException: Exception);
begin
	if AnsiContainsStr(AException.Message, IntToStr(10053)) or
		AnsiContainsStr(AException.Message, IntToStr(10054))
	then begin
		AConnection.Connection.Disconnect;
	end;
end;{OnException}
//------------------------------------------------------------------------------


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
	procedure TLoginServer.SendLoginError(var AClient: TIdContext; const Error : byte);
	var
		Buffer : TBuffer;
	begin
		WriteBufferWord( 0, $006a, Buffer);
		WriteBufferWord( 2, Error, Buffer);
		SendBuffer(AClient, Buffer ,GetPacketLength($006a));
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
//		January 20th, 2007 - Tsusai - Wrapped the console messages, now using
//			IdContext.Binding shortcut.
//
//------------------------------------------------------------------------------
	procedure TLoginServer.SendCharacterServers(AnAccount : TAccount; AClient: TIdContext);
	var
		Buffer  : TBuffer;
		Index   : integer;
		Size    : Word;
	begin
		//Packet Format...
		//R 0069 <len>.w <login ID1>.l <account ID>.l <login ID2>.l ?.32B <sex>.B (47 total)
		//{<IP>.l <port>.w <server name>.20B <login users>.w <maintenance>.w <new>.w}.32B*
		if fCharaServerList.Count > 0 then
		begin
			Size := 47 + (fCharaServerList.Count * 32);
			WriteBufferWord(0,$0069,Buffer);
			WriteBufferWord(2,Size,Buffer);
			WriteBufferLongWord(4,AnAccount.LoginKey[1],Buffer);
			WriteBufferLongWord(8,AnAccount.ID,Buffer);
			WriteBufferLongWord(12,AnAccount.LoginKey[2],Buffer);
			WriteBufferLongWord(16, 0, Buffer);
			WriteBufferString(20, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz'#0, AnAccount.LastLoginTime), 24, Buffer);
			WriteBufferWord(44, 0, Buffer);
			WriteBufferByte(46,AnAccount.GenderNum,Buffer);
			for Index := 0 to fCharaServerList.Count - 1 do begin
				WriteBufferLongWord(
					47+(index*32)+0,
					TCharaServerInfo(fCharaServerList.Objects[Index]).Address(
						AClient.Binding.PeerIP
					),
					Buffer
				);
				WriteBufferWord(47+(index*32)+4,TCharaServerInfo(fCharaServerList.Objects[Index]).Port,Buffer);
				WriteBufferString(47+(index*32)+6,TCharaServerInfo(fCharaServerList.Objects[Index]).ServerName,20,Buffer);
				WriteBufferWord(47+(index*32)+26,TCharaServerInfo(fCharaServerList.Objects[Index]).OnlineUsers,Buffer);
				WriteBufferWord(47+(index*32)+28,0,Buffer);
				WriteBufferWord(47+(index*32)+30,0,Buffer);
			end;
			SendBuffer(AClient, Buffer, Size);
		end else
		begin
			WriteBufferWord(0,$0081,Buffer);
			WriteBufferByte(2,1,Buffer); //01 Server Closed
			SendBuffer(AClient, Buffer, GetPacketLength($0081));
		end;

	end; // Proc SendCharacterServers
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ParseMF()                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		  Parses a client's login for _M/_F registration.
//
//	Changes -
//		January 4th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TLoginServer.ParseMF(AClient : TIdContext; var Username : string; Password : string);
	var
		GenderStr  : string;
	begin
		GenderStr := Uppercase(AnsiRightStr(Username,2)); {get _M or _F and cap it}
		if (GenderStr = '_M') or
		(GenderStr = '_F') then
		begin
			//Trim the MF off because people forget to take it off.
			Username := AnsiLeftStr(Username,Length(Username)-2);
			//Check to see if the account already exists.
			TThreadLink(AClient.Data).DatabaseLink.CommonData.Connect;
			if NOT TThreadLink(AClient.Data).DatabaseLink.CommonData.AccountExists(Username) then
			begin
				//Create the account.
				TThreadLink(AClient.Data).DatabaseLink.CommonData.CreateAccount(
					Username,Password,GenderStr[2]
				);
			end;
			TThreadLink(AClient.Data).DatabaseLink.CommonData.Disconnect;
		end;
	end;{ParseMF}
//----------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ValidateLogin                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Checks a clients authentication information against the database. On
//    authentication success, it sends the client a list of character servers.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//		January 3rd, 2007 - Tsusai - Added console messages.
//		January 20th, 2007 - Tsusai - Wrapped the console messages, now using
//			IdContext.Binding shortcut
//
//------------------------------------------------------------------------------
	procedure TLoginServer.ValidateLogin(
		AClient: TIdContext;
		InBuffer : TBuffer;
		Username : String;
		Password : String
	);
	var
		AnAccount : TAccount;
		AccountPassword : string;
		MD5Key    : string;
	begin
		Console.Message(
			'RO Client connection from ' +
			AClient.Binding.PeerIP, 'Login Server', MS_NOTICE
		);
		MD5Key := '';

		if (AClient.Data is TMD5String) then
		begin
			MD5Key := TMD5String(AClient.Data).Key;
		end;

		//If MF enabled, and NO MD5KEY LOGIN, parse _M/_F
		if Options.EnableMF and
		(MD5Key = '') then
		begin
			ParseMF(AClient, Username, Password);
		end;

		TThreadLink(AClient.Data).DatabaseLink.CommonData.Connect;
		AnAccount := TThreadLink(AClient.Data).DatabaseLink.CommonData.GetAccount(UserName);
		if Assigned(AnAccount) then begin
			AccountPassword := AnAccount.Password;
			if not(MD5Key = '') then
			begin
				AccountPassword := GetMD5(MD5Key + AccountPassword);
			end;
			if AccountPassword = Password then
			begin
				if not AnAccount.IsBanned then
				begin
					if not AnAccount.GameTimeUp then
					begin
						AnAccount.LoginKey[1] := Random($7FFFFFFF) + 1;
						AnAccount.LoginKey[2] := Random($7FFFFFFF) + 1;
						AnAccount.LastIP := AClient.Binding.PeerIP;
						AnAccount.LastLoginTime := Now;
						Inc(AnAccount.LoginCount);
						TThreadLink(AClient.Data).DatabaseLink.CommonData.SaveAccount(AnAccount);
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
		AnAccount.Free;
		TThreadLink(AClient.Data).DatabaseLink.CommonData.Disconnect;
	end;//ValidateLogin
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//VerifyCharaServer()                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		  Makes sure a character server is allowed to be added to the character
//    server list by verifying it's password.
//
//	Changes -
//		January 3rd, 2007 - Tsusai - Added console messages.
//		January 4th, 2007 - RaX - Created Header. Tsusai lazy =)
//		January 14th, 2007 - Tsusai - Updated error response
//		January 20th, 2007 - Tsusai - Wrapped the console messages, now using
//			IdContext.Binding shortcut
//
//------------------------------------------------------------------------------
	procedure TLoginServer.VerifyCharaServer(
		AClient: TIdContext;
		InBuffer : TBuffer;
		Password : String
	);
	var
		Validated : boolean;
		Servername : String;
		Port : word;
		CServerInfo : TCharaServerInfo;
	begin
		Console.Message(
			'Reading Character Server connection from ' +
			AClient.Binding.PeerIP, 'Login Server', MS_NOTICE
		);
		Validated := true;

		if Password <> GetMD5(Options.Key) then
		begin
			Console.Message('Character Server failed verification: Invalid Security Key.',
				'Login Server',
				MS_INFO);
			Validated := false;
		end;

		if Validated then
		begin
			Console.Message('Character Server connection validated.', 'Login Server', MS_INFO);
			Servername := BufferReadString(18,24,InBuffer);
			Port := BufferReadWord(42,InBuffer);
			CServerInfo := TCharaServerInfo.Create;
			CServerInfo.ServerName := ServerName;
			CServerInfo.Port := Port;
			AClient.Data := TCharaServerLink.Create(AClient);
			TCharaServerLink(AClient.Data).Info := CServerInfo;
			fCharaServerList.AddObject(ServerName,CServerInfo);
		end;
		SendValidateFlagToChara(AClient,Validated);
	end;{VerifyCharaServer}
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
//		January 3rd, 2007 - Tsusai - Added console messages.
//
//------------------------------------------------------------------------------
	procedure TLoginServer.ParseLogin(AClient: TIdContext);
	var
		Buffer    : TBuffer;
		UserName  : String;
		Password  : String;
		ID        : Word;
		MD5Len    : Integer;
		MD5Key    : TMD5String;
		Size      : Word;
	begin
		//Get ID
		RecvBuffer(AClient,Buffer,2);
		ID := BufferReadWord(0,Buffer);
		Case ID of
		$0064: //Basic login
			begin
				//Read the rest of the packet
				RecvBuffer(AClient,Buffer[2],GetPacketLength($0064)-2);
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
				RecvBuffer(AClient,Buffer[2],GetPacketLength($01DD)-2);
				UserName := BufferReadString(6,24,Buffer);
				Password := BufferReadMD5(30,Buffer);
				ValidateLogin(AClient,Buffer,Username,Password);
			end;
		$2000:
			begin
				RecvBuffer(AClient,Buffer[2],GetPacketLength($2000)-2);
				Password := BufferReadMD5(2,Buffer);
				VerifyCharaServer(AClient,Buffer,Password);
			end;
		$2002:
			begin
				if AClient.Data is TCharaServerLink then
				begin
					RecvBuffer(AClient,Buffer[2],2);
					Size := BufferReadWord(2,Buffer);
					RecvBuffer(AClient,Buffer[4],Size-4);
					TCharaServerLink(AClient.Data).Info.WAN := BufferReadString(4,Size-4,Buffer);
					Console.Message('Received updated Character Server WANIP.', 'Login Server', MS_NOTICE);
				end;
			end;
		$2003:
			begin
				if AClient.Data is TCharaServerLink then
				begin
					RecvBuffer(AClient,Buffer[2],2);
					Size := BufferReadWord(2,Buffer);
					RecvBuffer(AClient,Buffer[4],Size-4);
					TCharaServerLink(AClient.Data).Info.LAN := BufferReadString(4,Size-4,Buffer);
					Console.Message('Received updated Character Server LANIP.', 'Login Server', MS_NOTICE);
				end;
			end;
		$2004:
			begin
				if AClient.Data is TCharaServerLink then
				begin
					RecvBuffer(AClient,Buffer[2],GetPacketLength($2004)-2);
					TCharaServerLink(AClient.Data).Info.OnlineUsers := BufferReadWord(2,Buffer);
					Console.Message('Received updated Character Server Online Users.', 'Login Server', MS_DEBUG);
				end;
			end;
		else
			begin
				Console.Message('Unknown Login Packet : ' + IntToHex(ID,4), 'Login Server', MS_WARNING);
			end;
		end;
	end;  // Proc SendCharacterServers
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadOptions                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Creates and Loads the inifile.
//
//	Changes -
//		January 4th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TLoginServer.LoadOptions;
begin
	Options    := TLoginOptions.Create(MainProc.Options.ConfigDirectory+'/Login.ini');

	Options.Load;
end;{LoadOptions}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetPort                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the internal fPort variable to the value specified. Also sets the
//    TCPServer's port.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TLoginServer.SetPort(Value : Word);
begin
	fPort := Value;
	TCPServer.DefaultPort := Value;
end;//SetPort
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetStarted                                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Checks to see if the internal TCP server is active, if it is it returns
//    true.
//
//	Changes -
//		January 4th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Function TLoginServer.GetStarted() : Boolean;
begin
	Result := TCPServer.Active;
end;{SetPort}
//------------------------------------------------------------------------------
end.
