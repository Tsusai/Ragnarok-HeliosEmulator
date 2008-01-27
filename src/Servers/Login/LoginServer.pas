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

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
uses
	IdTCPServer,
	IdContext,
	PacketTypes,
	List32,
	SysUtils,
	Account,
	Server,
	LoginOptions;

type
//------------------------------------------------------------------------------
//TLoginServer                                                            CLASS
//------------------------------------------------------------------------------
	TLoginServer = class(TServer)
	protected
		fCharaServerList : TIntList32;
		fAccountList  : TIntList32;

		Procedure OnConnect(AConnection: TIdContext);override;
		Procedure OnDisconnect(AConnection: TIdContext); override;
		Procedure OnExecute(AConnection: TIdContext);override;
		Procedure OnException(AConnection: TIdContext;
			AException: Exception);override;

		Procedure LoadOptions;

		Procedure SendLoginError(var AClient: TIdContext; const Error : byte; const Error_Message : String='');
		procedure SendDCError(var AClient: TIdContext; const Error : byte);
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

		procedure UpdateToAccountList(
			AClient : TIdContext;
			InBuffer : TBuffer
		);

		procedure RemoveFromAccountList(
			AClient : TIdContext;
			InBuffer : TBuffer
		);

	public
		Options : TLoginOptions;

		Constructor Create();
		Destructor  Destroy();override;
		Procedure   Start();override;
		Procedure   Stop();override;
	end;
//------------------------------------------------------------------------------


implementation
uses
	//Delphi
	StrUtils,
	//Helios
	BufferIO,
	CharaLoginCommunication,
	CharacterServerInfo,
	Globals,
	LoginAccountInfo,
	Main,
	TCPServerRoutines
	;

	//Subclasses the TThreadlink, designed to store a MD5 Key to for use
	//with MD5 client connections
	type TLoginThreadLink = class(TThreadLink)
		MD5Key : string;
	end;

const
//ERROR REPLY CONSTANTS
	LOGIN_UNREGISTERED    = 0;
	LOGIN_INVALIDPASSWORD = 1;
	LOGIN_TIMEUP          = 2;
	LOGIN_REJECTED        = 3;
	LOGIN_BANNED          = 4;
	LOGIN_TEMPERARYBAN    = 6;
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

	fCharaServerList      := TIntList32.Create;
	fAccountList          := TIntList32.Create;
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
	fCharaServerList.Free;
	fAccountList.Free;
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
		ActivateServer('Login',TCPServer, Options.IndySchedulerType, Options.IndyThreadPoolSize);
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
//		May 1st, 2007 - Tsusai - Reversed the free'ing for loop to go backwards.
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
		for Index := fCharaServerList.Count - 1 downto 0 do
		begin
			fCharaServerList.Objects[Index].Free;
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
//		December 26th, 2007 - Tsusai - Resets the MD5 key on TLoginThreadLink just
//			incase
//
//------------------------------------------------------------------------------
procedure TLoginServer.OnConnect(AConnection: TIdContext);
begin
	AConnection.Data := TLoginThreadLink.Create(AConnection);
	TLoginThreadLink(AConnection.Data).DatabaseLink := Database;
	TLoginThreadLink(AConnection.Data).MD5Key := ''; //blank just incase.
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
	procedure TLoginServer.SendLoginError(var AClient: TIdContext; const Error : byte; const Error_Message : String='');
	var
		Buffer : TBuffer;
	begin
		WriteBufferWord( 0, $006a, Buffer);
		WriteBufferByte( 2, Error, Buffer);
		WriteBufferString(3, Error_Message, 20, Buffer);
		SendBuffer(AClient, Buffer ,GetPacketLength($006a));
	end; // proc SendLoginError
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendDCError		                                              PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Send connection error code to client
//
//	Changes -
//		April 11th, 2007 - Aeomin - Created header
//
//------------------------------------------------------------------------------
	procedure TLoginServer.SendDCError(var AClient: TIdContext; const Error : byte);
	var
		Buffer : TBuffer;
	begin
		WriteBufferWord( 0, $0081, Buffer);
		WriteBufferByte( 2, Error, Buffer);
		SendBuffer(AClient, Buffer ,GetPacketLength($0081));
	end; // proc SendDCError
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
//		April 11th, 2007 - Aeomin - Changed packet 0x0081 to SendDcError
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
			SendDcError(AClient, 1); //01 Server Closed
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
				if NOT TLoginThreadLink(AClient.Data).DatabaseLink.CommonData.AccountExists(Username) then
				begin
					//Create the account.
					TLoginThreadLink(AClient.Data).DatabaseLink.CommonData.CreateAccount(
						Username,Password,GenderStr[2]
					);
				end;
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
//		December 26th, 2007 - Tsusai - Fixed up MD5 client checking.
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
		Idx : Integer;
		CharSrvIdx: Integer;
		AccountInfo : TLoginAccountInfo;
	begin
		Console.Message(
			'RO Client connection from ' +
			AClient.Binding.PeerIP, 'Login Server', MS_NOTICE
		);
		MD5Key := '';

		if (TLoginThreadLink(AClient.Data).MD5Key <> '') then
		begin
			MD5Key := TLoginThreadLink(AClient.Data).MD5Key;
		end;

		//If MF enabled, and NO MD5KEY LOGIN, parse _M/_F
		if Options.EnableMF and
		(MD5Key = '') then
		begin
			ParseMF(AClient, Username, Password);
		end;

			AnAccount := TLoginThreadLink(AClient.Data).DatabaseLink.CommonData.GetAccount(AClient, UserName);
			if Assigned(AnAccount) then begin
				AccountPassword := AnAccount.Password;
				if	( not (MD5Key = '') and //If key isn't blank AND if one of the combinations
							( (Password = GetMD5(MD5Key + AccountPassword)) or
								(Password = GetMD5(AccountPassword+ MD5Key))
							)
						) or //or plain text to text compare
						(AccountPassword = Password) then
				begin
//				if not AnAccount.IsBanned then
					if AnAccount.State = 0 then
					begin
						if AnAccount.Bantime <= Now then
						begin
							if not AnAccount.GameTimeUp then
							begin
								Idx := fAccountList.IndexOf(AnAccount.ID);
								if Idx > -1 then
								begin
									AccountInfo := fAccountList.Objects[idx] as TLoginAccountInfo;
									if AccountInfo.OnCharSrvList then
									begin
										fAccountList.Delete(Idx);
										SendDcError(AClient, 8); //Server still recognizes your last login
									end else
									begin
										if AccountInfo.UnderKickQuery then
										begin
											SendDcError(AClient, 8); //Server still recognizes your last login
										end else
										begin
											CharSrvIdx := fCharaServerList.IndexOf(AccountInfo.CharServerID);
											if CharSrvIdx > -1 then
											begin
												AccountInfo.UnderKickQuery := True;
												SendKickAccountChara(TCharaServerInfo(fCharaServerList.Objects[CharSrvIdx]).Connection, AnAccount.ID);
												SendDcError(AClient, 8);  //Server still recognizes your last login
											end else
											begin
												SendLoginError(AClient, LOGIN_REJECTED);
											end;
										end;
									end;
								end else
								begin
									AnAccount.LoginKey[1] := Random($7FFFFFFF) + 1;
									AnAccount.LoginKey[2] := Random($7FFFFFFF) + 1;
									AnAccount.LastIP := AClient.Binding.PeerIP;
									AnAccount.LastLoginTime := Now;
									Inc(AnAccount.LoginCount);
									TLoginThreadLink(AClient.Data).DatabaseLink.CommonData.SaveAccount(AnAccount);

									AccountInfo := TLoginAccountInfo.Create(AnAccount.ID);
									AccountInfo.OnCharSrvList := True;
									fAccountList.AddObject(AnAccount.ID,AccountInfo);

									SendCharacterServers(AnAccount,AClient);
								end;
							end else
							begin
								SendLoginError(AClient,LOGIN_TIMEUP);
							end;
						end else
						begin
							SendLoginError(AClient, LOGIN_TEMPERARYBAN, AnAccount.GetBanUntilTimeString);
						end;
					end else
					begin
						SendLoginError(AClient, AnAccount.State-1);
					end;
				end else
				begin
					SendLoginError(AClient,LOGIN_INVALIDPASSWORD);
				end;
			end else
			begin
				SendLoginError(AClient,LOGIN_UNREGISTERED);
			end;
			AnAccount.Free;
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
		Servername : String;
		Port : word;
		CServerInfo : TCharaServerInfo;
		ID   : LongWord;
		Validated : byte;
	begin
		Console.Message(
			'Reading Character Server connection from ' +
			AClient.Binding.PeerIP, 'Login Server', MS_NOTICE
		);
		Validated := 0; //Assume true

		ID := BufferReadLongWord(2,InBuffer);

		if (fCharaServerList.IndexOf(ID) > -1) then
		begin
		Console.Message('Character Server failed verification. ID already in use.', 'Login Server', MS_WARNING);
		Validated := 1;
		end;

		if Password <> GetMD5(Options.Key) then
		begin
			Console.Message('Character Server failed verification: Invalid Security Key.',
				'Login Server',
				MS_INFO);
			Validated := 2;
		end;

		if Validated = 0 then
		begin
			Console.Message('Character Server connection validated.', 'Login Server', MS_INFO);
			Servername := BufferReadString(22, 24, InBuffer);
			Port := BufferReadWord(46, InBuffer);
			CServerInfo := TCharaServerInfo.Create;
			CServerInfo.ServerID := ID;
			CServerInfo.ServerName := ServerName;
			CServerInfo.Port := Port;
			CServerInfo.Connection := AClient;
			AClient.Data := TCharaServerLink.Create(AClient);
			TCharaServerLink(AClient.Data).DatabaseLink := Database;
			TCharaServerLink(AClient.Data).Info := CServerInfo;
			fCharaServerList.AddObject(ID, CServerInfo);
		end;
		SendValidateFlagToChara(AClient,Validated);
	end;{VerifyCharaServer}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//UpdateToAccountList                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Parse the data send by Char server.
//			Update account's data in account list
//
//	Changes -
//		April 12th, 2007 - Aeomin - header created
//
//------------------------------------------------------------------------------
procedure TLoginServer.UpdateToAccountList(
	AClient : TIdContext;
	InBuffer : TBuffer
);
var
	AccountID: LongWord;
	Idx	: Integer;
	AccountInfo : TLoginAccountInfo;
begin
	AccountID := BufferReadLongWord(2, InBuffer);
	Idx := fAccountList.IndexOf(AccountID);
	if Idx > -1 then
	begin
		AccountInfo := fAccountList.Objects[Idx] as TLoginAccountInfo;

		AccountInfo.CharServerID := BufferReadLongWord(6, InBuffer);
		AccountInfo.OnCharSrvList := False;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RemoveFromAccountList                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Parse the data send by Char server.
//			Remove account from account list (Logged off)
//
//	Changes -
//		April 12th, 2007 - Aeomin - header created
//		April 12th, 2007 - Aeomin - Reset login keys to 0 after DC
//		May 1st, 2007 - Tsusai - Commented out the key reset.  Causes issues with
//			multiexe zone usage.
//
//------------------------------------------------------------------------------
procedure TLoginServer.RemoveFromAccountList(
	AClient : TIdContext;
	InBuffer : TBuffer
);
var
	AccountID : LongWord;
	Idx	: Integer;
	AnAccount : TAccount;
begin
	AccountID := BufferReadLongWord(2, InBuffer);
	Idx := fAccountList.IndexOf(AccountID);
	if Idx > -1 then
	begin
			AnAccount := TLoginThreadLink(AClient.Data).DatabaseLink.CommonData.GetAccount(AClient, AccountID);
			if Assigned(AnAccount) then begin
				{AnAccount.LoginKey[1] := 0;
				AnAccount.LoginKey[2] := 0;}
				fAccountList.Delete(Idx);
				TLoginThreadLink(AClient.Data).DatabaseLink.CommonData.SaveAccount(AnAccount);
			end;
	end;
end;
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
//		April 10th, 2007 - Aeomin - Changed index from 2 to 6 to support Char server ID.
//		December 26th, 2007 - Tsusai - Fixed up MD5 client preping.
//
//------------------------------------------------------------------------------
	procedure TLoginServer.OnExecute(AConnection: TIdContext);
	var
		Buffer    : TBuffer;
		UserName  : String;
		Password  : String;
		ID        : Word;
		MD5Len    : Integer;
		MD5Key    : String;
		Size      : Word;
	begin
		//Get ID
		RecvBuffer(AConnection,Buffer,2);
		ID := BufferReadWord(0,Buffer);
		Case ID of
		$0064: //Basic login
			begin
				//Read the rest of the packet
				RecvBuffer(AConnection,Buffer[2],GetPacketLength($0064)-2);
				UserName := BufferReadString(6,24,Buffer);
				Password := BufferReadString(30,24,Buffer);
				ValidateLogin(AConnection,Buffer,Username,Password);
			end;
		$01DB: //Client connected asking for md5 key to send a secure password
			begin
				MD5Key   := MakeRNDString( Random(10)+1 );
				MD5Len   := Length(MD5Key);
				TLoginThreadLink(AConnection.Data).MD5Key := MD5Key;
				WriteBufferWord(0,$01DC,Buffer);
				WriteBufferWord(2,MD5Len+4,Buffer);
				WriteBufferString(4,MD5Key,MD5Len,Buffer);
				SendBuffer(AConnection,Buffer,MD5Len+4);
			end;
		$01DD: //Recieve secure login details
			begin
				RecvBuffer(AConnection,Buffer[2],GetPacketLength($01DD)-2);
				UserName := BufferReadString(6,24,Buffer);
				Password := BufferReadMD5(30,Buffer);
				ValidateLogin(AConnection,Buffer,Username,Password);
			end;
		$0277:// New login packet (kRO 2006-04-24aSakexe langtype 0)
			begin
				RecvBuffer(AConnection,Buffer[2],GetPacketLength($0277)-2);
				UserName := BufferReadString(6,24,Buffer);
				Password := BufferReadString(30,24,Buffer);
				ValidateLogin(AConnection,Buffer,Username,Password);
			end;
		$02b0:// New login packet (kRO 2007-05-14aSakexe langtype 0)
			begin
				RecvBuffer(AConnection,Buffer[2],GetPacketLength($02b0)-2);
				UserName := BufferReadString(6,24,Buffer);
				Password := BufferReadString(30,24,Buffer);
				ValidateLogin(AConnection,Buffer,Username,Password);
			end;
		$0200:  //Account name?
			begin
				RecvBuffer(AConnection,Buffer[2],GetPacketLength($0200)-2);
			end;
		$0204://Receive MD5 of client...
			begin
				// Do nothing...
				RecvBuffer(AConnection,Buffer[2],GetPacketLength($0204)-2);
			end;
		$2000:
			begin
				RecvBuffer(AConnection,Buffer[2],GetPacketLength($2000)-2);
				Password := BufferReadMD5(6,Buffer);
				VerifyCharaServer(AConnection,Buffer,Password);
			end;
		$2002:
			begin
				if AConnection.Data is TCharaServerLink then
				begin
					RecvBuffer(AConnection,Buffer[2],2);
					Size := BufferReadWord(2,Buffer);
					RecvBuffer(AConnection,Buffer[4],Size-4);
					TCharaServerLink(AConnection.Data).Info.WAN := BufferReadString(4,Size-4,Buffer);
					Console.Message('Received updated Character Server WANIP.', 'Login Server', MS_NOTICE);
				end;
			end;
		$2003:
			begin
				if AConnection.Data is TCharaServerLink then
				begin
					RecvBuffer(AConnection,Buffer[2],2);
					Size := BufferReadWord(2,Buffer);
					RecvBuffer(AConnection,Buffer[4],Size-4);
					TCharaServerLink(AConnection.Data).Info.LAN := BufferReadString(4,Size-4,Buffer);
					Console.Message('Received updated Character Server LANIP.', 'Login Server', MS_NOTICE);
				end;
			end;
		$2004:
			begin
				if AConnection.Data is TCharaServerLink then
				begin
					RecvBuffer(AConnection,Buffer[2],GetPacketLength($2004)-2);
					TCharaServerLink(AConnection.Data).Info.OnlineUsers := BufferReadWord(2,Buffer);
					Console.Message('Received updated Character Server Online Users.', 'Login Server', MS_DEBUG);
				end;
			end;
		$2005:
			begin
				if AConnection.Data is TCharaServerLink then
				begin
					RecvBuffer(AConnection,Buffer[2],GetPacketLength($2005)-2);
					UpdateToAccountList(AConnection,Buffer);
				end;
			end;
		$2006:
			begin
				if AConnection.Data is TCharaServerLink then
				begin
					RecvBuffer(AConnection,Buffer[2],GetPacketLength($2006)-2);
					RemoveFromAccountList(AConnection,Buffer);
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

end.
