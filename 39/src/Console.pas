unit Console;

interface
uses
	IdTCPServer,
	SysUtils,
	Classes;

type TMainProc = class(TComponent)
		LoginServer : TIdTCPServer;
		CharaServer: TIdTCPServer;
		ZoneServer: TIdTCPServer;
		procedure Console(Line : string);
		procedure LoginServerExecute(AThread: TIdPeerThread);
		procedure Startup;
		procedure Shutdown;
		procedure ServerException(AThread: TIdPeerThread;
			AException: Exception);
		procedure LoginServerConnect(AThread: TIdPeerThread);
		procedure CharaServerExecute(AThread: TIdPeerThread);
		constructor Create(AOwner : TComponent); override;
		destructor  Destroy; override;
		function ParseInput(InputText : String) : boolean;
		function CommandHelp : String;
end;

var
	MainProc : TMainProc;
	AppPath : String;

implementation
uses
	LoginProcesses,
	CharaServerTypes,
	CharaServerPacket,
	Globals;

procedure TMainProc.Console(Line : string);
begin
	Writeln(Line);
end;

procedure TMainProc.LoginServerExecute(AThread: TIdPeerThread);
begin
	ParseLogin(AThread);
end;

procedure TMainProc.Startup;
var
	LocalCharaServ : TCharaServ;
	Success : Boolean;
begin
	LoginServer.DefaultPort := 6900;
	CharaServer.DefaultPort := 6121;
	ZoneServer.DefaultPort  := 5121;
	LoginServer.Active := true;
	//CharaServer.Active := true;
	//ZoneServer.Active := true;
	Success := TRUE;

	AppPath  := ExtractFilePath(ParamStr(0));
	InitGlobals;
	//read ini file.
	MainProc.Console('');

	if CharaServer.Active then
	begin
		//Add local character server to the list
		LocalCharaServ := TCharaServ.Create;
		LocalCharaServ.IP := '127.0.0.1';
		LocalCharaServ.IPCardinal := 0;
		LocalCharaServ.InternalServer := TRUE;
		LocalCharaServ.ServerName := 'Helios';
		LocalCharaServ.Port := CharaServer.DefaultPort;
	end;

	if LoginServer.Active then
	begin
		if not ADatabase.LoadAccounts then begin  //changed to database object from functions - RaX
			Success := FALSE;
		end;
	end;

	if Success then
	begin
		Console('- Startup Success');
	end else
	begin
		Console('- Startup Failed');
	end;

	Console('------------------------------------');
	Console('For a list of console commands, input "/commands".');
end;

procedure TMainProc.Shutdown;
begin
	DestroyGlobals;//Make sure globals are Free'd on Application exit.
end;

procedure TMainProc.ServerException(AThread: TIdPeerThread;
	AException: Exception);
begin
	if AThread.Connection.Connected then begin
		AThread.Connection.Disconnect; //On server exception, make sure client disconnects.
  end;
end;

procedure TMainProc.LoginServerConnect(AThread: TIdPeerThread);
begin
	Console('Connection from ' + AThread.Connection.Socket.Binding.PeerIP);
end;

procedure TMainProc.CharaServerExecute(AThread: TIdPeerThread);
begin
	ParseCharaServ(AThread);
end;

constructor TMainProc.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
	LoginServer := TIdTCPServer.Create(nil);
	CharaServer := TIdTCPServer.Create(nil);
	ZoneServer  := TIdTCPServer.Create(nil);

	LoginServer.OnExecute := LoginServerExecute;
	LoginServer.OnConnect := LoginServerConnect;
	LoginServer.OnException := ServerException;

	CharaServer.OnConnect := CharaServerExecute;
	CharaServer.OnException := ServerException;

	ZoneServer.OnException := ServerException;
end;

destructor  TMainProc.Destroy;
begin
	//MUST KILL COMPONENTS
	if Assigned(LoginServer) then FreeAndNil(LoginServer);
	if Assigned(CharaServer) then FreeAndNil(CharaServer);
	if Assigned(ZoneServer) then FreeAndNil(ZoneServer);

	inherited Destroy;
end;

function TMainProc.ParseInput(InputText : String) : boolean;
var
	StringIn  : TStringList;
	Index     : Integer;
	Command   : String;
	Values    : TStringList;
	Error     : String;
begin
	Result := false; //Return value to shutdown
	Error := ' ';
	StringIn := TStringList.Create;
	try
		StringIn.DelimitedText := InputText;
		if StringIn.DelimitedText[1] = '/' then begin
			Command := LowerCase(StringIn.Strings[0]);  //Gets the command text
			Values := TStringList.Create;
			try
				for Index := 1 to StringIn.Count-1 do begin  //retrieves values after the command
					Values.Add(StringIn.Strings[Index]);
				end;
				{Start Command Parser}
				if Command = '/exit' then begin
					Result := true; //set terminate flag
					Error := '';
				end else if Command = '/reload' then begin
					if Assigned(ADatabase) then Error := ADataBase.Reload;
				end else if Command = '/commands' then begin
					Error := CommandHelp;
				end else begin
					Error := Command + ' does not exist!';
				end;
				{End Command Parser}
			finally  //Begin Cleanup
				Values.Free;
			end;
			if Error <> '' then begin  //Display Errors
				MainProc.Console('Command ' + Command + ' failed - ' + Error)
			end else begin
				MainProc.Console('Command ' + Command + ' success!');
			end;
		end;
	finally
		StringIn.Free;
	end;
end;

function TMainProc.CommandHelp : String;
begin
	MainProc.Console('The available console commands are as follows...');
	MainProc.Console('--------------------------------------');
	MainProc.Console('/reload - reloads account database');
	MainProc.Console('/exit - exits the program');
	MainProc.Console('/commands - list all console commands');
	MainProc.Console('--------------------------------------');
	Result := '';
end;

end.
