unit Main;

interface

uses
	{Other}
	SysUtils,
	Types,
	Classes,
	//Variants,
	QTypes,
	QControls,
	QForms,
	QDialogs,
	QStdCtrls,
	QMenus,
	IdBaseComponent,
	IdComponent,
	IdTCPServer,
	IdTCPConnection,
	IdTCPClient;

type
	TMainForm = class(TForm)
		LoginServer: TIdTCPServer;
		Console: TMemo;
		MainMenu1: TMainMenu;
		UserInput: TEdit;
		File1: TMenuItem;
		Exit1: TMenuItem;
		CharaServer: TIdTCPServer;
		CharaToLogin: TIdTCPClient;
		ZoneServer: TIdTCPServer;
		ZoneToChara: TIdTCPClient;
		procedure LoginServerExecute(AThread: TIdPeerThread);
		procedure FormCreate(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure UserInputReturnPressed(Sender: TObject);
		procedure ServerException(AThread: TIdPeerThread;
			AException: Exception);
    procedure LoginServerConnect(AThread: TIdPeerThread);
    procedure CharaServerExecute(AThread: TIdPeerThread);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
	MainForm: TMainForm;
	CharaServerList : TStringList;

procedure Output(Line : string);


implementation
uses
	{Project}
	Globals,
	Accounts,
	CharaServerTypes,
	LoginPacket,
	CharaServerPacket;

{$R *.xfm}

procedure Output(Line : string);
begin
	MainForm.Console.Lines.Add(Line);
end;

procedure TMainForm.LoginServerExecute(AThread: TIdPeerThread);
begin
	ParseLogin(AThread);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
	LocalCharaServ : TCharaServ;
	Success : boolean;
begin
	Success := true;
	AppPath  := ExtractFilePath(ParamStr(0));
	InitGlobals;
	//read ini file.
	Output('');

	if CharaServer.Active then
	begin
		//Add local character server to the list
		LocalCharaServ := TCharaServ.Create;
		LocalCharaServ.IP := '127.0.0.1';
		LocalCharaServ.IPCardinal := 0;
		LocalCharaServ.InternalServer := true;
		LocalCharaServ.ServerName := 'Kubia';
		CharaServerList.AddObject(LocalCharaServ.ServerName,LocalCharaServ);
	end else begin
		CharaToLogin.Connect;
	end;

	if LoginServer.Active then
	begin
		if not LoadAccounts then begin
			Success := false;
		end;
	end;

	if Success then
	begin
		Output('- Startup Success');
	end else
	begin
		Output('- Startup Failed');
	end;

	Output('------------------------------------');
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	DestroyGlobals;
end;

procedure TMainForm.UserInputReturnPressed(Sender: TObject);
begin
	Output(TEdit(Sender).Text);
	TEdit(Sender).Clear;
end;

procedure TMainForm.ServerException(AThread: TIdPeerThread;
	AException: Exception);
begin
	if Athread.Connection.Connected then
		Athread.Connection.Disconnect;
end;

procedure TMainForm.LoginServerConnect(AThread: TIdPeerThread);
begin
	Output('Connection from ' + AThread.Connection.Socket.Binding.PeerIP);
end;

procedure TMainForm.CharaServerExecute(AThread: TIdPeerThread);
begin
	ParseCharaServ(AThread);
end;

end.
