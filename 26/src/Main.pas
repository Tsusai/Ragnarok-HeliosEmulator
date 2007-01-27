unit Main;

interface

uses
	{System}
	SysUtils,
	Types,
	Classes,
	Controls,
	Forms,
	StdCtrls,
	Menus,
	ComCtrls,
	IdBaseComponent,
	IdComponent,
	IdTCPServer,
	IdTCPConnection,
	IdTCPClient;

type
	TMainForm = class(TForm)
		LoginServer: TIdTCPServer;
		Console: TMemo;
    MainMenu: TMainMenu;
		File1: TMenuItem;
		Exit1: TMenuItem;
		CharaServer: TIdTCPServer;
		CharaToLogin: TIdTCPClient;
		ZoneServer: TIdTCPServer;
		ZoneToChara: TIdTCPClient;
    ConsoleIn: TEdit;
    StatusBar: TStatusBar;
		procedure LoginServerExecute(AThread: TIdPeerThread);
		procedure FormCreate(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure UserInputReturnPressed(Sender: TObject);
		procedure ServerException(AThread: TIdPeerThread;
			AException: Exception);
    procedure LoginServerConnect(AThread: TIdPeerThread);
    procedure CharaServerExecute(AThread: TIdPeerThread);
    procedure Exit1Click(Sender: TObject);
    procedure ConsoleInKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
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
	Database,
	CharaServerTypes,
	LoginPacket,
  Commands,
	CharaServerPacket;

{$R *.dfm}
var
  Database : TDatabase; //Databasing object - RaX
  Commands : TCommands; //Console Commands - RaX

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
		LocalCharaServ.ServerName := 'Helios';
		CharaServerList.AddObject(LocalCharaServ.ServerName,LocalCharaServ);
	end else begin
		CharaToLogin.Connect;
	end;

	if LoginServer.Active then
	begin
		if not Database.LoadAccounts then begin  //changed to database object from functions - RaX
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

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Commands.Exit(Application); //Executes Application Exit command - RaX
end;

procedure TMainForm.ConsoleInKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) and (ConsoleIn.Text > ' ') then begin //When Enter is pressed and there is text...-RaX
    Commands.Parse(ConsoleIn.Text);  //Parse text for commands -RaX
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
	ConsoleIn.SetFocus;
end;

end.
