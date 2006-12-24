unit CommClient;
//Followed idea of TIdTelnet, with a thread checking data.
//Original Code by Tsusai, with corrections given by Gambit of the Indy team.

interface
uses
	IdTCPClient,
	Classes;

type
	TInterClient = class;

	TDataAvailableEvent = procedure (AClient : TInterClient) of object;

	TClientThread = class(TThread)
	protected
		FClient: TInterClient;
		procedure Execute; override;
	public
		constructor Create(AClient: TInterClient); reintroduce;
		property Client: TInterClient read FClient;
	end;

	TInterClient = class(TIdTCPClient)
	protected
		fReadThread : TClientThread;
		fOnRecieve : TDataAvailableEvent;
		fActive : boolean;
		procedure DoRecieveEvent;
		procedure SetActive(Value : boolean);
	public
		ReconnectTime : TDateTime;
		constructor Create;
		destructor Destroy; override;
	published
		property Active : boolean read fActive write SetActive;
		property OnRecieve : TDataAvailableEvent read fOnRecieve write fOnRecieve;
	end;

implementation
uses
	SysUtils,
	Console,
	Globals,
	WinLinux,
	IdStack;

	procedure TClientThread.Execute;
	begin
		while not Terminated do
		begin
			Sleep(1);
			if FClient.Active then
			begin
				if (Now > FClient.ReconnectTime) and
					(FClient.Connected = false) then
				begin
					try
						FClient.Connect;
					except
						on E:EIdSocketError do
						begin
							MainProc.Console('Character Server failed to connect to login server.');
							MainProc.Console('Retrying in 30 seconds.');
							FClient.ReconnectTime := IncSecond(Now,30);
						end;
					end;
				end;
				//READ CHECK
				if FClient.Connected then
				begin
					if Assigned(FClient.IOHandler) then
					begin
						FClient.IOHandler.CheckForDataOnSource(10);
						if not FClient.IOHandler.InputBufferIsEmpty then
						begin
							FClient.DoRecieveEvent;
						end;
					end;
				end;
			end;
		end;
	end;

	constructor TClientThread.Create(AClient : TInterClient);
	begin
		inherited Create(True);
		FreeOnTerminate := true;
		FClient := AClient;
		Priority := PriorityLow;
	end;

	constructor TInterClient.Create;
	begin
		inherited Create;
		fReadThread := TClientThread.Create(Self);
	end;

	destructor TInterClient.Destroy;
	begin
		Active := false;
		if Assigned(fReadThread) then
		begin
			fReadThread.Terminate;
		end;
		inherited Destroy;
	end;

	procedure TInterClient.SetActive(Value : boolean);
	begin
		if Assigned(fReadThread) then
		begin
			if Value then
			begin
				fReadThread.Resume;
			end else
			begin
				fReadThread.Suspend;
				Disconnect;
			end;
			fActive := Value;
		end;
	end;

	procedure TInterClient.DoRecieveEvent;
	begin
		try
			if Assigned(fOnRecieve) then fOnRecieve(Self);
		finally
			IOHandler.InputBuffer.Clear;
		end;
	end;

end.
