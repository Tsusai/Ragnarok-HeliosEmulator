//------------------------------------------------------------------------------
//CommClient                                                               UNIT
//------------------------------------------------------------------------------
//	What it does -
//			This is an extension of the tIdTCPClient which uses a thread to read
//    incoming data into a buffer.
//
//  Notes -
//      Followed idea of TIdTelnet, with a thread checking data.
//    Original Code by Tsusai, with corrections given by Gambit of the Indy
//    team.
//
//	Changes -
//		January 4th, 2007 - RaX - Created Header.
//		January 20th, 2007 - Tsusai - Reusing TIdThread. It doesn't raise as
//			many exceptions due to conflicts with the process threadlist
//
//------------------------------------------------------------------------------
unit CommClient;

interface
uses
	IdTCPClient,
	IdThread;

type
  //forward declaration
	TInterClient = class;

  //Event Type
	TDataAvailableEvent = procedure (AClient : TInterClient) of object;

//------------------------------------------------------------------------------
//TClientThread                                                           CLASS
//------------------------------------------------------------------------------
	TClientThread = class(TIdThread)
	protected
		FClient: TInterClient;
		procedure Run; override;
	public
		constructor Create(AClient: TInterClient); reintroduce;
		property Client: TInterClient read FClient;
	end;{TClientThread}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TInterClient                                                           CLASS
//------------------------------------------------------------------------------
	TInterClient = class(TIdTCPClient)
	protected
		fReadThread : TClientThread;
		fOnRecieve : TDataAvailableEvent;
		fActive : boolean;
		procedure DoRecieveEvent;
		procedure SetActive(Value : boolean);
	public
		ReconnectTime : TDateTime;
		SourceName : string;
		DestinationName : string;
		constructor Create(Source,Destination : string);
		destructor Destroy; override;
	published
		property Active : boolean read fActive write SetActive;
		property OnRecieve : TDataAvailableEvent read fOnRecieve write fOnRecieve;
	end;{TInterClient}
//------------------------------------------------------------------------------


implementation
uses
	SysUtils,
	Console,
	Globals,
	WinLinux,
	IdStack;

//------------------------------------------------------------------------------
//Run                                                            PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//			This is the actual executing code of the thread.
//
//	Changes -
//		January 4th, 2007 - RaX - Created Header.
//		January 14th, 2007 - Tsusai - Modified the error response.
//		January 20th, 2007 - Tsusai - Now TIdThread.Run, removed while statement
//
//------------------------------------------------------------------------------
	procedure TClientThread.Run;
	begin
		Sleep(1);
		if FClient.Active then
		begin
			try
				if (Now > FClient.ReconnectTime) and
					(FClient.Connected = false) then
				begin
					FClient.Connect;
				end;
			except
				on E:EIdSocketError do
				begin
					MainProc.Console(
						Format('%s Server failed to connect to %s Server.',
							[FClient.SourceName,FClient.DestinationName]
						)
					);
					MainProc.Console('Retrying in 30 seconds.');
					FClient.ReconnectTime := IncSecond(Now,30);
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
	end;{Run}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Create                                                           CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does -
//			Creates our client thread.
//
//	Changes -
//		January 4th, 2007 - RaX - Created Header.
//		January 20th, 2007 - Tsusai - Updated create call, removed FreeOnTerminate
//
//------------------------------------------------------------------------------
	constructor TClientThread.Create(AClient : TInterClient);
	begin
		inherited Create(True,True,AClient.SourceName + ' Client');
		FClient := AClient;
		Priority := PriorityLow;
	end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Create                                                           CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does -
//			Creates our interclient.
//
//	Changes -
//		January 4th, 2007 - RaX - Created Header.
//		January 14th, 2007 - Tsusai - Added setup of source and destination
//			Server names.
//
//------------------------------------------------------------------------------
	constructor TInterClient.Create(Source,Destination : string);
	begin
		inherited Create;
		SourceName := Source;
		DestinationName := Destination;
		fReadThread := TClientThread.Create(Self);
	end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy                                                           DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does -
//			Destroys our interclient.
//
//	Changes -
//		January 4th, 2007 - RaX - Created Header.
//		January 20th, 2007 - Tsusai - Changed how the readthread ends.
//
//------------------------------------------------------------------------------
	destructor TInterClient.Destroy;
	begin
		Active := false;
		if Assigned(fReadThread) then
		begin
			fReadThread.Stop;
			fReadThread.Destroy;
		end;
		inherited Destroy;
	end;{Destroy}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetActive                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//			This is the actual executing code of the thread.
//
//	Changes -
//		January 4th, 2007 - RaX - Created Header.
//		January 20th, 2007 - Tsusai - Updated for TIdThread.
//
//------------------------------------------------------------------------------
	procedure TInterClient.SetActive(Value : boolean);
	begin
		if Assigned(fReadThread) then
		begin
			if Value then
			begin
				fReadThread.Start;
				fActive := Value;
			end else
			begin
				fReadThread.Stop;
				if Connected then
				begin
					Disconnect;
				end;
				fActive := Value;
			end;
		end;
	end;{SetActive}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//DoReceiveEvent                                                      PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		 Receives data.
//
//	Changes -
//		January 4th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure TInterClient.DoRecieveEvent;
	begin
		try
			if Assigned(fOnRecieve) then fOnRecieve(Self);
		finally
			IOHandler.InputBuffer.Clear;
		end;
	end;{DoReceiveEvent}
//------------------------------------------------------------------------------

end.
