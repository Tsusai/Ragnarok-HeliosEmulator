//------------------------------------------------------------------------------
//CommClient                                                                UNIT
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
	Classes,
	IdTCPClient,
	IdThread;
 
type
  //forward declaration
	TInterClient = class;
 
  //Event Type
	TDataAvailableEvent = procedure (AClient : TInterClient) of object;
 
//------------------------------------------------------------------------------
//TClientThread                                                            CLASS
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
 
	TReconnector = class(TThread)
	protected
		FClient: TInterClient;
		procedure Execute; override;
	public
		constructor Create(AClient: TInterClient);
	end;
//------------------------------------------------------------------------------
//TInterClient                                                             CLASS
//------------------------------------------------------------------------------
	TInterClient = class(TIdTCPClient)
	protected
		fReadThread : TClientThread;
		fOnRecieve : TDataAvailableEvent;
		fActive : boolean;
		Reconnector : TReconnector;
		procedure DoRecieveEvent;
		procedure SetActive(Value : boolean);
	public
		ReconnectDelay : LongWord;
		SourceName : string;
		DestinationName : string;
		ShutDown : Boolean;
		constructor Create(Source,Destination : string;  ReconnectDelay : LongWord = 3000);
		destructor Destroy; override;
		procedure Connect; override;
		procedure Disconnect(ANotifyPeer: Boolean); override;
		procedure ReConnect;
	published
		property Active : boolean read fActive write SetActive;
		property OnRecieve : TDataAvailableEvent read fOnRecieve write fOnRecieve;
	end;{TInterClient}
//------------------------------------------------------------------------------
 
implementation
uses
	SysUtils,
	WinLinux,
  Globals;
 
//------------------------------------------------------------------------------
//Run                                                                  PROCEDURE
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
		try
      FClient.FIOHandler.CheckForDisconnect(TRUE);
			FClient.IOHandler.CheckForDataOnSource(10);
			if not FClient.IOHandler.InputBufferIsEmpty then
			begin
				FClient.DoRecieveEvent;
			end;
		except
      Console.Message('Connection to '+FClient.DestinationName+' Server lost.', fClient.SourceName, MS_ALERT);
			FClient.ReConnect;
		end;
	end;{Run}
 
//------------------------------------------------------------------------------
 
 
//------------------------------------------------------------------------------
//Create                                                             CONSTRUCTOR
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
		LowerPriority(Self);
		Start;
	end;{Create}
//------------------------------------------------------------------------------
 
constructor TReconnector.Create(AClient: TInterClient);
begin
	FClient := AClient;
	FreeOnTerminate:=True;
	inherited Create(false);
end;
 
procedure TReconnector.Execute;
begin
	Sleep(FClient.ReconnectDelay);
  Console.Message('Attempting to reconnect...', FClient.SourceName, MS_ALERT);
	if Assigned(FClient.fReadThread) then
	begin
		while not FClient.fReadThread.Terminated do
		begin
			Sleep(10);
		end;
		FreeAndNil(FClient.fReadThread);
	end;
	FClient.Connect;
end;
 
//------------------------------------------------------------------------------
//Create                                                             CONSTRUCTOR
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
	constructor TInterClient.Create(Source,Destination : string; ReconnectDelay : LongWord = 3000);
	begin
		inherited Create;
		SourceName := Source;
    self.ReconnectDelay := ReconnectDelay;
		DestinationName := Destination;
	end;{Create}
//------------------------------------------------------------------------------
 
 
//------------------------------------------------------------------------------
//Connect                                                              PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//			Connects our interclient.
//
//	Changes -
//		March 12th, 2007 - Aeomin - Created Header.
//
//------------------------------------------------------------------------------
	procedure TInterClient.Connect;
	begin
		try
			inherited Connect;
			try
				fReadThread := TClientThread.Create(Self);
			except
				Disconnect(False);
        Reconnect;
			end;
		except
      Disconnect(False);
			Reconnect;
		end;
	end;
//------------------------------------------------------------------------------
 
 
//------------------------------------------------------------------------------
//Disconnect                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//			Connects our interclient.
//
//	Changes -
//		March 12th, 2007 - Aeomin - Created Header.
//
//------------------------------------------------------------------------------
	procedure TInterClient.Disconnect(ANotifyPeer: Boolean);
	begin
		if Assigned(fReadThread) then fReadThread.Terminate;
		try
			inherited Disconnect(ANotifyPeer);
		finally
			if Assigned(fReadThread) then
			begin
				fReadThread.WaitFor;
				FreeAndNil(fReadThread);
			end;
		end;
	end;
//------------------------------------------------------------------------------
 
 
//------------------------------------------------------------------------------
//Destroy                                                             DESTRUCTOR
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
		if Assigned(Reconnector) then
		begin
			if not Reconnector.Terminated then
				Reconnector.Terminate;
			Reconnector := NIL;
		end;
		if Assigned(fReadThread) then
		begin
			fReadThread.Stop;
			fReadThread.Destroy;
		end;
 
		inherited Destroy;
	end;{Destroy}
//------------------------------------------------------------------------------
 
 
//------------------------------------------------------------------------------
//SetActive                                                            PROCEDURE
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
		ShutDown := not value;
		if Value then
		begin
			try
				Connect;
			except
				fActive := Connected;
			end;
		end else
		begin
			Disconnect;
			fActive := Connected;
		end;
	end;{SetActive}
//------------------------------------------------------------------------------
 
 
//------------------------------------------------------------------------------
//DoReceiveEvent                                                       PROCEDURE
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
			//
		end;
	end;{DoReceiveEvent}
//------------------------------------------------------------------------------
procedure TInterClient.ReConnect;
begin
	if not ShutDown then
	begin
		if not Connected then
		begin
			Reconnector := TReconnector.Create(Self);
			if Assigned(fReadThread) then
			begin
//				fReadThread.WaitFor;
				if not fReadThread.Terminated then
				begin
					fReadThread.Terminate;
				end;
			end;
		end;
	end;
end;
end.
