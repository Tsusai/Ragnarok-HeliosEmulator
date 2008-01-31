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

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}

interface

uses
	Classes,
	IdTCPClient,
	IdThread;
 
type
	//Forward declarations
	TInterClient = class;
 
	//Event Types
	TDataAvailableEvent = procedure (AClient : TInterClient) of object;
 
//------------------------------------------------------------------------------
//TClientThread                                                            CLASS
//------------------------------------------------------------------------------
	TClientThread = class(TIdThread)

	protected
		fClient										: TInterClient;

		procedure Run; override;


	public
		constructor Create(
			AClient									: TInterClient
		); reintroduce;

		property Client						: TInterClient
			read	fClient
		;

	end;{TClientThread}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TReconnectThread                                                        CLASS
//------------------------------------------------------------------------------
	TReconnectThread = class(TIdThread)

	protected
		fClient										: TInterClient;
		
		procedure Run(

		); override;


	public
		constructor Create(
			AClient									: TInterClient
		); reintroduce;

	end;{TReconnectThread}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TInterClient                                                             CLASS
//------------------------------------------------------------------------------
	TInterClient = class(TIdTCPClient)

	protected
		fReadThread								 : TClientThread;
		fReconnectThread					 : TReconnectThread;


		fOnReceive								: TDataAvailableEvent;
		fOnReconnect							: TNotifyEvent;

		fActive										: Boolean;

		procedure SetActive(
			Value : Boolean
		);
		
		procedure DoReceiveEvent;
		procedure DoReconnectEvent;


	public
		AutoReconnect							: Boolean;
		ReconnectDelay						: LongWord;
		SourceName								: string;
		DestinationName						: string;
		ShutDown									: Boolean;

		constructor Create(
			Source									: string;
			Destination							: string;
			AutoReconnect						: Boolean;
			ReconnectDelay					: LongWord = 3000
		);

		destructor Destroy(

		); override;

		procedure Connect(

		); override;

		procedure Disconnect(
			ANotifyPeer							: Boolean
		); override;

		procedure Reconnect(

		);

		property Active						: Boolean
			read	fActive
			write	SetActive
		;

		property OnReceive				: TDataAvailableEvent
			read	fOnReceive
			write	fOnReceive
		;

		property OnReconnect			: TNotifyEvent
			read	fOnReconnect
			write	fOnReconnect
		;
	end;{TInterClient}
//------------------------------------------------------------------------------
 
implementation

uses
	SysUtils,
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
			fClient.IOHandler.CheckForDisconnect(true);
			fClient.IOHandler.CheckForDataOnSource(10);

			if not fClient.IOHandler.InputBufferIsEmpty then
			begin
				fClient.DoReceiveEvent;
			end;

		except
			Console.Message('Connection to '+fClient.DestinationName+' Server lost.', fClient.SourceName + ' Server', MS_ALERT);
			fClient.Reconnect;
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
	constructor TClientThread.Create(
		AClient : TInterClient
	);
	begin
		inherited Create(True, True, AClient.SourceName + ' Client');
		fClient := AClient;
		Start;
	end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Create                                                             CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does -
//			Creates our reconnect thread.
//
//	Changes -
//		January 30th, 2008 - RaX - Created Header.
//
//------------------------------------------------------------------------------
constructor TReconnectThread.Create(
	AClient							: TInterClient
);
begin
	inherited Create(false);
	fClient									:= AClient;
end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Run			                                                           	PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//			Fires for each interation of our ReconnectThread. Attempts to reconnect
//		to the destination.
//
//	Changes -
//		January 30th, 2008 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TReconnectThread.Run(

);
begin
	Sleep(fClient.ReconnectDelay);
	Console.Message('Attempting to reconnect...', fClient.SourceName + ' Server', MS_ALERT);

	if Assigned(fClient.fReadThread) then
	begin
		fClient.fReadThread.Terminate;
		fClient.fReadThread.WaitFor;
		FreeAndNil(fClient.fReadThread);
	end;

	fClient.Connect;

	//Apparently indy threads' FreeOnTerminate does NOT work, this was added to 
	//force it.
	Terminate;
end;{Execute}
//------------------------------------------------------------------------------


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
	constructor TInterClient.Create(
		Source						: string;
		Destination				: string;
		AutoReconnect			: Boolean;
		ReconnectDelay		: LongWord = 3000
	);
	begin
		inherited Create;

		SourceName						:= Source;
		self.AutoReconnect		:= AutoReconnect;
		self.ReconnectDelay		:= ReconnectDelay;
		DestinationName				:= Destination;

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
	procedure TInterClient.Connect(

	);
	begin
	
		try
			inherited Connect;
			fReadThread := TClientThread.Create(Self);

		except
			Disconnect(false);
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
	procedure TInterClient.Disconnect(
		ANotifyPeer: Boolean
	);
	begin

		if Assigned(fReadThread) then
		begin
			fReadThread.Terminate;
		end;

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
	destructor TInterClient.Destroy(

	);
	begin
		Active := false;

		if Assigned(fReconnectThread) then
		begin

			if not fReconnectThread.Terminated then
			begin
				fReconnectThread.Terminate;
			end;

		end;

		if Assigned(fReadThread) then
		begin
			fReadThread.Terminate;
			fReadThread.WaitFor;
			fReadThread.Free;
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
	procedure TInterClient.SetActive(
		Value : Boolean
	);
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
//Reconnect                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//			Starts the reconnect thread which tries to reconnect to the destination.
//
//	Changes -
//		January 30th, 2008 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TInterClient.Reconnect(

);
begin

	if not ShutDown then
	begin

		DoReconnectEvent;
		fReconnectThread	:= TReconnectThread.Create(Self);

		if Assigned(fReadThread) then
		begin

			if not fReadThread.Terminated then
			begin
				fReadThread.Terminate;
			end;

		end;

	end;
end;{Reconnect}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//DoReconnectEvent                                                    PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//			Attempts to fire the OnReconnect event.
//
//	Changes -
//		January 30th, 2008 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TInterClient.DoReceiveEvent(

);
begin
	try

		if Assigned(fOnReceive) then
		begin
			fOnReceive(self);
		end;

	finally
		//nothing to do
	end;


end;{DoReceiveEvent}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//DoReconnectEvent                                                    PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//			Attempts to fire the OnReconnect event.
//
//	Changes -
//		January 30th, 2008 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TInterClient.DoReconnectEvent(

);
begin
	try

		if Assigned(fOnReconnect) then
		begin
			fOnReconnect(self);
		end;

	finally
		//nothing to do
	end;


end;{DoReconnectEvent}
//------------------------------------------------------------------------------

end.
