//------------------------------------------------------------------------------
//CGroundItemRemovalEventThread                                             UNIT
//------------------------------------------------------------------------------
//	What it does-
//		This thread currently only for one purpose:
//			To clean up ground items...
//
//	Changes -
//		[2008/12/05] Aeomin - Created.
//
//------------------------------------------------------------------------------
unit GroundItemRemovalEventThread;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
	{RTL/VCL}
	{Project}
	EventList,
	{Third Party}
	IdThread
	;

type
//------------------------------------------------------------------------------
//TCharacterEventThread
//------------------------------------------------------------------------------
	TGroundItemEventThread = class(TIdThread)
	public
		EventList : TEventList;
		constructor Create(AnEventList : TEventList);reintroduce;
		procedure 	Run;override;
	end;
//------------------------------------------------------------------------------

implementation

uses
	{RTL/VCL}
	SysUtils,
	Classes,
	SyncObjs,
	{Project}
	Main,
	Event,
	{3rd Party}
	WinLinux
	;

//------------------------------------------------------------------------------
//Create                                                             CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//		Initializes our EventThread
//
//	Changes -
//		[2008/12/05] Aeomin - Created
//
//------------------------------------------------------------------------------
Constructor TGroundItemEventThread.Create(AnEventList : TEventList);
begin
	inherited Create(TRUE, TRUE, 'GroundItemEventThread');
	EventList := AnEventList;
end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Run                                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Let's do it!
//
//	Changes -
//		[2008/12/05] Aeomin - Created
//
//------------------------------------------------------------------------------
Procedure TGroundItemEventThread.Run;
var
	EventIndex		: Integer;
	CurrentTime		: LongWord;
	AnEvent			: TRootEvent;
	AnEventList		: TList;
	CriticalSection		: TCriticalSection;
begin
	//Get the current "Tick" or time.
	CurrentTime := GetTick;
	CriticalSection := TCriticalSection.Create;
	//Loop through the character list
	CriticalSection.Enter;
	AnEventList := EventList.LockList;
	try
		//Loop through each character's eventlist.
		for EventIndex := (AnEventList.Count - 1) downto 0 do
		begin
			AnEvent := AnEventList[EventIndex];
			//Check to see if the event needs to be fired.
			if CurrentTime >= AnEvent.ExpiryTime then
			begin
				//If it does, execute the event, then delete it from the list.
				//(The list "owns" the events, so this does not leak)   {not right now though}
				AnEventList.Delete(EventIndex);
				AnEvent.Execute;
				AnEvent.Free;
			end;
		end;
	finally
		EventList.UnlockList;
	end;
	CriticalSection.Leave;
	CriticalSection.Free;
	//Free up the processor
	Sleep(MainProc.ZoneServer.Options.EventTick);
end;//Run
//------------------------------------------------------------------------------
end.