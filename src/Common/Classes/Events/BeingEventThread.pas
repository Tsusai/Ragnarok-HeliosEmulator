//------------------------------------------------------------------------------
//BeingEventThread                                                          UNIT
//------------------------------------------------------------------------------
//	What it does-
//      Contains the CharacterEventThread class, which is our thread that
//    handles character events.
//
//	Changes -
//		January 31st,  2007 - RaX - Created Header.
//		[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
//		[2008/12/18] Aeomin - Renamed from CharacterEventThread
//
//------------------------------------------------------------------------------
unit BeingEventThread;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
	{RTL/VCL}
	{Project}
	BeingList,
	{Third Party}
	IdThread
	;


type
//------------------------------------------------------------------------------
//TBeingEventThread
//------------------------------------------------------------------------------
	TBeingEventThread = class(TIdThread)
	public
		BeingList : TBeingList;
		Constructor Create(ABeingList : TBeingList;AName:String);reintroduce;
		Destructor  Destroy;override;
		Procedure 	Run;override;
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
	GameObject,
	Being,
	Character,
	{3rd Party}
	WinLinux
	//none
	;


//------------------------------------------------------------------------------
//Create                                                             CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//      Initializes our EventThread
//
//	Changes -
//		January 31st,  2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Constructor TBeingEventThread.Create(ABeingList : TBeingList;AName:String);
begin
	inherited Create(TRUE, TRUE, AName);
	BeingList := ABeingList;
end;//Create
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy                                                             DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//      Destroys our EventThread
//
//	Changes -
//		January 31st,  2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Destructor TBeingEventThread.Destroy;
begin
	inherited;
end;//Destroy
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Run                                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		The actual thread executing code.
//
//	Changes -
//		January 31st,  2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TBeingEventThread.Run;
var
	BeingIndex		: Integer;
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
	for BeingIndex :=BeingList.Count - 1 downto 0 do
	begin
		if BeingIndex < BeingList.Count then
		begin
			if (BeingList[BeingIndex] is TCharacter) AND NOT(BeingList[BeingIndex].ZoneStatus = isOnline) then
				Continue;
			CriticalSection.Enter;

			AnEventList := BeingList[BeingIndex].EventList.LockList;
			try
				//Loop through each character's eventlist.
				if AnEventList.Count > 0 then
				begin
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
				end;
			finally
				BeingList[BeingIndex].EventList.UnlockList;
			end;
			CriticalSection.Leave;
		end;
	end;
	CriticalSection.Free;
	//Free up the processor
	Sleep(MainProc.ZoneServer.Options.EventTick);
end;//Run
//------------------------------------------------------------------------------
end.
