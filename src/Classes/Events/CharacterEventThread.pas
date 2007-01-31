//------------------------------------------------------------------------------
//CharacterEventThread                                                     UNIT
//------------------------------------------------------------------------------
//	What it does-
//      Contains the CharacterEventThread class, which is our thread that
//    handles character events.
//
//	Changes -
//		January 31st,  2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit CharacterEventThread;

interface
uses
	SyncObjs,
	IdThread,
	CharaList;
type
//------------------------------------------------------------------------------
//TCharacterEventThread
//------------------------------------------------------------------------------
	TCharacterEventThread = class(TIdThread)
	private
		CriticalSection : TCriticalSection;

	public
		CharacterList : TCharacterList;
		Constructor Create(ACharacterList : TCharacterList);reintroduce;
		Destructor  Destroy;override;
		Procedure 	Run;override;
	end;
//------------------------------------------------------------------------------

implementation
uses
	SysUtils,
	WinLinux;

//------------------------------------------------------------------------------
//Create                                                          	CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//      Initializes our EventThread
//
//	Changes -
//		January 31st,  2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Constructor TCharacterEventThread.Create(ACharacterList : TCharacterList);
begin
	inherited Create(TRUE, TRUE, 'CharacterEventThread');
	CharacterList := ACharacterList;
	CriticalSection := TCriticalSection.Create;
end;//Create
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy                                                          	 DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//      Destroys our EventThread
//
//	Changes -
//		January 31st,  2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Destructor TCharacterEventThread.Destroy;
begin
	CriticalSection.Free;
	inherited;
end;//Destroy
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Run																																	PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      The actual thread executing code.
//
//	Changes -
//		January 31st,  2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TCharacterEventThread.Run;
var
	CharacterIndex	: Integer;
	EventIndex			: Integer;
	CurrentTime			: Int64;
begin
	inherited;
	//Get the current "Tick" or time.
	CurrentTime := GetTick;

	//Enter a critical section to avoid access violations.
	CriticalSection.Enter;

	//Loop through the character list
	for CharacterIndex := CharacterList.Count - 1 downto 0 do
	begin
		//Loop through each character's eventlist.
		for EventIndex := CharacterList[CharacterIndex].EventList.Count - 1 downto 0 do
		begin
			//Check to see if the event needs to be fired.
			if CurrentTime <= CharacterList[CharacterIndex].EventList[EventIndex].ExpiryTime then
			begin
				//If it does, execute the event, then delete it from the list.
				//(The list "owns" the events, so this does not leak)
				CharacterList[CharacterIndex].EventList[EventIndex].Execute;
				CharacterList[CharacterIndex].EventList.Delete(EventIndex);
			end;
		end;
	end;

	//Exit our critical section and sleep for 1ms (OS dependant, actually 55ms on
	//win32) If anyone has a better way to wait between events, let me know!
	CriticalSection.Leave;
	Sleep(1);
end;//Run
//------------------------------------------------------------------------------
end.
