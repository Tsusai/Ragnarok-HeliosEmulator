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
	Event,
	SysUtils,
	WinLinux,
	Main;

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
	CurrentTime			: LongWord;
	AEvent          : TRootEvent;
begin
	//Get the current "Tick" or time.
	CurrentTime := GetTick;

	//Loop through the character list
	for CharacterIndex := CharacterList.Count - 1 downto 0 do
	begin
		//Loop through each character's eventlist.
		for EventIndex := CharacterList[CharacterIndex].EventList.Count - 1 downto 0 do
		begin
			//Check to see if the event needs to be fired.
			if CurrentTime <= CharacterList[CharacterIndex].EventList[EventIndex].ExpiryTime then
			begin
				//Enter a critical section to avoid access violations.
				CriticalSection.Enter;
				//If it does, execute the event, then delete it from the list.
				//(The list "owns" the events, so this does not leak)   {not right now though}
				CharacterList[CharacterIndex].EventList[EventIndex].Execute;
				CharacterList[CharacterIndex].EventList.Delete(EventIndex);
				CriticalSection.Leave;
			end;
		end;
	end;
	//Free up the processor
	Sleep(MainProc.ZoneServer.Options.EventTick);
end;//Run
//------------------------------------------------------------------------------
end.
