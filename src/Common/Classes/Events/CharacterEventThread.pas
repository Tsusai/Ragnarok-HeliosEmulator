//------------------------------------------------------------------------------
//CharacterEventThread                                                     UNIT
//------------------------------------------------------------------------------
//	What it does-
//      Contains the CharacterEventThread class, which is our thread that
//    handles character events.
//
//	Changes -
//		January 31st,  2007 - RaX - Created Header.
//		[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
//
//------------------------------------------------------------------------------
unit CharacterEventThread;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
	{RTL/VCL}
	{Project}
	CharaList,
	{Third Party}
	IdThread
	;


type
//------------------------------------------------------------------------------
//TCharacterEventThread
//------------------------------------------------------------------------------
	TCharacterEventThread = class(TIdThread)
	public
		CharacterList : TCharacterList;
		Constructor Create(ACharacterList : TCharacterList);reintroduce;
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
	Being,
	{3rd Party}
	WinLinux
	//none
	;


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
	inherited;
end;//Destroy
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Run                                                                  PROCEDURE
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
	CharacterIndex		: Integer;
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
	for CharacterIndex := CharacterList.Count - 1 downto 0 do
	begin
		if CharacterIndex < CharacterList.Count then
		begin
			if CharacterList[CharacterIndex].ZoneStatus = isOnline then
			begin
				CriticalSection.Enter;
				AnEventList := CharacterList[CharacterIndex].EventList.LockList;
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
					CharacterList[CharacterIndex].EventList.UnlockList;
				end;
				CriticalSection.Leave;
			end;
		end;
	end;
	CriticalSection.Free;
	//Free up the processor
	Sleep(MainProc.ZoneServer.Options.EventTick);
end;//Run
//------------------------------------------------------------------------------
end.
