//------------------------------------------------------------------------------
//EventList                                                            UNIT
//------------------------------------------------------------------------------
//  What it does -
//      A list of TRootEvents
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
unit EventList;

interface
uses
	SyncObjs,
	SysUtils,
	Event;

type
	PEvent = ^TRootEvent;

//------------------------------------------------------------------------------
//TEventList                                                          CLASS
//------------------------------------------------------------------------------
	TEventList = Class(TObject)

	Private
		MsCount  : Integer; // Count of Events in the list
		MaxCount : Integer; // Maximum Events that can fit into current storage
		MemStart : Pointer; // Start of the memory holding the list
		NextSlot : PEvent; // Points to the next free slot in memory

    OwnsEvents : Boolean;//If we own the Events, we handle free'ing them.

		CriticalSection : TCriticalSection;

		Function GetValue(Index : Integer) : TRootEvent;
    Procedure SetValue(Index : Integer; Value : TRootEvent);
		Procedure Expand(const Size : Integer);
    Procedure Shrink(const Size : Integer);

	Public
    Constructor Create(OwnsEvents : Boolean);
		Destructor Destroy; override;
		Property Items[Index : Integer] : TRootEvent
			read GetValue write SetValue;default;


		Procedure Add(const AEvent : TRootEvent);
		Procedure Delete(Index : Integer);
		Procedure Clear();

		Property Count : Integer
		read MsCount;
	end;
//------------------------------------------------------------------------------


implementation

const
	ALLOCATE_SIZE = 20; // How many Events to store in each incremental memory block

//------------------------------------------------------------------------------
//Create                                                            CONSTRUCTOR
//------------------------------------------------------------------------------
//  What it does -
//      Initializes our Eventlist. Creates a storage area.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
constructor TEventList.Create(OwnsEvents : Boolean);
begin
	CriticalSection := TCriticalSection.Create;
	CriticalSection.Enter;
	inherited Create;
	MsCount  := 0; // No Events in the list yet
  MaxCount := 0; //no mem yet!
	MemStart := NIL;//no memory yet
	self.OwnsEvents := OwnsEvents;
	CriticalSection.Leave;
end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy                                                            DESTRUCTOR
//------------------------------------------------------------------------------
//  What it does -
//      Destroys our list and frees any memory used.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
destructor TEventList.Destroy;
var
	Index : Integer;
begin
	CriticalSection.Enter;
	//if we own the Events, free all of them in the list.
  if OwnsEvents then
  begin
    for Index := 0 to MsCount - 1 do
    begin
			Items[Index].Free;
		end;
  end;

	// Free the allocated memory
	FreeMem(MemStart);

	// Call TObject destructor
	CriticalSection.Leave;
	CriticalSection.Free;
	inherited;
end;{Destroy}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Expand                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Increases the memory area size by Size Address.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TEventList.Expand(const Size : Integer);
var
	NewMemoryStart : Pointer;
	OldPointer, NewPointer : PEvent;
	Index : Integer;
begin
	CriticalSection.Enter;
	// First allocate a new, bigger memory space
	GetMem(NewMemoryStart, (MaxCount + Size) * SizeOf(TRootEvent));
	if(Assigned(MemStart)) then
	begin
	  // Copy the data from the old memory here
	  OldPointer := MemStart;
	  NewPointer := NewMemoryStart;
	  for Index := 1 to MaxCount do
	  begin
		  // Copy one Event at a time
		  NewPointer^ := OldPointer^;
		  Inc(OldPointer);
		  Inc(NewPointer);
	  end;
    // Free the old memory
    FreeMem(MemStart);
  end;

  // And now refer to the new memory
	MemStart := NewMemoryStart;
	NextSlot := MemStart;
	Inc(NextSlot, MaxCount);
	Inc(MaxCount, Size);
	CriticalSection.Leave;
end;{Expand}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Shrink                                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Decreases the memory area size by Size PEvent.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TEventList.Shrink(const Size: Integer);
var
	NewMemoryStart : Pointer;
	OldPointer, NewPointer : PEvent;
	Index : Integer;
begin
	CriticalSection.Enter;
  if MaxCount > Size then
  begin
    //first allocate a new, smaller memory space
    GetMem(NewMemoryStart, (MaxCount - Size) * SizeOf(TRootEvent));
    if(Assigned(MemStart)) then
	  begin
	    // Copy the data from the old memory here
	    OldPointer := MemStart;
	    NewPointer := NewMemoryStart;
	    for Index := 1 to MaxCount do
	    begin
		    // Copy one Event at a time
		    NewPointer^ := OldPointer^;
		    Inc(OldPointer);
		    Inc(NewPointer);
	    end;
      // Free the old memory
      FreeMem(MemStart);
    end;

    // And now refer to the new memory
	  MemStart := NewMemoryStart;
	  NextSlot := MemStart;
	  Inc(NextSlot, MaxCount);
	  Inc(MaxCount, Size);
	end;
	CriticalSection.Leave;
end;{Shrink}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Add                                                                 PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Adds a TRootEvent to the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TEventList.Add(const AEvent : TRootEvent);
begin
	CriticalSection.Enter;
	// If we do not have enough space to add the Event, then get more space!
	if MsCount = MaxCount then
	begin
		Expand(ALLOCATE_SIZE);
	end;

	// Now we can safely add the Event to the list
	NextSlot^ := AEvent;

	// And update things to suit
	Inc(MsCount);
	Inc(NextSlot);
	CriticalSection.Leave;
end;{Add}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Delete                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Removes a TRootEvent at Index from the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TEventList.Delete(Index : Integer);
var
	CurrentItem : PEvent;
	NextItem    : PEvent;
begin
	CriticalSection.Enter;
	if (Index >= 0) and (Index < Count) then
	begin
		//if we own the Event, free it.
		if OwnsEvents then
		begin
			Items[Index].Free;
		end;
  
		if (MaxCount-ALLOCATE_SIZE) = (MsCount) then
		begin
			Shrink(ALLOCATE_SIZE);
		end;
		for Index := Index to MsCount - 1 do
		begin
			CurrentItem := MemStart;
			inc(CurrentItem, Index);
			NextItem := CurrentItem;
			Inc(NextItem,1);
			CurrentItem^ := NextItem^;
		end;

		Dec(MsCount,  1);
		Dec(NextSlot, 1);
	end;
	CriticalSection.Leave;
end;{Delete}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Clear                                                               PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Clears the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TEventList.Clear;
var
  Index : Integer;
begin
	CriticalSection.Enter;
  //if we own the Events, the free them.
  if OwnsEvents then
  begin
    for Index := 0 to MsCount - 1 do
    begin
      Items[Index].Free;
    end;
  end;

  // Free the allocated memory
  if Assigned(MemStart) then
  begin
    FreeMem(MemStart);
    MsCount  := 0;  // No Events in the list yet
		MaxCount := 0;  //no max size
    MemStart := NIL;//no memory yet
	end;
	CriticalSection.Leave;
end;{Clear}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetValue                                                             FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Returns a TRootEvent at the index.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
function TEventList.GetValue(Index : Integer): TRootEvent;
var
	EventPtr : PEvent;
begin
	// Simply get the value at the given TRootEvent index position
	EventPtr := MemStart;
	Inc(EventPtr, Index); // Point to the index'th TRootEvent in storage

	Result := EventPtr^; // And get the TRootEvent it points to
end;{GetValue}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetValue                                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sets a TRootEvent into the list at Index.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TEventList.SetValue(Index : Integer; Value : TRootEvent);
var
	EventPtr : PEvent;
begin
	CriticalSection.Enter;
	// Simply set the value at the given TRootEvent index position
	EventPtr := MemStart;
	Inc(EventPtr, Index); // Point to the index'th TRootEvent in storage
	EventPtr^ := Value;
	CriticalSection.Leave;
end;{SetValue}
//------------------------------------------------------------------------------
end.
