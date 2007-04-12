//------------------------------------------------------------------------------
//MapList                                                            UNIT
//------------------------------------------------------------------------------
//  What it does -
//      A list of TMaps
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
unit MapList;

interface
uses
  Map;

type
	PMap = ^TMap;

//------------------------------------------------------------------------------
//TMapList                                                          CLASS
//------------------------------------------------------------------------------
	TMapList = Class(TObject)

	Private
		MsCount  : Integer; // Count of Maps in the list
		MaxCount : Integer; // Maximum Maps that can fit into current storage
		MemStart : Pointer; // Start of the memory holding the list
		NextSlot : PMap; // Points to the next free slot in memory

    OwnsMaps : Boolean;//If we own the Maps, we handle free'ing them.

		Function GetValue(Index : Integer) : TMap;
    Procedure SetValue(Index : Integer; Value : TMap);
		Procedure Expand(const Size : Integer);
    Procedure Shrink(const Size : Integer);

	Public
    Constructor Create(OwnsMaps : Boolean);
		Destructor Destroy; override;
		Property Items[Index : Integer] : TMap
		read GetValue write SetValue;default;


		Procedure Add(const AMap : TMap);
    Procedure Insert(const AMap : TMap; Index : Integer);
    Procedure Delete(Index : Integer);
    Procedure Clear();
    Function IndexOf(const MapName : String) : Integer;

		Property Count : Integer
		read MsCount;
	end;
//------------------------------------------------------------------------------


implementation

const
	ALLOCATE_SIZE = 700; // How many Maps to store in each incremental memory block

//------------------------------------------------------------------------------
//Create                                                            CONSTRUCTOR
//------------------------------------------------------------------------------
//  What it does -
//      Initializes our Maplist. Creates a storage area.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
constructor TMapList.Create(OwnsMaps : Boolean);
begin
  inherited Create;
	MsCount  := 0; // No Maps in the list yet
  MaxCount := 0; //no mem yet!
	MemStart := NIL;//no memory yet
  self.OwnsMaps := OwnsMaps;
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
destructor TMapList.Destroy;
var
  Index : Integer;
begin
  //if we own the Maps, free all of them in the list.
  if OwnsMaps then
  begin
    for Index := 0 to MsCount - 1 do
    begin
      Items[Index].Free;
    end;
  end;

	// Free the allocated memory
	FreeMem(MemStart);

	// Call TObject destructor
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
procedure TMapList.Expand(const Size : Integer);
var
	NewMemoryStart : Pointer;
	OldPointer, NewPointer : PMap;
	Index : Integer;
begin
	// First allocate a new, bigger memory space
	GetMem(NewMemoryStart, (MaxCount + Size) * SizeOf(TMap));
	if(Assigned(MemStart)) then
	begin
	  // Copy the data from the old memory here
	  OldPointer := MemStart;
	  NewPointer := NewMemoryStart;
	  for Index := 1 to MaxCount do
	  begin
		  // Copy one Map at a time
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
end;{Expand}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Shrink                                                             PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Decreases the memory area size by Size PMap.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TMapList.Shrink(const Size: Integer);
var
	NewMemoryStart : Pointer;
	OldPointer, NewPointer : PMap;
	Index : Integer;
begin
  if MaxCount > Size then
  begin
    //first allocate a new, smaller memory space
    GetMem(NewMemoryStart, (MaxCount - Size) * SizeOf(TMap));
    if(Assigned(MemStart)) then
	  begin
	    // Copy the data from the old memory here
	    OldPointer := MemStart;
	    NewPointer := NewMemoryStart;
	    for Index := 1 to MaxCount do
	    begin
		    // Copy one Map at a time
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
end;{Shrink}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Add                                                                 PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Adds a TMap to the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TMapList.Add(const AMap : TMap);
begin
	// If we do not have enough space to add the Map, then get more space!
	if MsCount = MaxCount then
	begin
		Expand(ALLOCATE_SIZE);
	end;

	// Now we can safely add the Map to the list
	NextSlot^ := AMap;

	// And update things to suit
	Inc(MsCount);
	Inc(NextSlot);
end;{Add}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Insert                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Inserts a TMap at Index Position.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TMapList.Insert(const AMap : TMap; Index: Integer);
var
  CurrentMap  : PMap;
  NextMap     : PMap;
  TempMap     : TMap;
  TempMap2    : TMap;
begin
  if MaxCount = (MsCount) then
  begin
    Expand(ALLOCATE_SIZE);
  end;

  CurrentMap := MemStart;
  Inc(CurrentMap, Index);
  NextMap := CurrentMap;
  Inc(NextMap, 1);
  TempMap := AMap;
  for Index := Index to MsCount - 1 do
  begin
    TempMap2    := @NextMap;
    NextMap^    := @CurrentMap;
    CurrentMap^ := TempMap;
    TempMap     := TempMap2;
    Inc(CurrentMap, 1);
    Inc(NextMap, 1);
  end;
  CurrentMap^ := TempMap;

  Inc(MsCount,  1);
  Inc(NextSlot, 1);
end;{Insert}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Delete                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Removes a TMap at Index from the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TMapList.Delete(Index : Integer);
var
  CurrentItem : PMap;
  NextItem    : PMap;
begin
	//if we own the Map, free it.
  if OwnsMaps then
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
end;{Delete}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//IndexOf                                                              FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Returns the index in the list of the TMap;
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
function TMapList.IndexOf(const MapName : String): Integer;
var
  Index : Integer;
begin
  Index := MsCount-1;
  Result := -1;
  while (Index >= 0) do
  begin
    if MapName = Items[Index].Name then
    begin
      Result := Index;
      Exit;
    end;
    dec(Index,  1);
  end;
end;{IndexOf}
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
procedure TMapList.Clear;
var
  Index : Integer;
begin

  //if we own the Maps, the free them.
  if OwnsMaps then
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
    MsCount  := 0;  // No Maps in the list yet
    MaxCount := 0;  //no max size
    MemStart := NIL;//no memory yet
  end

end;{Clear}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetValue                                                             FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Returns a TMap at the index.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
function TMapList.GetValue(Index : Integer): TMap;
var
	MapPtr : PMap;
begin
	// Simply get the value at the given TMap index position
	MapPtr := MemStart;
	Inc(MapPtr, Index); // Point to the index'th TMap in storage

	Result := MapPtr^; // And get the TMap it points to
end;{GetValue}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetValue                                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sets a TMap into the list at Index.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TMapList.SetValue(Index : Integer; Value : TMap);
var
	MapPtr : PMap;
begin
	// Simply set the value at the given TMap index position
	MapPtr := MemStart;
	Inc(MapPtr, Index); // Point to the index'th TMap in storage
  MapPtr^ := Value;
end;{SetValue}
//------------------------------------------------------------------------------
end.

