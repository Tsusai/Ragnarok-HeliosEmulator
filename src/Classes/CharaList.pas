//------------------------------------------------------------------------------
//CharacterList                                                            UNIT
//------------------------------------------------------------------------------
//  What it does -
//      A list of TCharacters
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
unit CharaList;

interface
uses
  Character;

type
	PCharacter = ^TCharacter;

//------------------------------------------------------------------------------
//TCharacterList                                                          CLASS
//------------------------------------------------------------------------------
	TCharacterList = Class(TObject)

	Private
		MsCount : Integer; // Count of Characters in the list
		MaxCount : Integer; // Maximum Characters that can fit into current storage
		MemStart : Pointer; // Start of the memory holding the list
		NextSlot : PCharacter; // Points to the next free slot in memory

    OwnsCharacters : Boolean;//If we own the characters, we handle free'ing them.

		Function GetValue(Index : Integer) : TCharacter;
    Procedure SetValue(Index : Integer; Value : TCharacter);
		Procedure Expand(const Size : Integer);
    Procedure Shrink(const Size : Integer);

	Public
    Constructor Create(OwnsCharacters : Boolean);
		Destructor Destroy; override;
		Property Items[Index : Integer] : TCharacter
		read GetValue write SetValue;default;


		Procedure Add(const ACharacter : TCharacter);
    Procedure Insert(const ACharacter : TCharacter; Index : Integer);
    Procedure Delete(Index : Integer);
    Procedure Clear();
    Function IndexOf(const CID : Cardinal) : Integer;
    Function IndexOfAID(const AID : Cardinal) : Integer;

		Property Count : Integer
		read MsCount;
	end;
//------------------------------------------------------------------------------


implementation

const
	ALLOCATE_SIZE = 10; // How many characters to store in each incremental memory block

//------------------------------------------------------------------------------
//Create                                                            CONSTRUCTOR
//------------------------------------------------------------------------------
//  What it does -
//      Initializes our characterlist. Creates a storage area.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
constructor TCharacterList.Create(OwnsCharacters : Boolean);
begin
  inherited Create;
	MsCount  := 0; // No characters in the list yet
  MaxCount := 0; //no mem yet!
	MemStart := NIL;//no memory yet
  self.OwnsCharacters := OwnsCharacters;
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
destructor TCharacterList.Destroy;
var
  Index : Integer;
begin
  //if we own the characters, free all of them in the list.
  if OwnsCharacters then
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
procedure TCharacterList.Expand(const Size : Integer);
var
	NewMemoryStart : Pointer;
	OldPointer, NewPointer : PCharacter;
	Index : Integer;
begin
	// First allocate a new, bigger memory space
	GetMem(NewMemoryStart, (MaxCount + Size) * SizeOf(TCharacter));
	if(Assigned(MemStart)) then
	begin
	  // Copy the data from the old memory here
	  OldPointer := MemStart;
	  NewPointer := NewMemoryStart;
	  for Index := 1 to MaxCount do
	  begin
		  // Copy one character at a time
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
//      Decreases the memory area size by Size PCharacter.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TCharacterList.Shrink(const Size: Integer);
var
	NewMemoryStart : Pointer;
	OldPointer, NewPointer : PCharacter;
	Index : Integer;
begin
  if MaxCount > Size then
  begin
    //first allocate a new, smaller memory space
    GetMem(NewMemoryStart, (MaxCount - Size) * SizeOf(TCharacter));
    if(Assigned(MemStart)) then
	  begin
	    // Copy the data from the old memory here
	    OldPointer := MemStart;
	    NewPointer := NewMemoryStart;
	    for Index := 1 to MaxCount do
	    begin
		    // Copy one character at a time
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
//      Adds a TCharacter to the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TCharacterList.Add(const ACharacter : TCharacter);
begin
	// If we do not have enough space to add the character, then get more space!
	if MsCount = MaxCount then
	begin
		Expand(ALLOCATE_SIZE);
	end;

	// Now we can safely add the character to the list
	NextSlot^ := ACharacter;

	// And update things to suit
	Inc(MsCount);
	Inc(NextSlot);
end;{Add}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Insert                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Inserts a TCharacter at Index Position.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TCharacterList.Insert(const ACharacter : TCharacter; Index: Integer);
var
  CurrentCharacter  : PCharacter;
  NextCharacter     : PCharacter;
  TempCharacter     : TCharacter;
  TempCharacter2    : TCharacter;
begin
  if MaxCount = (MsCount) then
  begin
    Expand(ALLOCATE_SIZE);
  end;

  CurrentCharacter := MemStart;
  Inc(CurrentCharacter, Index);
  NextCharacter := CurrentCharacter;
  Inc(NextCharacter, 1);
  TempCharacter := ACharacter;
  for Index := Index to MsCount - 1 do
  begin
    TempCharacter2    := @NextCharacter;
    NextCharacter^    := @CurrentCharacter;
    CurrentCharacter^ := TempCharacter;
    TempCharacter     := TempCharacter2;
    Inc(CurrentCharacter, 1);
    Inc(NextCharacter, 1);
  end;
  CurrentCharacter^ := TempCharacter;

  Inc(MsCount,  1);
  Inc(NextSlot, 1);
end;{Insert}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Delete                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Removes a TCharacter at Index from the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TCharacterList.Delete(Index : Integer);
var
  CurrentItem : PCharacter;
  NextItem    : PCharacter;
begin
	//if we own the Character, free it.
  if OwnsCharacters then
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
//      Returns the index in the list of the TCharacter;
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
function TCharacterList.IndexOf(const CID: Cardinal): Integer;
var
  Index : Integer;
begin
  Index := MsCount-1;
  Result := -1;
  while (Index >= 0) do
  begin
    if CID = Items[Index].CID then
    begin
      Result := Index;
      Exit;
    end;
    dec(Index,  1);
  end;
end;{IndexOf}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//IndexOfAID                                                           FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Returns the index in the list of the TCharacter;
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
function TCharacterList.IndexOfAID(const AID: Cardinal): Integer;
var
  Index : Integer;
begin
  Index := MsCount-1;
  Result := -1;
  while (Index >= 0) do
  begin
    if AID = Items[Index].Account.ID then
    begin
      Result := Index;
      Exit;
    end;
    dec(Index,  1);
  end;
end;{IndexOfAID}
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
procedure TCharacterList.Clear;
var
  Index : Integer;
begin

  //if we own the characters, the free them.
  if OwnsCharacters then
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
    MsCount  := 0;  // No characters in the list yet
    MaxCount := 0;  //no max size
    MemStart := NIL;//no memory yet
  end

end;{Clear}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetValue                                                             FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Returns a TCharacter at the index.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
function TCharacterList.GetValue(Index : Integer): TCharacter;
var
	CharacterPtr : PCharacter;
begin
	// Simply get the value at the given TCharacter index position
	CharacterPtr := MemStart;
	Inc(CharacterPtr, Index); // Point to the index'th TCharacter in storage

	Result := CharacterPtr^; // And get the TCharacter it points to
end;{GetValue}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetValue                                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sets a TCharacter into the list at Index.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TCharacterList.SetValue(Index : Integer; Value : TCharacter);
var
	CharacterPtr : PCharacter;
begin
	// Simply set the value at the given TCharacter index position
	CharacterPtr := MemStart;
	Inc(CharacterPtr, Index); // Point to the index'th TCharacter in storage
  CharacterPtr^ := Value;
end;{SetValue}
//------------------------------------------------------------------------------
end.

