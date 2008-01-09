//------------------------------------------------------------------------------
//PointList                                                            UNIT
//------------------------------------------------------------------------------
//  What it does -
//      A list of TPoints
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
unit PointList;

interface
uses
	SyncObjs,
	Types,
	Classes;

type
	PPoint = ^TPoint;

//------------------------------------------------------------------------------
//TPointList                                                          CLASS
//------------------------------------------------------------------------------
	TPointList = Class(TObject)

	Private
		MsCount  : Integer; // Count of Points in the list
		MaxCount : Integer; // Maximum Points that can fit into current storage
		MemStart : Pointer; // Start of the memory holding the list
		NextSlot : PPoint; // Points to the next free slot in memory

		Function GetValue(Index : Integer) : TPoint;
    Procedure SetValue(Index : Integer; Value : TPoint);
		Procedure Expand(const Size : Integer);
    Procedure Shrink(const Size : Integer);

	Public
		Constructor Create();
		Destructor Destroy; override;
		Property Items[Index : Integer] : TPoint
		read GetValue write SetValue;default;


		Procedure Add(const APoint : TPoint);
		Procedure Delete(Index : Integer);
		Procedure Clear();

		Procedure Assign(APointList : TPointList);

		Property Count : Integer
		read MsCount;
	end;
//------------------------------------------------------------------------------


implementation

const
	ALLOCATE_SIZE = 20; // How many Points to store in each incremental memory block

//------------------------------------------------------------------------------
//Create                                                            CONSTRUCTOR
//------------------------------------------------------------------------------
//  What it does -
//      Initializes our Pointlist. Creates a storage area.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
constructor TPointList.Create();
begin
	inherited Create;
	MsCount  := 0; // No Points in the list yet
  MaxCount := 0; //no mem yet!
	MemStart := NIL;//no memory yet
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
destructor TPointList.Destroy;
begin
	// Free the allocated memory
	FreeMem(MemStart);
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
procedure TPointList.Expand(const Size : Integer);
var
	NewMemoryStart : Pointer;
	OldPointer, NewPointer : PPoint;
	Index : Integer;
begin
	// First allocate a new, bigger memory space
	GetMem(NewMemoryStart, (MaxCount + Size) * SizeOf(TPoint));
	if(Assigned(MemStart)) then
	begin
		// Copy the data from the old memory here
		OldPointer := MemStart;
		NewPointer := NewMemoryStart;
		for Index := 1 to MaxCount do
		begin
			// Copy one Point at a time
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
//      Decreases the memory area size by Size PPoint.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TPointList.Shrink(const Size: Integer);
var
	NewMemoryStart : Pointer;
	OldPointer, NewPointer : PPoint;
	Index : Integer;
begin
	if MaxCount > Size then
	begin
		//first allocate a new, smaller memory space
		GetMem(NewMemoryStart, (MaxCount - Size) * SizeOf(TPoint));
		if(Assigned(MemStart)) then
		begin
			// Copy the data from the old memory here
			OldPointer := MemStart;
			NewPointer := NewMemoryStart;
			for Index := 1 to MaxCount do
			begin
				// Copy one Point at a time
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
//      Adds a TPoint to the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TPointList.Add(const APoint : TPoint);
begin
	// If we do not have enough space to add the Point, then get more space!
	if MsCount = MaxCount then
	begin
		Expand(ALLOCATE_SIZE);
	end;

	// Now we can safely add the Point to the list
	NextSlot^ := APoint;

	// And update things to suit
	Inc(MsCount);
	Inc(NextSlot);
end;{Add}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Assign                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Assigns the point list another point lists items.
//
//  Changes -
//    March 12th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure TPointList.Assign(APointList : TPointList);
var
	Index : Integer;
begin
	Self.Clear;
	for Index := 0 to APointList.Count - 1 do
	begin
		Self.Add(APointList[Index]);
	end;
end;{Assign}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Delete                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Removes a TPoint at Index from the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TPointList.Delete(Index : Integer);
var
	CurrentItem : PPoint;
	NextItem    : PPoint;
begin

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
//Clear                                                               PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Clears the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TPointList.Clear;
begin

	// Free the allocated memory
	if Assigned(MemStart) then
	begin
		FreeMem(MemStart);
		MsCount  := 0;  // No Points in the list yet
		MaxCount := 0;  //no max size
		MemStart := NIL;//no memory yet
	end;
end;{Clear}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetValue                                                             FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Returns a TPoint at the index.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
function TPointList.GetValue(Index : Integer): TPoint;
var
	PointPtr : PPoint;
begin
	// Simply get the value at the given TPoint index position
	PointPtr := MemStart;
	Inc(PointPtr, Index); // Point to the index'th TPoint in storage

	Result := PointPtr^; // And get the TPoint it points to
end;{GetValue}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetValue                                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sets a TPoint into the list at Index.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TPointList.SetValue(Index : Integer; Value : TPoint);
var
	PointPtr : PPoint;
begin
	// Simply set the value at the given TPoint index position
	PointPtr := MemStart;
	Inc(PointPtr, Index); // Point to the index'th TPoint in storage
	PointPtr^ := Value;
end;{SetValue}
//------------------------------------------------------------------------------
end.
