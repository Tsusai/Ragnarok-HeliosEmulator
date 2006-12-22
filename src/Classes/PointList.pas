//------------------------------------------------------------------------------
//PointList                                                               UNIT
//------------------------------------------------------------------------------
//  What it does -
//      A list of TPoints
//
//  Changes -
//    October 30th, 2006 - RaX - Created.
//------------------------------------------------------------------------------
unit PointList;

interface
uses
  Types;

type
	PPoint = ^TPoint;

//------------------------------------------------------------------------------
//TPointList                                                              CLASS
//------------------------------------------------------------------------------
	TPointList = class

	private
		MsCount : Integer; // Count of Points in the list
		MaxCount : Integer; // Maximum Points that can fit into current storage
		MemStart : Pointer; // Start of the memory holding the list
		NextSlot : PPoint; // Points to the next free slot in memory

		function GetValue(Index : Integer) : TPoint;
    procedure SetValue(Index : Integer; Value : TPoint);
		procedure Expand(const Size : Integer);
    procedure Shrink(const Size : Integer);

	public
		property Items[Index : Integer] : TPoint
		read GetValue write SetValue;default;
    procedure Assign(AnArray : array of TPoint);overload;

	published
		constructor Create;
		destructor Destroy; override;
		procedure Add(const APoint : TPoint);
    procedure Insert(const APoint : TPoint; Index : Integer);
    procedure Delete(Index : Integer);
    procedure Clear();
    function IndexOf(const APoint : TPoint) : Integer;
    procedure Assign(APointList : TPointList);overload;
		property Count : Integer
		read MsCount;
	end;
//------------------------------------------------------------------------------


implementation
uses
  Classes;
const
	ALLOCATE_SIZE = 20; // How many points to store in each incremental memory block

//------------------------------------------------------------------------------
//Create                                                            CONSTRUCTOR
//------------------------------------------------------------------------------
//  What it does -
//      Initializes our pointlist. Creates a storage area.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created Header.
//------------------------------------------------------------------------------
constructor TPointList.Create;
begin
	MsCount  := 0; // No numbers in the list yet
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
//    December 22nd, 2006 - RaX - Created Header.
//------------------------------------------------------------------------------
destructor TPointList.Destroy;
begin
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
//      Increases the memory area size by Size TPoints.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created Header.
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
		  // Copy one number at a time
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
//      Decreases the memory area size by Size TPoints.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created Header.
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
		    // Copy one number at a time
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
//    December 22nd, 2006 - RaX - Created Header.
//------------------------------------------------------------------------------
procedure TPointList.Add(const APoint : TPoint);
begin
	// If we do not have enough space to add the number, then get more space!
	if MsCount = MaxCount then
	begin
		Expand(ALLOCATE_SIZE);
	end;

	// Now we can safely add the number to the list
	NextSlot^ := APoint;

	// And update things to suit
	Inc(MsCount);
	Inc(NextSlot);
end;{Add}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Insert                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Inserts a TPoint at Index Position.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created Header.
//------------------------------------------------------------------------------
procedure TPointList.Insert(const APoint : TPoint; Index: Integer);
var
  CurrentPoint  : PPoint;
  NextPoint     : PPoint;
  TempPoint     : TPoint;
  TempPoint2    : TPoint;
begin
  if MaxCount = (MsCount+1) then
  begin
    Expand(ALLOCATE_SIZE);
  end;

  CurrentPoint := MemStart;
  Inc(CurrentPoint, Index);
  NextPoint := CurrentPoint;
  Inc(NextPoint, 1);
  TempPoint := APoint;
  for Index := Index to MsCount - 1 do
  begin
    TempPoint2 := NextPoint^;
    NextPoint^ := CurrentPoint^;
    CurrentPoint^ := TempPoint;
    TempPoint := TempPoint2;
    Inc(CurrentPoint, 1);
    Inc(NextPoint, 1);
  end;
  CurrentPoint^ := TempPoint;

  Inc(MsCount);
  Inc(NextSlot);
end;{Insert}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Delete                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Removes a TPoint at Index from the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created Header.
//------------------------------------------------------------------------------
procedure TPointList.Delete(Index : Integer);
var
  CurrentItem : PPoint;
  NextItem    : PPoint;
begin
	//
	if (MaxCount-ALLOCATE_SIZE) = (MsCount-1) then
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
  Dec(NextSlot, 1);
end;{Delete}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//IndexOf                                                              FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Returns the index in the list of the TPoint;
//
//  Changes -
//    December 22nd, 2006 - RaX - Created Header.
//------------------------------------------------------------------------------
function TPointList.IndexOf(const APoint: TPoint): Integer;
var
  Found : Boolean;
  Index : Integer;
begin
  Index := MsCount;
  Found := FALSE;
  Result := -1;
  while (Index >= 0) AND (NOT Found) do
  begin
    if PointsEqual(GetValue(Index),APoint) then
    begin
      Found := TRUE;
      Result := Index;
    end;
    dec(Index);
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
//    December 22nd, 2006 - RaX - Created Header.
//------------------------------------------------------------------------------
procedure TPointList.Clear;
begin
  // Free the allocated memory
  if Assigned(MemStart) then
  begin
    FreeMem(MemStart);
    MsCount  := 0; // No numbers in the list yet
    MaxCount := 0; //no max size
    MemStart := NIL;//no memory yet
  end
end;{Clear}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Assign                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Assign the list the same values as another list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created Header.
//------------------------------------------------------------------------------
procedure TPointList.Assign(APointList : TPointList);
var
  Index : Integer;
begin
  Clear;
  for Index := 0 to APointList.Count-1 do
  begin
    Add(APointList[Index]);
  end;
end;{Assign}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Assign                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Adds all the items in an array to the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created Header.
//------------------------------------------------------------------------------
procedure TPointList.Assign(AnArray : array of TPoint);
var
  Index : Integer;
begin
  Clear;
  for Index := 0 to Length(AnArray)-1 do
  begin
    Add(AnArray[Index]);
  end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetValue                                                             FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Returns a TPoint (not a pointer) at the index.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created Header.
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
//    December 22nd, 2006 - RaX - Created Header.
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

