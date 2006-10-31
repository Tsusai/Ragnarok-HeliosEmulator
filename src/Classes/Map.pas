//------------------------------------------------------------------------------
//Map                                                                    UNIT
//------------------------------------------------------------------------------
//  What it does -
//      Our Map class, holds everything to do with our map object.
//
//  Changes -
//    October 30th, 2006 - RaX - Created.
//------------------------------------------------------------------------------
unit Map;

interface

uses
	Types,
  PointList;

type
  //graph related types
  TCell = record
    Attribute : Byte;
  end;

  TGraph = array of array of TCell;

  //flood types
	TFloodItem = record
		Position : TPoint;
		Path : array of TPoint;
    PathLength : Integer;
	end;

  TFloodList = array of TFloodItem;

//------------------------------------------------------------------------------
//TMap                                                                  CLASS
//------------------------------------------------------------------------------
  TMap = class(TObject)
  public
    Graph : TGraph;
  private

  protected

  published
    Constructor Create();
    Destructor Destroy();override;
    Function GetPath(StartPoint : TPoint; EndPoint : TPoint) : TPointList;
  end;
//------------------------------------------------------------------------------

implementation

const
  CHAR_CLICKAREA = 16;//the distance away a character can click from itself
  //this array holds the possible directions from a point.
  Directions : array[0..7] of TPoint = ((X:0;Y:1),(X:1;Y:1),(X:1;Y:0),(X:1;Y:-1),(X:0;Y:-1),(X:-1;Y:-1),(X:-1;Y:0),(X:-1;Y:1));

//------------------------------------------------------------------------------
//TMap.Create()
//------------------------------------------------------------------------------
//  What it does -
//      Builds our TMap Object.
//
//  Changes -
//    October 30th, 2006 - RaX - Created.
//------------------------------------------------------------------------------
Constructor TMap.Create();
begin
  inherited;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMap.Destroy()
//------------------------------------------------------------------------------
//  What it does -
//      Destroys our TMap Object.
//
//  Changes -
//    October 30th, 2006 - RaX - Created.
//------------------------------------------------------------------------------
Destructor TMap.Destroy();
begin
  inherited;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMap.GetPath()
//------------------------------------------------------------------------------
//  What it does -
//      This is a pathing algorithm that I like to call a "wave" algorithm
//    because of the way it propagates out from the starting point much like the
//    behavior of a wave. It finds paths by searching all possible paths to a
//    certain extent and using the one that arrives at the target first, the
//    shortest path.
//
//  Changes -
//    October 30th, 2006 - RaX - Created.
//------------------------------------------------------------------------------
function TMap.GetPath(StartPoint : TPoint; EndPoint : TPoint) : TPointList;
var
	Done		          : Boolean;
  AnArea            : TGraph;
	APath		          : TPointList;
	AFloodList	      : TFloodList;
  AFloodListLength  : Integer;
	AFloodItem	      : TFloodItem;
	NewFloodItem	    : TFloodItem;
  APoint            : TPoint;
	NewFloodList	    : TFloodList;
  NewFloodListLength: Integer;
	Index		          : Integer;
	Directions	      : array of TPoint;
	DirectionIndex    : Integer;
  PossiblePos       : TPoint;
  XMax              : Integer;//maximum y value in our area
  YMax              : Integer;//maximum x value in our area

begin
  //check to make sure we're wasting clock cycles for a reason...
  //(checking to see if the endpoint is within range);
  if ((StartPoint.X + CHAR_CLICKAREA) <= EndPoint.X) OR
     ((StartPoint.X - CHAR_CLICKAREA) > EndPoint.X) OR
     ((StartPoint.Y + CHAR_CLICKAREA) <= EndPoint.Y) OR
     ((StartPoint.Y - CHAR_CLICKAREA) > EndPoint.Y) then
  begin
    Result := TPointList.Create;
    Exit;
  end;

  //initialize variables
	Done		      := FALSE;
  APath         := TPointList.Create;

  //copy our map's graph
  AnArea := copy(Graph);
  for Index := 0 to Length(Graph) - 1 do
  begin
    AnArea[Index] := copy(Graph[Index]);
  end;
  XMax          := Length(AnArea);
  YMax          := Length(AnArea[0]);

  //initialize our first flood item
	AFloodItem.Position:= StartPoint;
  AFloodItem.PathLength := 0;

  //initialize our flood lists
  AFloodListLength := 1;
  SetLength(AFloodList, AFloodListLength);
  AFloodList[AFloodListLength-1] := AFloodItem;

	//while we havn't found the end point...
	while (AFloodListLength > 0) AND (NOT Done) do
	begin
    //reinitialize Index and NewFloodListLen for each AFloodList
    Index := 0;
    NewFloodListLength := 0;
		while (Index < AFloodListLength) AND (NOT Done) do
		begin
			//initialize our flooditem which we will propagate from.
			AFloodItem  := AFloodList[Index];
			//loop through each direction, create a new flood item for each.
      DirectionIndex := 0;
      while (DirectionIndex < 8) AND (NOT Done) do
      begin
        //get the expected position of our new flood item
        PossiblePos.X := AFloodItem.Position.X + Directions[DirectionIndex].X;
        PossiblePos.Y	:= AFloodItem.Position.Y + Directions[DirectionIndex].Y;
        //check if coordinates are in range
        if    (PossiblePos.X >= 0) AND (PossiblePos.X < XMax)
          AND (PossiblePos.Y >= 0) AND (PossiblePos.Y < YMax)
          AND (PossiblePos.X <= StartPoint.X+CHAR_CLICKAREA) AND (PossiblePos.X >= StartPoint.X-CHAR_CLICKAREA)
          AND (PossiblePos.Y <= StartPoint.Y+CHAR_CLICKAREA) AND (PossiblePos.Y >= StartPoint.Y-CHAR_CLICKAREA) then
        begin
          //check if the square is passable, if it is...
          if (AnArea[PossiblePos.X][PossiblePos.Y].Attribute <> 1) AND
             (AnArea[PossiblePos.X][PossiblePos.Y].Attribute <> 5) then
          begin
            //build our flood item.
            NewFloodItem.Position := PossiblePos;
            NewFloodItem.Path := AFloodItem.Path;
            NewFloodItem.PathLength := AFloodItem.PathLength;
            APoint.X := NewFloodItem.Position.X;
            APoint.Y := NewFloodItem.Position.Y;
            inc(NewFloodItem.PathLength);
            SetLength(NewFloodItem.Path, NewFloodItem.PathLength);
            NewFloodItem.Path[NewFloodItem.PathLength-1] := APoint;
            //add it to the NewFloodList
            inc(NewFloodListLength);
            SetLength(NewFloodList, NewFloodListLength);
            NewFloodList[NewFloodListLength-1] := NewFloodItem;
            //set it's position impassable in our area
            AnArea[NewFloodItem.Position.X][NewFloodItem.Position.Y].Attribute := 1;

            //check to see if we've found the end point... if we have...
            if (NewFloodItem.Position.X = EndPoint.X) AND (NewFloodItem.Position.Y = EndPoint.Y) then
            begin
              //congratulations, we've done it.
              APath.Assign(NewFloodItem.Path);
              Done  := TRUE;
            end
          end;
        end;
        //start propagating our next flood item in the next direction.
        inc(DirectionIndex);
      end;
      //go to the next item in AFloodList
      inc(Index);
    end;
    //swap our arrays around to propagate the next generation.
    AFloodList := NewFloodList;
    AFloodListLength := NewFloodListLength;
	end;

  //return our discovered path
	Result := APath;
end;
//------------------------------------------------------------------------------
end.
