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
	PointList,
  List32,
  MapTypes;

type
//------------------------------------------------------------------------------
//TMap                                                                  CLASS
//------------------------------------------------------------------------------
  TMap = class(TObject)
	private
    Procedure GetArea(
      const APoint : TPoint;
      var AnArea : TGraph;
      var XMod : Integer;
      var YMod : Integer
    );

	public
    Name : String;
		Cell : TGraph;
    Size : TPoint;
    Flags: TFlags;

		Constructor Create();
		Destructor Destroy();override;

    Function LoadFromFile(Path : String) : Boolean;

		Function GetPath(
			const StartPoint  : TPoint;
			const EndPoint    : TPoint;
			var APath         : TPointList
		) : boolean;

end;
//------------------------------------------------------------------------------

implementation
uses
  GameConstants,
  Classes,
  SysUtils,
  Math,
  Console,
  Globals;

//checks to see if a cell is blocked.
	function IsBlocked(
		const AGraph : TGraph;
		const APoint : TPoint
	) : boolean; Forward;

const
	//this array holds the possible directions from a point.
  //shouldn't be needed anywhere else, and shouldn't need to be changed.
  //therefore, it's here instead of in GameConstants =)
	Directions : array[0..7] of TPoint = (
		(X:0;Y:1),(X:1;Y:1),(X:1;Y:0),(X:1;Y:-1),
		(X:0;Y:-1),(X:-1;Y:-1),(X:-1;Y:0),(X:-1;Y:1)
	);

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

  //Set Size to 0
	Size.X := 0;
  Size.Y := 0;
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
var
  XIndex : Integer;
  YIndex : Integer;
begin

  //Free up each Graph Cell.
  for XIndex := 0 to Size.X - 1 do
  begin
    for YIndex := 0 to Size.Y - 1 do
    begin
      Cell[XIndex][YIndex].Mobs.Free;
      Cell[XIndex][YIndex].Characters.Free;
      Cell[XIndex][YIndex].NPCs.Free;
    end;
    SetLength(Cell[XIndex], 0);
  end;
	inherited;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMap.GetArea()                                                     PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      It fills the TGraph, AnArea, with the contents of TMap.Graph based on
//		the distance defined by CHAR_CLICKAREA away from the character in the 4
//		cardinal directions.
//
//  Changes -
//    November 1st, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TMap.GetArea(const APoint : TPoint; var AnArea : TGraph; var XMod : Integer; var YMod : Integer);
var
	Index			: Integer;
	XMax			: Integer;//last point in x axis on map
	YMax			: Integer;//last point in y axis on map
	XAreaMax	: Integer;//last x point in area
	YAreaMax	: Integer;//last y point in area
begin
	XMax := Length(Cell)-1;
	YMax := Length(Cell[0])-1;

	//first, we get our areas position(really nasty condition follows)
	//get XMod and XAreaMax
	if (APoint.X-CHAR_CLICKAREA >= 0) AND (APoint.X+CHAR_CLICKAREA <= XMax) then
	begin
		XMod := APoint.X - CHAR_CLICKAREA;
		XAreaMax := APoint.X + CHAR_CLICKAREA;
	end else
	if (APoint.X - CHAR_CLICKAREA < 0) AND (APoint.X + CHAR_CLICKAREA <= XMax) then
	begin
		XMod := 0;
		XAreaMax := APoint.X + CHAR_CLICKAREA;
	end else
	if (APoint.X - CHAR_CLICKAREA >= 0) AND (APoint.X + CHAR_CLICKAREA > XMax) then
	begin
		XMod := APoint.X - CHAR_CLICKAREA;
		XAreaMax := XMax;
	end else
	begin
		XMod := 0;
		XAreaMax := XMax;
	end;

	//Get YMod and YAreaMax
		if (APoint.Y-CHAR_CLICKAREA >= 0) AND (APoint.Y+CHAR_CLICKAREA <= YMax) then
	begin
		YMod := APoint.Y - CHAR_CLICKAREA;
		YAreaMax := APoint.Y + CHAR_CLICKAREA;
	end else
	if (APoint.Y - CHAR_CLICKAREA < 0) AND (APoint.Y + CHAR_CLICKAREA <= YMax) then
	begin
		YMod := 0;
		YAreaMax := APoint.Y + CHAR_CLICKAREA;
	end else
	if (APoint.Y - CHAR_CLICKAREA >= 0) AND (APoint.Y + CHAR_CLICKAREA > YMax) then
	begin
		YMod := APoint.Y - CHAR_CLICKAREA;
		YAreaMax := YMax;
	end else
	begin
		YMod := 0;
		YAreaMax := YMax;
	end;

	//finally, build our area
	AnArea := copy(Cell,XMod,XAreaMax);
	for Index := XMod to (XAreaMax) do
	begin
		AnArea[Index-XMod] := copy(Cell[Index], YMod, YAreaMax);
	end;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMap.GetPath()                                                      FUNCTION
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
//    November 1st, 2006 - Tsusai - moved the check for passability into
//     IsBlocked. Changed to a boolean function and our path is assigned via a
//     TPointList parameter passed by address, APath.
//------------------------------------------------------------------------------
function TMap.GetPath(
	const StartPoint  : TPoint;
	const EndPoint    : TPoint;
	var APath : TPointList
) : boolean;
var
	AFloodList				: TFloodList;
	AFloodListLength	: Integer;
  NewFloodList			: TFloodList;
	NewFloodListLength: Integer;
	AFloodItem				: TFloodItem;
	ClosestPath				: TFloodItem;
	NewFloodItem			: TFloodItem;
	APoint						: TPoint;
	Index							: Integer;
	DirectionIndex		: Integer;
	PossiblePosition  : TPoint;
  AnArea						: TGraph;//our search area
	XMod							: Integer;//x offset in AnArea from the point in TMap.Graph
	YMod							: Integer;//y offset in AnArea from the point in TMap.Graph
	XMax							: Integer;//maximum y value in our area
	YMax							: Integer;//maximum x value in our area

begin
	Result := false;
	//check to make sure we're wasting clock cycles for a reason...
	//(checking to see if the endpoint is within range);
	if ((StartPoint.X + CHAR_CLICKAREA) < EndPoint.X) OR
		 ((StartPoint.X - CHAR_CLICKAREA) > EndPoint.X) OR
		 ((StartPoint.Y + CHAR_CLICKAREA) < EndPoint.Y) OR
		 ((StartPoint.Y - CHAR_CLICKAREA) > EndPoint.Y) then
	begin
		Exit;
	end;


	//Get Our Area and area constraints
	GetArea(StartPoint, AnArea, XMod, YMod);
	XMax          := Length(AnArea);
	YMax          := Length(AnArea[0]);

	//initialize our first flood item
	AFloodItem.Position:= StartPoint;
	AFloodItem.PathLength := 0;

	//initialize our closest path
	ClosestPath := AFloodItem;

	//initialize our flood lists
	AFloodListLength := 1;
	SetLength(AFloodList, AFloodListLength);
	AFloodList[AFloodListLength-1] := AFloodItem;

	//while we havn't found the end point...
	while (AFloodListLength > 0) AND (NOT Result) do
	begin
		//reinitialize Index and NewFloodListLen for each AFloodList
		Index := 0;
		NewFloodListLength := 0;
		while (Index < AFloodListLength) AND (NOT Result) do
		begin
			//initialize our flooditem which we will propagate from.
			AFloodItem  := AFloodList[Index];
			//loop through each direction, create a new flood item for each.
			DirectionIndex := 0;
			while (DirectionIndex < 8) AND (NOT Result) do
			begin
				//get the expected position of our new flood item
				PossiblePosition.X := AFloodItem.Position.X + Directions[DirectionIndex].X;
				PossiblePosition.Y	:= AFloodItem.Position.Y + Directions[DirectionIndex].Y;
				//check if coordinates are in range
				if    (PossiblePosition.X >= 0) AND (PossiblePosition.X < XMax)
					AND (PossiblePosition.Y >= 0) AND (PossiblePosition.Y < YMax) then
				begin
					//check if the square is passable, if it is...
					if not IsBlocked(AnArea,PossiblePosition) then
					begin
						//build our flood item.
						NewFloodItem.Position := PossiblePosition;
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
						//set it's position impassable in our area so we don't search it anymore
						AnArea[NewFloodItem.Position.X][NewFloodItem.Position.Y].Attribute := 1;

						//check to see if we've found the end point... if we have...
						if (NewFloodItem.Position.X = EndPoint.X) AND (NewFloodItem.Position.Y = EndPoint.Y) then
						begin
							//congratulations, we've done it.
							APath.Assign(NewFloodItem.Path);
							Result  := TRUE;
							Exit;
						end else
						if (abs(NewFloodItem.Position.X-EndPoint.X)+abs(NewFloodItem.Position.Y-EndPoint.Y)) <
							 (abs(ClosestPath.Position.X-EndPoint.X)+abs(ClosestPath.Position.Y-EndPoint.Y)) then
						begin
							ClosestPath := NewFloodItem;
            end;
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
	//use closest path
	if Result = FALSE then
	begin
		APath.Assign(ClosestPath.Path);
  end;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//IsBlocked()                                                     FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      It figures out if a specified cell in a TGraph is blocked.
//
//  Changes -
//    November 1st, 2006 - Tsusai - Created.
//------------------------------------------------------------------------------
function IsBlocked(const AGraph : TGraph; const APoint : TPoint) : boolean;
begin
	//Assume it is not.
	Result := false;
	if (AGraph[APoint.X][APoint.Y].Attribute in [1,5]) OR
		(AGraph[APoint.X][APoint.Y].ObstructionCount > 0 ) then
	begin
		Result := true
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadFromFile()                                                     FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Loads a map from a .pms file.
//
//  Changes -
//    January 22nd, 2007 - RaX - Created.
//------------------------------------------------------------------------------
Function TMap.LoadFromFile(Path : String) : Boolean;
Var
	AByte   : byte;
	MapFile : TMemoryStream;
	MapTag  : string;
	MapSize : TPoint;
  XIndex  : Integer;
  YIndex  : Integer;
Begin
  Result := TRUE;

  MapFile := TMemoryStream.Create;
  MapFile.LoadFromFile(Path);

  SetLength(MapTag, 13);
  MapFile.Read(MapTag[1], 13);

  if MapTag <> 'PrometheusMap' then //Check type
  begin
    MainProc.Console('The Map :'+Path+' is not a Prometheus Map.');
    Result := False;
  end;

  MapFile.Read(Abyte,1); //Check version.

  if AByte <> 1 then
  begin
    MainProc.Console('The Map :'+Path+' failed the version check.');
    Result := False;
  end;


  MapFile.Read(MapSize.X, 4);
  MapFile.Read(MapSize.Y, 4);

  //check size.
  if NOT (InRange(MapSize.X, 0, 511) AND InRange(MapSize.Y, 0, 511)) then
  begin
    MainProc.Console('The Map :'+Path+'''s size is out of range.');
    Result := False;
  end;

  if Result then //If we've passed the checks then...
  begin
    //Load Map Information.
    Name := ExtractFileName(LowerCase(ChangeFileExt(Path, '')));
    Size := MapSize;
    Flags:= ADatabase.StaticData.GetMapFlags(Name);

    //Load Cell Information
    SetLength(Cell, Size.X, Size.Y);
    for YIndex := 0 to Size.Y - 1 do begin
      for XIndex := 0 to Size.X - 1 do begin
        MapFile.Read(Cell[XIndex][YIndex].Attribute,1);
      end;
    end;

  end;

  MapFile.Free;//finally, free the memory stream.
End;//LoadFromFile
//------------------------------------------------------------------------------

end.
