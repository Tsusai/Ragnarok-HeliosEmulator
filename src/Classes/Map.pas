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
	MapTypes,
	EventList;

type
//------------------------------------------------------------------------------
//TMap                                                                  CLASS
//------------------------------------------------------------------------------
  TMap = class(TObject)
	private
		Path : String;

	public
    Name : String;
		Cell : TGraph;
    Size : TPoint;
		Flags: TFlags;
		State: TMapMode;
		

		EventList : TEventList;

		Constructor Create();
		Destructor Destroy();override;

		Function IsBlocked(
			const APoint : TPoint
		) : boolean;

    Function LoadFromFile(Path : String) : Boolean;
    Procedure Load;
    Procedure Unload;

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
	Main,
	Globals,
	WinLinux;

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
	State := UNLOADED;
  //Set Size to 0
	Size.X := 0;
	Size.Y := 0;
	EventList := TEventList.Create(TRUE);
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
  if State = LOADED then
  begin
    Unload;
  end;
  EventList.Free;
	inherited;
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
	AFloodList				: TFloodList;//our main flood list which we spawn newflooditems from
	AFloodListLength	: Integer;//the length of afloodlist
	NewFloodList			: TFloodList;//Our list of new flood items.
	NewFloodListLength: Integer;//The length of the newflooditem list.
	AFloodItem				: TFloodItem;
	ClosestPath				: TFloodItem;//the point closest to the destination.
	NewFloodItem			: TFloodItem;
	APoint						: TPoint;
	Index							: Integer;
	DirectionIndex		: Integer;
	PossiblePosition  : TPoint;
	AnArea						: TGraph;
	AnAreaSize				: TPoint;
	XMod							: Integer;
	YMod							: Integer;

begin
	Result := false;

	//grab our area
	XMod := StartPoint.X-CHAR_CLICKAREA;
	YMod := StartPoint.Y-CHAR_CLICKAREA;
	AnArea := Copy(Cell, Max(XMod, 0), Min((CHAR_CLICKAREA*2)+1, Abs(XMod - Size.X)));
	for Index := 0 to Length(AnArea)-1 do
	begin
		AnArea[Index] := Copy(Cell[XMod+Index], Max(YMod, 0), Min((CHAR_CLICKAREA*2)+1, Abs(YMod - Size.Y)));
	end;
	AnAreaSize := Point(Length(AnArea),Length(AnArea[0]));

	//initialize our first flood item
	AFloodItem.Position.X := StartPoint.X-AnArea[0][0].Position.X;
	AFloodItem.Position.Y := StartPoint.Y-AnArea[0][0].Position.Y;
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
				if    (PossiblePosition.X >= 0) AND (PossiblePosition.X < AnAreaSize.X)
					AND (PossiblePosition.Y >= 0) AND (PossiblePosition.Y < AnAreaSize.Y) then
				begin
					//check if the square is passable, if it is...
					if (NOT IsBlocked(AnArea[PossiblePosition.X][PossiblePosition.Y].Position)) then
					begin
						//build our flood item.
						NewFloodItem.Position := PossiblePosition;
						NewFloodItem.Path := AFloodItem.Path;
						NewFloodItem.PathLength := AFloodItem.PathLength;
						APoint:= NewFloodItem.Position;
						inc(NewFloodItem.PathLength);
						SetLength(NewFloodItem.Path, NewFloodItem.PathLength);
						NewFloodItem.Path[NewFloodItem.PathLength-1] := AnArea[PossiblePosition.X][PossiblePosition.Y].Position;
						//add it to the NewFloodList
						inc(NewFloodListLength);
						SetLength(NewFloodList, NewFloodListLength);
						NewFloodList[NewFloodListLength-1] := NewFloodItem;

						//set it impassable in the area to prevent an unecessary amount of items being generated.
						AnArea[PossiblePosition.X][PossiblePosition.Y].Attribute := 1;

						//check to see if we've found the end point... if we have...
						if PointsEqual(AnArea[NewFloodItem.Position.X][NewFloodItem.Position.Y].Position, EndPoint) then
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
	if NOT Result then
	begin
		if Length(ClosestPath.Path) > 0 then
		begin
			Result := TRUE;
			APath.Assign(ClosestPath.Path);
		end;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//IsBlocked()                                                     FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      It figures out if a specified cell is blocked.
//
//  Changes -
//    November 1st, 2006 - Tsusai - Created.
//------------------------------------------------------------------------------
function TMap.IsBlocked(const APoint : TPoint) : boolean;
begin
	//Assume it is not.
	Result := false;
	if (Cell[APoint.X][APoint.Y].Attribute in [1,5]) OR
		(Cell[APoint.X][APoint.Y].ObstructionCount > 0 ) then
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
//		January 25th, 2007 - Tsusai - Removed complicated path name code, replaced
//			with ExtractFileNameMod (WinLinux)
//------------------------------------------------------------------------------
Function TMap.LoadFromFile(Path : String) : Boolean;
Var
	AByte   : byte;
	MapFile : TMemoryStream;
	MapTag  : array[1..13] of Char;
	MapSize : TPoint;
Begin
  Result := TRUE;

  MapFile := TMemoryStream.Create;
  MapFile.LoadFromFile(Path);
	Self.Path := Path;

  MapFile.Read(MapTag[1], 13);

  if MapTag <> 'PrometheusMap' then //Check type
  begin
		Console.WriteLn('The Map :'+Path+' is not a Prometheus Map.');
    Result := False;
  end;

  MapFile.Read(AByte,1); //Check version.

  if AByte <> 1 then
  begin
		Console.WriteLn('The Map :'+Path+' failed the version check.');
    Result := False;
  end;


  MapFile.Read(MapSize.X, 4);
  MapFile.Read(MapSize.Y, 4);

  //check size.
  if NOT (InRange(MapSize.X, 0, 511) AND InRange(MapSize.Y, 0, 511)) then
  begin
    Console.WriteLn('The Map :'+Path+'''s size is out of range.');
    Result := False;
  end;

  if Result then //If we've passed the checks then...
	begin
		//Load Map Information.
		Name := ExtractFileNameMod(Path);
		Size := MapSize;
  end;

  MapFile.Free;//finally, free the memory stream.
End;//LoadFromFile
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Load()                                                               FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Loads a map's cells from a .pms file.
//
//  Changes -
//    January 25th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
Procedure TMap.Load;
Var
	MapFile : TMemoryStream;
  XIndex  : Integer;
	YIndex  : Integer;
Begin
  State  := LOADING;

  MapFile := TMemoryStream.Create;
  MapFile.LoadFromFile(Path);

  Flags:= ADatabase.StaticData.GetMapFlags(Name);
  MapFile.Seek(22,0);//skip other non-cell information

  //Load Cell Information
  SetLength(Cell, Size.X, Size.Y);
  for YIndex := 0 to Size.Y - 1 do begin
    for XIndex := 0 to Size.X - 1 do begin
			MapFile.Read(Cell[XIndex][YIndex].Attribute,1);
			Cell[XIndex][YIndex].Position := Point(XIndex, YIndex);
			Cell[XIndex][YIndex].ObstructionCount := 0;
      Cell[XIndex][YIndex].Beings := TIntList32.Create;
    end;
  end;
  State := LOADED;

  MapFile.Free;//finally, free the memory stream.
End;//Load
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Unload()                                                             FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Unloads a map's cells and flags.
//
//  Changes -
//    January 25th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
Procedure TMap.Unload;
var
  XIndex  : Integer;
	YIndex  : Integer;
Begin
  if State = LOADED then
  begin
    //Free up each Cell.
    for XIndex := 0 to Size.X - 1 do
    begin
      for YIndex := 0 to Size.Y - 1 do
      begin
        Cell[XIndex][YIndex].Beings.Free;
      end;
    end;
    SetLength(Cell, 0, 0);
    State := UNLOADED;
  end;
End;//Unload
//------------------------------------------------------------------------------
end.
