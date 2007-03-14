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
	MapTypes,
	EventList,
	SysUtils;

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
			const APoint : TPoint;
			AGraph : TGraph = NIL
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
	Classes,
	Math,
	List32,
	Main,
	Globals,
	GameConstants,
	WinLinux;

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
	AFloodList				: TList;
	AFloodItem				: TFloodItem;
	NewFloodItem			: TFloodItem;
	Index							: Integer;
	DirectionIndex		: Integer;
	PossiblePosition	: TPoint;
	CharClickArea			: Integer;

	CostTick					: Cardinal;

	XMod							: Integer;
	YMod							: Integer;

	AnArea						: TGraph;
	AnAreaSize				: TPoint;

	Done							: Boolean;
	EndPointMod				: TPoint;

begin
	Result				:= FALSE;
	Done 					:= FALSE;
	CostTick			:=  0;
	CharClickArea := MainProc.ZoneServer.Options.CharClickArea;
	AFloodItem		:= TFloodItem.Create;
	AFloodList		:= TList.Create;

	//grab our area
	XMod := EndPoint.X-CharClickArea;
	YMod := EndPoint.Y-CharClickArea;
	AnArea := Copy(Cell, Max(XMod, 0), Min((CharClickArea*2)+1, Abs(XMod - Size.X)));
	for Index := 0 to Length(AnArea)-1 do
	begin
		AnArea[Index] := Copy(Cell[XMod+Index], Max(YMod, 0), Min((CharClickArea*2)+1, Abs(YMod - Size.Y)));
	end;
	AnAreaSize := Point(Length(AnArea),Length(AnArea[0]));

	EndPointMod := Point(StartPoint.X-Abs(XMod),StartPoint.Y-Abs(YMod));

	//initialize our first flood item
	AFloodItem.Position.X := EndPoint.X-abs(XMod);
	AFloodItem.Position.Y := EndPoint.Y-abs(YMod);
	AFloodItem.Cost := 0;
	AFloodList.Add(AFloodItem);

	While (NOT Result) AND (NOT Done) do
	begin
		Index := AFloodList.Count;
		While (Index > 0) AND (NOT Result) do
		begin
			Dec(Index);
			AFloodItem := AFloodList[Index];
			if AFloodItem.Cost <= CostTick then
			begin
				DirectionIndex := 8;
				While (DirectionIndex > 0) AND (NOT Result) do
				begin
					Dec(DirectionIndex);
					PossiblePosition.X := AFloodItem.Position.X + Directions[DirectionIndex].X;
					PossiblePosition.Y := AFloodItem.Position.Y + Directions[DirectionIndex].Y;

					//Make sure the new point is inside our search boundaries.
					if	(PossiblePosition.X < AnAreaSize.X) AND
							(PossiblePosition.X >= 0)						AND
							(PossiblePosition.Y < AnAreaSize.Y) AND
							(PossiblePosition.Y >= 0)						then
					begin
						//make sure we can move to the new point.
						if NOT IsBlocked(PossiblePosition, AnArea) then
						begin
							//Create and add our new flood item
							NewFloodItem := TFloodItem.Create;

							NewFloodItem.Path.Assign(AFloodItem.Path);
							NewFloodItem.Path.Add(AnArea[PossiblePosition.X][PossiblePosition.Y].Position);

							NewFloodItem.Position		:= PossiblePosition;

							if not (abs(Directions[DirectionIndex].X) = abs(Directions[DirectionIndex].Y)) then
							begin
								NewFloodItem.Cost	:= AFloodItem.Cost + 5;
							end else begin
								NewFloodItem.Cost	:= AFloodItem.Cost + 7;
							end;

							AFloodList.Add(NewFloodItem);

							AnArea[NewFloodItem.Position.X][NewFloodItem.Position.Y].Attribute := 1;

							if PointsEqual(NewFloodItem.Position, EndPointMod) then
							begin
								Result	:= TRUE;
								APath.Assign(NewFloodItem.Path);
							end;
						end;
					end;
				end;
				TFloodItem(AFloodList[Index]).Free;
				AFloodList.Delete(Index);
			end;
		end;

		if (AFloodList.Count = 0) then
		begin
			Done := TRUE;
		end;

		Inc(CostTick);
	end;

	//Free our lists + items
	for Index := 0 to AFloodList.Count - 1 do
	begin
		TFloodItem(AFloodList[Index]).Free;
	end;
	AFloodList.Free;
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
function TMap.IsBlocked(const APoint : TPoint; AGraph : TGraph = NIL) : boolean;
begin
	if NOT Assigned(AGraph) then
	begin
		AGraph := Cell;
	end;
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
