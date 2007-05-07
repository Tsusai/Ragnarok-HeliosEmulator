//------------------------------------------------------------------------------
//Map                                                                    UNIT
//------------------------------------------------------------------------------
//  What it does -
//      Our Map class, holds everything to do with our map object.
//
//  Changes -
//    October 30th, 2006 - RaX - Created.
//		[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
//		[2007/04/23] Tsusai - Added lua
//		[2007/04/28] Tsusai - Removed lua
//
//------------------------------------------------------------------------------
unit Map;


interface


uses
	{RTL/VCL}
	Types,
	{Project}
	EventList,
	MapTypes,
	PointList
	{3rd Party}
	//none
	;


type

//------------------------------------------------------------------------------
//TMap								CLASS
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

		Constructor Create;
		Destructor Destroy;override;

		Function IsBlocked(
			const
				APoint : TPoint;
				AGraph : TGraph = NIL
			) : Boolean;

		Function LoadFromFile(
			const
				PmsFile : String
			) : Boolean;

		Procedure Load;
		Procedure Unload;

		Function GetPath(
			const
				StartPoint : TPoint;
			const
				EndPoint   : TPoint;
			var
				APath      : TPointList
		) : Boolean;

end;
//------------------------------------------------------------------------------


implementation


uses
	{RTL/VCL}
	Classes,
	Math,
	WinLinux,
	{Project}
	GameConstants,
	Globals,
	Main,
	{3rd Party}
	List32
	;

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


(*- Function ------------------------------------------------------------------*
TMap.GetPath
--------------------------------------------------------------------------------
Overview:
--
	This is a pathing algorithm that I like to call a "wave" algorithm
because of the way it propagates out from the starting point much like the
behavior of a wave. It finds paths by searching all possible paths to a
certain extent and using the one that arrives at the target first, the
shortest path.

[2007/04/29] CR - N.B.  On further discussion with RaX to find out more about
	this still hard to read algorithm, he made an important note:

	Pathing on the client (which the server has to match), seems to be done from
	the End point to the Start point, not the usual Start to End used on most
	pathing algorithms.

[2007/05/07] CR - After MUCH testing to find out why the Cost to beat was
	IGNORED, the proper setup was found.  CostTick is reevaluated at the
	end of this while loop, setting it to the Diagonal distance from the last
	AFloodItem looked up in the list.  This is still not ideal but it prunes
	the search area down markedly -- and turns this algorithm into a Pseudo
	A* algorithm, instead of Djikstra's search.

	Search times on ring 1 and ring 2 are better than the original algorithm, but
	not markedly.  For further out pathing, times are roughly halved vs. the
	algorithm used in revision 254 and prior*.

	* Using the test harness I used.  RaX said his initial tests yielded roughly
	execution times of 20ns or so, but I know I'm not testing in the same manner.
	My tests using the client and server on the same PC, always yielded individual
	timings in the micro-second range.

--
Revisions:
--
[2006/10/30] RaX - Created.
[2006/11/01] Tsusai - moved the check for passability into IsBlocked. Changed
	to a boolean function and our path is assigned via a TPointList parameter
	passed by address, APath.
[2007/04/29] RaX - Commented this routine thoroughly, to allow others
	to understand it and perhaps learn something =)
[2007/05/07] CR - Added to comment header to describe this routine's behavior.
	Refactored StepCost out of main routine, and improved the efficiency of that
	cost lookup by simplifying the comparison.
	Refactored out routines InitializeAnArea and GetOptimalDirection.
	Renamed local variables:  XMod and YMode now are Offset (TPoint), and
	EndPointMod is now AreaEndPoint.
	Using DiagonalCost to PROPERLY focus the search area.  In testing, this
	yields speeds half the time of the algorithm used in rev 254.
	This proper const comparison, along with the proper stepping DOWN the compared
	cost (not incrementing it!), and with use of GetOptimalDirection, we are now
	aiming the search toward the goal, and avoiding expensive searches that don't
	compute to a low-cost path.
[yyyy/mm/dd] <Author> - <Comment>
*-----------------------------------------------------------------------------*)
Function TMap.GetPath(
		const
			StartPoint : TPoint;
		const
			EndPoint   : TPoint;
		var
			APath      : TPointList
	) : Boolean;
const
	BLOCKED_CELL = 1;
var
	AFloodList       : TList;//Our main flood list, contains FloodItems
	AFloodItem       : TFloodItem;
	NewFloodItem     : TFloodItem;//Created with supplied data to test if an item
											//is valid. If it is, it is added to the floodlist
	Index            : Integer;
	WriteIndex       : Integer;

	DirectionIndex   : SmallInt;
	PossDir          : Byte;
	DirOffset        : Byte;

	PosPosition      : TPoint;
	CharClickArea    : Integer;

	CostTick         : Cardinal;

	//Offsets that let us map the cells in AnArea to the map's coordinates.
	Offset           : TPoint;

	AnArea           : TGraph;//A section of the map used for keeping track of
											//where a flood item is.
	AnAreaSize       : TPoint;

	Done             : Boolean;
	AreaEndPoint     : TPoint;

	(*- Local Function ..................*
	StepCost
		Returns a step cost of:
		5 -- when a straightline movement is found.
		7 -- when a diagonal movement is found.
	--
	[2007/04/24] CR - Extracted from main body.  After further inspection,
		the boolean condition was simplified to a direct assignment from an
		array lookup.  The cost of this is simply 2 hidden Inc() calls to step to
		the appropriate array index for the result.
	*...................................*)
	function StepCost(
		const
			Direction : Byte
		) : Byte;
	const
		DirCost : array[Boolean] of Byte = (5, 7);
		{[2007/04/29] CR - 5 is the N-S-E-W straightline cost,
		and 7 is the NE-NW-SE-SW diagonal cost. }
	var
		Remainder : Boolean;
	begin
		Remainder := (Direction mod 2 = 0);
		Result := DirCost[Remainder];
	end;(* StepCost
	*...................................*)

	(*- Local Procedure .................*
	InitializeAnArea
		Sets up AnArea, copying a subsection of Self.Cell, all the Cell values
		contained in that patch of the entire grid, and setting AnAreaSize
		appropriately.
	--
	[2007/04/24] CR - Extracted
		from main body.
	*...................................*)
	procedure InitializeAnArea;
	var
		Index : Word;
	begin
		//copy the X axis' cells from the map from the modifier to CharClickAres*2+1
		//(33 by default. 16 away from the character in any direction + 1 for the
		//space the character is on)
		AnArea := Copy(
			Cell,
			Max(Offset.X, 0),
			Min((CharClickArea*2)+1, Abs(Offset.X - Size.X))
		);

		for Index := 0 to Length(AnArea)-1 do
		begin //copy each of the Y axis' cells in the area constraints
			AnArea[Index] := Copy(
				Cell[Offset.X + Index],
				Max(Offset.Y, 0),
				Min((CharClickArea*2)+1, Abs(Offset.Y - Size.Y))
			);
		end;

		//Grab the length of the area's X+Y axis' as a TPoint
		// All Y columns are the same length, BTW, so use AnArea[0]
		AnAreaSize := Point(Length(AnArea),Length(AnArea[0]));
	end;(* InitializeAnArea
	*...................................*)


	(*- Local Function .................*
	GetOptimalDirection
		Based on Current position and
		the AreaEndPoint, find the best
		direction to search first.
	--
	[2007/04/24] CR - Created
	*...................................*)
	function GetOptimalDirection : Byte;
	var
		RelDir : TPoint;
	begin
		Result := NORTH;

		RelDir.X := (AreaEndPoint.X - AFloodItem.Position.X);
		RelDir.Y := (AreaEndPoint.Y - AFloodItem.Position.Y);

		if (RelDir.X < 0) then
		begin
			if (RelDir.Y < 0) then
			begin
				Result := SOUTHWEST;
			end
			else if (RelDir.Y > 0) then
			begin
				Result := NORTHWEST;
			end else begin
				Result := WEST;
			end;
		end
		else if (RelDir.X > 0) then
		begin
			if (RelDir.Y < 0) then
			begin
				Result := SOUTHEAST;
			end
			else if (RelDir.Y > 0) then
			begin
				Result := NORTHEAST;
			end else begin
				Result := EAST;
			end;
		end else begin//X is zero.
			if (RelDir.Y < 0) then
			begin//1,-1
				Result := SOUTH;
			end
			else if (RelDir.Y > 0) then
			begin
				Result := NORTH;
			end;
			//Can't have a branch for zero-zero!
		end;
	end;(* GetOptimalDirection
	*...................................*)

Begin
	//Initialize data
	Result				:= FALSE;
	Done 					:= FALSE;
	CharClickArea := MainProc.ZoneServer.Options.CharClickArea;
	AFloodItem		:= TFloodItem.Create;
	AFloodList		:= TList.Create;

	//grab our area from the map
	Offset.X := EndPoint.X - CharClickArea; //get our modifiers
	Offset.Y := EndPoint.Y - CharClickArea;

	InitializeAnArea;

	//Get the endpoint's position in AnArea, we are searching from the end to the
	//start to emulate the client.
	AreaEndPoint := Point(
		StartPoint.X - Abs(Offset.X),
		StartPoint.Y - Abs(Offset.Y)
	);

	//initialize our first flood item, the start point is the endpoint that we're
	//searching. This is to emulate the client.
	AFloodItem.Position.X := EndPoint.X - Abs(Offset.X);
	AFloodItem.Position.Y := EndPoint.Y - Abs(Offset.Y);
	AFloodItem.Path.Add(
		AnArea[AFloodItem.Position.X, AFloodItem.Position.Y].Position
	);

	{[2007/05/06] CR - Init Cost to beat as Diagonal distance }
	AFloodItem.Cost := AFloodItem.DiagonalCost(AreaEndPoint);
	CostTick := AFloodItem.Cost;
	AFloodList.Add(AFloodItem);

	//While endpoint not found and still cells to check do
	While (NOT Result) AND (NOT Done) do
	begin
		//Start at end of list, and go to the beginning (so removing items is FAST)
		Index := AFloodList.Count;
		//While items in floodlist to search, and not at the endpoint, do.
		While (Index > 0) AND (NOT Result) do
		begin
			//Grab next flood item for spawning new items in each direction
			Dec(Index);
			AFloodItem := AFloodList[Index];

			{[2007/05/07] CR - CostTick compared to the diagonal ideal cost for
			AFloodItem.  After much searching, this is a better optimal comparison. }
			if (AFloodItem.DiagonalCost(AreaEndPoint) <= CostTick) then
			begin
				//Loop backwards through the directions, emulating gravity's client.
				DirectionIndex := 8;

				{[2007/04/29] CR - Try to optimize search start. }
				PossDir := GetOptimalDirection;
				DirOffset     := 7 - PossDir;

				//While there are still directions to search and we havn't found the endpoint,
				//do
				While (DirectionIndex > 0) AND (NOT Result) do
				begin
					//decrease our direction index(remember, we started at 8 for a reason!)
					Dec(DirectionIndex);

					PossDir := DirectionIndex;
					if (DirOffset > DirectionIndex) then
					begin
						Inc(PossDir, 8);
					end;
					Dec(PossDir, DirOffset);

					//grab our possible position
					PosPosition.X := AFloodItem.Position.X +
						Directions[PossDir].X;
					PosPosition.Y := AFloodItem.Position.Y +
						Directions[PossDir].Y;

					//Make sure the new point is inside our search boundaries.
					//AND Make sure we can move to the new point.
					if (
							(PosPosition.X < AnAreaSize.X) AND
							(PosPosition.X >= 0)						AND
							(PosPosition.Y < AnAreaSize.Y) AND
							(PosPosition.Y >= 0)
						) AND (
							NOT IsBlocked(PosPosition, AnArea)
						) then
					begin
						//Create and add our new flood item
						NewFloodItem := TFloodItem.Create;

						NewFloodItem.Path.Assign(AFloodItem.Path);
						NewFloodItem.Position := PosPosition;
						NewFloodItem.Cost := AFloodItem.Cost + StepCost(PossDir);
						AFloodList.Add(NewFloodItem);

						AnArea[NewFloodItem.Position.X, NewFloodItem.Position.Y].Attribute :=
							BLOCKED_CELL;

						//check to see if we've found the end point.
						{[2007/05/07] CR - PointsEqual removed - doing the math local is
						faster, even if less legible. }
						if (NewFloodItem.Position.X = AreaEndPoint.X) AND
							(NewFloodItem.Position.Y = AreaEndPoint.Y) then
						begin
							//We've found it!
							Result := TRUE;
							APath.Clear;
							for WriteIndex := (NewFloodItem.Path.Count - 1) downto 0 do
							begin
								APath.Add(NewFloodItem.Path[WriteIndex]);
							end;
							{ Tsusai Mar 16 2007: The Assign does copy..but the problem is
							that the NewFloodItem.Path starts from the destination and goes
							to the source.  So the server never updates the character
							position because we finish walking where we start. }
						end else begin
							//If we've not found it, add the new point to the list
							//( we're searching backwards!)
							NewFloodItem.Path.Add(
								AnArea[PosPosition.X, PosPosition.Y].Position
							);
						end;
					end;
				end;
				//free the flood item, and remove it from the list.
				TFloodItem(AFloodList[Index]).Free;
				AFloodList.Delete(Index);
			end;
		end;//wh

		{[2007/05/06] CR - Other than finding a path, finding NO path means we're
		finished searching too.
		N.B. Both Running out of cells to check AND finding a path may occur. }
		Done := (AFloodList.Count = 0);

		{[2007/05/06] CR - Drop CostTick down by 5 -- the cost of a NSWE move. }
		if NOT Done AND (CostTick >= 5) then
		begin
			Dec(CostTick, 5);
		end;
	end;//wh

	//Free our lists + items
	for Index := 0 to (AFloodList.Count - 1) do
	begin
		TFloodItem(AFloodList[Index]).Free;
	end;
	AFloodList.Free;

End; (* Func GetPath
*-----------------------------------------------------------------------------*)

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
	Result := False;
	if (AGraph[APoint.X][APoint.Y].Attribute in [1,5]) OR
		(AGraph[APoint.X][APoint.Y].ObstructionCount > 0 ) then
	begin
		Result := True
	end;
end;
//------------------------------------------------------------------------------


(*- Function ------------------------------------------------------------------*
		TMap.LoadFromFile()
--------------------------------------------------------------------------------
Overview:
----------------------------------------
	Loads a map from a .pms file.

	Modelled after Borland's TStrings.LoadFromFile.
----------------------------------------
Revisions:
----------------------------------------
January 22nd, 2007 - RaX - Created.
January 25th, 2007 - Tsusai - Removed complicated path name code, replaced
	with ExtractFileNameMod (WinLinux)
[2007/03/24] CR - Parameter made constant (optimization).
	Renamed Parameter "Path" to "PmsFile" to avoid ambiguity with TMap.Path
	It's a more appropriate name than "Path" in the first place! :)
	Surrounded TMemoryStream with a try-finally to protect the resource.
	Fixed major logic flaws -- Result is NOT handled like C/C++'s "return"
	keyword.  Thus, we NEED to use if-then-else branching, not just sequential
	if-thens.  Malformed/hand-mangled maps could cause fatal errors with the old
	code flow, because it couldn't avoid continuing on instead of bailing early.
*-----------------------------------------------------------------------------*)
Function TMap.LoadFromFile(
	const
		PmsFile : String
	) : Boolean;
Var
	AByte   : Byte;
	MapFile : TMemoryStream;
	MapTag  : array[1..MAX_PMS_HEADER_LENGTH] of Char;
	MapSize : TPoint;
Begin
	Result := TRUE;

	MapFile := TMemoryStream.Create;
	try
		MapFile.LoadFromFile(PmsFile);
		Path := PmsFile;

		MapFile.Read(MapTag[1], MAX_PMS_HEADER_LENGTH);
		if (MapTag <> 'PrometheusMap') then //Check type
		begin
			Console.WriteLn('The Map :' + Path + ' is not a Prometheus Map.');
			Result := FALSE;
		end
		else
		begin
			MapFile.Read(AByte,1); //Check version.

			if (AByte <> 1) then
			begin
				Console.WriteLn('The Map :' + Path + ' failed the version check.');
				Result := FALSE;
			end
			else
			begin
				MapFile.Read(MapSize.X, 4);
				MapFile.Read(MapSize.Y, 4);

				//check size.
				if NOT (InRange(MapSize.X, 0, 511) AND InRange(MapSize.Y, 0, 511)) then
				begin
					Console.WriteLn('The Map :' + Path + '''s size is out of range.');
					Result := FALSE;
				end;
			end;
		end;

		if Result then //If we've passed the checks then...
		begin
			//Load Map Information.
			Name := ExtractFileNameMod(Path);
			Size := MapSize;
		end;

	finally
		MapFile.Free; //finally, free the memory stream.
	end;
End;//LoadFromFile
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Load()                                                               FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Loads a map's cells from a .pms file.
//
//  Changes -
//		January 25th, 2007 - RaX - Created.
//		[2007/04/23] Tsusai - Added lua setup and execution
//		[2007/04/28] Tsusai - Removed lua
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

	Flags:= MainProc.ZoneServer.ZoneLocalDatabase.StaticData.GetMapFlags(Name);
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
