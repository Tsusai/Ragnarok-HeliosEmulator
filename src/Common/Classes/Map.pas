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

		Function PointInRange(const APoint : TPoint) : Boolean;

		function RandomCell: TPoint;
		function SafeLoad: Boolean;

end;
//------------------------------------------------------------------------------


implementation


uses
	{RTL/VCL}
	SysUtils,
	Classes,
	Math,
	WinLinux,
	{Project}
	GameConstants,
	Globals,
	Main,
	Npc,
	Being,
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
//		April 29th, 2007 - RaX - Commented this routine thoroughly, to allow others
//		 to understand it and perhaps learn something =)
//
//------------------------------------------------------------------------------
function TMap.GetPath(
	const StartPoint  : TPoint;
	const EndPoint    : TPoint;
	var APath : TPointList
) : boolean;

var
	AFloodList				: TList;//Our main flood list, contains FloodItems
	AFloodItem				: TFloodItem;
	NewFloodItem			: TFloodItem;//Created with supplied data to test if an item
																	//is valid. If it is, it is added to the floodlist
	Index							: Integer;
	WriteIndex        : Integer;
	DirectionIndex		: Integer;
	PossiblePosition	: TPoint;
	CharClickArea			: Integer;

	CostTick					: Cardinal;

	XMod							: Integer;//The modifier between AnArea(a piece of the map)
															//and the real X Index for that cell.
	YMod							: Integer;//y version of XMod

	AnArea						: TGraph;//A section of the map used for keeping track of
															//where a flood item is.
	AnAreaSize				: TPoint;

	Done							: Boolean;
	EndPointMod				: TPoint;

	BeingIndex				: Integer;

begin
	//Initialize data
	Result				:= FALSE;
	Done 					:= FALSE;
	CostTick			:=  0;
	CharClickArea := MainProc.ZoneServer.Options.CharClickArea;
	AFloodItem		:= TFloodItem.Create;
	AFloodList		:= TList.Create;

	//grab our area from the map
	XMod := Max(EndPoint.X-CharClickArea, 0);//get our modifiers
	YMod := Max(EndPoint.Y-CharClickArea, 0);
	//copy the X axis' cells from the map from the modifier to CharClickAres*2+1
	//(33 by default. 16 away from the character in any direction + 1 for the
	//space the character is on)
	AnArea := Copy(Cell, XMod, Min((CharClickArea*2)+1, Size.X-XMod));
	for Index := 0 to Length(AnArea)-1 do
	begin
		//copy each of the Y axis' cells in the area constraints
		AnArea[Index] := Copy(Cell[XMod+Index], YMod, Min((CharClickArea*2)+1, Size.Y-YMod));
	end;
	//Grab the length of the area's X+Y axis' as a TPoint
	AnAreaSize := Point(Length(AnArea),Length(AnArea[0]));

	//Get the endpoint's position in AnArea, we are searching from the end to the
	//start to emulate the client.
	EndPointMod := Point(StartPoint.X-XMod,StartPoint.Y-YMod);

	//initialize our first flood item, the start point is the endpoint that we're
	//searching. This is to emulate the client.
	AFloodItem.Position.X := EndPoint.X-XMod;
	AFloodItem.Position.Y := EndPoint.Y-YMod;
	AFloodItem.Path.Add(AnArea[AFloodItem.Position.X][AFloodItem.Position.Y].Position);
	AFloodItem.Cost := 0;
	AFloodList.Add(AFloodItem);

	//While we havn't found the endpoint and we havn't run out of cells to check do
	While (NOT Result) AND (NOT Done) do
	begin
		//start from the end of the list, and go to the beginning(so removing items
		//is FAST)
		Index := AFloodList.Count;
		//While we've still got items on our floodlist to search and we havn't found
		//the endpoint, do.
		While (Index > 0) AND (NOT Result) do
		begin
			//decrease the index and grab a flood item for spawning new items in each
			//direction
			Dec(Index);
			AFloodItem := AFloodList[Index];
			//To see if this flood item is ready to propagate, we check it's weight.
			//If this "Cost" weight is less than the cost we are currently at then we
			//propagate the flood item. Increasing costs are a problem many large
			//corporations face today! It's a good thing here.
			if AFloodItem.Cost <= CostTick then
			begin
				//Loop backwards through the directions, emulating gravity's client.
				DirectionIndex := 8;
				//While there are still directions to search and we havn't found the endpoint,
				//do
				While (DirectionIndex > 0) AND (NOT Result) do
				begin
					//decrease our direction index(remember, we started at 8 for a reason!)
					Dec(DirectionIndex);
					//grab our possible position
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

							NewFloodItem.Position		:= PossiblePosition;
							//calculate the cost of this new flood item.
							if not (abs(Directions[DirectionIndex].X) = abs(Directions[DirectionIndex].Y)) then
							begin
								NewFloodItem.Cost	:= AFloodItem.Cost + 5;
							end else begin
								NewFloodItem.Cost	:= AFloodItem.Cost + 7;
							end;
							//add the item to the flood list.
							AFloodList.Add(NewFloodItem);

							AnArea[NewFloodItem.Position.X][NewFloodItem.Position.Y].Attribute := 1;
							//check to see if we've found the end point.
							if PointsEqual(NewFloodItem.Position, EndPointMod) then
							begin
								APath.Clear;

								//if the destination point has beings in it, remove it from the list
								//this is for attacking and other functions who move to the mob's
								//position, it's our responsibility to prevent us from standing on
								//them =P
								for BeingIndex := 0 to Cell[NewFloodItem.Path[0].X][NewFloodItem.Path[0].Y].Beings.Count - 1 do
								begin
									if Cell[NewFloodItem.Path[0].X][NewFloodItem.Path[0].Y].Beings.Objects[BeingIndex] is TBeing then
                  begin
										NewFloodItem.Path.Delete(0);
										break;
                  end;
								end;

								{if Cell[NewFloodItem.Path[0].X][NewFloodItem.Path[0].Y].Beings.Count > 0 then
								begin
									NewFloodItem.Path.Delete(0);
								end;}

								if NewFloodItem.Path.Count > 0 then
								begin
									//We've found it!
									Result	:= TRUE;
									for WriteIndex := NewFloodItem.Path.Count -1 downto 0 do
									begin
										APath.Add(NewFloodItem.Path[WriteIndex]);
									end;
								end;

								(*Tsusai Mar 16 2007: The Assign does copy..but the problem is
								that the NewFloodItem.Path starts from the destination and goes
								to the source.  So the server never updates the character position
								because we finish walking where we start.
								{//APath.Assign(NewFloodItem.Path);
								for WriteIndex := 0 to APath.Count -1 do
								begin
									//Output path that is made.
									writeln(format('Path index %d - Pt (%d,%d)',[WriteIndex,APath[WriteIndex].X,APath[WriteIndex].y]));
								end;}*)

							end else
							begin
								//If we've not found it, add the new point to the list(we're searching backwards!)
								NewFloodItem.Path.Add(AnArea[PossiblePosition.X][PossiblePosition.Y].Position);
							end;
						end;
					end;
				end;
				//free the flood item.
				TFloodItem(AFloodList[Index]).Free;
				//remove it from the list.
				AFloodList.Delete(Index);
			end;
		end;
		//if we've run out of cells to check, die.
		if (AFloodList.Count = 0) then
		begin
			Done := TRUE;
		end;
		//increment our cost tick
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


//------------------------------------------------------------------------------
//RandomCell                                                            FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Random a walkable cell
//
//  Changes -
//	[2007/08/13] Aeomin - Created.
//------------------------------------------------------------------------------
function TMap.RandomCell: TPoint;
var
	LoopTrials : Byte;
	LoopOk     : Boolean;
	idxX       : Word;
	idxY       : Word;
begin
	LoopTrials := 0;
	LoopOk     := False;
	while LoopTrials < 20 do
	begin
	//Random one!
		Result.X := Random(Size.X -1);
		Result.Y := Random(Size.Y -1);
		if IsBlocked(Result) then
			Inc(LoopTrials)
		else begin
			LoopOk := True;
			Break;
		end;
	end;

	//We just tried 10 times, if still cant find one..
	if not LoopOK then
	begin
		for idxX := 0 to Size.X - 1 do
		begin
			if LoopOK then
				Break;
			for idxY := 0 to Size.Y - 1 do
			begin
				//Then just get one that works..
				Result.X := idxX;
				Result.Y := idxY;
				if not IsBlocked(Result) then
				begin
					LoopOk := True;
					Break;
				end;
			end;
		end;
	end;

	//Really.. i can't help anymore...
	if not LoopOk then
	begin
		Result.X := 0;
		Result.Y := 0;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SafeLoad                                                              FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Check and load map, return false of failed.
//
//  Changes -
//	[2007/08/14] Aeomin - Created.
//------------------------------------------------------------------------------
function TMap.SafeLoad: Boolean;
var
	LoopTries : Byte;
	NPCIndex    : Integer;
	AnNPC       : TNPC;
begin
	Result := False;
	case State of
		UNLOADED: begin
			// Just load it
			Load;
			if State = LOADED then
			begin
				Result := True;
				//Enable all npcs on this map.
				for NPCIndex := 0 to MainProc.ZoneServer.NPCList.Count -1 do
				begin
					AnNPC := TNPC(MainProc.ZoneServer.NPCList.Objects[NPCIndex]);
					if AnNPC.Map = Name then
					begin
						AnNPC.MapInfo := Self;
						Cell[AnNPC.Position.X][AnNPC.Position.Y].Beings.AddObject(AnNPC.ID, AnNPC);
						AnNPC.Enabled := True;
					end;
				end;
			end;
		end;
		LOADING: begin
			//We can't do anything but wait...
			LoopTries := 0;
			while LoopTries <= 10 do
			begin
				if State = LOADED then
				begin
					Result := True;
					Break;
				end;
				Sleep(1000);
			end;
		end;
		LOADED: begin
			//Simple!
			Result := True;
		end;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//PointInRange                                                        FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Checks to see if a point is inside the range of the map.
//
//  Changes -
//	[2008/01/02] RaX - Created
//------------------------------------------------------------------------------
Function TMap.PointInRange(const APoint : TPoint) : Boolean;
begin
	if (APoint.X < Size.X) AND
		 (APoint.Y < Size.Y) AND
		 (APoint.X > -1) AND
		 (APoint.Y > -1) then
  begin
		Result := TRUE;
	end else
	begin
		Result := FALSE;
	end;
end;
//------------------------------------------------------------------------------
end.

