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

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	Types,
	contnrs,
	{Project}
	EventList,
	MapTypes
	{3rd Party}
	//none
	;


type

//------------------------------------------------------------------------------
//TMap                                                                     CLASS
//------------------------------------------------------------------------------
TMap = class(TObject)
	private
		Path : String;

	public
		ID		: LongWord;
		Name	: String;
		Cell	: TGraph;
		Size	: TPoint;
		Flags	: TFlags;
		State	: TMapMode;
		
		EventList : TEventList;
		MobList  : TObjectList;

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
	Globals,
	Main,
	NPC,
	Mob,
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
	//We own mob objects here!
	MobList := TObjectList.Create(TRUE);
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
	MobList.Free;
	inherited;
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
		Result := True;
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
	ObjIndex    : Integer;
	AnNPC       : TNPC;
	AnMob       : TMob;
Begin
	State  := LOADING;

	MapFile := TMemoryStream.Create;
	MapFile.LoadFromFile(Path);

	MainProc.ZoneServer.Database.Map.LoadFlags(Flags, Name);
	MapFile.Seek(22,0);//skip other non-cell information

	//Load Cell Information
	SetLength(Cell, Size.X, Size.Y);
	for YIndex := 0 to Size.Y - 1 do begin
		for XIndex := 0 to Size.X - 1 do begin
			MapFile.Read(Cell[XIndex][YIndex].Attribute,1);
			Cell[XIndex][YIndex].Position := Point(XIndex, YIndex);
			Cell[XIndex][YIndex].ObstructionCount := 0;
			Cell[XIndex][YIndex].Beings := TIntList32.Create;
			Cell[XIndex][YIndex].Items  := TIntList32.Create;
		end;
	end;

	//Enable all npcs on this map.
	for ObjIndex := 0 to MainProc.ZoneServer.NPCList.Count -1 do
	begin
		AnNPC := TNPC(MainProc.ZoneServer.NPCList.Objects[ObjIndex]);
		if AnNPC.Map = Name then
		begin
			if PointInRange(AnNPC.Position) then
			begin
				// We don't want add npc outside of map..
				AnNPC.MapInfo := Self;
				Cell[AnNPC.Position.X][AnNPC.Position.Y].Beings.AddObject(AnNPC.ID, AnNPC);
				AnNPC.Enabled := True;
			end;
		end;
	end;

	//Add mobs that is already in list
	for ObjIndex := 0 to MobList.Count -1 do
	begin
		AnMob := TMob(MobList[ObjIndex]);
		if PointInRange(AnMob.Position) then
		begin
			AnMob.MapInfo := Self;
			Cell[AnMob.Position.X][AnMob.Position.Y].Beings.AddObject(AnMob.ID, AnMob);
		end;
	end;
	MainProc.ZoneServer.Database.Items.FillMapGround(Self);

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
				Cell[XIndex][YIndex].Items.Free;
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
begin
	Result := False;
	case State of
		UNLOADED: begin
			// Just load it
			Load;
			if State = LOADED then
			begin
				Result := True;
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


