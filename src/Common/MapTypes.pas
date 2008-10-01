//------------------------------------------------------------------------------
//MapTypes                                                                  UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Contains Map related Types 
//
//	Changes -
//		January 22d, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------

unit MapTypes;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	Types,
	{Project}
	PointList,
	{Third Party}
	List32
	;


const
	{[2007/03/24] CR - Max length that a .pms map name in file can be. }
	MAX_PMS_HEADER_LENGTH = 13; //Length of 'PrometheusMap'

const
	{[2007/04/29] CR - Borrowed from Prometheus' constants we used long ago.
	And then... corrected - our logic was reversed, for naming the constants.
	:P~~~~~~~
	Expanded explanation:
		Even numbered codes are unwalkable (but sometimes pathable for attacks)
		Odd numbered codes are walkable.

	Map GAT Bit values (can be combined - ANDs or ORs...)  }
	GAT_WALKABLE   = $00; // unwalkable terrain (land, cliffs, obstacles)
	GAT_UNWALKABLE = $01; // bit1 = walkable point (land)
	GAT_PUDDLE     = $02; // bit2 = Puddle (water)
	GAT_W_PUDDLE   = $03; // (walkable + water)
	GAT_WARP       = $04; // bit3 = Warp
	GAT_W_WARP     = $05; // (walkable + warp)


type

	TMapMode = (UNLOADED,LOADING,LOADED);

	TFlags = record
		//Warping
		Memo          : Boolean;
		NoReturnOnDC  : Boolean;
		Teleport      : Boolean;

		//Items/Skills
		FlyWings      : Boolean;
		ButterflyWings: Boolean;
		DeadBranches  : Boolean;
		Skill         : Boolean;
		Items         : Boolean;

		//PVP
		PvP           : Boolean;
		GuildPvP      : Boolean;
		PvPNightmare  : Boolean;
		ExpLoss       : Boolean;
		ItemDrop      : Boolean;

		//Guilds + Parties
		NoGuild       : Boolean;
		NoParty       : Boolean;

		//Turbo Track
		TurboTrack    : Boolean;

		//Weather
		Rain      : Boolean;
		Snow      : Boolean;
		Sakura    : Boolean;
		Fog       : Boolean;
		Leaves    : Boolean;
		Smog      : Boolean;

		//Objects tracking
		AutoId    : LongWord;
	end;

	//graph related types
	TCell = record
		//The place in a graph the cell is
		Position        : TPoint;
		//what kind of tile is it
		Attribute       : Byte;
		//Tsusai 11/09/06: Keep track of the number of things in the way, like icewall(s)
		ObstructionCount: Byte;
		//TBeings in this cell (NPC/Mob/Chara)
		Beings          : TIntList32;
		//Items!!
		Items		: TIntList32;
	end;

	TGraph = array of array of TCell;

	TFloodItem = class
	public
		Path     : TPointList;
		Position : TPoint;
		Cost     : LongWord;

		Constructor Create;
		Destructor Destroy; override;

		function  ManhattanCost(
			const
				GoalPt : TPoint
			) : Word;

		function  DiagonalCost(
			const
				GoalPt : TPoint
			) : Word;

	end;


implementation


//------------------------------------------------------------------------------
//Create                                                             CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Creates this object with empty Path
//
//	Changes -
//		March 13th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
Constructor TFloodItem.Create;
begin
	inherited;
	Path		:= TPointList.Create;
	Cost		:= 0;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy                                                             DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroy this object and its path
//
//	Changes -
//		March 13th, 2007 - RaX - Created Header
//
//------------------------------------------------------------------------------
Destructor TFloodItem.Destroy;
begin
	Path.Free;
	inherited;
end;
//------------------------------------------------------------------------------


(*- Function ------------------------------------------------------------------*
TFloodItem.ManhattanCost
--------------------------------------------------------------------------------
Overview:
--
	Computes the Manhattan distance (like travelling in a grid of blocks in
downtown Manhattan) from the Position to the GoalPt.  Simple, easy to compute,
but not preferred when Diagonal pathing is allowed.

--
Revisions:
--
[2007/04/30] CR - Added Routine
*-----------------------------------------------------------------------------*)
Function TFloodItem.ManhattanCost(
	const
		GoalPt : TPoint
	) : Word;
Begin
	Result := 5 * (
		Abs(GoalPt.X - Position.X) + Abs(GoalPt.Y - Position.Y)
	);
End;(* Func TFloodItem.ManhattanCost
*-----------------------------------------------------------------------------*)


(*- Function ------------------------------------------------------------------*
TFloodItem.DiagonalCost
--------------------------------------------------------------------------------
Overview:
--
	Computes the diagonal cost from the Position point to the supplied GoalPt.
This heuristic distance is usually prefered to the Manhattan distance when
diagonal moves are allowed.

	In our case, diagonal moves ARE allowed on RO maps.

	The 5's and 7's are the integer equivalents of 1.0 and 2^(1/2) for the true
distance cost for straight line and diagonal moves.

--
Revisions:
--
[2007/04/30] CR - Added Routine
*-----------------------------------------------------------------------------*)
Function TFloodItem.DiagonalCost(
	const
		GoalPt : TPoint
	) : Word;
Var
	AbsX : Integer;
	AbsY : Integer;
Begin
	AbsX := Abs(GoalPt.X - Position.X);
	AbsY := Abs(GoalPt.Y - Position.Y);

	if (AbsX > AbsY) then
	begin
		if (AbsY > 0) then
		begin
			Dec(AbsX);
		end;
		Result := 7 * AbsY + 5 * AbsX;
	end else begin
		if (AbsX > 0) then
		begin
			Dec(AbsY);
		end;
		Result := 7 * AbsX + 5* AbsY;
	end;
End;(* Func TFloodItem.DiagonalCost
*-----------------------------------------------------------------------------*)


end.
