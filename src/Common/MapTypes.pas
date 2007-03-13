//------------------------------------------------------------------------------
//MapTypes                                                                 UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Contains Map related Types 
//
//	Changes -
//		January 22d, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
unit MapTypes;
interface
uses
  Types,
	List32,
	PointList;

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
  end;

	//graph related types
	TCell = record
		//The place in a graph the cell is
		Position : TPoint;
		//what kind of tile is it
		Attribute       : Byte;
		//Tsusai 11/09/06: Keep track of the number of things in the way, like icewall(s)
		ObstructionCount: Byte;
    //TBeings in this cell (NPC/Mob/Chara)
		Beings    : TIntList32;
	end;

	TGraph = array of array of TCell;

	TFloodItem = class
		public
			Path					: TPointList;
			Position			: TPoint;
			Cost					: Cardinal;

			Constructor Create;
			Destructor Destroy; override;
  end;
implementation

Constructor TFloodItem.Create;
begin
	inherited;
	Path		:= TPointList.Create;
	Cost		:= 0;
end;

Destructor TFloodItem.Destroy;
begin
	Path.Free;
	inherited;
end;

end.
