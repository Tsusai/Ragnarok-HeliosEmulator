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
  List32;

type

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
    //what kind of tile is it
		Attribute       : Byte;
		//Tsusai 11/09/06: Keep track of the number of things in the way, like icewall(s)
		ObstructionCount: Byte;
    //TBeings in this cell (NPC/Mob/Chara)
    Beings    : TIntList32;
	end;

	TGraph = array of array of TCell;

	//flood types
	TFloodItem = record
		Position : TPoint;
		Path : array of TPoint;
		PathLength : Integer;
	end;

	TFloodList = array of TFloodItem;

implementation
end.
