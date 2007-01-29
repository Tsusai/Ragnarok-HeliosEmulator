//------------------------------------------------------------------------------
//Being                                                                   UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Contains RO environment type TBeing.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit Being;

interface
uses
	Types,
	GameConstants,
	Map,
	EventList;

//------------------------------------------------------------------------------
//TBeing                                                                  CLASS
//------------------------------------------------------------------------------
type TBeing = class
	private
	public
		ID  : Cardinal;
		Name      : string;
		JID       : Word;
		BaseLV    : Byte;
		JobLV     : Byte;
		ParamBase : array[STR..LUK] of byte;
		MaxHP     : Word;
		HP        : Word;
		MaxSP     : Word;
		SP        : Word;
		Option    : Word;
		Map       : string;
		Point     : TPoint;
		Speed : word;

		IsWalking : boolean;
		DestinationPoint : TPoint;


		Direction : byte;

		AttackRange : word;
		//No idea what 0..5 is from.  Stats?
		ATK : Word;

		//For Mobs and NPCs, Leave #2's alone (0), and use #1s
		MATK1 : word;
		MATK2 : word;
		DEF1 : word;
		DEF2 : word;
		MDEF1 : word;
		MDEF2 : word;
		HIT : word;
		FLEE1 : word;
		Lucky : word;
		Critical : word;
		ASpeed : word;

		MapInfo : TMap;
		EventList : TEventList;

		Constructor Create();
		Destructor Destroy();override;
end;{TBeing}
//------------------------------------------------------------------------------


implementation

Constructor TBeing.Create;
begin
	inherited;
	EventList := TEventList.Create(TRUE);
end;

Destructor TBeing.Destroy;
begin
	inherited;
	EventList.Free;
end;
end.
