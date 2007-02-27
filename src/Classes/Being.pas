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
	EventList,
	PointList;

//------------------------------------------------------------------------------
//TBeing                                                                  CLASS
//------------------------------------------------------------------------------
type TBeing = class
	private
	public
		ID        : LongWord;
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
		MapPointer: Pointer;
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
		Path      : TPointList;
		PathIndex : Word;
		MoveTick : LongWord;

		procedure Walk;
		Constructor Create();
		Destructor Destroy();override;
end;{TBeing}
//------------------------------------------------------------------------------


implementation
uses
	Character,
	Math,
	WinLinux;

procedure TBeing.Walk;
var
	spd : word;
	OldPt : TPoint;
	ABeing : TBeing;
	dx : ShortInt;
	dy : ShortInt;
	idxY : ShortInt;
	idxX : ShortInt;
	BIdx : integer;
begin
	//Setup first speed
	OldPt := Point;
	Point := Path[PathIndex];
	dx := Point.X - OldPt.X;
	dy := Point.Y - OldPt.Y;
	//Check to see if we're moving diagonally, if we are, we adjust the speed
	//accordingly.
	if not (abs(dx) = abs(dy)) then
	begin
		spd := Speed * 7 div 5;
	end else begin
		spd := Speed;
	end;

	//Apply speed adjustments.
	MoveTick := GetTick + spd;

	while {Gamestate = walking}true do
	begin
		if GetTick < Movetick then Continue; //haven't made it to the next cell yet
//			for loop := 1 to Integer((Gettime - Movetick) div spd) do
//			begin
//			OltPt := Point;
//			Point := Path[PathIndex];
//			dx := Point.X - OltPt.X;
//			dy := Point.Y - OltPt.Y;

		PathIndex := Min(PathIndex + 1, Path.Count-1);

		//16 covers the old 15x15 grid, no matter which dir we go I think
		for idxY := Max(OldPt.Y-CHAR_SHOWAREA,0) to Min(OldPt.Y+CHAR_SHOWAREA,MapInfo.Size.Y) do
		begin
			for idxX := Max(OldPt.X-CHAR_SHOWAREA,0) to Min(OldPt.X+CHAR_SHOWAREA,MapInfo.Size.X) do
			begin
				for BIdx := MapInfo.Cell[idxX][idxY].Beings.Count - 1 to 0 do
				begin
					ABeing := MapInfo.Cell[idxX][idxY].Beings.Objects[BIdx] as TBeing;

					if Self = ABeing then Continue;

					if ((dx <> 0) and (abs(OldPt.Y - ABeing.Point.Y) < CHAR_SHOWAREA) and (OldPt.X = ABeing.Point.X + dx * (CHAR_SHOWAREA-1))) OR
						((dy <> 0) and (abs(OldPt.X - ABeing.Point.X) < CHAR_SHOWAREA) and (OldPt.Y = ABeing.Point.Y + dy * (CHAR_SHOWAREA-1))) then
					begin
						//Packets for base being if its a character
						if Self is TCharacter then
						begin
							if ABeing is TCharacter then
							begin
								//Send First Being disapearing to ABeing
							end;
							//if ABeing is NPC
							//Special npc packets
							//else
							//Send basic disapear packet of ABeing to First Being
						end;
					end;

					if ((dx <> 0) and (abs(Point.Y - ABeing.Point.Y) < CHAR_SHOWAREA) and (Point.X = ABeing.Point.X - dx * (CHAR_SHOWAREA-1))) or
						((dy <> 0) and (abs(Point.X - ABeing.Point.X) < CHAR_SHOWAREA) and (Point.Y = ABeing.Point.Y - dy * (CHAR_SHOWAREA-1))) then
					begin
						if Self is TCharacter then
						begin
							if ABeing is TCharacter then
							begin
								//Send First Being apearing to ABeing
								{if Point in range with ABeing, 15 then
								begin
									SendWalking packet
								end;}
							end;
							//if ABeing is NPC
							//Special npc packets
							//else
							//Send basic Appear packet of ABeing to First Being
						end;
					end;
				end;
			end;
		end;

		if (PathIndex = Path.Count - 1) {or (GameState = charaStand)} then
		begin
			{if GameState = charaStand then
			begin
				//UpdateLocation
				//So we can make as if we just steped on a trap or were frozen, etc, stop where
				//we are and update location.
			end;}

			//GameState := charaStand;

			{if (AChara.Skill[144].Lv = 0) then
			begin
				HPTick := Tick;
			end;

			HPRTick := Tick - 500;
			SPRTick := Tick;
			PathIndex := 0;}

			Break;
		end;

		//Setup first speed
		OldPt := Point;
		Point := Path[PathIndex];
		dx := Point.X - OldPt.X;
		dy := Point.Y - OldPt.Y;
		if not (abs(dx) = abs(dy)) then
		begin
			spd := Speed * 7 div 5;
		end else begin
			spd := Speed;
		end;

		MoveTick := MoveTick + spd;
	end;
end;

Constructor TBeing.Create;
begin
	inherited;
	EventList := TEventList.Create(TRUE);
	Path := TPointList.Create;
end;

Destructor TBeing.Destroy;
begin
	inherited;
	EventList.Free;
	Path.Free;
end;
end.
