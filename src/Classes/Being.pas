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
type
TBeing = class;

TLoopCall = procedure(X, Y: Integer; ACurrentBeing, ABeing: TBeing);

TBeing = class
	protected
		fName             : String;
		fJID              : Word;
		fBaseLV           : Byte;
		fJobLV            : Byte;
		fBaseEXP          : LongWord;
		fJobEXP           : LongWord;
		fZeny             : LongWord;
		fParamBase        : Array[STR..LUK] of Byte;
		fMaxHP            : Word;
		fHP               : Word;
		fMaxSP            : Word;
		fSP               : Word;
		fStatus           : Word;
		fAilments         : Word;
		fOption           : Word;
		fMap              : String;
		fPosition         : TPoint;

		procedure SetName(Value : string); virtual;
		procedure SetClass(Value : word); virtual;
		procedure SetBaseLV(Value : byte); virtual;
		procedure SetJobLV(Value : byte); virtual;
		procedure SetBaseEXP(Value : LongWord); virtual;
		procedure SetJobEXP(Value : LongWord); virtual;
		procedure SetZeny(Value : LongWord); virtual;
		function  GetBaseStats(Index : Byte) : byte;  virtual;
		procedure SetBaseStats(Index: byte; Value: byte); virtual;
		procedure SetMaxHP(Value : word); virtual;
		procedure SetHP(Value : word); virtual;
		procedure SetMaxSP(Value : word); virtual;
		procedure SetSP(Value : word); virtual;
		Procedure SetStatus(Value : word); virtual;
		Procedure SetAilments(Value : word); virtual;
		Procedure SetOption(Value : word); virtual;
		procedure SetMap(Value : string); virtual;
		procedure SetPosition(Value : TPoint); virtual;
	private
		procedure AreaLoop(ALoopCall:TLoopCall; AIgnoreCurrentBeing:Boolean=True);

	public
		ID					: LongWord;
		MapPointer	: Pointer;
		Speed 			: word;
		HeadDirection	: word;
		Direction 	: byte;

		AttackRange	: word;
		//No idea what 0..5 is from.  Stats?
		ATK					: Word;

		//For Mobs and NPCs, Leave #2's alone (0), and use #1s
		MATK1				: word;
		MATK2				: word;
		DEF1				: word;
		DEF2				: word;
		MDEF1				: word;
		MDEF2				: word;
		HIT					: word;
		FLEE1				: word;
		Lucky				: word;
		Critical		: word;
		ASpeed			: word;

		MapInfo			: TMap;
		EventList		: TEventList;
		Path				: TPointList;
		PathIndex		: Word;
		MoveTick		: LongWord;

		property Name      : string     read fName write SetName;
		property JID       : Word       read fJID write SetClass;
		property BaseLV    : Byte       read fBaseLV write SetBaseLV;
		property JobLV     : Byte       read fJobLV write SetJobLV;
		property ParamBase[Index : byte] : byte read GetBaseStats write SetBaseStats;
		property MaxHP     : Word       read fMaxHP write SetMaxHP;
		property HP        : Word       read fHP write SetHP;
		property MaxSP     : Word       read fMaxSP write SetMaxSP;
		property SP        : Word       read fSP write SetSP;
		property Status    : Word       read fStatus write SetStatus;
		property Ailments  : Word       read fAilments write SetAilments;
		property Option    : Word       read fOption write SetOption;
		property Map       : string     read fMap write SetMap;
		property Position  : TPoint     read fPosition write SetPosition;

		procedure Walk;
		procedure CalcMaxHP; virtual;
		procedure CalcMaxSP; virtual;
		procedure CalcSpeed; virtual;

		procedure ShowCharactersWalking;
		procedure ShowTeleportIn;
		procedure ShowTeleportOut;
		procedure UpdateDirection;

		Constructor Create();
		Destructor Destroy();override;
end;{TBeing}
//------------------------------------------------------------------------------


implementation
uses
	Event,
	MovementEvent,
	Character,
	Globals,
	Math,
	Classes,
	SysUtils,
	WinLinux,
	Main,
	ZoneSend,
	AreaLoopEvents;


//------------------------------------------------------------------------------
//Walk								                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      The internal walk routine, moves a character cell by cell attempting to
//		follow the way the client is moving on screen.
//
//  Changes -
//    February 27th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
procedure TBeing.Walk;
var
	spd					: word;
	AMoveEvent	: TMovementEvent;
	OldPt				: TPoint;
	ABeing			: TBeing;
	idxY				: SmallInt;
	idxX				: SmallInt;
	BeingIdx		: integer;

	//Gets our new direction
	function GetDirection(OldPoint : TPoint; NewPoint : TPoint) : Byte;
	var
		DirectionPoint	: TPoint;
		Index						: Integer;
	begin
		Result := 0;
		DirectionPoint := Point(NewPoint.X-OldPoint.X, NewPoint.Y-OldPoint.Y);

		for Index := 0 to 7 do
		begin
			if PointsEqual(DirectionPoint, Directions[Index]) then
			begin
				Result := Byte(Index);
				Break;
			end;
		end;
	end;

begin
	if Self is TCharacter then
	begin
		TCharacter(Self).CharaState := charaWalking;
	end;

	MapInfo.Cell[Position.X,Position.Y].Beings.Delete(
		MapInfo.Cell[Position.X,Position.Y].Beings.IndexOfObject(Self));

	OldPt			:= Position;
	Position	:= Path[PathIndex];
	Direction := GetDirection(OldPt, Position);
	
	MapInfo.Cell[Position.X,Position.Y].Beings.AddObject(Self.ID,Self);

	//16 covers the old 15x15 grid, no matter which dir we go I think
	for idxY := Max(OldPt.Y-MainProc.ZoneServer.Options.CharShowArea,0) to Min(OldPt.Y+MainProc.ZoneServer.Options.CharShowArea,MapInfo.Size.Y) do
	begin
		for idxX := Max(OldPt.X-MainProc.ZoneServer.Options.CharShowArea,0) to Min(OldPt.X+MainProc.ZoneServer.Options.CharShowArea,MapInfo.Size.X) do
		begin
			for BeingIdx := MapInfo.Cell[idxX,idxY].Beings.Count - 1 downto 0 do
			begin
				ABeing := MapInfo.Cell[idxX,idxY].Beings.Objects[BeingIdx] as TBeing;
				if Self = ABeing then Continue;
				if ((Directions[Direction].X <> 0) and (abs(OldPt.Y - ABeing.Position.Y) < MainProc.ZoneServer.Options.CharShowArea) and (OldPt.X = ABeing.Position.X + Directions[Direction].X * (MainProc.ZoneServer.Options.CharShowArea-1))) OR
					((Directions[Direction].Y <> 0) and (abs(OldPt.X - ABeing.Position.X) < MainProc.ZoneServer.Options.CharShowArea) and (OldPt.Y = ABeing.Position.Y + Directions[Direction].Y * (MainProc.ZoneServer.Options.CharShowArea-1))) then
				begin
					//Packets for base being if its a character
					if Self is TCharacter then
					begin
						if ABeing is TCharacter then
						begin
							 ZoneDisappearChar(TCharacter(Self),TCharacter(ABeing).ClientInfo);
							 ZoneDisappearChar(TCharacter(ABeing),TCharacter(Self).ClientInfo);
							//Send First Being disapearing to ABeing
						end;
						//if ABeing is NPC
						//Special npc packets
						//else
						//Send basic disapear packet of ABeing to First Being
					end;
				end;

				if ((Directions[Direction].X <> 0) and (abs(Position.Y - ABeing.Position.Y) < MainProc.ZoneServer.Options.CharShowArea) and (Position.X = ABeing.Position.X - Directions[Direction].X * (MainProc.ZoneServer.Options.CharShowArea-1))) or
					((Directions[Direction].Y <> 0) and (abs(Position.X - ABeing.Position.X) < MainProc.ZoneServer.Options.CharShowArea) and (Position.Y = ABeing.Position.Y - Directions[Direction].Y * (MainProc.ZoneServer.Options.CharShowArea-1))) then
				begin
					if Self is TCharacter then
					begin
						if ABeing is TCharacter then
						begin
							ZoneSendChar(TCharacter(Self),TCharacter(ABeing).ClientInfo);
							ZoneSendChar(TCharacter(ABeing),TCharacter(Self).ClientInfo);
							ZoneWalkingChar(TCharacter(Self),Path[Path.count-1],Position,TCharacter(ABeing).ClientInfo);
						end;
						{ TODO : Send NPC visual packets }
						//if ABeing is NPC
						//Special npc packets
						//else
						//Send basic Appear packet of ABeing to First Being
					end;
				end;
			end;
		end;
	end;

	if Self is TCharacter then
	begin
		if not (TCharacter(Self).CharaState = charaWalking) then
		begin
			PathIndex := Path.Count -1;
		end;
	end;

	if (PathIndex = Path.Count - 1) then
	begin
		if Self is TCharacter then
		begin
			TCharacter(Self).CharaState := charaStanding;
		end;

		{ TODO : Need to call this so we can interupt as if a trap sprung. }
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
	end else
	begin
		//Setup first speed
		Inc(PathIndex);

		if (Self.Direction in Diagonals) then
		begin
			spd := Speed * 7 DIV 5;
		end else begin
			spd := Speed;
		end;
		MoveTick := MoveTick + spd;

		AMoveEvent := TMovementEvent.Create(MoveTick,Self);
		AMoveEvent.ExpiryTime := MoveTick;
		Self.EventList.Add(AMoveEvent);
	end;

end;//Walk
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ViewAreaLoop                                                         PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      master procedure for Area loop, and call Dynamic Sub Procedure.
//
//  Changes -
//    March 20th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure TBeing.AreaLoop(ALoopCall:TLoopCall; AIgnoreCurrentBeing:Boolean=True);
var
		idxY : integer;
		idxX : integer;
		BeingIdx : integer;
		ABeing : TBeing;
begin
	for idxY := Max(0,Position.Y-MainProc.ZoneServer.Options.CharShowArea) to Min(Position.Y+MainProc.ZoneServer.Options.CharShowArea, MapInfo.Size.Y) do
	begin
		for idxX := Max(0,Position.X-MainProc.ZoneServer.Options.CharShowArea) to Min(Position.X+MainProc.ZoneServer.Options.CharShowArea, MapInfo.Size.X) do
		begin
			for BeingIdx := MapInfo.Cell[idxX][idxY].Beings.Count -1 downto 0 do
			begin
				ABeing := MapInfo.Cell[idxX][idxY].Beings.Objects[BeingIdx] as TBeing;
				if (Self = ABeing) and (AIgnoreCurrentBeing) then Continue;
				ALoopCall(Position.X, Position.Y, Self, ABeing);
			end;
		end;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ShowCharactersWalking                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//	Show other character that you are walking...
//
//  Changes -
//	March 20th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure TBeing.ShowCharactersWalking;
begin
	AreaLoop(ShowCharWalk, True);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ShowTeleportIn                                                       PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
// 	Show teleport in effect to other characters
//
//  Changes -
//	March 20th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure TBeing.ShowTeleportIn;
begin
	AreaLoop(ShowTeleIn, True);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ShowTeleportOut                                                      PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
// 	Show teleport out effect to other characters
//
//  Changes -
//	March 20th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure TBeing.ShowTeleportOut;
begin
	AreaLoop(TeleOut, True);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//UpdateDirection                                                      PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
// 	Update Direction to other characters
//
//  Changes -
//	March 20th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure TBeing.UpdateDirection;
begin
	AreaLoop(UpdateDir, True);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Create							                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Creates our TBeing.
//
//  Changes -
//    February 27th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
Constructor TBeing.Create;
begin
	inherited;
	EventList := TEventList.Create(TRUE);
	Path := TPointList.Create;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy							                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Destroys our TBeing
//
//  Changes -
//    February 27th, 2007 - RaX - Created Header;
//------------------------------------------------------------------------------
Destructor TBeing.Destroy;
begin
	inherited;
	EventList.Free;
	Path.Free;
end;
//------------------------------------------------------------------------------

procedure TBeing.SetName(Value : string); begin fName := Value; end;
procedure TBeing.SetClass(Value : Word); begin fJID := Value; end;
procedure TBeing.SetBaseLV(Value : byte); begin fBaseLV := Value; end;
procedure TBeing.SetJobLV(Value : byte); begin fJobLV := Value; end;
procedure TBeing.SetBaseEXP(Value : LongWord); begin fBaseEXP := Value; end;
procedure TBeing.SetJobEXP(Value : LongWord); begin fJobEXP := Value; end;
procedure TBeing.SetZeny(Value : LongWord); begin fZeny := Value; end;
function  TBeing.GetBaseStats(Index : Byte) : Byte; begin Result := fParamBase[Index]; end;
procedure TBeing.SetBaseStats(Index: Byte; Value: Byte); begin fParamBase[Index] := Value; end;
procedure TBeing.SetMaxHP(Value : word); begin fMaxHP := Value; end;
procedure TBeing.SetHP(Value : word); begin fHP := Value; end;
procedure TBeing.SetMaxSP(Value : word); begin fMaxSP := Value; end;
procedure TBeing.SetSP(Value : word); begin fSP := Value; end;
Procedure TBeing.SetStatus(Value : word); begin fStatus := Value; end;
Procedure TBeing.SetAilments(Value : word); begin fAilments := Value; end;
Procedure TBeing.SetOption(Value : word); begin fOption := Value; end;
procedure TBeing.SetMap(Value : string); begin fMap := Value; end;
procedure TBeing.SetPosition(Value : TPoint); begin fPosition := Value; end;

procedure TBeing.CalcMaxHP; begin end;
procedure TBeing.CalcMaxSP; begin end;
procedure TBeing.CalcSpeed; begin Speed := 150; end;


end.
