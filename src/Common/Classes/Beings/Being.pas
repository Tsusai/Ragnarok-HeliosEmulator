(*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

Unit
Being

*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

[2006/??/??] Helios - No author stated

================================================================================
License:  (FreeBSD, plus commercial with written permission clause.)
================================================================================

Project Helios - Copyright (c) 2005-2007

All rights reserved.

Please refer to Helios.dpr for full license terms.

================================================================================
Overview:
================================================================================

Class, interface, and common defaults for a "Being" are defined here.
Characters, NPCs, and Monsters will all be derived from this base.

================================================================================
Revisions:
================================================================================
(Format: [yyyy/mm/dd] <Author> - <Desc of Changes>)
[2006/12/22] RaX - Created Header.
[2007/03/28] CR - Cleaned up uses clauses - unneeded units removed.
[2007/03/28] CR - Made changes to parameter lists for the TLoopCall
	declaration.  All parameteters are constant, and eliminated the entirely
	uncalled X,Ys so that we only have 2 parameters left (faster calls this
	way, especially when called repeatedly in triple nested loops!).
[2007/03/28] CR - Cleaned up uses clauses using Icarus as a guide.
[2007/04/28] CR - Altered unit header.  Improved unit description.  Added class
	header for TBeing, described minor changes to the interface there.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)
unit Being;


interface


uses
	{RTL/VCL}
	Types,
	Classes,
	{Project}
	GameConstants,
	EventList,
	Map,
	PointList,
	SysUtils
	{Third Party}
	//none
	;

type

StatArray = array [STR..LUK] of Integer;

TBeingZoneStatus = (
		isOffline,
		isOnline
	);

TBeing = class; //Forward Declaration.

{[2007/03/28] CR - No X,Y parameters needed -- reduced and eliminated. }
TLoopCall = procedure(
		const ACurrentBeing : TBeing;
		const ABeing        : TBeing;
		const AParameters   : Cardinal
		);

(*= CLASS =====================================================================*
TBeing

*------------------------------------------------------------------------------*
Overview:
*------------------------------------------------------------------------------*

	Common class for the Character, NPC, and Monster classes (some of which have
yet to be defined).  The base properties and routines common to all will be
contained here.

*------------------------------------------------------------------------------*
Revisions:
*------------------------------------------------------------------------------*
(Format: [yyyy/mm/dd] <Author> - <Description of Change>)
[2007/04/28] CR - Altered header, included description.  Made empty routines
	CalcMaxHP and CalcMaxSP abstract.  Eliminated private section, moved
	AreaLoop into protected.
[2007/04/28] CR - Changed fParamBase from an array to an equivalent type of
	ByteStatArray.
[2007/05/28] Tsusai - Removed MapPointer
*=============================================================================*)
TBeing = class(TObject)
protected
	fName             : String;
	fJID              : Word;
	fBaseLV           : Word;
	fJobLV            : Word;
	fBaseEXP          : LongWord;
	fJobEXP           : LongWord;
	fBaseEXPToNextLevel: LongWord;
	fJobEXPToNextLevel: LongWord;
	fZeny             : Integer;
	fParamBase        : StatArray;
	fMaxHP            : Word;
	fHP               : Word;
	fMaxSP            : Word;
	fSP               : Word;
	fStatus           : Word;
	fAilments         : Word;
	fOption           : Word;
	fMap              : String;
	fPosition         : TPoint;
	fSpeed            : Word;
	fASpeed						: Word;

	AttackDelay				: LongWord;

	procedure SetName(
		const
			Value : String
		); virtual;

	procedure SetClass(Value : word); virtual;
	procedure SetBaseLV(Value : word); virtual;
	procedure SetJobLV(Value : word); virtual;
	procedure SetBaseEXP(Value : LongWord); virtual;
	procedure SetJobEXP(Value : LongWord); virtual;
	procedure SetZeny(Value : Integer); virtual;

	Function  GetBaseStats(
		const
			Index : Byte
		) : Integer;  virtual;

	procedure SetBaseStats(
		const
			Index : Byte;
		const
			Value : Integer
		); virtual;

	procedure SetMaxHP(Value : word); virtual;
	procedure SetHP(Value : word); virtual;
	procedure SetMaxSP(Value : word); virtual;
	procedure SetSP(Value : word); virtual;
	Procedure SetStatus(Value : word); virtual;
	Procedure SetAilments(Value : word); virtual;
	Procedure SetOption(Value : word); virtual;
	procedure SetMap(Value : string); virtual;
	procedure SetPosition(Value : TPoint); virtual;
	procedure SetSpeed(Value : Word); virtual;
	procedure SetASpeed(Value : Word); virtual;
	function GetHpPercent: Byte;
	procedure SetHPPercent(Value : Byte);
	function GetSpPercent: Byte;
	procedure SetSPPercent(Value : Byte);
public
	ID					: LongWord;
	HeadDirection	: Word;
	Direction 	: Byte;

	AttackRange	: word;
	//No idea what 0..5 is from.  Stats?
	ATK					: Word;

	//For Mobs and NPCs, Leave #2's alone (0), and use #1s
	MATK1				: Word;
	MATK2				: Word;
	DEF1				: Word;
	DEF2				: Word;
	MDEF1				: Word;
	MDEF2				: Word;
	HIT					: Word;
	FLEE1				: Word;
	Lucky				: Word;
	Critical		: Word;

	MapInfo			: TMap;
	EventList		: TEventList;
	Path				: TPointList;
	PathIndex		: Word;
	MoveTick		: LongWord;
	ZoneStatus		: TBeingZoneStatus; //Is the being online in the Zone or not?
	
	property Name      : string     read fName    write SetName;
	property JID       : Word       read fJID     write SetClass;
	property BaseLV    : Word       read fBaseLV  write SetBaseLV;
	property JobLV     : Word       read fJobLV   write SetJobLV;
	property BaseEXP   : LongWord   read fBaseEXP write fBaseEXP;
	property JobEXP    : LongWord   read fJobEXP  write fJobEXP;
	property BaseEXPToNextLevel : LongWord read fBaseEXPToNextLevel write fBaseEXPToNextLevel;
	property JobEXPToNextLevel  : LongWord read fJobEXPToNextLevel  write fJobEXPToNextLevel;
	property ParamBase[const Index : Byte] : Integer
		read  GetBaseStats
		write SetBaseStats;
	property MaxHP     : Word       read fMaxHP write SetMaxHP;
	property HP        : Word       read fHP write SetHP;
	property HPPercent : Byte       read GetHpPercent write SetHPPercent;
	property SPPercent : Byte       read GetSpPercent write SetSPPercent;
	property MaxSP     : Word       read fMaxSP write SetMaxSP;
	property SP        : Word       read fSP write SetSP;
	property Status    : Word       read fStatus write SetStatus;
	property Ailments  : Word       read fAilments write SetAilments;
	property Option    : Word       read fOption write SetOption;
	property Map       : string     read fMap write SetMap;
	property Position  : TPoint     read fPosition write SetPosition;
	property Speed     : Word       read fSpeed write SetSpeed;
	property ASpeed		 : Word				read fASpeed write SetASpeed;
	Procedure Walk;

	Procedure CalcMaxHP; virtual; abstract;

	Procedure CalcMaxSP; virtual; abstract;

	Procedure CalcSpeed; virtual;
	procedure CalcASpeed; virtual;
	procedure DelayDisconnect(ExpireTime:Integer);

	procedure ShowBeingWalking;
	procedure ShowTeleportIn;
	procedure ShowTeleportOut;
	procedure UpdateDirection;
	procedure ShowEffect(EffectID:LongWord);
	procedure Attack(ATargetID : LongWord; AttackContinuous : Boolean = false);virtual;

	procedure AreaLoop(
			ALoopCall           : TLoopCall;
			AIgnoreCurrentBeing : Boolean = True;
			IgnoreMob           : Boolean = True;
			IgnoreNPC           : Boolean = True;
			AParameter          : Cardinal = 0
		);

	procedure Death; virtual;

	procedure RemoveFromMap;
  procedure AddToMap;
	Constructor Create;
	Destructor Destroy;override;

End;(* TBeing
*== CLASS ====================================================================*)


implementation


uses
	{RTL/VCL}
	Math,
	SyncObjs,
	{Project}
	AreaLoopEvents,
	Character,
	Event,
	Main,
	MovementEvent,
	DelayDisconnectEvent,
	ZoneSend,
	OnTouchCellEvent,
	Globals
	{Third Party}
	//none
	;


(*- Procedure -----------------------------------------------------------------*
TBeing.Walk
--------------------------------------------------------------------------------
Overview:
--
	The internal walk routine, moves a character cell by cell attempting to
follow the way the client is moving on screen.

--
Pre:
	TODO
Post:
	TODO

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/02/27] RaX - Created Header
[2007/04/28] CR - Altered comment header.  Added scathing comments for the lack
	of explanation for what REALLY goes in within this routine when two of the
	devs working on this project had to deal with poorly documented code before-
	hand and SHOULD KNOW BETTER than to leave code of this complexity unadorned
	with ANY sensible comments.  On a positive note, the explanation for the
	routine was fine.
[2007/05/28] Tsusai - Mixed OnTouch to only run just once if a character is
	in the walking ontouch field.
*-----------------------------------------------------------------------------*)
Procedure TBeing.Walk;
Var
	spd        : Word;
	Index			 : Integer;
	AMoveEvent : TMovementEvent;
	OldPt      : TPoint;
	idxY       : SmallInt;
	idxX       : SmallInt;
	Radius     : Word;
	OnTouchCellFound : boolean;
	OnTouchCell : TOnTouchCellEvent;

	(*. local function ...................*
	Gets our new direction
	Using basic mathematics and an existing array, we are able to get a direction in the form of the following
	where X is the current cell, 0 = North, 4 = South, 2 = West, 6 = East
		107
		2X6
		345

	[2007/04/28] CR - Header, made parameters constant, changed for loop Index
		from Integer to Byte, to eliminate a cast to Byte.

	*....................................*)
	function GetDirection(
		const
			OldPoint : TPoint;
		const
			NewPoint : TPoint
		) : Byte;
	var
		DirectionPoint : TPoint;
		Index          : Byte;
	begin
		Result := 0;
		DirectionPoint := Point(NewPoint.X-OldPoint.X, NewPoint.Y-OldPoint.Y);

		for Index := 0 to 7 do
		begin
			if PointsEqual(DirectionPoint, Directions[Index]) then
			begin
				Result := Index;
				Break;
			end;
		end;
	end;(*. local function .............*)

	procedure HideBeings;
	var
		ABeing : TBeing;
		BeingIdx : Integer;
	begin
		//Ok this took me some time to setup just right
		//First, what we're trying to determine is if we moved in a particular
		//location, a row or column of cells should be leaving the being's field
		//of view

		//Example (using change of X), We are changing X.  Then we see if they are
		//within our Y visual range. Last check is that they left our visual range
		//on the X factor

		//Check to make sure that we're inside the map edges by one to prevent
		//looking outside of the map bounds.
		if (IdxX < Mapinfo.Size.X-1) AND (IdxX > 0) AND
			 (IdxY < MapInfo.Size.Y-1) AND (IdxY > 0) then
		begin
			if {X axis}((Directions[Direction].X <> 0) and
				(abs(OldPt.Y - idxY) < Radius) and
				(OldPt.X = idxX + Directions[Direction].X * (Radius - 1 )))
			OR {Y axis}((Directions[Direction].Y <> 0) and
				(abs(OldPt.X - idxX) < Radius) and
				(OldPt.Y = idxY + Directions[Direction].Y * (Radius - 1))) then
			begin
				for BeingIdx := MapInfo.Cell[idxX,idxY].Beings.Count -1  downto 0 do
				begin
					if MapInfo.Cell[idxX,idxY].Beings.Objects[BeingIdx] is TBeing then
					begin
						ABeing := MapInfo.Cell[idxX,idxY].Beings.Objects[BeingIdx] as TBeing;
						if ABeing = Self then Continue;

						//Packets for base being if its a character
						if Self is TCharacter then
						begin
							//if the target is also a TCharacter, they need OUR info
							if ABeing is TCharacter then
							begin
								ZoneDisappearBeing(Self,   TCharacter(ABeing).ClientInfo);
								ZoneDisappearBeing(ABeing, TCharacter(Self).ClientInfo);
								//Send First Being disapearing to ABeing
							end else  //Npc/Mob/Pet/Homunculus/Mercenary
							begin
								{Todo: events for NPC}
								ZoneDisappearBeing(ABeing,TCharacter(Self).ClientInfo);
							end;
						end;
					end;
				end;
			end;
		end;
	end;

	procedure ShowBeings;
	var
		ABeing : TBeing;
		BeingIdx : Integer;
	begin
		//Check to make sure that we're inside the map edges by one to prevent
		//looking outside of the map bounds.
		if (IdxX < Mapinfo.Size.X-1) AND (IdxX > 0) AND
			 (IdxY < MapInfo.Size.Y-1) AND (IdxY > 0) then
		begin
			//This is the opposite of the above.  We check to see if we are making the apropriate change, and seeing if a tbeing will be in the visual range if we made the step forward
			if {X axis}((Directions[Direction].X <> 0) and
				(abs(Position.Y - idxY) < Radius) and
				(Position.X = idxX - Directions[Direction].X * (Radius - 1)))
			OR {Y axis}((Directions[Direction].Y <> 0) and
				(abs(Position.X - idxX) < Radius) and
				(Position.Y = idxY - Directions[Direction].Y * (Radius - 1))) then
			begin
				for BeingIdx := MapInfo.Cell[idxX,idxY].Beings.Count -1  downto 0 do
				begin
					if MapInfo.Cell[idxX,idxY].Beings.Objects[BeingIdx] is TBeing then
					begin
						ABeing := MapInfo.Cell[idxX,idxY].Beings.Objects[BeingIdx] as TBeing;
						if ABeing = Self then Continue;

						//If we are a tcharacter, we need packets!
						if Self is TCharacter then
						begin
							//If the target TBeing is also a character, they need info on us.
							if ABeing is TCharacter then
							begin
								ZoneSendBeing(Self, TCharacter(ABeing).ClientInfo);
								ZoneSendBeing(ABeing, TCharacter(Self).ClientInfo);
								ZoneWalkingBeing(Self,Path[Path.count-1],Position,TCharacter(ABeing).ClientInfo);
							end else  //Npc/Mob/Pet/Homunculus/Mercenary packets to the client
							begin
								{Todo: events for NPC}
								ZoneSendBeing(ABeing,TCharacter(Self).ClientInfo);
							end;
						end;
					end;
				end;
			end;
		end;
	end;

Begin
	if PathIndex < Path.Count then
	begin
		//Setup visual radius
		Radius := MainProc.ZoneServer.Options.CharShowArea + 1;
		OnTouchCellFound := false;

		if Self is TCharacter then
		begin
			TCharacter(Self).CharaState := charaWalking;
		end;

		OldPt     := Position;

		Position	:= Path[PathIndex];
		Direction := GetDirection(OldPt, Position);

	//-Tsusai
	//17 (Radius) covers the old 16x16 grid, no matter which dir we go I think
	//This is some complicated mathematics here, so I hope I do explain this right.

	//Bounds are set on the for loop from -> to to prevent searching from outside
	//the known map if the being is close to the edge or corner.
	//In a pure situation, first two rows and last 2 rows, and first 2 columns and
	//the last two columns are checked.
	// This is how things should look on a 9x9 grid.
	(*
	XXXXXXXXXX
	XXXXXXXXXX
	XXOOOOOOXX
	XXOOOOOOXX
	XXOOOOOOXX
	XXOOOOOOXX
	XXOOOOOOXX
	XXXXXXXXXX
	XXXXXXXXXX
	*)

		//Go up the entire vertical axis
		for idxY := Max(OldPt.Y - Radius,0) to
			Min(OldPt.Y + Radius,MapInfo.Size.Y-1) do
		begin
			//if we are on the top 2 or bottom 2 rows, go across
			if (idxY = OldPt.Y - Radius) or
				(idxY = (OldPt.Y - Radius) + 1) or
				(idxY = OldPt.Y + Radius) or
				(idxY = (OldPt.Y + Radius) - 1) then
			begin
				//Go across the entire row
				for idxX := Max(OldPt.X - Radius,0) to
					Min(OldPt.X + Radius,MapInfo.Size.X-1) do
				begin
					HideBeings;
					ShowBeings;
				end;
			end else
			begin
				//Left most side
				idxX := Max(OldPt.X - Radius,0);
				HideBeings;
				ShowBeings;
				//Left 2nd column
				idxX := Max((OldPt.X - Radius) + 1,0);
				HideBeings;
				ShowBeings;
				//Right most side
				idxX := Min(OldPt.X + Radius,MapInfo.Size.X);
				HideBeings;
				ShowBeings;
				//2nd from right
				idxX := Min((OldPt.X + Radius) -1 ,MapInfo.Size.X);
				HideBeings;
				ShowBeings;

			end;
		end;

		if Self is TCharacter then
		begin
			if NOT (TCharacter(Self).CharaState = charaWalking) then
			begin
				PathIndex := Path.Count;
			end;

			//Check for ontouch events.
			for Index := MapInfo.Cell[Position.X][Position.Y].Beings.Count-1 downto 0 do
			begin
				if MapInfo.Cell[Position.X][Position.Y].Beings.Objects[Index] is TOnTouchCellEvent then
				begin
					if Self IS TCharacter then
					begin
						OnTouchCellFound := true;
						OnTouchCell := TOnTouchCellEvent(MapInfo.Cell[Position.X][Position.Y].Beings.Objects[Index]);
						if TCharacter(Self).OnTouchIDs.IndexOf(OnTouchCell.ScriptNPC.ID) = -1 then
						begin
							TCharacter(Self).OnTouchIDs.Add(OnTouchCell.ScriptNPC.ID);
							TCharacter(Self).CharaState := charaStanding;
							OnTouchCell.Execute(TCharacter(Self));
						end;
					end;
				end;
			end;

			if (not OnTouchCellFound) and (Self is TCharacter) then
			begin
				if TCharacter(Self).OnTouchIDs.Count > 0 then
				begin
					TCharacter(Self).OnTouchIDs.Clear;
				end;
			end;

		end;

		//Move to the next element in the path list.
		Inc(PathIndex);

		{[2007/04/28] CR - Why isn't this if branch part of the Speed Property? }
		if (Self.Direction IN Diagonals) then
		begin
			spd := Speed * 7 DIV 5;
		end else begin
			spd := Speed;
		end;
		MoveTick := MoveTick + spd;

		AMoveEvent := TMovementEvent.Create(MoveTick, Self);
		AMoveEvent.ExpiryTime := MoveTick;
		Self.EventList.Add(AMoveEvent);
	end else
	begin
		PathIndex := 0;

		if Self IS TCharacter then
		begin
			TCharacter(Self).CharaState := charaStanding;
		end;

  end;
End; (* Proc TBeing.Walk
*-----------------------------------------------------------------------------*)


//------------------------------------------------------------------------------
//DelayDisconnect                                                      PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Disconnect after time expire
//
//  Changes -
//    April 6h, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure TBeing.DelayDisconnect(ExpireTime:Integer);
var
	ADelayEvent:	TDelayDisconnectEvent;
begin
	ADelayEvent := TDelayDisconnectEvent.Create(ExpireTime,Self);
	ADelayEvent.ExpiryTime := ExpireTime;
	EventList.Add(ADelayEvent);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ViewAreaLoop                                                         PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      master procedure for Area loop, and call Dynamic Sub Procedure.
//
//  Changes -
//    March 20th, 2007 - Aeomin - Created
//------------------------------------------------------------------------------
procedure TBeing.AreaLoop(ALoopCall:TLoopCall; AIgnoreCurrentBeing:Boolean=True; IgnoreMob:Boolean=True; IgnoreNPC:Boolean=True;AParameter : Cardinal = 0);
var
		idxY : integer;
		idxX : integer;
		BeingIdx : integer;
		ABeing : TBeing;
begin
	for idxY := Max(0,Position.Y-MainProc.ZoneServer.Options.CharShowArea) to Min(Position.Y+MainProc.ZoneServer.Options.CharShowArea, MapInfo.Size.Y-1) do
	begin
		for idxX := Max(0,Position.X-MainProc.ZoneServer.Options.CharShowArea) to Min(Position.X+MainProc.ZoneServer.Options.CharShowArea, MapInfo.Size.X-1) do
		begin
			for BeingIdx := MapInfo.Cell[idxX][idxY].Beings.Count -1 downto 0 do
			begin
				if MapInfo.Cell[idxX][idxY].Beings.Objects[BeingIdx] is TBeing then
				begin
					if MapInfo.Cell[idxX][idxY].Beings.Objects[BeingIdx] is TBeing then
					begin
						ABeing := MapInfo.Cell[idxX][idxY].Beings.Objects[BeingIdx] as TBeing;
						if (Self = ABeing) and AIgnoreCurrentBeing then Continue;
						//if (ABeing is TNPC) and IgnoreNPC then Continue;
						{TODO : support other than TCharacter...}
						ALoopCall(Self, ABeing, AParameter);
					end;
				end;
			end;
		end;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ShowBeingWalking                                                     PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//	Show other character that you are walking...
//
//  Changes -
//	March 20th, 2007 - Aeomin - Created Header
//	March 23th, 2007 - Aeomin - Rename from ShowCharWalking to ShowBeingWalking
//------------------------------------------------------------------------------
procedure TBeing.ShowBeingWalking;
begin
	AreaLoop(ShowBeingWalk, True);
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
//Attack			                                                      PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
// 	Shows an attack and calculates damage.
//
//  Changes -
//	December 26th, 2007 - RaX - Created Header
//------------------------------------------------------------------------------
procedure TBeing.Attack(ATargetID : LongWord; AttackContinuous : Boolean = false);
begin

end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ShowEffect                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Send effect packet
//
//	Changes-
//		[2007/11/24] Aeomin - Created.
//------------------------------------------------------------------------------
procedure TBeing.ShowEffect(EffectID:LongWord);
begin
	AreaLoop(Effect, False, True, True, EffectID);
end;{ShowEffect}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Death                                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Fires when character dead.
//
//	Changes-
//		[2007/12/28] Aeomin - Created.
//------------------------------------------------------------------------------
procedure TBeing.Death;
begin
	if HP > 0 then
		fHP := 0;
end;{Death}
//------------------------------------------------------------------------------


(*- Cons ----------------------------------------------------------------------*
TBeing.Create
--------------------------------------------------------------------------------
Overview:
--

Creates our TBeing.

--
Post:
	EventList and Path lists are both created and initialized.

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/02/27] RaX - Created Header
[2007/04/28] CR - Altered Comment Header.  Added comment regarding inherited
	behavior.
*-----------------------------------------------------------------------------*)
Constructor TBeing.Create;
begin
	inherited;
	//Set inherited properties AFTER inherited call - ditto for creating owned
	//objects

	EventList := TEventList.Create(TRUE);
	Path := TPointList.Create;
End; (* Cons TBeing.Create
*-----------------------------------------------------------------------------*)


(*- Dest ----------------------------------------------------------------------*
TBeing.Destroy

--
Overview:
--
	Cleans up EventList and Path objects before the ancestor frees up its owned
resources.

--
Pre:
	EventList and Path are NON-NIL Objects
Post:
	EventList and Path are freed.

--
Revisions:
--
[2007/02/27] RaX - Created Header
[2007/04/28] CR - Altered header, added a better description.  Defined Pre and
	Post Conditons.  Added a comment stating that the inherited call must come
	last.  Bugfix: moved inherited call last so ancestor's cleanup happens after
	the EventList/Path are freed.
[yyyy/mm/dd] <Author> - <Comment>
*-----------------------------------------------------------------------------*)
Destructor TBeing.Destroy;
Begin
	//Pre
	Assert(Assigned(EventList), 'Pre: EventList is NIL');
	Assert(Assigned(Path), 'Pre: Path is NIL');
	//--

	EventList.Free;
	Path.Free;

	//Always clean up your owned objects/memory first, then call ancestor.
	inherited;
End;(* Dest TBeing.Destroy
*-----------------------------------------------------------------------------*)


(*- Procedure -----------------------------------------------------------------*
TBeing.SetName
--------------------------------------------------------------------------------
Overview:
--

	Property method for Name.

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/04/24] CR - Added Comment Header, made Value parameter const.
*-----------------------------------------------------------------------------*)
Procedure TBeing.SetName(
	const
		Value : String
	);
Begin
	fName := Value;
End; (* Proc TBeing.SetName
*-----------------------------------------------------------------------------*)

procedure TBeing.SetClass(
		Value : Word
	);
begin
	fJID := EnsureRange(Value, 0, High(Word));
end;

procedure TBeing.SetBaseLV(
		Value : Word
	);
begin
	fBaseLV := EnsureRange(Value, 0, High(Word));
end;

procedure TBeing.SetJobLV(
		Value : Word
	);
begin
	fJobLV := EnsureRange(Value, 0, High(Word));
end;

procedure TBeing.SetBaseEXP(
		Value : LongWord
	);
begin
	fBaseEXP := EnsureRange(Value, 0, High(LongWord));
end;

procedure TBeing.SetJobEXP(
		Value : LongWord
	);
begin
	fJobEXP := EnsureRange(Value, 0, High(LongWord));
end;

procedure TBeing.SetZeny(
		Value : Integer
	);
begin
	fZeny := EnsureRange(Value, 0, High(Integer));
end;


(*- Function ------------------------------------------------------------------*
TBeing.GetBaseStats
--------------------------------------------------------------------------------
Overview:
--

	Returns the given base stat at Index.  Development-time Asserts included for
range checking.

--
Pre:
	Index must be between STR..LUK

--
Revisions:
--
[2007/04/28] CR - Added Comment Header. Added precondition for Index range.
[yyyy/mm/dd] <Author> - <Comment>
*-----------------------------------------------------------------------------*)
Function  TBeing.GetBaseStats(
	const
		Index : Byte
	) : Integer;
Begin
	//Pre
	Assert(InRange(Index, STR, LUK), 'Pre: Index not in range for ParamBase.');
	//--

	Result := fParamBase[Index];
End; (* Func TBeing.GetBaseStats
*-----------------------------------------------------------------------------*)


(*- Procedure -----------------------------------------------------------------*
TBeing.SetBaseStats
--------------------------------------------------------------------------------
Overview:
--

	Sets the given Value for the base stat at Index.  Development-time Asserts
used for checking Index range.

--
Pre:
	Index must be within STR..LUK

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/04/28] CR - Added Comment Header. Added precondition for Index range.
*-----------------------------------------------------------------------------*)
Procedure TBeing.SetBaseStats(
	const
		Index: Byte;
	const
		Value: Integer
	);
Begin
	//Pre
	Assert(InRange(Index, STR, LUK), 'Pre: Index not in range for ParamBase.');
	//--

	fParamBase[Index] := EnsureRange(Value, 0, 9999);
End; (* Proc TBeing.SetBaseStats
*-----------------------------------------------------------------------------*)

Procedure TBeing.SetMaxHP(Value : word);
Begin
	fMaxHP := Value;
End; (* Proc TBeing.SetMaxHP
*-----------------------------------------------------------------------------*)

Procedure TBeing.SetHP(Value : word);
Begin
	fHP := Value;
End; (* Proc TBeing.SetHP
*-----------------------------------------------------------------------------*)
procedure TBeing.SetMaxSP(Value : word); begin fMaxSP := Value; end;
procedure TBeing.SetSP(Value : word); begin fSP := Value; end;
Procedure TBeing.SetStatus(Value : word); begin fStatus := Value; end;
Procedure TBeing.SetAilments(Value : word); begin fAilments := Value; end;
Procedure TBeing.SetOption(Value : word); begin fOption := Value; end;
procedure TBeing.SetMap(Value : string); begin fMap := Value; end;

procedure TBeing.SetSpeed(Value : Word); begin fSpeed := Value; end;

procedure TBeing.SetASpeed(Value : Word);
begin
	fASpeed := Value;
end;

procedure TBeing.SetPosition(Value : TPoint);
var
	OldPt : TPoint;

begin
	OldPt := Position;
	fPosition := Value;
	//check if we're online before we add/remove ourselves from the map...
	if ZoneStatus = isOnline then
	begin
		//Position is initialized to -1, -1 on create, so if we're not on the map
		//yet, don't remove us!
		if MapInfo.PointInRange(OldPt) then
		begin
			MapInfo.Cell[OldPt.X][OldPt.Y].Beings.Delete(
				MapInfo.Cell[OldPt.X][OldPt.Y].Beings.IndexOf(ID)
			);
		end;

		//same as above, if we've set position to -1, -1, then don't add us to a
		//non-existant cell!
		if MapInfo.PointInRange(Position) then
		begin
			MapInfo.Cell[Position.X][Position.Y].Beings.AddObject(ID, self);
		end;
	end;
end;

(*- Procedure -----------------------------------------------------------------*
TBeing.CalcSpeed
--------------------------------------------------------------------------------
Overview:
--

Sets Speed to default of 150.

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/04/28] CR - Added Comment Header
*-----------------------------------------------------------------------------*)
Procedure TBeing.CalcSpeed;
Begin
	Speed := 150;
End; (* Proc TBeing.CalcSpeed
*-----------------------------------------------------------------------------*)


//------------------------------------------------------------------------------
//GetHpPercent                                                          FUNCTION
//------------------------------------------------------------------------------
//  What it does-
//	Get current HP percentage
// --
//   Pre:
//	TODO
//   Post:
//	TODO
// --
//	Changes -
//		[2007/12/31] Aeomin - Added
//
//------------------------------------------------------------------------------
function TBeing.GetHpPercent: Byte;
begin
	Result := Trunc(HP/MaxHP*100);
end;{GetHpPercent}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetHpPercent                                                          FUNCTION
//------------------------------------------------------------------------------
//  What it does-
//	Set current HP percentage
// --
//   Pre:
//	TODO
//   Post:
//	TODO
// --
//	Changes -
//		[2007/12/31] Aeomin - Added
//
//------------------------------------------------------------------------------
procedure TBeing.SetHPPercent(Value : Byte);
begin
	Value := EnsureRange(Value,0,100);
	HP    := Trunc(Value/100*MaxHP);
end;{SetHPPercent}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetSpPercent                                                          FUNCTION
//------------------------------------------------------------------------------
//  What it does-
//	Get current SP percentage
// --
//   Pre:
//	TODO
//   Post:
//	TODO
// --
//	Changes -
//		[2007/12/31] Aeomin - Added
//
//------------------------------------------------------------------------------
function TBeing.GetSpPercent: Byte;
begin
	Result := Trunc(SP/MaxSP*100);
end;{GetHpPercent}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetSpPercent                                                          FUNCTION
//------------------------------------------------------------------------------
//  What it does-
//	Set current SP percentage
// --
//   Pre:
//	TODO
//   Post:
//	TODO
// --
//	Changes -
//		[2007/12/31] Aeomin - Added
//
//------------------------------------------------------------------------------
procedure TBeing.SetSPPercent(Value : Byte);
begin
	Value := EnsureRange(Value,0,100);
	SP    := Trunc(Value/100*MaxSP);
end;{SetHPPercent}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RemoveFromMap                                                       Procedure
//------------------------------------------------------------------------------
//  What it does-
//	Removes this being from the map
// --
//   Pre:
//	TODO
//   Post:
//	TODO
// --
//	Changes -
//		[2008/01/02] RaX - Created
//
//------------------------------------------------------------------------------
procedure TBeing.RemoveFromMap;
var
	Index : Integer;
begin
	Index := MapInfo.Cell[Position.X][Position.Y].Beings.IndexOf(ID);
	if Index <> -1 then
	begin
		MapInfo.Cell[Position.X][Position.Y].Beings.Delete(
			Index
		);
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//AddToMap                                                       Procedure
//------------------------------------------------------------------------------
//  What it does-
//	Adds this being to the map
// --
//   Pre:
//	TODO
//   Post:
//	TODO
// --
//	Changes -
//		[2008/01/02] RaX - Created
//
//------------------------------------------------------------------------------
procedure TBeing.AddToMap;
begin
	MapInfo.Cell[Position.X][Position.Y].Beings.AddObject(ID, self);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//CalcASpeed                                                      Procedure
//------------------------------------------------------------------------------
//  What it does-
//	Attack speed calculation
// --
//   Pre:
//	TODO
//   Post:
//	TODO
// --
//	Changes -
//		[2008/01/06] RaX - Created
//
//------------------------------------------------------------------------------
procedure TBeing.CalcASpeed;
begin
	ASpeed := EnsureRange(
		200-(Round((250-ParamBase[AGI]-(ParamBase[DEX]/4))*(200{-ASPD Base}-150)/250)) {* DelayDecrease}
	, 0, ASPEED_MAX);
	AttackDelay := 20*(200-ASpeed);
end;
//------------------------------------------------------------------------------
end.
