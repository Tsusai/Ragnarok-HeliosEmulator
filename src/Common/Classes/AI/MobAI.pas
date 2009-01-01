//------------------------------------------------------------------------------
//MobAI                                                                     UNIT
//------------------------------------------------------------------------------
//	What it does-
//		Basic AI for Mob.
//
//	Changes -
//		[2008/12/22] Aeomin - Created.
//
//------------------------------------------------------------------------------
unit MobAI;

interface

uses
	Types,
	Mob,
	AI,
	GameObject
	;

type
	TMobAIStatus = (msIdle, msWandering, msChasing, msAttacking);
	TMobAI = class(TAI)
	private
		Mob  : TMob;
		fAIStatus : TMobAIStatus;
	protected
		procedure SetAIStatus(const AValue : TMobAIStatus);
	public
		property AIStatus : TMobAIStatus read fAIStatus write SetAIStatus;
		procedure Initiate;override;
		procedure Probe;override;
		procedure FoundObject(const AnObj:TGameObject); override;
		procedure ObjectNear(const AnObj:TGameObject); override;
		procedure RandomWalk;
		procedure WalkTo(const APoint : TPoint);
		function GetIdleTicket:Word;
		procedure FinishWalk;override;
		constructor Create(const AMob : TMob);
	end;

implementation

uses
	Math,
	ContNrs,
	Main,
	Character,
	ItemInstance,
	Event,
	MovementEvent,
	MobMovementEvent,
	WinLinux,
	GameConstants
	;

//Initiate AI.
procedure TMobAI.Initiate;
begin
	AIStatus := msIdle;

	Probe;
end;

procedure TMobAI.Probe;
var
	Beings : TObjectList;
	idxX,idxY:Integer;
	ObjectIdx : Integer;
	AObject : TCharacter;
begin
	writeln('LETS FIND CHKN');
	Beings := TObjectList.Create(FALSE);
	try
		for idxY := Max(0,Mob.Position.Y-MainProc.ZoneServer.Options.CharShowArea) to Min(Mob.Position.Y+MainProc.ZoneServer.Options.CharShowArea, Mob.MapInfo.Size.Y-1) do
		begin
			for idxX := Max(0,Mob.Position.X-MainProc.ZoneServer.Options.CharShowArea) to Min(Mob.Position.X+MainProc.ZoneServer.Options.CharShowArea, Mob.MapInfo.Size.X-1) do
			begin
				for ObjectIdx := Mob.MapInfo.Cell[idxX][idxY].Beings.Count -1 downto 0 do
				begin
					if Mob.MapInfo.Cell[idxX][idxY].Beings.Objects[ObjectIdx] is TCharacter then
					begin
						AObject := Mob.MapInfo.Cell[idxX][idxY].Beings.Objects[ObjectIdx] as TCharacter;
						Beings.Add(AObject);
					end;
				end;
			end;
		end;
		if Beings.Count > 0 then
		begin
			AObject := Beings.Items[Random(Beings.Count)] as TCharacter;
			FoundObject(AObject);
			writeln('FOUND ', AObject.Name);
		end;
	finally
		Beings.Free;
	end;
end;

procedure TMobAI.FoundObject(const AnObj:TGameObject);
begin
	writeln('I SAW CHIKEN!');
	if AnObj is TCharacter then
	begin
		{is agressive?}
	end
	else
	if AnObj is TItemInstance then
	begin
		{pick 'em?}
	end;
end;

procedure TMobAI.ObjectNear(const AnObj:TGameObject);
begin
	writeln('CHIKEN SAW ME!');
	FoundObject(AnObj);
end;

procedure TMobAI.RandomWalk;
var
	APoint : TPoint;
	Retry:Byte;
	Pass : Boolean;
	MoveEvent : TRootEvent;
	function RandomPoint:TPoint;
	begin
		with MainProc.ZoneServer.Options do
		begin
			Result.X := Mob.Position.X + (Random($FFFF) mod (CharShowArea*2) - CharShowArea);
			Result.Y := Mob.Position.Y + (Random($FFFF) mod (CharShowArea*2) - CharShowArea);
		end;
	end;
begin
	writeln('time to strech my butt');
	Pass := False;
	for Retry := 1 to 10 do
	begin
		APoint := RandomPoint;
		if Mob.MapInfo.PointInRange(APoint) AND
		NOT Mob.MapInfo.IsBlocked(APoint) then
		begin
			Pass := True;
			Break;
		end;
	end;
	if Pass then
		WalkTo(APoint)
	else
	begin
		//Still..heh
		MoveEvent := TMobMovementEvent.Create(
							GetTick + GetIdleTicket,
							Mob
						);
		Mob.EventList.Add(MoveEvent);
	end;
end;

procedure TMobAI.WalkTo(const APoint : TPoint);
var
	MoveEvent : TRootEvent;
	Speed     : LongWord;
begin
	if Mob.GetPath(Mob.Position, APoint, Mob.Path) then
	begin
		Mob.EventList.DeleteAttackEvents;
		Mob.EventList.DeleteMovementEvents;

		Mob.PathIndex := 0;

		if AIStatus = msIdle then
		begin
			//Gotta delay !
			MoveEvent := TMobMovementEvent.Create(
								GetTick + GetIdleTicket,
								Mob
							);
		end else
		begin
			if (Mob.Direction in Diagonals) then
			begin
				Speed := Mob.Speed * 3 DIV 2;
			end else
			begin
				Speed := Mob.Speed;
			end;

			Mob.MoveTick := GetTick + Speed DIV 2;
			MoveEvent := TMovementEvent.Create(Mob.MoveTick,Mob);
		end;
		Mob.EventList.Add(MoveEvent);
	end;
end;

function TMobAI.GetIdleTicket:Word;
begin
	Result := 1000;
	case fAIStatus of
		msIdle: Result := (Random($FFFFFF) mod 5000)+1000;
//		msWandering: ;
//		msChasing: ;
//		msAttacking: ;
	end;
end;

procedure TMobAI.FinishWalk;
begin
	writeln('finished walking');
	if AIStatus = msWandering then
	begin
		AIStatus := msIdle;
	end;
end;

procedure TMobAI.SetAIStatus(const AValue : TMobAIStatus);
begin
	fAIStatus := AValue;
	case fAIStatus of
		msIdle:
			begin
				RandomWalk;
			end;
		msWandering: ;
		msChasing: ;
		msAttacking: ;
	end;
end;

constructor TMobAI.Create(const AMob : TMob);
begin
	Mob := AMob;
end;
end.