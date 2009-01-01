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
		constructor Create(const AMob : TMob);
	end;

implementation

uses
	Math,
	ContNrs,
	Main,
	Character,
	ItemInstance,
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
begin
	writeln('time to strech my butt');
	APoint.X := Mob.Position.X +3;
	APoint.Y := Mob.Position.Y +3;
	WalkTo(APoint);
end;

procedure TMobAI.WalkTo(const APoint : TPoint);
var
	MoveEvent : TMobMovementEvent;
	Speed     : LongWord;
begin
	if Mob.GetPath(Mob.Position, APoint, Mob.Path) then
	begin
		Mob.EventList.DeleteAttackEvents;
		Mob.EventList.DeleteMovementEvents;

		Mob.PathIndex := 0;

		if (Mob.Direction in Diagonals) then
		begin
			Speed := Mob.Speed * 3 DIV 2;
		end else
		begin
			Speed := Mob.Speed;
		end;

		Mob.MoveTick := GetTick + Speed DIV 2;

		MoveEvent := TMobMovementEvent.Create(Mob.MoveTick,Mob);
		Mob.EventList.Add(MoveEvent);
		Mob.ShowBeingWalking;
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