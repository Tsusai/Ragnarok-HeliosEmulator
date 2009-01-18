//------------------------------------------------------------------------------
//Mob                                                                       UNIT
//------------------------------------------------------------------------------
//	What it does-
//		A mob that uses TMobAI to take control of everything.
//
//	Changes -
//		[2008/12/?] Aeomin - Created (actually created long ago).
//
//------------------------------------------------------------------------------
unit Mob;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}

interface
uses
	Types,
	AIBeing,
	AI
	;

type
	TSummonType = (stREPEAT,stONELIFE);
	TMob = class(TAIBeing)
	protected

	public
		SpriteName : String;

		Defense    : Word;
		MDEF       : Word;

		Race  : Byte;
		Scale : Byte;
		TamingItem : LongWord;
		FoodItem  : LongWord;

		InitPosition : TPoint;
		RadiusX : Word;
		RadiusY : Word;
		SummonType : TSummonType;

		procedure CalcMaxHP; override;
		procedure CalcMaxSP; override;

		procedure AddToList;
		procedure Initiate;

		constructor Create;
		destructor Destroy; override;
	end;

implementation

uses
	Main,
	MobAI,
	AreaLoopEvents,
	MapTypes
	;

procedure TMob.CalcMaxHP;
begin
	MaxHP := HP;
end;

procedure TMob.CalcMaxSP;
begin
	MaxHP := HP;
end;

procedure TMob.AddToList;
begin
	MainProc.ZoneServer.MobList.Add(Self);
	MapInfo.MobList.Add(Self);
end;

//------------------------------------------------------------------------------
//Initiate                                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Initiate mob, call this procedure after place into map.
//	This routine should initiate mob AI and all that good stuffs
//
//	Changes-
//		[2008/12/13] Aeomin - Create.
//------------------------------------------------------------------------------
procedure TMob.Initiate;
var
	Pass : Boolean;
	Trials:Byte;
function RandomRadiusCell:Boolean;
var
	NewPosition : TPoint;
begin
	NewPosition.X := InitPosition.X + (Random($FFFF) mod (RadiusX*2) - RadiusX);
	NewPosition.Y := InitPosition.Y + (Random($FFFF) mod (RadiusY*2) - RadiusY);
	Position := NewPosition;
	Result := MapInfo.PointInRange(Position) AND NOT MapInfo.IsBlocked(Position);
end;
begin
	if MapInfo.State = LOADED then
	begin
		if SummonType = stREPEAT then
		begin
			if (InitPosition.X = 0) AND
			(InitPosition.Y = 0) then
			begin
				Position := MapInfo.RandomCell;
			end else
			begin
				Pass := False;
				for Trials := 1 to 100 do
				begin
					if RandomRadiusCell then
					begin
						Pass:=True;
						Break;
					end;

				end;
				if NOT Pass then
				begin
					WriteLN('Position failed');
					Exit;
				end;
			end;
		end;
		MapInfo.Cell[Position.X, Position.Y].Beings.AddObject(ID,Self);
		AreaLoop(SpawnMob);
		{Let's see what we have around here...}
		AI.Initiate;
	end;
end;{Initiate}
//------------------------------------------------------------------------------

constructor TMob.Create;
begin
	inherited;
	AI := TMobAI.Create(Self);
end;
destructor TMob.Destroy;
begin
	AI.Free;
end;

end.