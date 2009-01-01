unit Mob;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}

interface
uses
	Being,
	AI
	;

type
	TMob = class(TBeing)
	protected

	public
		SpriteName : String;

		Defense    : Word;
		MDEF       : Word;

		Race  : Byte;
		Scale : Byte;
		TamingItem : LongWord;
		FoodItem  : LongWord;

		AI : TAI;

		procedure CalcMaxHP; override;
		procedure CalcMaxSP; override;

		procedure Initiate;

		constructor Create;
		destructor Destroy; override;
	end;

implementation

uses
	Main,
	MobAI,
	AreaLoopEvents
	;

procedure TMob.CalcMaxHP;
begin
	MaxHP := HP;
end;

procedure TMob.CalcMaxSP;
begin
	MaxHP := HP;
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
begin
	MapInfo.Cell[Position.X, Position.Y].Beings.AddObject(ID,Self);
	MainProc.ZoneServer.MobList.Add(Self);
	MapInfo.MobList.Add(Self);
	AreaLoop(SpawnMob);
	{Let's see what we have around here...}
	AI.Initiate;
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