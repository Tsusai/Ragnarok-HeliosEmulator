//------------------------------------------------------------------------------
//MobMovement                                                               UNIT
//------------------------------------------------------------------------------
//	What it does-
//		A layer on top of MovementEvent to active AI
//
//	Changes -
//		[2008/12/22] Aeomin - Created
//
//------------------------------------------------------------------------------
unit MobMovementEvent;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	//none
	{Project}
	Event,
	Being,
	MovementEvent
	{3rd Party}
	//none
	;


type
//------------------------------------------------------------------------------
//TMobMovementEvent
//------------------------------------------------------------------------------
	TMobMovementEvent = class(TRootEvent)
	private
		ABeing : TBeing;
	public
		Procedure Execute; override;
		constructor Create(SetExpiryTime : LongWord; Being : TBeing);
	end;
//------------------------------------------------------------------------------

implementation
uses
	Mob,
	MobAI,
	WinLinux,
	GameConstants
	;

//------------------------------------------------------------------------------
Procedure TMobMovementEvent.Execute;
var
	MoveEvent : TMovementEvent;
	Speed     : LongWord;
begin
	if TMobAI(TMob(Abeing).AI).AIStatus = msIdle then
	begin
		TMobAI(TMob(Abeing).AI).AIStatus := msWandering;
		if (Abeing.Direction in Diagonals) then
		begin
			Speed := Abeing.Speed * 3 DIV 2;
		end else
		begin
			Speed := Abeing.Speed;
		end;

		Abeing.MoveTick := GetTick + Speed DIV 2;
		MoveEvent := TMovementEvent.Create(Abeing.MoveTick, Abeing);
		Abeing.EventList.Add(MoveEvent);
		Abeing.ShowBeingWalking;
	end else
	if TMobAI(TMob(Abeing).AI).AIStatus = msWandering then
	begin
		//Something wrong =(
		//Reset
		TMobAI(TMob(Abeing).AI).AIStatus := msIdle;
	end;
end;//Execute
//------------------------------------------------------------------------------

constructor TMobMovementEvent.Create(SetExpiryTime : LongWord; Being : TBeing);
begin
	inherited Create(SetExpiryTime);
	Self.ABeing := Being;
end;
end.