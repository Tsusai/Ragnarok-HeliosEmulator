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
	Being,
	MovementEvent
	{3rd Party}
	//none
	;


type
//------------------------------------------------------------------------------
//TMobMovementEvent
//------------------------------------------------------------------------------
	TMobMovementEvent = class(TMovementEvent)
	private
		ABeing : TBeing;
	public
		Procedure Execute; override;
	end;
//------------------------------------------------------------------------------

implementation
uses
	Mob,
	MobAI
	;

//------------------------------------------------------------------------------
Procedure TMobMovementEvent.Execute;
begin
	if TMobAI(TMob(Abeing).AI).AIStatus = msIdle then
	begin
		TMobAI(TMob(Abeing).AI).AIStatus := msWandering;
		Abeing.ShowBeingWalking;
		inherited;
	end;
end;//Execute
//------------------------------------------------------------------------------

end.