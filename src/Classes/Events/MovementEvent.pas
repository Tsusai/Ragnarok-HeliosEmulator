//------------------------------------------------------------------------------
//Movement																																UNIT
//------------------------------------------------------------------------------
//	What it does-
//      An event which will be instantiated when a character requests to move.
//
//	Changes -
//		January 31st, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit MovementEvent;

interface

uses
	Being,
	Event;

type
//------------------------------------------------------------------------------
//TMovementEvent
//------------------------------------------------------------------------------
	TMovementEvent = class(TRootEvent)
	private
		ABeing : TBeing;
	public
		Procedure Execute; override;
		constructor Create(Being : TBeing); reintroduce;
	end;
//------------------------------------------------------------------------------

implementation
uses
	Globals;

//------------------------------------------------------------------------------
//Execute																																	UNIT
//------------------------------------------------------------------------------
//	What it does-
//      The real executing code of the event, actually does whatever the event
//		needs to do.
//
//	Changes -
//		January 31st, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TMovementEvent.Execute;
begin
	ABeing.Walk;
end;//Execute
//------------------------------------------------------------------------------

constructor TMovementEvent.Create(Being : TBeing);
begin
	inherited Create;
	Self.ABeing := Being;
end;

end.