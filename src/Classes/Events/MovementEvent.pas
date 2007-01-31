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
	Event;

type
//------------------------------------------------------------------------------
//TMovementEvent
//------------------------------------------------------------------------------
	TMovementEvent = class(TEvent)
		Procedure Execute; override;
  end;
//------------------------------------------------------------------------------

implementation

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
	inherited;
	WriteLn('MovementEvent Executed!');
end;//Execute
//------------------------------------------------------------------------------
end.