unit MovementEvent;

interface
uses
	Event;
type
	TMovementEvent = class(TEvent)
		Procedure Execute; override;
  end;

implementation

Procedure TMovementEvent.Execute;
begin
	inherited;
	WriteLn('MovementEvent Executed!');
end;
end.