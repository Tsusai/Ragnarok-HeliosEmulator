unit Event;

interface

type
	TEvent = class
		ExpiryTime	: TDateTime;
		Procedure Execute; Virtual;
	end;

implementation

Procedure TEvent.Execute;
begin
end;

end.
