//------------------------------------------------------------------------------
//Event																																		UNIT
//------------------------------------------------------------------------------
//	What it does-
//      The base Event class, provides the shared attributes of the specialized
//		events.
//
//	Changes -
//		January 31st, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit Event;

interface

type

//------------------------------------------------------------------------------
//TEvent
//------------------------------------------------------------------------------
	TRootEvent = class
		ExpiryTime	: LongWord;//The time at which this event is set to go off
		Procedure Execute; Virtual;
	end;
//------------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------
//Execute																																	UNIT
//------------------------------------------------------------------------------
//	What it does-
//      Just a random virtual event so the other child classes know what to
//		have =)
//
//	Changes -
//		January 31st, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TRootEvent.Execute;
begin
end;//Execute
//------------------------------------------------------------------------------
end.
