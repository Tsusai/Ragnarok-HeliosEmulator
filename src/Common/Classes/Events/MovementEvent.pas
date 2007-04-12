//------------------------------------------------------------------------------
//Movement																																UNIT
//------------------------------------------------------------------------------
//	What it does-
//      An event which will be instantiated when a character requests to move.
//
//	Changes -
//		January 31st, 2007 - RaX - Created Header.
//		[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
//
//------------------------------------------------------------------------------
unit MovementEvent;


interface


uses
	{RTL/VCL}
	//none
	{Project}
	Being,
	Event
	{3rd Party}
	//none
	;


type
//------------------------------------------------------------------------------
//TMovementEvent
//------------------------------------------------------------------------------
	TMovementEvent = class(TRootEvent)
	private
		ABeing : TBeing;
	public
		Procedure Execute; override;
		constructor Create(SetExpiryTime : LongWord; Being : TBeing);
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
	ABeing.Walk;
end;//Execute
//------------------------------------------------------------------------------

constructor TMovementEvent.Create(SetExpiryTime : LongWord; Being : TBeing);
begin
	inherited Create(SetExpiryTime);
	Self.ABeing := Being;
end;

end.