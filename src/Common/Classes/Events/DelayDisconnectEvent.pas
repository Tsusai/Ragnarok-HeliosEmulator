//------------------------------------------------------------------------------
//DelayDisconnect								UNIT
//------------------------------------------------------------------------------
//	What it does-
//      An event kill try to disconnect player after time expire.
//
//	Changes -
//		Marc 31st, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
unit DelayDisconnectEvent;


interface


uses
	{RTL/VCL}
	//none
	{Project}
	Being,
	Character,
	Event
	{3rd Party}
	//none
	;


type
//------------------------------------------------------------------------------
//TMovementEvent
//------------------------------------------------------------------------------
	TDelayDisconnectEvent = class(TRootEvent)
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
Procedure TDelayDisconnectEvent.Execute;
begin
	if TCharacter(ABeing).ClientInfo.Connection.Connected then
	TCharacter(ABeing).ClientInfo.Connection.Disconnect;
end;//Execute
//------------------------------------------------------------------------------

constructor TDelayDisconnectEvent.Create(SetExpiryTime : LongWord; Being : TBeing);
begin
	inherited Create(SetExpiryTime);
	Self.ABeing := Being;
end;

end.