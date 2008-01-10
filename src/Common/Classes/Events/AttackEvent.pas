//------------------------------------------------------------------------------
//AttackEvent                                                              UNIT
//------------------------------------------------------------------------------
//	What it does-
//      An event which will be instantiated when a character requests to move.
//
//	Changes -
//		December 26th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
unit AttackEvent;


interface


uses
	{RTL/VCL}
	//none
	{Project}
	Character,
	Being,
	Event
	{3rd Party}
	//none
	;


type
//------------------------------------------------------------------------------
//TAttackEvent
//------------------------------------------------------------------------------
	TAttackEvent = class(TRootEvent)
	private
		Character : TCharacter;
	public
		JustAttacked : Boolean;
		TargetBeing : TBeing;
		Procedure Execute; override;
		constructor Create(SetExpiryTime : LongWord; ACharacter : TCharacter; ATargetBeing : TBeing; AJustAttacked : Boolean);
	end;
//------------------------------------------------------------------------------

implementation


//------------------------------------------------------------------------------
//Execute                                                                   UNIT
//------------------------------------------------------------------------------
//	What it does-
//      The real executing code of the event, actually does whatever the event
//		needs to do.
//
//	Changes -
//		December 26th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Procedure TAttackEvent.Execute;
begin
	Character.Attack(TargetBeing.ID, true, JustAttacked);
end;//Execute
//------------------------------------------------------------------------------

constructor TAttackEvent.Create(SetExpiryTime : LongWord; ACharacter : TCharacter; ATargetBeing : TBeing; AJustAttacked : Boolean);
begin
	inherited Create(SetExpiryTime);
	Character := ACharacter;
	JustAttacked := AJustAttacked;
	TargetBeing := ATargetBeing;
end;

end.
