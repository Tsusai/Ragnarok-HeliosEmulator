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
		TargetID : LongWord;
		Procedure Execute; override;
		constructor Create(SetExpiryTime : LongWord; ACharacter : TCharacter; ATargetID : LongWord);
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
	Character.Attack(TargetID, true);
end;//Execute
//------------------------------------------------------------------------------

constructor TAttackEvent.Create(SetExpiryTime : LongWord; ACharacter : TCharacter; ATargetID : LongWord);
begin
	inherited Create(SetExpiryTime);
	Character := ACharacter;
	TargetID := ATargetID;
end;

end.