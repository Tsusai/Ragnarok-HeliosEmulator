unit ZoneRecv;

interface
uses
	Character,
	PacketTypes;

	procedure NoCommand(
			AChara : TCharacter;
		const
			ReadPts : TReadPts
		); overload;

	procedure NoCommand(
		const
			AChara : TCharacter;
		const
			AvoidSelf:boolean = False
		); overload;

implementation

	Procedure NoCommand(
			AChara  : TCharacter;
		const
			ReadPts : TReadPts
		);
	Begin
		//Dummy Command for processes that don't have one.
	End;

	Procedure NoCommand(
		const
			AChara    : TCharacter;
		const
			AvoidSelf : Boolean = False
		);
	Begin
		//Dummy Command for processes that don't have one.
	End;


end.