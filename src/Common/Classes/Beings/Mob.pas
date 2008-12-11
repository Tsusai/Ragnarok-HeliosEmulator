unit Mob;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}

interface
uses
	Being;

type
	TMob = class(TBeing)
	protected

	public
		SpriteName : String;

		Defense    : Word;
		MDEF       : Word;
		
		Race  : Byte;
		Scale : Byte;
		TamingItem : LongWord;
		FoodItem  : LongWord;
	end;

implementation

end.