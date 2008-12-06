unit ItemInstance;

interface

uses
	GameObject,
	Item
	;
type

//------------------------------------------------------------------------------
//TItemInstance                                                            CLASS
//------------------------------------------------------------------------------
TItemInstance = Class(TGameObject)
	private
	public
		Index		: Word;  //Should only modify by TInventoryList
		ID		: LongWord; //Instance ID

		Item		: TItem;
		Quantity	: Word;

		Identified : Boolean;
		Refined : Byte;
		Broken : Boolean;
		Equipped :Boolean;

		X,Y : Word;
		SubX,SubY : Byte;
		MapID : LongWord;
		procedure GenerateSubCoordinate;
		procedure RemoveFromGround;
		constructor Create;
end;
//------------------------------------------------------------------------------


implementation


procedure TItemInstance.GenerateSubCoordinate;
var
	RandomValue: LongWord;
begin
	RandomValue := Random( $FFFFFF );
	SubX := (RandomValue AND 3) * 3 + 3;
	SubY := ((RandomValue SHR 2) AND 3) * 3 + 3;
end;{GenerateSubCoordinate}
//------------------------------------------------------------------------------


procedure TItemInstance.RemoveFromGround;
begin
	writeln('time to eat!');
end;

constructor TItemInstance.Create;
begin
	GenerateSubCoordinate;
end;{Create}
//------------------------------------------------------------------------------
end.