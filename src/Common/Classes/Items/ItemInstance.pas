unit ItemInstance;

interface

uses
	Item
	;
type

//------------------------------------------------------------------------------
//TItemInstance                                                            CLASS
//------------------------------------------------------------------------------
TItemInstance = Class(TObject)
	Index		: Word;  //Should only modify by TInventoryList
	ID		: LongWord; //Instance ID

	Item		: TItem;
	Quantity	: Word;

	Identified : Boolean;
	Refined : Byte;

	X,Y : Word;
	MapID : LongWord;
end;
//------------------------------------------------------------------------------


implementation

end.