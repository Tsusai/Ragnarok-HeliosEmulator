//------------------------------------------------------------------------------
//InventoryList                                                            UNIT
//------------------------------------------------------------------------------
//  What it does -
//      A list of Inventory Items
//
//  Changes -
//    October 30th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
unit InventoryList;

interface
uses
	Item,
	ContNrs;

type

//------------------------------------------------------------------------------
//TInventoryItem                                                         CLASS
//------------------------------------------------------------------------------
TInventoryItem = Class(TObject)
	Item			: TItem;
	Quantity	: Word;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TInventoryList                                                          CLASS
//------------------------------------------------------------------------------
	TInventoryList = Class(TObject)

	Private
		fList : TObjectList;

		Function GetValue(Index : Integer) : TInventoryItem;
		Procedure SetValue(Index : Integer; Value : TInventoryItem);
		Function GetCount : Integer;

	Public
		OwnsItems : Boolean;

		Constructor Create(OwnsItems : Boolean);
		Destructor Destroy; override;
		Property Items[Index : Integer] : TInventoryItem
		read GetValue write SetValue;default;
		Property Count : Integer read GetCount;

		Procedure Add(const AnItem : TItem; const Quantity : Word);
		Procedure Insert(const AnItem : TItem; const Quantity : Word; Index : Integer);
		Procedure Delete(const Index : Integer);
		Procedure Clear();

		Function IndexOf(const ID : LongWord) : Integer;
	end;
//------------------------------------------------------------------------------


implementation

//------------------------------------------------------------------------------
//Create                                                            CONSTRUCTOR
//------------------------------------------------------------------------------
//  What it does -
//      Initializes our itemlist.
//
//  Changes -
//   October 30th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
constructor TInventoryList.Create(OwnsItems : Boolean);
begin
	inherited Create;
	fList := TObjectList.Create(TRUE);
	self.OwnsItems := OwnsItems;
end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy                                                            DESTRUCTOR
//------------------------------------------------------------------------------
//  What it does -
//      Destroys our list and frees any memory used.
//
//  Changes -
//    October 30th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
destructor TInventoryList.Destroy;
var
	Index : Integer;
begin
	//if we own the items, free all of them in the list.

	for Index := 0 to fList.Count-1 do
	begin
		if OwnsItems then
		begin
			Items[Index].Item.Free;
		end;
	end;
	fList.Free;
	// Call TObject destructor
	inherited;
end;{Destroy}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Add                                                                 PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Adds a TInventoryItem to the list.
//
//  Changes -
//    October 30th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure TInventoryList.Add(const AnItem : TItem; const Quantity : Word);
var
	AnInventoryItem : TInventoryItem;
begin
	AnInventoryItem := TInventoryItem.Create;
	AnInventoryItem.Item := AnItem;
	AnInventoryItem.Quantity := Quantity;
	fList.Add(AnInventoryItem);
end;{Add}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Insert                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Inserts a TInventoryItem at Index Position.
//
//  Changes -
//    October 30th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure TInventoryList.Insert(const AnItem : TItem; const Quantity : Word; Index: Integer);
var
	AnInventoryItem : TInventoryItem;
begin
	AnInventoryItem := TInventoryItem.Create;
	AnInventoryItem.Item := AnItem;
	AnInventoryItem.Quantity := Quantity;
	fList.Insert(Index, AnInventoryItem);
end;{Insert}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Delete                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Removes a TInventoryItem at Index from the list.
//
//  Changes -
//    October 30th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure TInventoryList.Delete(const Index : Integer);
begin
	if OwnsItems then
	begin
		Items[Index].Item.Free;
	end;
	fList.Delete(Index);
end;{Delete}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//IndexOf                                                              FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Returns the index in the list of the TInventoryItem;
//
//  Changes -
//    October 30th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
function TInventoryList.IndexOf(const ID: LongWord): Integer;
var
	Index : Integer;
begin
	Index := fList.Count-1;
	Result := -1;
	while (Index >= 0) do
	begin
		if ID = Items[Index].Item.ID then
		begin
			Result := Index;
			Exit;
		end;
		dec(Index,  1);
	end;
end;{IndexOf}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Clear                                                               PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Clears the list.
//
//  Changes -
//    October 30th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure TInventoryList.Clear;
var
	Index : Integer;
begin

	//if we own the characters, the free them.
		for Index := 0 to fList.Count - 1 do
		begin
			if OwnsItems then
			begin
				Items[Index].Item.Free;
			end;
		end;


	fList.Clear;

end;{Clear}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetValue                                                             FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Returns a TInventoryItem at the index.
//
//  Changes -
//    October 30th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
function TInventoryList.GetValue(Index : Integer): TInventoryItem;
begin
	Result := TInventoryItem(fList.Items[Index]);
end;{GetValue}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetValue                                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sets a TInventoryItem into the list at Index.
//
//  Changes -
//    October 30th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
procedure TInventoryList.SetValue(Index : Integer; Value : TInventoryItem);
begin
	fList.Items[Index] := Value;
end;{SetValue}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetCount                                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Gets the count from the fList object
//
//  Changes -
//    October 30th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
Function TInventoryList.GetCount : Integer;
begin
	Result := fList.Count;
end;{GetCount}
//------------------------------------------------------------------------------
end.

