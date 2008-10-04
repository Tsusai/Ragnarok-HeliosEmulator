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

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
uses
	Item,
	ItemInstance,
	List32,
	Contnrs;

type

//------------------------------------------------------------------------------
//TInventoryList                                                          CLASS
//------------------------------------------------------------------------------
	TInventoryList = Class(TObject)
	private
		fList : TObjectList;
		fSlotList : TIntList32;
		fIndexList : TIntList32;
		fNextID : Word;

		function GetValue(Index : Integer) : TItemInstance;
		procedure SetValue(Index : Integer; Value : TItemInstance);
		function GetIndexItem(Index : Integer) : TItemInstance;
		function GetCount : Integer;
		function RegisterIndex : Word;
	public
		OwnsItems : Boolean;
		StackableList	: TIntList32;

		constructor Create(OwnsItems : Boolean);
		destructor Destroy; override;
		property Items[Index : Integer] : TItemInstance
			read GetValue write SetValue;default;
		property IndexItems[Index : Integer] : TItemInstance read GetIndexItem;
		Property Count : Integer read GetCount;

		//procedure Add(const AnItem : TItem; const Quantity : Word);overload;
		procedure Add(const AnInventoryItem:TItemInstance;const Stack:Boolean = False);overload;
//		procedure Insert(const AnItem : TItem; const Quantity : Word; Index : Integer);
		procedure Delete(const Index : Integer);overload;
		procedure Delete(const ItemInstance:TItemInstance;const DontFree:Boolean=False);overload;
		procedure Clear();

		function IndexOf(const ID : LongWord) : Integer;
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
	//We will manually free 'em
	fList := TObjectList.Create(FALSE);
	self.OwnsItems := OwnsItems;
	fSlotList := TIntList32.Create;
	fIndexList := TIntList32.Create;
	StackableList	:= TIntList32.Create;
	fNextID := 0;
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
begin
	Clear;
	fList.Free;
	fSlotList.Free;
	fIndexList.Free;
	StackableList.Free;
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
{procedure TInventoryList.Add(const AnItem : TItem; const Quantity : Word);
var
	AnInventoryItem : TItemInstance;
begin
	AnInventoryItem := TItemInstance.Create;
	AnInventoryItem.Item := AnItem;
	AnInventoryItem.Quantity := Quantity;
	Add(AnInventoryItem);
end;}{Add}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Add                                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Adds a TInventoryItem to the list.
//
//	Changes -
//		[2008/09/20] Aeomin - Created.
//------------------------------------------------------------------------------
procedure TInventoryList.Add(const AnInventoryItem:TItemInstance;const Stack:Boolean = False);
begin
	AnInventoryItem.Index := RegisterIndex;
	fList.Add(AnInventoryItem);
	fIndexList.AddObject(AnInventoryItem.Index, AnInventoryItem);
	if Stack then
	begin
		StackableList.AddObject(AnInventoryItem.Item.ID,AnInventoryItem);
	end;
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
//	[2008/09/21] Aeomin - Banned this procedure.
//------------------------------------------------------------------------------
{procedure TInventoryList.Insert(const AnItem : TItem; const Quantity : Word; Index: Integer);
var
	AnInventoryItem : TInventoryItem;
begin
	AnInventoryItem := TInventoryItem.Create;
	AnInventoryItem.Item := AnItem;
	AnInventoryItem.Quantity := Quantity;
	fList.Insert(Index, AnInventoryItem);
end;}{Insert}
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
var
	AnIndex : Integer;
begin
	AnIndex := fIndexList.IndexOf(Index);
	if AnIndex > -1 then
	begin
		Delete(TItemInstance(fIndexList.Objects[AnIndex]));
	end;
end;{Delete}
//------------------------------------------------------------------------------


procedure TInventoryList.Delete(const ItemInstance:TItemInstance;const DontFree:Boolean=False);
var
	Index:Integer;
	AnIndex : Integer;
begin
	Index:=fList.IndexOf(ItemInstance);
	if Index>-1 then
	begin
		fSlotList.Add(ItemInstance.Index);
		fList.Delete(Index);
		AnIndex := StackableList.IndexOf(ItemInstance.Item.ID);
		if AnIndex > -1 then
		begin
			StackableList.Delete(AnIndex);
		end;
		AnIndex := fIndexList.IndexOf(ItemInstance.Index);
		if AnIndex > -1 then
		begin
			fIndexList.Delete(AnIndex);
		end;
		if (NOT DontFree) AND OwnsItems then
		begin
			Items[Index].Item.Free;
			Items[Index].Free;
		end;
	end;
end;
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
	if fList.Count > 0 then
	begin
		if OwnsItems then
		begin
			for Index := 0 to fList.Count - 1 do
			begin
				Items[Index].Item.Free;
				Items[Index].Free;
			end;
		end;
		fList.Clear;
	end;
	fSlotList.Clear;
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
function TInventoryList.GetValue(Index : Integer): TItemInstance;
begin
	Result := TItemInstance(fList.Items[Index]);
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
procedure TInventoryList.SetValue(Index : Integer; Value : TItemInstance);
begin
	fList.Items[Index] := Value;
end;{SetValue}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetIndexItem                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Get iteminstance, but using Index ID of a"rray way"
//
//	Changes -
//		[2008/09/29] Aeomin - Created;
//------------------------------------------------------------------------------
function TInventoryList.GetIndexItem(Index : Integer): TItemInstance;
var
	ItemIdex : Integer;
begin
	Result := nil;
	ItemIdex := fIndexList.IndexOf(Index);
	if ItemIdex > -1 then
	begin
		Result := TItemInstance(fIndexList.Objects[ItemIdex]);
	end;
end;{GetIndexItem}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetCount                                                             PROCEDURE
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


//------------------------------------------------------------------------------
//RegisterIndex                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Attempt to find next ID.
//
//	Changes -
//		[2008/09/21] Aeomin - Created
//------------------------------------------------------------------------------
function TInventoryList.RegisterIndex : Word;
begin
	if fSlotList.Count > 0 then
	begin
		Result := fSlotList[0];
		fSlotList.Delete(0);
	end else
	begin
		Result := fNextID;
		Inc(fNextID);
	end;
end;{RegisterIndex}
//------------------------------------------------------------------------------
end.

