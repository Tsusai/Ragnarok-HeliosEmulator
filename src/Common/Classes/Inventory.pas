(*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

Unit
Inventory

*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

[2007/10/29] Helios - RaX

================================================================================
License:  (FreeBSD, plus commercial with written permission clause.)
================================================================================

Project Helios - Copyright (c) 2005-2007

All rights reserved.

Please refer to Helios.dpr for full license terms.

================================================================================
Overview:
================================================================================

A Character Inventory object, holds item objects and handles packets to show
items in a character's inventory.

================================================================================
Revisions:
================================================================================
(Format: [yyyy/mm/dd] <Author> - <Desc of Changes>)
[2007/10/29] RaX - Created.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)
unit Inventory;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	Classes,
	{Project}
	Item,
	ItemInstance,
	InventoryList,
	{Third Party}
	IdContext,
	List32
	//none
	;

type

(*= CLASS =====================================================================*
TInventory

*------------------------------------------------------------------------------*
Overview:
*------------------------------------------------------------------------------*

A Character Inventory object, holds item objects and handles packets to show
items in a character's inventory.

*------------------------------------------------------------------------------*
Revisions:
*------------------------------------------------------------------------------*
(Format: [yyyy/mm/dd] <Author> - <Description of Change>)
[2007/10/29] RaX - Created.
September 29th 2008 - Tsusai - Corrected InventoryID spelling error.
*=============================================================================*)
TInventory = class(TObject)
protected
	fItemList : TInventoryList;
	ClientInfo : TIdContext;
	fCountItem	: Word;
	fCountEquip	: Word;
	fWeight : LongWord;
	function GetItem(Index : Integer) : TItem;
	procedure IncreaseWeight(const AWeight:LongWord);
	procedure DecreaseWeight(const AWeight:LongWord);
	function IsStackable(const AnItem : TItemInstance):Boolean;
	function Move(var AnInventoryItem : TItemInstance;var New:Boolean;const IgnoreWeight:Boolean=False;const UpdateWeight:Boolean=True;const DontFree:Boolean=False):Byte;
public
	InventoryID : LongWord;
	StorageID  : LongWord;
	UseID      : LongWord;
	EquipID    : LongWord;
	EtcID      : LongWord;
	
	property ItemList : TInventoryList read fItemList;
	property Items[Index : Integer] : TItem Read GetItem;
	property Countitem	: Word read fCountItem;
	property CountEquip	: Word read fCountEquip;
	property Weight : LongWord read fWeight;
	procedure Pickup(const ID : LongWord);
	procedure Add(AnItem : TItem; Quantity : Word;const DontSend:Boolean=False);overload;
	function Add(var AnInventoryItem : TItemInstance;const DontSend:Boolean=False;const Transfer:Boolean=False):Boolean;overload;
	function Add(const ID:Word;const Quantity:Word):Boolean;overload;
	procedure Drop(const Index:Word;const Quantity:Word);
	procedure Remove(const OldItem:TItemInstance;const Quantity:Word;var NewItem:TItemInstance);overload;
	function Remove(const OldItem:TItemInstance;const Quantity:Word):Word;overload;
	procedure Remove(const ID : LongWord;Quantity:Word);overload;
	procedure Remove(const Name : String;const Quantity:Word);overload;
	function GetInstance(const ID:LongWord;var AnInsctance:TItemInstance):Boolean;
	function AmountOf(const ID:LongWord):LongWord;overload;
	function AmountOf(const Name:String):LongWord;overload;
	constructor Create(Parent : TObject);
	destructor Destroy;override;
end;(* TInventory
*== CLASS ====================================================================*)


implementation
uses
	{RTL/VCL}
	Types,
	Math,
	{Project}
	Character,
	PacketTypes,
	ZoneSend,
	UseableItem,
	EquipmentItem,
	MiscItem,
	Main,
	ErrorConstants,
	ParameterList,
	AreaLoopEvents
	;
	{Third Party}
	//none

(*- Cons ----------------------------------------------------------------------*
TInventory.Create
--------------------------------------------------------------------------------
Overview:
--

Creates our TInventory.
Pre:
	Call ancestor Create
--
Post:
	ItemList is created.

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/10/29] RaX - Created.
*-----------------------------------------------------------------------------*)
Constructor TInventory.Create(Parent : TObject);
begin
	inherited Create;
	self.ClientInfo := TCharacter(Parent).ClientInfo;
	fItemList := TInventoryList.Create(FALSE);
	fCountItem := 0;
	fCountEquip := 0;
	fWeight := 0;
End; (* Cons TInventory.Create
*-----------------------------------------------------------------------------*)


(*- Dest ----------------------------------------------------------------------*
TInventory.Destroy

--
Overview:
--
	Destroys our TInventory

--
Pre:
	Free up our ItemList
Post:
	Call ancestor Destroy.

--
Revisions:
--
[2007/10/29] RaX - Created.
*-----------------------------------------------------------------------------*)
destructor TInventory.Destroy;
begin
	fItemList.Free;

	inherited;
end;{Destroy}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
function TInventory.GetItem(Index: Integer) : TItem;
begin
	Result := TItem(fItemList[Index]);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
procedure TInventory.IncreaseWeight(const AWeight:LongWord);
begin
	fWeight := EnsureRange(AWeight + fWeight,0,High(LongWord));
	TClientLink(ClientInfo.Data).CharacterLink.Weight := fWeight;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
procedure TInventory.DecreaseWeight(const AWeight:LongWord);
begin
	fWeight := EnsureRange(fWeight - AWeight,0,High(LongWord));
	TClientLink(ClientInfo.Data).CharacterLink.Weight := fWeight;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
function TInventory.IsStackable(const AnItem : TItemInstance):Boolean;
begin
	Result := ((AnItem.Item is TUseableItem)OR
		(AnItem.Item is TMiscItem));
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Move                                                                  FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		I don't know...(honestly)
//
//	Changes -
//		[2008/10/03] Aeomin - Created
//------------------------------------------------------------------------------
function TInventory.Move(var AnInventoryItem : TItemInstance;var New:Boolean;const IgnoreWeight:Boolean=False;const UpdateWeight:Boolean=True;const DontFree:Boolean=False):Byte;
var
	Index : Integer;
	Amount : Word;
begin
	Result := 0;
	New := False;
	Amount := AnInventoryItem.Quantity;
	if fItemList.Count >= MainProc.ZoneServer.Options.MaxItems then
	begin
		Result := ADDITEM_TOOMUCH;
	end else
	begin
		if (not IgnoreWeight) AND ((TClientLink(ClientInfo.Data).CharacterLink.Weight + (AnInventoryItem.Item.Weight*AnInventoryItem.Quantity))
		> TClientLink(ClientInfo.Data).CharacterLink.MaxWeight) then
		begin
			Result := ADDITEM_OVERWEIGHT;
		end else
		begin
			if IsStackable(AnInventoryItem) then
			begin
				Index := fItemList.StackableList.IndexOf(AnInventoryItem.Item.ID);
				if Index > -1 then
				begin
					if (TItemInstance(fItemList.StackableList.Objects[Index]).Quantity + AnInventoryItem.Quantity)
					> MainProc.ZoneServer.Options.MaxStackItem then
					begin
						Result := ADDITEM_TOOMUCHSTACKING;
					end else
					begin
						Inc(TItemInstance(fItemList.StackableList.Objects[Index]).Quantity,AnInventoryItem.Quantity);
						if not DontFree then
						begin
							AnInventoryItem.Item.Free;
							AnInventoryItem.Free;
						end;
						AnInventoryItem := TItemInstance(fItemList.StackableList.Objects[Index]);
					end;
				end else
				begin
					fItemList.Add(AnInventoryItem,True);
					Inc(fCountItem);
					New := True;
				end;
				Inc(fCountItem);
			end else
			if AnInventoryItem.Item is TEquipmentItem then
			begin
				Inc(fCountEquip);
				AnInventoryItem.Quantity := 1;
				fItemList.Add(AnInventoryItem);

				New := True;
			end;

			if Result = 0 then
			begin
				fWeight := EnsureRange(AnInventoryItem.Item.Weight*Amount + fWeight,0,High(LongWord));
				if UpdateWeight then
					TClientLink(ClientInfo.Data).CharacterLink.Weight := fWeight;
			end;
		end;
	end;
end;{Move}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Pickup                                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Pickup craps
//
//	Changes -
//		[2008/10/03] Aeomin - Created
//------------------------------------------------------------------------------
procedure TInventory.Pickup(const ID : LongWord);
var
	AnInventoryItem : TItemInstance;
	AChara : TCharacter;
	Index : Integer;
	CellIndex : Integer;
	X,Y:Word;
	AParameters : TParameterList;
	function TransferToInventory:Boolean;
	var
		Amount : Word;
		Failed : Byte;
		New : Boolean;
		OldInstance:TItemInstance;
	begin
		Amount := AnInventoryItem.Quantity;
		OldInstance := AnInventoryItem;
		Failed := Move(AnInventoryItem,New,False,True,True);
		if Failed > 0 then
		begin
			SendNewItemFailed(
				ClientInfo,
				Failed
			);
		end else
		begin
			if New then
			begin
				AnInventoryItem.X := 0;
				AnInventoryItem.Y := 0;
				AnInventoryItem.MapID := 0;
				TThreadLink(ClientInfo.Data).DatabaseLink.Items.Save(
					AnInventoryItem,
					Self
				);
			end else
			begin
				TThreadLink(ClientInfo.Data).DatabaseLink.Items.Delete(
					OldInstance.ID
				);
				OldInstance.Item.Free;
				OldInstance.Free;
				TThreadLink(ClientInfo.Data).DatabaseLink.Items.Save(
					AnInventoryItem,
					Self
				);
			end;
			SendNewItem(
				ClientInfo,
				AnInventoryItem,
				AnInventoryItem.Index,
				Amount
			);
		end;
		Result := (Failed = 0);
	end;
begin
	AChara := TClientLink(ClientInfo.Data).CharacterLink;
	Index := AChara.MapInfo.ItemList.IndexOf(ID);
	if Index > -1 then
	begin
		AnInventoryItem := AChara.MapInfo.ItemList.Objects[Index] as TItemInstance;
		X := AnInventoryItem.X;
		Y := AnInventoryItem.Y;
		//Make sure within 1 cell distance
		if (Abs(AChara.Position.X-X)<=1)AND
		(Abs(AChara.Position.Y-Y)<=1) then
		begin
			CellIndex := AChara.MapInfo.Cell[
				X,
				Y
			].Items.IndexOf(ID);
			if CellIndex > -1 then
			begin
				if TransferToInventory then
				begin
					AChara.MapInfo.Cell[
						X,
						Y
					].Items.Delete(CellIndex);
					AChara.MapInfo.ItemList.Delete(Index);

					AParameters := TParameterList.Create;
					AParameters.AddAsLongWord(1,ID);
					AChara.AreaLoop(ShowPickupItem, FALSE,AParameters);
					AChara.AreaLoop(RemoveGroundItem, FALSE,AParameters);
					AParameters.Free;
				end;
			end;
		end;
	end;
end;{Pickup}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
procedure TInventory.Add(AnItem: TItem; Quantity: Word;const DontSend:Boolean=False);
var
	Item : TItemInstance;
begin
	Item := TItemInstance.Create;
	Item.Item := AnItem;
	Item.Quantity := Quantity;
	Add(Item,DontSend);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
function TInventory.Add(var AnInventoryItem : TItemInstance;const DontSend:Boolean=False;const Transfer:Boolean=False):Boolean;
var
	Amount : Word;
	Failed : Byte;
	New : Boolean;
begin
	Amount := AnInventoryItem.Quantity;
	Failed := Move(AnInventoryItem,New,DontSend,NOT DontSend);
	if (not DontSend) then
	begin
		if New then
		begin
			TThreadLink(ClientInfo.Data).DatabaseLink.Items.New(
				AnInventoryItem,
				Self
			);
		end else
		begin
			TThreadLink(ClientInfo.Data).DatabaseLink.Items.Save(
				AnInventoryItem,
				Self
			);
		end;
	end;

	if not DontSend then
	begin
		if Failed > 0 then
		begin
			SendNewItemFailed(
				ClientInfo,
				Failed
			);
		end else
		begin
			SendNewItem(
				ClientInfo,
				AnInventoryItem,
				AnInventoryItem.Index,
				Amount
			);
		end;
	end;
	Result := (Failed = 0);
end;{Add}
//------------------------------------------------------------------------------


function TInventory.Add(const ID:Word;const Quantity:Word):Boolean;
var
	AnItem : TItemInstance;
begin
	Result := False;
	if TThreadLink(ClientInfo.Data).DatabaseLink.Items.Find(ID) then
	begin
		AnItem := TItemInstance.Create;
		AnItem.Item := TItem.Create;
		AnItem.Item.ID := ID;
		AnItem.Quantity := Quantity;
		AnItem.Identified := True;
		TThreadLink(ClientInfo.Data).DatabaseLink.Items.Load(AnItem.Item);
		Add(AnItem);
		Result := True;
	end;
end;{Add}
//------------------------------------------------------------------------------

procedure TInventory.Drop(const Index:Word;const Quantity:Word);
var
	AChara : TCharacter;
	LoopIndex : Byte;
	Position : TPoint;
	FoundPosition : Boolean;
	TheItem : TItemInstance;
	NewItem : TItemInstance;
	ParameterList : TParameterList;
begin
	AChara := TClientLink(ClientInfo.Data).CharacterLink;
	if AChara.MapInfo.IsBlocked(AChara.Position) then
	begin
		//Standing on unwalkable area?
		Exit;
	end;
	TheItem := fItemList.IndexItems[Index-1];
	if (TheItem <> nil)AND(Quantity>0)AND(TheItem.Quantity >= Quantity) then
	begin
		Position.X:= AChara.Position.X + (Random(3)-1);
		Position.Y:= AChara.Position.Y + (Random(3)-1);
		FoundPosition := False;
		for LoopIndex := 1 to 6 do
		begin
			if  NOT AChara.MapInfo.IsBlocked(Position) then
			begin
				FoundPosition := True;
				Break;
			end;
			Position.X:= AChara.Position.X + (Random(3)-1);
			Position.Y:= AChara.Position.Y + (Random(3)-1);
		end;
		if not FoundPosition then
		begin
			Position := AChara.Position;
		end;
		{Improvement needed}

		if Quantity >= TheItem.Quantity then
		begin
			TheItem.X := Position.X;
			TheItem.Y := Position.Y;
			TheItem.MapID := AChara.MapInfo.ID;
			TThreadLink(ClientInfo.Data).DatabaseLink.Items.Save(
				TheItem,
				nil
			);
			{Deletem 'em all}
			SendDeleteItem(
				AChara,
				TheItem.Index,
				TheItem.Quantity
			);
			DecreaseWeight(Quantity*TheItem.Item.Weight);

			ParameterList := TParameterList.Create;
			ParameterList.AddAsObject(1,TheItem);
			ParameterList.AddAsLongWord(2,Quantity);
			AChara.AreaLoop(ShowDropitem,False,ParameterList);
			ParameterList.Free;

			fItemList.Delete(TheItem,True);
		end else
		begin
			Remove(
				TheItem,
				Quantity,
				NewItem
			);
			NewItem.X := Position.X;
			NewItem.Y := Position.Y;
			NewItem.MapID := AChara.MapInfo.ID;
			{FIX ME}
			TThreadLink(ClientInfo.Data).DatabaseLink.Items.New(
				NewItem,
				nil
			);

			ParameterList := TParameterList.Create;
			ParameterList.AddAsObject(1,NewItem);
			ParameterList.AddAsLongWord(2,Quantity);
			AChara.AreaLoop(ShowDropitem,False,ParameterList);
			ParameterList.Free;

			TheItem := NewItem;
		end;
		AChara.MapInfo.Cell[TheItem.X,TheItem.Y].Items.AddObject(TheItem.ID,TheItem);
		AChara.MapInfo.ItemList.AddObject(TheItem.ID,TheItem);
	end;
end;{Drop}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Remove                                                                FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		In situation such drop item,player not always drop everything.
//	In this case, item will split into two.
//	Quantity is amount for newitem.
//	You will also need to New in database.
//
//	Changes -
//		[2008/09/29] Aeomin - Created
//------------------------------------------------------------------------------
procedure TInventory.Remove(const OldItem:TItemInstance;const Quantity:Word;var NewItem:TItemInstance);
begin
	NewItem := nil;
	if Quantity < OldItem.Quantity then
	begin
		NewItem := TItemInstance.Create;
		OldItem.Quantity:= OldItem.Quantity - Quantity;
		NewItem.Quantity := Quantity;
		NewItem.Item := TItem.Create;
		NewItem.Item.ID := OldItem.Item.ID;
		NewItem.Identified := OldItem.Identified;
		TThreadLink(ClientInfo.Data).DatabaseLink.Items.Save(OldItem,Self);
		TThreadLink(ClientInfo.Data).DatabaseLink.Items.Load(NewItem.Item);
		SendDeleteItem(
			TClientLink(ClientInfo.Data).CharacterLink,
			OldItem.Index,
			Quantity
		);
		DecreaseWeight(Quantity*OldItem.Item.Weight);
	end else
	begin
		{Remove 'em}
		SendDeleteItem(
			TClientLink(ClientInfo.Data).CharacterLink,
			OldItem.Index,
			OldItem.Quantity
		);
		fItemList.Delete(OldItem);
		DecreaseWeight(OldItem.Quantity*OldItem.Item.Weight);
	end;
end;{Remove}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Remove                                                                FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Similar to above, except this removes item (such sell to npc)
//
//	Changes -
//		[2008/09/30] Aeomin - Created
//------------------------------------------------------------------------------
function TInventory.Remove(const OldItem:TItemInstance;const Quantity:Word):Word;
begin
	if Quantity < OldItem.Quantity then
	begin
		OldItem.Quantity:= OldItem.Quantity - Quantity;
		TThreadLink(ClientInfo.Data).DatabaseLink.Items.Save(OldItem,Self);
		SendDeleteItem(
			TClientLink(ClientInfo.Data).CharacterLink,
			OldItem.Index,
			Quantity
		);
		DecreaseWeight(Quantity*OldItem.Item.Weight);
		Result := Quantity;
	end else
	begin
		TThreadLink(ClientInfo.Data).DatabaseLink.Items.Delete(OldItem.ID);
		{Remove 'em}
		SendDeleteItem(
			TClientLink(ClientInfo.Data).CharacterLink,
			OldItem.Index,
			OldItem.Quantity
		);
		Result := OldItem.Quantity;
		fItemList.Delete(OldItem);
		DecreaseWeight(OldItem.Quantity*OldItem.Item.Weight);
	end;
end;{Remove}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//Remove                                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Remove item from inventory using definition ID.
//
//	Changes -
//		[2008/10/03] Aeomin - Created
//------------------------------------------------------------------------------
procedure TInventory.Remove(const ID : LongWord;Quantity:Word);
var
	ItemInstance : TItemInstance;
begin
	while Quantity > 0 do
	begin
		if GetInstance(ID,ItemInstance) then
		begin
			Quantity := Quantity - Remove(ItemInstance,Quantity);
		end else
			Break; {Can't find any}
	end;
end;{Remove}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Remove                                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Remove item from inventory using definition Name
//
//	Changes -
//		[2008/10/03] Aeomin - Created
//------------------------------------------------------------------------------
procedure TInventory.Remove(const Name : String;const Quantity:Word);
var
	ID : LongWord;
begin
	ID := TThreadLink(ClientInfo.Data).DatabaseLink.Items.Find(Name);
	if ID > 0 then
	begin
		Remove(ID,Quantity);
	end;
end;{Remove}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetInstance                                                           FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Find instance using definition ID; return FALSE if not found.
//
//	Changes -
//		[2008/10/03] Aeomin - Created
//------------------------------------------------------------------------------
function TInventory.GetInstance(const ID:LongWord;var AnInsctance:TItemInstance):Boolean;
var
	Index : Integer;
	Instance : TItemInstance;
begin
	Result := False;
	if fItemList.Count > 0 then
	begin
		for Index := 0 to fItemList.Count - 1 do
		begin
			Instance := fItemList[Index] as TItemInstance;
			if Instance.Item.ID = ID then
			begin
				AnInsctance := Instance;
				Result := True;
				Break;
			end;
		end;
	end;
end;{GetInstance}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//AmountOf                                                              FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Find amount of an item using definition ID.
//
//	Changes -
//		[2008/10/03] Aeomin - Created
//------------------------------------------------------------------------------
function TInventory.AmountOf(const ID:LongWord):LongWord;
var
	Index : Integer;
	Instance : TItemInstance;
begin
	Result := 0;
	if fItemList.Count > 0 then
	begin
		for Index := 0 to fItemList.Count - 1 do
		begin
			Instance := fItemList[Index] as TItemInstance;
			if Instance.Item.ID = ID then
			begin
				Result := EnsureRange(Result + Instance.Quantity,0,High(LongWord));
			end;
		end;
	end;
end;{AmountOf}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//AmountOf                                                              FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Find amount of an item using item name
//
//	Changes -
//		[2008/10/03] Aeomin - Created
//------------------------------------------------------------------------------
function TInventory.AmountOf(const Name:String):LongWord;
var
	ID : LongWord;
begin
	Result := 0;
	ID := TThreadLink(ClientInfo.Data).DatabaseLink.Items.Find(Name);
	if ID > 0 then
	begin
		Result := AmountOf(ID);
	end;
end;{AmountOf}
//------------------------------------------------------------------------------
end{Inventory}.
