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
*=============================================================================*)
TInventory = class(TObject)
protected
	fItemList : TInventoryList;
	ClientInfo : TIdContext;
	fCountItem	: Word;
	fCountEquip	: Word;
	fStackableList	: TIntList32;
	function GetItem(Index : Integer) : TItem;
	procedure UpdateItemQuantity(const Index : Integer; const Quantity : Word);
public
	IventoryID : LongWord;
	StorageID  : LongWord;
	UseID      : LongWord;
	EquipID    : LongWord;
	EtcID      : LongWord;
	
	property ItemList : TInventoryList read fItemList;
	property Items[Index : Integer] : TItem Read GetItem;
	property Countitem	: Word read fCountItem;
	property CountEquip	: Word read fCountEquip;
	procedure Add(AnItem : TItem; Quantity : Word;const DontSend:Boolean=False);overload;
	procedure Add(var AnInventoryItem : TItemInstance;const DontSend:Boolean=False);overload;
	function Add(const ID:Word;const Quantity:Word):Boolean;overload;
	procedure Remove(AnItem : TItem; Quantity : Word);
	procedure Delete(Index : Integer);
	constructor Create(Parent : TObject);
	destructor Destroy;override;
end;(* TInventory
*== CLASS ====================================================================*)


implementation
uses
	{RTL/VCL}
	{Project}
	Character,
	PacketTypes,
	ZoneSend,
	UseableItem,
	EquipmentItem,
	MiscItem
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
	//Stores item that is stackable, this list does not own object
	fStackableList := TIntList32.Create;
	fCountItem := 0;
	fCountEquip := 0;
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
Destructor TInventory.Destroy;
var
	Index : Integer;
begin
	fStackableList.Free;
	//Pre
	//TODO, list needs to own items, I sense a custom list coming on...
	for Index := 0 to fItemList.Count - 1 do
	begin
		TItem(fItemList.Items[Index]).Free;
	end;

	fItemList.Free;
	//--

	//Always clean up your owned objects/memory first, then call ancestor.
	inherited;
End;(* Dest TInventory.Destroy
*-----------------------------------------------------------------------------*)


Function TInventory.GetItem(Index: Integer) : TItem;
begin
	Result := TItem(fItemList[Index]);
end;


procedure TInventory.Add(AnItem: TItem; Quantity: Word;const DontSend:Boolean=False);
var
	Item : TItemInstance;
begin
	Item := TItemInstance.Create;
	Item.Item := AnItem;
	Item.Quantity := Quantity;
	Add(Item,DontSend);
end;

procedure TInventory.Add(var AnInventoryItem : TItemInstance;const DontSend:Boolean=False);
var
	ItemIndex : Word;
	Index : Integer;
	Amount : Word;
begin
	Amount := AnInventoryItem.Quantity;
	ItemIndex := 0;
	if (AnInventoryItem.Item is TUseableItem)OR
	(AnInventoryItem.Item is TMiscItem) then
	begin
		Index := fStackableList.IndexOf(AnInventoryItem.Item.ID);
		if Index > -1 then
		begin
			Inc(TItemInstance(fStackableList.Objects[Index]).Quantity,AnInventoryItem.Quantity);
			ItemIndex := TItemInstance(fStackableList.Objects[Index]).Index;
			AnInventoryItem.Item.Free;
			AnInventoryItem.Free;
			AnInventoryItem := TItemInstance(fStackableList.Objects[Index]);

			if not DontSend then
				TThreadLink(ClientInfo.Data).DatabaseLink.Items.Save(
					AnInventoryItem,
					Self
				);
		end else
		begin
			fItemList.Add(AnInventoryItem);
			fStackableList.AddObject(AnInventoryItem.Item.ID,AnInventoryItem);
			Inc(fCountItem);
			ItemIndex := AnInventoryItem.Index;
			if not DontSend then
				TThreadLink(ClientInfo.Data).DatabaseLink.Items.New(
					AnInventoryItem,
					Self
				);
		end;
		Inc(fCountItem);
	end else
	if AnInventoryItem.Item is TEquipmentItem then
	begin
		Inc(fCountEquip);
		AnInventoryItem.Quantity := 1;
		Amount := 1;
		fItemList.Add(AnInventoryItem);
		ItemIndex := AnInventoryItem.Index;
		if not DontSend then
			TThreadLink(ClientInfo.Data).DatabaseLink.Items.New(
				AnInventoryItem,
				Self
			);
	end;
	if not DontSend then
	begin
		SendNewItem(
			ClientInfo,
			AnInventoryItem,
			ItemIndex,
			Amount
		);
	end;
end;


function TInventory.Add(const ID:Word;const Quantity:Word):Boolean;
begin
	Result := False;
	{TODO:Implement}
end;


Procedure TInventory.Delete(Index : Integer);
begin
	fItemList.Delete(Index);
	//Send packet here.
end;


Procedure TInventory.Remove(AnItem: TItem; Quantity: Word);
var
	ItemIndex : Integer;
begin
	ItemIndex := fItemList.IndexOf(AnItem.ID);
	if ItemIndex > -1 then
	begin
		if fItemList.Items[ItemIndex].Quantity <= Quantity then
		begin
			self.Delete(ItemIndex);
		end else
		begin
			UpdateItemQuantity(ItemIndex, fItemList.Items[ItemIndex].Quantity - Quantity);
		end;
	end;
end;


Procedure TInventory.UpdateItemQuantity(const Index: Integer; const Quantity: Word);
begin
	fItemList.Items[Index].Quantity := Quantity;
	//Send Packets Here
end;
end.
