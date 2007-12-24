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


interface


uses
	{RTL/VCL}
	Types,
	Classes,
	{Project}
	Item,
	InventoryList,
	{Third Party}
	IdContext
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
	Function GetItem(Index : Integer) : TItem;
	Procedure UpdateItemQuantity(const Index : Integer; const Quantity : Word);
public
	Property Items[Index : Integer] : TItem Read GetItem;
	Procedure Add(AnItem : TItem; Quantity : Word);
	Procedure Remove(AnItem : TItem; Quantity : Word);
	Procedure Delete(Index : Integer);
	Constructor Create(Parent : TObject);
	Destructor Destroy;override;

End;(* TInventory
*== CLASS ====================================================================*)


implementation
uses
	Character;

//uses
	{RTL/VCL}
	{Project}

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
	Call ancestor Create.

--
Revisions:
--
[2007/10/29] RaX - Created.
*-----------------------------------------------------------------------------*)
Destructor TInventory.Destroy;
var
	Index : Integer;
Begin
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


Procedure TInventory.Add(AnItem: TItem; Quantity: Word);
begin
	fItemList.Add(AnItem, Quantity);
	//Send Packet here
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