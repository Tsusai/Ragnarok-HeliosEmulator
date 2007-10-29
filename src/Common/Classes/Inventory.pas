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
	Item
	{Third Party}
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
	fItemList : TList;

	Function GetItem(Index : Integer) : TItem;

public
	Property Items[Index : Integer] : TItem Read GetItem;
	Procedure Add(AnItem : TItem; Quantity : Word);
	Procedure Remove(AnItem : TItem; Quantity : Word);

	Constructor Create;
	Destructor Destroy;override;

End;(* TInventory
*== CLASS ====================================================================*)


implementation


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
Constructor TInventory.Create;
begin
	inherited;
	fItemList := TList.Create;
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
	//Needs to attach quantity somehow...
	fItemList.Add(AnItem);
end;

Procedure TInventory.Remove(AnItem: TItem; Quantity: Word);
begin
	//See Add
	//Needs to remove by ID rather than address.
	fItemList.Remove(AnItem);
end;
end.