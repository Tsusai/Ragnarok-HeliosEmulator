(*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

Unit
Item

*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

[2007/10/23] Helios - RaX

================================================================================
License:  (FreeBSD, plus commercial with written permission clause.)
================================================================================

Project Helios - Copyright (c) 2005-2007

All rights reserved.

Please refer to Helios.dpr for full license terms.

================================================================================
Overview:
================================================================================

Item base class, useableItem, miscitem, and equipmentitem will be derived from
here.

================================================================================
Revisions:
================================================================================
(Format: [yyyy/mm/dd] <Author> - <Desc of Changes>)
[2007/10/23] RaX - Created.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)
unit Item;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	{Project}
	ItemTypes;
	{Third Party}
	//none


type

(*= CLASS =====================================================================*
TItem

*------------------------------------------------------------------------------*
Overview:
*------------------------------------------------------------------------------*

	Common class for Items, TEquipmentItem, TMiscItem and TUseableItem will be
	 descendants.

*------------------------------------------------------------------------------*
Revisions:
*------------------------------------------------------------------------------*
(Format: [yyyy/mm/dd] <Author> - <Description of Change>)
[2007/10/23] RaX - Created.
[2007/10/26] RaX - Added basic item properties.
*=============================================================================*)
TItem = class(TObject)
protected
	fName		: String;
	fID			: LongWord;
	fWeight : LongWord;
	fPrice	: LongWord; //The price of the item in a standard shop
	fSell		: LongWord; //The selling price of an item to a shop
	fType		: TItemType;

public
	Property Name		: String Read fName Write fName;
	Property ID			: LongWord Read fID Write fID;
	Property Weight : LongWord Read fWeight Write fWeight;
	Property Price	: LongWord Read fPrice Write fPrice;
	Property Sell		: LongWord Read fSell Write fSell;


	Constructor Create;
	Destructor Destroy;override;

End;(* TItem
*== CLASS ====================================================================*)


implementation


//uses
	{RTL/VCL}
	{Project}

	{Third Party}
	//none

(*- Cons ----------------------------------------------------------------------*
TItem.Create
--------------------------------------------------------------------------------
Overview:
--

Creates our TItem.

--
Post:
	EventList and Path lists are both created and initialized.

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/10/23] RaX - Created.
*-----------------------------------------------------------------------------*)
Constructor TItem.Create;
begin
	inherited;

End; (* Cons TBeing.Create
*-----------------------------------------------------------------------------*)


(*- Dest ----------------------------------------------------------------------*
TItem.Destroy

--
Overview:
--
	Destroys our TItem

--
Pre:

Post:


--
Revisions:
--
[2007/10/23] RaX - Created.
*-----------------------------------------------------------------------------*)
Destructor TItem.Destroy;
Begin
	//Pre

	//--

	//Always clean up your owned objects/memory first, then call ancestor.
	inherited;
End;(* Dest TItem.Destroy
*-----------------------------------------------------------------------------*)

end.
