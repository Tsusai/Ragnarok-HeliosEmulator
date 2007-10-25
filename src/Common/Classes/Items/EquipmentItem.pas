(*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

Unit
EquipmentItem

*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

[2007/10/25] Helios - RaX

================================================================================
License:  (FreeBSD, plus commercial with written permission clause.)
================================================================================

Project Helios - Copyright (c) 2005-2007

All rights reserved.

Please refer to Helios.dpr for full license terms.

================================================================================
Overview:
================================================================================

EquipmentItem - Any sort of item that can be equipped to a character/pet/etc.

================================================================================
Revisions:
================================================================================
(Format: [yyyy/mm/dd] <Author> - <Desc of Changes>)
[2007/10/25] RaX - Created.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)
unit EquipmentItem;


interface


uses
	{RTL/VCL}
	Types,
	{Project}
	Item
	{Third Party}
	//none
	;

type

(*= CLASS =====================================================================*
TItem

*------------------------------------------------------------------------------*
Overview:
*------------------------------------------------------------------------------*

	Equipment Item - any item that can be equipped.

*------------------------------------------------------------------------------*
Revisions:
*------------------------------------------------------------------------------*
(Format: [yyyy/mm/dd] <Author> - <Description of Change>)
[2007/10/25] RaX - Created.
*=============================================================================*)
TEquipmentItem = class(TItem)
protected

public

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
TEquipmentItem.Create
--------------------------------------------------------------------------------
Overview:
--

Creates our TEquipmentItem.

--
Post:


--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/10/23] RaX - Created.
*-----------------------------------------------------------------------------*)
Constructor TEquipmentItem.Create;
begin
	inherited;

End; (* Cons TEquipmentItem.Create
*-----------------------------------------------------------------------------*)


(*- Dest ----------------------------------------------------------------------*
TEquipmentItem.Destroy

--
Overview:
--
	Destroys our TEquipmentItem

--
Pre:

Post:


--
Revisions:
--
[2007/10/25] RaX - Created.
*-----------------------------------------------------------------------------*)
Destructor TEquipmentItem.Destroy;
Begin
	//Pre

	//--

	//Always clean up your owned objects/memory first, then call ancestor.
	inherited;
End;(* Dest TEquipmentItem.Destroy
*-----------------------------------------------------------------------------*)

end.