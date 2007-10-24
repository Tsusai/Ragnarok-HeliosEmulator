(*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

Unit
Item

*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

[2006/??/??] Helios - No author stated

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


interface


uses
	{RTL/VCL}
	Types
	{Project}
	{Third Party}
	//none
	;

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
*=============================================================================*)
TItem = class(TObject)
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