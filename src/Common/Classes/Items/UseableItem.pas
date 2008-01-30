(*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

Unit
UseableItem

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

Any useable item.

================================================================================
Revisions:
================================================================================
(Format: [yyyy/mm/dd] <Author> - <Desc of Changes>)
[2007/10/25] RaX - Created.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)
unit UseableItem;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	{Project}
	Item
	{Third Party}
	//none
	;

type

(*= CLASS =====================================================================*
TUseableItem

*------------------------------------------------------------------------------*
Overview:
*------------------------------------------------------------------------------*

	Any item that can be used.

*------------------------------------------------------------------------------*
Revisions:
*------------------------------------------------------------------------------*
(Format: [yyyy/mm/dd] <Author> - <Description of Change>)
[2007/10/25] RaX - Created.
[2007/10/26] RaX - Added OnUse property.
*=============================================================================*)
TUseableItem = class(TItem)
protected
	fOnUse : String;//Script function, executes when an item is used.
public
	Property OnUse : String Read fOnUse Write fOnUse;

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
TUseableItem.Create
--------------------------------------------------------------------------------
Overview:
--

Creates our TUseableItem.

--
Post:


--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/10/25] RaX - Created.
*-----------------------------------------------------------------------------*)
Constructor TUseableItem.Create;
begin
	inherited;

End; (* Cons TUseableItem.Create
*-----------------------------------------------------------------------------*)


(*- Dest ----------------------------------------------------------------------*
TUseableItem.Destroy

--
Overview:
--
	Destroys our TUseableItem

--
Pre:

Post:


--
Revisions:
--
[2007/10/25] RaX - Created.
*-----------------------------------------------------------------------------*)
Destructor TUseableItem.Destroy;
Begin
	//Pre

	//--

	//Always clean up your owned objects/memory first, then call ancestor.
	inherited;
End;(* Dest TUseableItem.Destroy
*-----------------------------------------------------------------------------*)

end.
