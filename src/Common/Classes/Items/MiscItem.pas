(*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

Unit
MiscItem

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

all misc Items.

================================================================================
Revisions:
================================================================================
(Format: [yyyy/mm/dd] <Author> - <Desc of Changes>)
[2007/10/25] RaX - Created.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)
unit MiscItem;


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
TMiscItem

*------------------------------------------------------------------------------*
Overview:
*------------------------------------------------------------------------------*

	All Misc Items

*------------------------------------------------------------------------------*
Revisions:
*------------------------------------------------------------------------------*
(Format: [yyyy/mm/dd] <Author> - <Description of Change>)
[2007/10/25] RaX - Created.
*=============================================================================*)
TMiscItem = class(TItem)
protected

public

	Constructor Create;
	Destructor Destroy;override;

End;(* TMiscItem
*== CLASS ====================================================================*)


implementation


//uses
	{RTL/VCL}
	{Project}

	{Third Party}
	//none

(*- Cons ----------------------------------------------------------------------*
TMiscItem.Create
--------------------------------------------------------------------------------
Overview:
--

Creates our TMiscItem.

--
Post:

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/10/25] RaX - Created.
*-----------------------------------------------------------------------------*)
Constructor TMiscItem.Create;
begin
	inherited;

End; (* Cons TBeing.Create
*-----------------------------------------------------------------------------*)


(*- Dest ----------------------------------------------------------------------*
TMiscItem.Destroy

--
Overview:
--
	Destroys our TMiscItem

--
Pre:

Post:


--
Revisions:
--
[2007/10/25] RaX - Created.
*-----------------------------------------------------------------------------*)
Destructor TMiscItem.Destroy;
Begin
	//Pre

	//--

	//Always clean up your owned objects/memory first, then call ancestor.
	inherited;
End;(* Dest TMiscItem.Destroy
*-----------------------------------------------------------------------------*)

end.