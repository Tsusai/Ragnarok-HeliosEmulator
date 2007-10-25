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
	fName : String;
	fID   : LongWord;

	Procedure SetName(Value : String);
	Function GetName : String;

	Procedure SetID(Value : LongWord);
	Function GetID : LongWord;

public
	Property Name : String Read GetName Write SetName;
	Property ID : LongWord Read GetID Write SetID;

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


Procedure TItem.SetName(Value: string);
begin
	fName := Value;
end;

Function TItem.GetName;
begin
	Result := fName;
end;

Procedure TItem.SetID(Value: Cardinal);
begin
	fID := Value;
end;

Function TItem.GetID;
begin
	Result := fID;
end;

end.