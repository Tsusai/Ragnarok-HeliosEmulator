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

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	{Project}
	Item,
	ItemTypes,
	GameTypes
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
[2007/10/26] RaX - Added numerous equipment related properties.
*=============================================================================*)
TEquipmentItem = class(TItem)
protected
	fViewID			: LongWord; //The view id, for the client to show the correct sprite.
	fEquipmentType		: TEquipTypes;//The equipment type(IE::Sword, Armor)
	fEquipmentLocation	: TEquipLocations; //What equipment locations this item resides in
	fAttack			: Word;  //this item's attack
	fDefense		: Word; //This item's defense
	fRange 			: Word; //The range of this item.
	fSlots			: Byte; //The number of slots this item has.
	fJob			: Word; //The required job.
	fGender			: TGenders; //The required gender, if applicable.
	fMinimumLevel		: Word;//The minimum base level required to equip this item
	fWeaponLevel		: Byte; //The weapon level of an item.
	fOnEquip		: String;//Script function, executes when the item is equipped.
	fOnDisarm		: String;//Script Function, executes when the item is unequipped.
	fOnAttack		: String;//Script function, executes when an Item is used to attack
	fOnDefend		: String;//Script function, executes when an Item is used to defend.

public
	Property ViewID		: LongWord
		Read fViewID
		Write fViewID;
	Property EquipmentType: TEquipTypes
		Read fEquipmentType
		write fEquipmentType;
	Property EquipmentLocation: TEquipLocations
		Read fEquipmentLocation
		write fEquipmentLocation;
	Property Attack		: Word
		Read fAttack
		Write fAttack;
	Property Defense	: Word
		Read fDefense
		Write fDefense;
	Property Range		: Word
		Read fRange
		Write fRange;
	Property Job 		: Word
		Read fJob
		Write fJob;
	Property Gender		: TGenders
		Read fGender
		Write fGender;
	Property MinimumLevel	: Word
		Read fMinimumLevel
		Write fMinimumLevel;
	Property WeaponLevel	: Byte
		Read fWeaponLevel
		Write fWeaponLevel;
	Property OnEquip	: String
		Read fOnEquip
		Write fOnEquip;
	Property OnDisarm	: String
		Read fOnDisarm
		Write fOnDisarm;
	Property OnAttack	: String
		Read fOnAttack
		Write fOnAttack;
	Property OnDefend	: String
		Read fOnDefend
		Write fOnDefend;

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
