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
September 29th 2008 - Tsusai - Corrected InventoryID spelling error.
*=============================================================================*)
TInventory = class(TObject)
protected
	fItemList : TInventoryList;
	ClientInfo : TIdContext;
	fCountItem	: Word;
	fCountEquip	: Word;
	fStackableList	: TIntList32;
	fWeight : LongWord;
	function GetItem(Index : Integer) : TItem;
	procedure IncreaseWeight(const AWeight:LongWord);
	procedure DecreaseWeight(const AWeight:LongWord);
	function IsStackable(const AnItem : TItemInstance):Boolean;
public
	InventoryID : LongWord;
	StorageID  : LongWord;
	UseID      : LongWord;
	EquipID    : LongWord;
	EtcID      : LongWord;
	
	property ItemList : TInventoryList read fItemList;
	property Items[Index : Integer] : TItem Read GetItem;
	property Countitem	: Word read fCountItem;
	property CountEquip	: Word read fCountEquip;
	property Weight : LongWord read fWeight;
	procedure Add(AnItem : TItem; Quantity : Word;const DontSend:Boolean=False);overload;
	procedure Add(var AnInventoryItem : TItemInstance;const DontSend:Boolean=False);overload;
	function Add(const ID:Word;const Quantity:Word):Boolean;overload;
	procedure Drop(const Index:Word;const Quantity:Word);
	procedure Remove(const OldItem:TItemInstance;const Quantity:Word;var NewItem:TItemInstance);overload;
	procedure Remove(const OldItem:TItemInstance;const Quantity:Word);overload;
	constructor Create(Parent : TObject);
	destructor Destroy;override;
end;(* TInventory
*== CLASS ====================================================================*)


implementation
uses
	{RTL/VCL}
	Types,
	Math,
	{Project}
	Character,
	PacketTypes,
	ZoneSend,
	UseableItem,
	EquipmentItem,
	MiscItem,
	Main,
	ErrorConstants,
	ParameterList,
	AreaLoopEvents
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
	fWeight := 0;
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
destructor TInventory.Destroy;
begin
	fStackableList.Free;

	fItemList.Free;

	inherited;
end;{Destroy}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
function TInventory.GetItem(Index: Integer) : TItem;
begin
	Result := TItem(fItemList[Index]);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
procedure TInventory.IncreaseWeight(const AWeight:LongWord);
begin
	fWeight := EnsureRange(AWeight + fWeight,0,High(LongWord));
	TClientLink(ClientInfo.Data).CharacterLink.Weight := fWeight;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
procedure TInventory.DecreaseWeight(const AWeight:LongWord);
begin
	fWeight := EnsureRange(fWeight - AWeight,0,High(LongWord));
	TClientLink(ClientInfo.Data).CharacterLink.Weight := fWeight;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
function TInventory.IsStackable(const AnItem : TItemInstance):Boolean;
begin
	Result := ((AnItem.Item is TUseableItem)OR
		(AnItem.Item is TMiscItem));
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
procedure TInventory.Add(AnItem: TItem; Quantity: Word;const DontSend:Boolean=False);
var
	Item : TItemInstance;
begin
	Item := TItemInstance.Create;
	Item.Item := AnItem;
	Item.Quantity := Quantity;
	Add(Item,DontSend);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
procedure TInventory.Add(var AnInventoryItem : TItemInstance;const DontSend:Boolean=False);
var
	ItemIndex : Word;
	Index : Integer;
	Amount : Word;
	Failed : Byte;
begin
	Failed := 0;
	Amount := AnInventoryItem.Quantity;
	ItemIndex := 0;
	if fItemList.Count >= MainProc.ZoneServer.Options.MaxItems then
	begin
		Failed := ADDITEM_TOOMUCH;
	end else
	begin
		{Ignore checking when loading inventory}
		if (not DontSend) AND ((TClientLink(ClientInfo.Data).CharacterLink.Weight + (AnInventoryItem.Item.Weight*Amount))
		> TClientLink(ClientInfo.Data).CharacterLink.MaxWeight) then
		begin
			Failed := ADDITEM_OVERWEIGHT;
		end else
		begin
			if IsStackable(AnInventoryItem) then
			begin
				Index := fStackableList.IndexOf(AnInventoryItem.Item.ID);
				if Index > -1 then
				begin
					if (TItemInstance(fStackableList.Objects[Index]).Quantity + AnInventoryItem.Quantity)
					> MainProc.ZoneServer.Options.MaxStackItem then
					begin
						Failed := ADDITEM_TOOMUCHSTACKING;
					end else
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
					end;
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
		end;
		if Failed = 0 then
		begin
			fWeight := EnsureRange(AnInventoryItem.Item.Weight*AnInventoryItem.Quantity + fWeight,0,High(LongWord));
		end;
	end;
	if not DontSend then
	begin
		if Failed > 0 then
		begin
			SendNewItemFailed(
				ClientInfo,
				Failed
			);
		end else
		begin
			TClientLink(ClientInfo.Data).CharacterLink.Weight :=
				TClientLink(ClientInfo.Data).CharacterLink.Weight + AnInventoryItem.Item.Weight*Amount;
			SendNewItem(
				ClientInfo,
				AnInventoryItem,
				ItemIndex,
				Amount
			);
		end;
	end;
end;{Add}
//------------------------------------------------------------------------------


function TInventory.Add(const ID:Word;const Quantity:Word):Boolean;
var
	AnItem : TItemInstance;
begin
	Result := False;
	if TThreadLink(ClientInfo.Data).DatabaseLink.Items.Find(ID) then
	begin
		AnItem := TItemInstance.Create;
		AnItem.Item := TItem.Create;
		AnItem.Item.ID := ID;
		AnItem.Quantity := Quantity;
		AnItem.Identified := True;
		TThreadLink(ClientInfo.Data).DatabaseLink.Items.Load(AnItem.Item);
		Add(AnItem);
		Result := True;
	end;
end;{Add}
//------------------------------------------------------------------------------

procedure TInventory.Drop(const Index:Word;const Quantity:Word);
var
	AChara : TCharacter;
	LoopIndex : Byte;
	Position : TPoint;
	FoundPosition : Boolean;
	TheItem : TItemInstance;
	NewItem : TItemInstance;
	ParameterList : TParameterList;
begin
	AChara := TClientLink(ClientInfo.Data).CharacterLink;
	if AChara.MapInfo.IsBlocked(AChara.Position) then
	begin
		//Standing on unwalkable area?
		Exit;
	end;
	TheItem := fItemList.IndexItems[Index-1];
	if (TheItem <> nil)AND(Quantity>0)AND(TheItem.Quantity >= Quantity) then
	begin
		Position.X:= AChara.Position.X + (Random(3)-1);
		Position.Y:= AChara.Position.Y + (Random(3)-1);
		FoundPosition := False;
		for LoopIndex := 1 to 6 do
		begin
			if  NOT AChara.MapInfo.IsBlocked(Position) then
			begin
				FoundPosition := True;
				Break;
			end;
			Position.X:= AChara.Position.X + (Random(3)-1);
			Position.Y:= AChara.Position.Y + (Random(3)-1);
		end;
		if not FoundPosition then
		begin
			Position := AChara.Position;
		end;
		{Improvement needed}

		if Quantity >= TheItem.Quantity then
		begin
			TheItem.X := Position.X;
			TheItem.Y := Position.Y;
			TheItem.MapID := AChara.MapInfo.ID;
			TThreadLink(ClientInfo.Data).DatabaseLink.Items.Save(
				TheItem,
				nil
			);
			{Deletem 'em all}
			SendDeleteItem(
				AChara,
				TheItem.Index,
				TheItem.Quantity
			);

			ParameterList := TParameterList.Create;
			ParameterList.AddAsObject(1,TheItem);
			ParameterList.AddAsLongWord(2,Quantity);
			AChara.AreaLoop(ShowDropitem,False,ParameterList);
			ParameterList.Free;

			fItemList.Delete(TheItem,True);
			DecreaseWeight(Quantity*TheItem.Item.Weight);
		end else
		begin
			Remove(
				TheItem,
				Quantity,
				NewItem
			);
			NewItem.X := Position.X;
			NewItem.Y := Position.Y;
			NewItem.MapID := AChara.MapInfo.ID;
			{FIX ME}
			TThreadLink(ClientInfo.Data).DatabaseLink.Items.New(
				NewItem,
				nil
			);

			ParameterList := TParameterList.Create;
			ParameterList.AddAsObject(1,NewItem);
			ParameterList.AddAsLongWord(2,Quantity);
			AChara.AreaLoop(ShowDropitem,False,ParameterList);
			ParameterList.Free;

			TheItem := NewItem;
		end;
		AChara.MapInfo.Cell[TheItem.X,TheItem.Y].Items.AddObject(TheItem.ID,TheItem);
		AChara.MapInfo.ItemList.AddObject(TheItem.ID,TheItem);
	end;
end;{Drop}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Remove                                                                FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		In situation such drop item,player not always drop everything.
//	In this case, item will split into two.
//	Quantity is amount for newitem.
//	You will also need to New in database.
//
//	Changes -
//		[2008/09/29] Aeomin - Created
//------------------------------------------------------------------------------
procedure TInventory.Remove(const OldItem:TItemInstance;const Quantity:Word;var NewItem:TItemInstance);
begin
	NewItem := nil;
	if Quantity < OldItem.Quantity then
	begin
		NewItem := TItemInstance.Create;
		OldItem.Quantity:= OldItem.Quantity - Quantity;
		NewItem.Quantity := Quantity;
		NewItem.Item := TItem.Create;
		NewItem.Item.ID := OldItem.Item.ID;
		NewItem.Identified := OldItem.Identified;
		TThreadLink(ClientInfo.Data).DatabaseLink.Items.Save(OldItem,Self);
		TThreadLink(ClientInfo.Data).DatabaseLink.Items.Load(NewItem.Item);
		SendDeleteItem(
			TClientLink(ClientInfo.Data).CharacterLink,
			OldItem.Index,
			Quantity
		);
		DecreaseWeight(Quantity*OldItem.Item.Weight);
	end else
	begin
		{Remove 'em}
		SendDeleteItem(
			TClientLink(ClientInfo.Data).CharacterLink,
			OldItem.Index,
			OldItem.Quantity
		);
		fItemList.Delete(OldItem);
		DecreaseWeight(OldItem.Quantity*OldItem.Item.Weight);
	end;
end;{Remove}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Remove                                                                FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Similar to above, except this removes item (such sell to npc)
//
//	Changes -
//		[2008/09/30] Aeomin - Created
//------------------------------------------------------------------------------
procedure TInventory.Remove(const OldItem:TItemInstance;const Quantity:Word);
begin
	if Quantity < OldItem.Quantity then
	begin
		OldItem.Quantity:= OldItem.Quantity - Quantity;
		TThreadLink(ClientInfo.Data).DatabaseLink.Items.Save(OldItem,Self);
		SendDeleteItem(
			TClientLink(ClientInfo.Data).CharacterLink,
			OldItem.Index,
			Quantity
		);
		DecreaseWeight(Quantity*OldItem.Item.Weight);
	end else
	begin
		{Remove 'em}
		SendDeleteItem(
			TClientLink(ClientInfo.Data).CharacterLink,
			OldItem.Index,
			OldItem.Quantity
		);
		fItemList.Delete(OldItem);
		DecreaseWeight(OldItem.Quantity*OldItem.Item.Weight);
	end;
end;{Remove}
//------------------------------------------------------------------------------

end{Inventory}.
