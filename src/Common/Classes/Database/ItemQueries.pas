//------------------------------------------------------------------------------
//ItemQueries                                                               UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Item related database routines
//
//	Changes -
//		February 28th, 2008 - RaX - Created
//
//------------------------------------------------------------------------------
unit ItemQueries;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	Classes,
	{Project}
	Item,
	EquipmentItem,
	UseableItem,
	MiscItem,
	Inventory,
	ItemInstance,
	QueryBase,
	Map,
	{3rd Party}
	ZSqlUpdate
	;


type

//------------------------------------------------------------------------------
//TItemQueries                                                             CLASS
//------------------------------------------------------------------------------
	TItemQueries = class(TQueryBase)

	protected
		Procedure LoadDefinition(
			const AnItem : TItem
		);

		Procedure LoadEquipmentDefinition(
			const AnItem : TEquipmentItem
		);

		procedure LoadUseableDefinition(
			const AnItem : TUseableItem
		);

		procedure LoadMiscDefinition(
			const AnItem : TMiscItem
		);

		{Procedure LoadInstance(
			const AnItem : TItem
		); }

	public
		procedure Load(
			var AnItem : TItem
		);

		procedure Save(
			const AnItem : TItemInstance;
			const AnInventory : TInventory
		);

		procedure New(
			const AnItem : TItemInstance;
			const AnInventory : TInventory
		);

		procedure Delete(
			const ID : LongWord
		);

		procedure FillInventory(
			const AnInventory : TInventory
		);
		procedure FillMapGround(
			const Map:TMap
		);

		function Find(
			const ID : Word
		):Boolean;overload;
		function Find(
			const Name : String
		):Word;overload;

	end;
//------------------------------------------------------------------------------


implementation


uses
	{RTL/VCL}
	SysUtils,
	Types,
	{Project}
	GameTypes,
	ItemTypes,
	{3rd Party}
	ZDataset,
	DB
	//none
	;


//------------------------------------------------------------------------------
//Load                                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Loads an item by ID or Name
//
//	Changes -
//		February 28th, 2008 - RaX - Created.
//		[2008/09/19] Aeomin - Small tweak.
//
//------------------------------------------------------------------------------
Procedure TItemQueries.Load(
	var AnItem : TItem
);
var
	EquipmentItem : TEquipmentItem;
	UseableItem	: TUseableItem;
	MiscItem	: TMiscItem;

	procedure ChangeType(const Input:TItem;const Output:TItem);
	begin
		Output.Name	:= Input.Name;
		Output.ID	:= Input.ID;
		Output.Weight	:= Input.Weight;
		Output.Price	:= Input.Price;
		Output.Sell	:= Input.Sell;
		Output.ItemType	:= Input.ItemType;
		Output.SpriteID	:= Input.SpriteID;
	end;
begin
	LoadDefinition(AnItem);
	//LoadInstance(AnItem);

	if AnItem.ItemType = Equipment then
	begin
		EquipmentItem := TEquipmentItem.Create;
		ChangeType(AnItem, EquipmentItem);
		AnItem.Free;
		LoadEquipmentDefinition(EquipmentItem);
		AnItem:=EquipmentItem;
	end else
	if AnItem.ItemType = Useable then
	begin
		UseableItem := TUseableItem.Create;
		ChangeType(AnItem, UseableItem);
		AnItem.Free;
		LoadUseableDefinition(UseableItem);
		AnItem:=UseableItem;
	end else
	if AnItem.ItemType = Misc then
	begin
		MiscItem := TMiscItem.Create;
		ChangeType(AnItem, MiscItem);
		AnItem.Free;
		LoadMiscDefinition(MiscItem);
		AnItem:=MiscItem;
	end;

end;//Load
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadDefinition                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Loads an item by ID or Name
//
//	Changes -
//		February 28th, 2008 - RaX - Created.
//		[2008/09/19] Aeomin - Changed AQuery from var to const.
//
//------------------------------------------------------------------------------
Procedure TItemQueries.LoadDefinition(
	const AnItem : TItem
);
const
	AQuery =
		'SELECT name, price_buy, weight, item_type, sprite_id '+
		'FROM itemdefinitions WHERE id=:ID';
var
	ADataSet		: TZQuery;
	AParam			: TParam;
begin
	ADataSet			:= TZQuery.Create(nil);
	try
		//Level
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := AnItem.ID;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		ADataset.First;
		if NOT ADataSet.Eof then
		begin
			AnItem.Name			:= ADataSet.Fields[0].AsString;
			AnItem.Price		:= ADataSet.Fields[1].AsInteger;
			AnItem.Sell		:= AnItem.Price DIV 2;
			AnItem.Weight		:= ADataSet.Fields[2].AsInteger;
			AnItem.ItemType	:= ByteToItemType(ADataSet.Fields[3].AsInteger);
			AnItem.SpriteID	:= ADataSet.Fields[4].AsInteger;
		end;


	finally
		ADataSet.Free;
	end;
end;//LoadDefinition
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadEquipmentDefinition                                              PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Loads an equipment item by definition ID
//
//	Changes -
//		February 28th, 2008 - RaX - Created.
//		[2008/09/19] Aeomin - Changed AQuery from var to const.
//
//------------------------------------------------------------------------------
Procedure TItemQueries.LoadEquipmentDefinition(
	const AnItem : TEquipmentItem
);
const
	AQuery =
		'SELECT slots, weapon_level, refinable, on_equip_function, on_unequip_function, '+
		'allowed_jobs, allowed_gender, equip_location, on_defend_function, '+
		'attack_rating, attack_range, on_attack_function '+
		'FROM itemdefinitionsequip WHERE item_definition_id=:ID';
var
	ADataSet		: TZQuery;
	AParam			: TParam;
begin
	ADataSet			:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := AnItem.ID;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		ADataset.First;
		if NOT ADataSet.Eof then
		begin
			AnItem.Slots			:= ADataset.Fields[0].AsInteger;
			AnItem.WeaponLevel		:= ADataset.Fields[1].AsInteger;
			AnItem.Refineable		:= StrToBoolDef(ADataset.Fields[2].AsString, FALSE);
			AnItem.OnEquip			:= ADataSet.Fields[3].AsString;
			AnItem.OnDisarm			:= ADataset.Fields[4].AsString;
			AnItem.Job			:= ADataset.Fields[5].AsInteger;
			AnItem.Gender			:= CharToGender(ADataset.Fields[6].AsString[1]);
			{AnItem.Defense			:= ADataset.Fields[7].AsInteger;}
			AnItem.EquipmentLocation	:= BytetoEquipLocations(ADataset.Fields[7].AsInteger);
			AnItem.OnDefend			:= ADataset.Fields[8].AsString;
			AnItem.Attack			:= ADataset.Fields[9].AsInteger;
			AnItem.Range			:= ADataset.Fields[10].AsInteger;
			AnItem.OnAttack			:= ADataset.Fields[11].AsString;
		end;


	finally
		ADataSet.Free;
	end;
end;//LoadDefinition
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadUseableDefinition                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Loads an usable item by definition ID
//
//	Changes -
//		[208/07/22] Aeomin - Created.
//		[2008/09/19] Aeomin - Changed AQuery from var to const.
//
//------------------------------------------------------------------------------
procedure TItemQueries.LoadUseableDefinition(
	const AnItem : TUseableItem
);
const
	AQuery =
	'SELECT on_use_function FROM itemdefinitionsuseable WHERE item_definition_id=:ID';
var
	ADataSet	: TZQuery;
	AParam		: TParam;
begin
	ADataSet	:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := AnItem.ID;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		ADataset.First;
		if NOT ADataSet.Eof then
		begin
			AnItem.OnUse		:= ADataset.Fields[0].AsString;
		end;
	finally
		ADataSet.Free;
	end;
end;{LoadUseableDefinition}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadMiscDefinition                                                   PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Loads an usable item by definition ID
//
//	Changes -
//		[2008/07/22] Aeomin - Created.
//		[2008/09/19] Aeomin - Changed AQuery from var to const.
//
//------------------------------------------------------------------------------
procedure TItemQueries.LoadMiscDefinition(
	const AnItem : TMiscItem
);
const
	AQuery = 'SELECT on_compound_function FROM itemdefinitionsmisc WHERE item_definition_id=:ID';
var
	ADataSet	: TZQuery;
	AParam		: TParam;
begin
	ADataSet	:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := AnItem.ID;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		ADataset.First;
		if NOT ADataSet.Eof then
		begin
			AnItem.OnCompound	:= ADataset.Fields[0].AsString;
		end;
	finally
		ADataSet.Free;
	end;
end;{LoadMiscDefinition}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Save                                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Save an item instance
//
//	Changes -
//		[2008/09/21] Aeomin - Created.
//
//------------------------------------------------------------------------------
procedure TItemQueries.Save(
	const AnItem : TItemInstance;
	const AnInventory : TInventory
);
const
	AQuery = 'UPDATE items SET item_storage_id=:StorageID, amount=:Amount, '+
	'`identified`=:Identified,`refined`:=Refined, last_x=:X, last_y=:Y, last_map_id=:MapID WHERE `id`=:ID';
var
	ADataSet	: TZQuery;
	AParam		: TParam;
begin
	ADataSet	:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := AnItem.ID;
		ADataSet.Params.AddParam(
			AParam
		);
		//StorageID
		AParam := ADataset.Params.CreateParam(ftInteger, 'StorageID', ptInput);
		if AnInventory = nil then
		begin
			AParam.AsInteger := 0;
		end else
		begin
			if AnItem.Item is TUseableItem then
				AParam.AsInteger := AnInventory.UseID
			else
			if AnItem.Item is TEquipmentItem then
				AParam.AsInteger := AnInventory.EquipID
			else
			if AnItem.Item is TMiscItem then
				AParam.AsInteger := AnInventory.EtcID;
		end;

		ADataSet.Params.AddParam(
			AParam
		);
		//Amount
		AParam := ADataset.Params.CreateParam(ftInteger, 'Amount', ptInput);
		AParam.AsInteger := AnItem.Quantity;
		ADataSet.Params.AddParam(
			AParam
		);
		//Identified
		AParam := ADataset.Params.CreateParam(ftBoolean, 'Identified', ptInput);
		AParam.AsInteger := Byte(AnItem.Identified);
		ADataSet.Params.AddParam(
			AParam
		);
		//Refined
		AParam := ADataset.Params.CreateParam(ftInteger, 'Refined', ptInput);
		AParam.AsInteger := AnItem.Refined;
		ADataSet.Params.AddParam(
			AParam
		);
		//X
		AParam := ADataset.Params.CreateParam(ftInteger, 'X', ptInput);
		AParam.AsInteger := AnItem.X;
		ADataSet.Params.AddParam(
			AParam
		);
		//Y
		AParam := ADataset.Params.CreateParam(ftInteger, 'Y', ptInput);
		AParam.AsInteger := AnItem.Y;
		ADataSet.Params.AddParam(
			AParam
		);
		//MapID
		AParam := ADataset.Params.CreateParam(ftInteger, 'MapID', ptInput);
		AParam.AsInteger := AnItem.MapID;
		ADataSet.Params.AddParam(
			AParam
		);
		QueryNoResult(ADataSet, AQuery);
	finally
		ADataSet.Free;
	end;
end;{Save}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//New                                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Create a new record in database
//
//	Changes -
//		[2008/09/21] Aeomin - Created.
//
//------------------------------------------------------------------------------
procedure TItemQueries.New(
	const AnItem : TItemInstance;
	const AnInventory : TInventory
);
const
	AQuery = 'INSERT INTO items (`item_definition_id`,`item_storage_id`,`amount`,`identified`,`last_x`,`last_y`,`last_map_id`) VALUES '+
	'(:DefinitionID, :StorageID, :Amount, :Identified, :X, :Y, :MapID);';
var
	ADataSet	: TZQuery;
	AParam		: TParam;
begin
	ADataSet	:= TZQuery.Create(nil);
	try
		//DefinitionID
		AParam := ADataset.Params.CreateParam(ftInteger, 'DefinitionID', ptInput);
		AParam.AsInteger := AnItem.Item.ID;
		ADataSet.Params.AddParam(
			AParam
		);
		//StorageID
		AParam := ADataset.Params.CreateParam(ftInteger, 'StorageID', ptInput);
		if AnInventory = nil then
		begin
			AParam.AsInteger := 0;
		end else
		begin
			if AnItem.Item is TUseableItem then
				AParam.AsInteger := AnInventory.UseID
			else
			if AnItem.Item is TEquipmentItem then
				AParam.AsInteger := AnInventory.EquipID
			else
			if AnItem.Item is TMiscItem then
				AParam.AsInteger := AnInventory.EtcID;
		end;
		ADataSet.Params.AddParam(
			AParam
		);
		//Amount
		AParam := ADataset.Params.CreateParam(ftInteger, 'Amount', ptInput);
		AParam.AsInteger := AnItem.Quantity;
		ADataSet.Params.AddParam(
			AParam
		);
		//Identified
		AParam := ADataset.Params.CreateParam(ftBoolean, 'Identified', ptInput);
		AParam.AsInteger := Byte(AnItem.Identified);
		ADataSet.Params.AddParam(
			AParam
		);
		//X
		AParam := ADataset.Params.CreateParam(ftInteger, 'X', ptInput);
		AParam.AsInteger := AnItem.X;
		ADataSet.Params.AddParam(
			AParam
		);
		//Y
		AParam := ADataset.Params.CreateParam(ftInteger, 'Y', ptInput);
		AParam.AsInteger := AnItem.Y;
		ADataSet.Params.AddParam(
			AParam
		);
		//MapID
		AParam := ADataset.Params.CreateParam(ftInteger, 'MapID', ptInput);
		AParam.AsInteger := AnItem.MapID;
		ADataSet.Params.AddParam(
			AParam
		);
		QueryNoResult(ADataSet, AQuery);
		AnItem.ID := LastID('`id`','`items`','WHERE item_definition_id='+IntToStr(AnItem.Item.ID));
	finally
		ADataSet.Free;
	end;
end;{New}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Delete                                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Delete an item instance
//
//	Changes -
//		[2008/10/03] Aeomin - Created.
//
//------------------------------------------------------------------------------
procedure TItemQueries.Delete(
	const ID : LongWord
);
const
	AQuery = 'DELETE FROM `items` WHERE id=:ID;';
var
	ADataSet	: TZQuery;
	AParam		: TParam;
begin
	ADataSet	:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := ID;
		ADataSet.Params.AddParam(
			AParam
		);
		QueryNoResult(ADataSet, AQuery);
	finally
		ADataSet.Free;
	end;
end;{Delete}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//FillInventory                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Load inventory of item
//
//	Changes -
//		[2008/09/17] Aeomin - Created.
//
//------------------------------------------------------------------------------
procedure TItemQueries.FillInventory(
	const AnInventory : TInventory
);
const
	AQuery = 'SELECT `id`,`item_definition_id`,`amount`,`identified`,`refined`,`last_x`,`last_y`,`last_map_id` '+
	'FROM items ' +
	'WHERE `item_storage_id` IN (:UseID,:EquipID,:MiscID) AND `last_map_id`=0;';
var
	ADataSet	: TZQuery;
	AParam		: TParam;
	AItem		: TItemInstance;
begin
	ADataSet	:= TZQuery.Create(nil);
	try
		//UseID
		AParam := ADataset.Params.CreateParam(ftInteger, 'UseID', ptInput);
		AParam.AsInteger := AnInventory.UseID;
		ADataSet.Params.AddParam(
			AParam
		);
		//EquipID
		AParam := ADataset.Params.CreateParam(ftInteger, 'EquipID', ptInput);
		AParam.AsInteger := AnInventory.EquipID;
		ADataSet.Params.AddParam(
			AParam
		);
		//MiscID
		AParam := ADataset.Params.CreateParam(ftInteger, 'MiscID', ptInput);
		AParam.AsInteger := AnInventory.EtcID;
		ADataSet.Params.AddParam(
			AParam
		);
		Query(ADataSet, AQuery);
		ADataSet.First;
		while NOT ADataSet.Eof do
		begin
			AItem := TItemInstance.Create;
			AItem.ID := ADataSet.Fields[0].AsInteger;
			AItem.Item := TItem.Create;
			AItem.Item.ID := ADataSet.Fields[1].AsInteger;
			AItem.Quantity := ADataSet.Fields[2].AsInteger;
			AItem.Identified := Boolean(ADataSet.Fields[3].AsInteger);
			AItem.Refined := ADataSet.Fields[4].AsInteger;;
			AItem.X := ADataSet.Fields[5].AsInteger;
			AItem.Y := ADataSet.Fields[6].AsInteger;
			AItem.MapID := ADataSet.Fields[7].AsInteger;
			Load(AItem.Item);
			AnInventory.Add(
				AItem,
				True
			);
			ADataSet.Next;
		end;
	finally
		ADataSet.Free;
	end;
end;{FillInventory}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//FillMapGround                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Load all items into map
//
//	Changes -
//		[2008/09/30] Aeomin - Created.
//
//------------------------------------------------------------------------------
procedure TItemQueries.FillMapGround(
	const Map:TMap
);
const
	AQuery = 'SELECT `id`,`item_definition_id`,`amount`,`identified`,`refined`,`last_x`,`last_y`,`last_map_id` '+
	'FROM items ' +
	'WHERE `last_map_id`=:MapID;';
var
	ADataSet	: TZQuery;
	AParam		: TParam;
	AItem		: TItemInstance;
begin
	ADataSet	:= TZQuery.Create(nil);
	try
		//MapID
		AParam := ADataset.Params.CreateParam(ftInteger, 'MapID', ptInput);
		AParam.AsInteger := Map.ID;
		ADataSet.Params.AddParam(
			AParam
		);
		Query(ADataSet, AQuery);
		ADataSet.First;
		while NOT ADataSet.Eof do
		begin
			AItem := TItemInstance.Create;
			AItem.ID := ADataSet.Fields[0].AsInteger;
			AItem.Item := TItem.Create;
			AItem.Item.ID := ADataSet.Fields[1].AsInteger;
			AItem.Quantity := ADataSet.Fields[2].AsInteger;
			AItem.Identified := Boolean(ADataSet.Fields[3].AsInteger);
			AItem.Refined := ADataSet.Fields[4].AsInteger;;
			AItem.X := ADataSet.Fields[5].AsInteger;
			AItem.Y := ADataSet.Fields[6].AsInteger;
			AItem.MapID := ADataSet.Fields[7].AsInteger;
			Load(AItem.Item);
			Map.Cell[AItem.X, AItem.Y].Items.AddObject(
				AItem.ID,
				AItem
			);
			ADataSet.Next;
		end;
	finally
		ADataSet.Free;
	end;
end;{FillMapGround}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Find                                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Check if item exists
//
//	Changes -
//		[2008/09/20] Aeomin - Created.
//
//------------------------------------------------------------------------------
function TItemQueries.Find(
	const ID : Word
):Boolean;
const
	AQuery = 'SELECT id FROM itemdefinitions WHERE id=:ID';
var
	ADataSet	: TZQuery;
	AParam		: TParam;
begin
	ADataSet	:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := ID;
		ADataSet.Params.AddParam(
			AParam
		);
		Query(ADataSet, AQuery);
		ADataSet.First;
		Result := NOT ADataSet.Eof;
	finally
		ADataSet.Free;
	end;
end;{Find}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Find                                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Check if item exists; return zero is not found.
//
//	Changes -
//		[2008/09/20] Aeomin - Created.
//
//------------------------------------------------------------------------------
function TItemQueries.Find(
	const Name : String
):Word;
const
	AQuery = 'SELECT id FROM itemdefinitions WHERE name=:Name';
var
	ADataSet	: TZQuery;
	AParam		: TParam;
begin
	Result := 0;
	ADataSet	:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftString, 'Name', ptInput);
		AParam.AsString := Name;
		ADataSet.Params.AddParam(
			AParam
		);
		Query(ADataSet, AQuery);
		ADataSet.First;
		if NOT ADataSet.Eof then
		begin
			Result := ADataSet.Fields[0].AsInteger;
		end;
	finally
		ADataSet.Free;
	end;
end;{Find}
//------------------------------------------------------------------------------
end.
