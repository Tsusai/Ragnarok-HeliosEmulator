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
	QueryBase,
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
		Procedure  Load(
			var AnItem : TItem
		);

		{Procedure Save(
			const AnItem : TItem
		); }

		{procedure  New(
			const AnItem : TItem
		);}

		{Procedure  Delete(
			const AnItem : TItem
		); }

		procedure FillInventory(
			const AnInventory : TInventory
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
	AQuery = 'SELECT `item_definition_id`,`amount`,`last_x`,`last_y`,`last_map_id` '+
	'FROM items ' +
	'WHERE `item_storage_id`=:ID';
var
	ADataSet	: TZQuery;
	AParam		: TParam;
	AItem		: TItem;
	Amount		: LongWord;
	procedure LoadUseable;
	begin
		ADataSet	:= TZQuery.Create(nil);
		try
			//UseID
			AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
			AParam.AsInteger := AnInventory.UseID;
			ADataSet.Params.AddParam(
				AParam
			);
			Query(ADataSet, AQuery);
			ADataSet.First;
			while NOT ADataSet.Eof do
			begin
				AItem := TItem.Create;
				AItem.ID := ADataSet.Fields[0].AsInteger;
				Amount := ADataSet.Fields[1].AsInteger;
				Load(AItem);
				AnInventory.Add(
					AItem,
					Amount,
					True
				);
				ADataSet.Next;
			end;
		finally
			ADataSet.Free;
		end;
	end;
	procedure LoadEquipment;
	begin
		ADataSet	:= TZQuery.Create(nil);
		try
			//EquipID
			AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
			AParam.AsInteger := AnInventory.EquipID;
			ADataSet.Params.AddParam(
				AParam
			);
			Query(ADataSet, AQuery);
			ADataSet.First;
			while NOT ADataSet.Eof do
			begin
				AItem := TItem.Create;
				AItem.ID := ADataSet.Fields[0].AsInteger;
				Amount := ADataSet.Fields[1].AsInteger;
				Load(AItem);
				AnInventory.Add(
					AItem,
					Amount,
					True
				);
				ADataSet.Next;
			end;
		finally
			ADataSet.Free;
		end;
	end;
	procedure LoadEtc;
	begin
		ADataSet	:= TZQuery.Create(nil);
		try
			//EtcID
			AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
			AParam.AsInteger := AnInventory.EtcID;
			ADataSet.Params.AddParam(
				AParam
			);
			Query(ADataSet, AQuery);
			ADataSet.First;
			while NOT ADataSet.Eof do
			begin
				AItem := TItem.Create;
				AItem.ID := ADataSet.Fields[0].AsInteger;
				Amount := ADataSet.Fields[1].AsInteger;
				Load(AItem);
				AnInventory.Add(
					AItem,
					Amount,
					True
				);
				ADataSet.Next;
			end;
		finally
			ADataSet.Free;
		end;
	end;
begin
	LoadUseable;
	LoadEquipment;
	LoadEtc;
end;{FillInventory}
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
