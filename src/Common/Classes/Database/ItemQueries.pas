//------------------------------------------------------------------------------
//ItemQueries						                                                   UNIT
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
	QueryBase,
	{3rd Party}
	ZSqlUpdate
	;


type

//------------------------------------------------------------------------------
//TItemQueries                                                       CLASS
//------------------------------------------------------------------------------
	TItemQueries = class(TQueryBase)

	protected
		Procedure LoadDefinition(
			const AnItem : TItem
		);

		Procedure LoadEquipmentDefinition(
			const AnItem : TEquipmentItem
		);

		{Procedure LoadUseableDefinition(
			const AnItem : TUseableItem
		);

		Procedure LoadMiscDefinition(
			const AnItem : TMiscItem
		);

		Procedure LoadInstance(
			const AnItem : TItem
		); }

	public
		Procedure  Load(
			const AnItem : TItem
		);

		{Procedure Save(
			const AnItem : TItem
		);

		Procedure  New(
			const AnItem : TItem
		);

		Procedure  Delete(
			const AnItem : TItem
		);

		Procedure  FillInventory(
			const AnInventory : TInventory
		);}


	end;
//------------------------------------------------------------------------------


implementation


uses
	{RTL/VCL}
	SysUtils,
	Types,
	{Project}
	GameTypes,
	UseableItem,
	MiscItem,
	{3rd Party}
	ZDataset,
	DB
	//none
	;


//------------------------------------------------------------------------------
//Load				          		                                      		PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Loads an item by ID or Name
//
//	Changes -
//		February 28th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TItemQueries.Load(
	const AnItem : TItem
);
begin

	LoadDefinition(AnItem);
	//LoadInstance(AnItem);

	if AnItem IS TEquipmentItem then
	begin
		LoadEquipmentDefinition(TEquipmentItem(AnItem));
	end;

	if AnItem IS TUseableItem then
	begin
	 //	LoadUseableDefinition(TUseableItem(AnItem));
	end;

	if AnItem IS TMiscItem then
	begin
	 //	LoadMiscDefinition(TMiscItem(AnItem));
	end;

end;//Load
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadDefinition        		                                      		PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Loads an item by ID or Name
//
//	Changes -
//		February 28th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TItemQueries.LoadDefinition(
	const AnItem : TItem
);
var
	ADataSet		: TZQuery;
	AParam			: TParam;
	AQuery			: String;
begin

	AQuery :=
		'SELECT name, price_buy, price_sell, weight, item_type, sprite_id '+
		'FROM itemdefinitions WHERE id=:ID';

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
			AnItem.Sell			:= ADataSet.Fields[2].AsInteger;
			AnItem.Weight		:= ADataSet.Fields[3].AsInteger;
			//AnItem.ItemType	:= ByteToItemType(ADataSet.Fields[4].AsInteger);
			AnItem.SpriteID	:= ADataSet.Fields[5].AsInteger;
		end;


	finally
		ADataSet.Free;
	end;
end;//LoadDefinition
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadEquipmentDefinition        		                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Loads an equipment item by definition ID
//
//	Changes -
//		February 28th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TItemQueries.LoadEquipmentDefinition(
	const AnItem : TEquipmentItem
);
var
	ADataSet		: TZQuery;
	AParam			: TParam;
	AQuery			: String;
begin

	AQuery :=
		'SELECT slots, refinement_level, refineable, on_equip_function, on_unequip_function, '+
		'allowed_jobs, allowed_gender, defense_rating, body_region, on_defend_function, '+
		'attack_rating, range, on_attack_function '+
		'FROM itemdefinitionsequip WHERE item_definition_id=:ID';

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
			AnItem.Slots							:= ADataset.Fields[0].AsInteger;
			AnItem.WeaponLevel				:= ADataset.Fields[1].AsInteger;
			AnItem.Refineable					:= StrToBoolDef(ADataset.Fields[2].AsString, FALSE);
			AnItem.OnEquip						:= ADataSet.Fields[3].AsString;
			AnItem.OnDisarm						:= ADataset.Fields[4].AsString;
			AnItem.Job								:= ADataset.Fields[5].AsInteger;
			AnItem.Gender							:= CharToGender(ADataset.Fields[6].AsString[1]);
			AnItem.Defense						:= ADataset.Fields[7].AsInteger;
			//AnItem.EquipmentLocation	:= BytetoEquipLocations(ADataset.Fields[8].AsInteger);
			AnItem.OnDefend						:= ADataset.Fields[9].AsString;
			AnItem.Attack							:= ADataset.Fields[10].AsInteger;
			AnItem.Range							:= ADataset.Fields[11].AsInteger;
			AnItem.OnAttack						:= ADataset.Fields[12].AsString;
		end;


	finally
		ADataSet.Free;
	end;
end;//LoadDefinition
//------------------------------------------------------------------------------
{
//------------------------------------------------------------------------------
//GetMaxSP				          		                                      FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets the max sp for a job at a level
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
function TitemQueries.GetMaxSP(
	const ACharacter: TCharacter
) : LongWord;

var
	ADataSet		: TZQuery;
	AParam			: TParam;
	AQuery			: String;
begin

	AQuery :=
		'SELECT '+ACharacter.JobName+' FROM sp WHERE level=:Level;';
	Result := 0;
	ADataSet			:= TZQuery.Create(nil);
	try
		//JobName
		AParam := ADataset.Params.CreateParam(ftString, 'JobName', ptInput);
		AParam.AsString := ACharacter.JobName;
		ADataSet.Params.AddParam(
			AParam
		);
		//Level
		AParam := ADataset.Params.CreateParam(ftInteger, 'Level', ptInput);
		AParam.AsInteger := ACharacter.BaseLV;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		ADataset.First;
		if NOT ADataSet.Eof then
		begin
			Result := ADataset.Fields[0].AsInteger;
		end;


	finally
		ADataSet.Free;
	end;
end;//GetMaxSP
//------------------------------------------------------------------------------
}
end.
