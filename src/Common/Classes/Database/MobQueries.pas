unit MobQueries;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}

interface

uses
	{RTL/VCL}
	{Project}
	Mob,
	QueryBase,
	{3rd Party}
	ZSqlUpdate
	;

//------------------------------------------------------------------------------
//TMobQueries                                                              CLASS
//------------------------------------------------------------------------------
type
	TMobQueries = class(TQueryBase)
	protected

	public
		procedure Load(
			const AnMob : TMob
		);
	end;

implementation

uses
	GameConstants,
	ZDataset,
	DB;

//------------------------------------------------------------------------------
//Load                                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Load mob data by ID/sprite name
//
//	Changes -
//		[03/24/2008] Aeomin - Forged.
//
//------------------------------------------------------------------------------
procedure TMobQueries.Load(
	const AnMob : TMob
);
const
	AQuery =  //Changed Query Code slightly to conform with sql. [Spre]
		'SELECT `id`, `sprite_name`, `name`, `aRan` , `LV`, `HP`, `SP`, `str`, `int`, `vit`, `dex`, `agi`, `luk`, '+
		'`attack_rating_min`, `attack_rating_max`, `def`, `exp`, `jexp`, `as`, `es`, `mspeed`, `attackedmt`, `attackmt`, ' +
		'`property`, `scale`, `class`, `race`, `mdef`, `taming_item`, `food_item` ' +
		'FROM mobs';
var
	ADataSet	: TZQuery;
	AParam		: TParam;
	WhereClause	: String;
begin

	if AnMob.ID > 0 then
	begin
		WhereClause := ' WHERE id=:ID;';
	end else
	begin
		WhereClause := ' WHERE sprite_name=:Name;'
	end;

	ADataSet := TZQuery.Create(nil);

	//ID
	AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
	AParam.AsInteger := AnMob.ID;
	ADataSet.Params.AddParam(
		AParam
	);
	//Name
	AParam := ADataset.Params.CreateParam(ftString, 'Name', ptInput);
	AParam.AsString := AnMob.SpriteName;
	ADataSet.Params.AddParam(
		AParam
	);
	try
		Query(ADataSet, AQuery+WhereClause);
		ADataSet.First;
		if NOT ADataSet.Eof then
		begin
			with AnMob do
			begin
				ID               := ADataSet.Fields[0].AsInteger;
				SpriteName       := ADataSet.Fields[1].AsString;
				Name             := ADataSet.Fields[2].AsString;
				AttackRange      := ADataSet.Fields[3].AsInteger;
				BaseLV           := ADataSet.Fields[4].AsInteger;
				MaxHP            := ADataSet.Fields[5].AsInteger;
				MaxSP            := ADataSet.Fields[6].AsInteger;
				ParamBase[STR]   := ADataSet.Fields[7].AsInteger;
				ParamBase[AGI]   := ADataSet.Fields[8].AsInteger;
				ParamBase[VIT]   := ADataSet.Fields[9].AsInteger;
				ParamBase[INT]   := ADataSet.Fields[10].AsInteger;
				ParamBase[DEX]   := ADataSet.Fields[11].AsInteger;
				ParamBase[LUK]   := ADataSet.Fields[12].AsInteger;
				//Attack_Rating_min and Max Fields below [Spre]
				MinimumHit       := ADataSet.Fields[13].AsInteger;
				MaximumHit       := ADataSet.Fields[14].AsInteger;
				Defence          := ADataSet.Fields[15].AsInteger;
				BaseExp          := ADataSet.Fields[16].AsInteger;
				JobExp           := ADataSet.Fields[17].AsInteger;
				ASpeed           := ADataSet.Fields[18].AsInteger; //as = AttackSpeed [Spre]
				//es - Unknown
				Speed            := ADataSet.Fields[20].AsInteger;
				//attackedmt & attackmt - Unknown
				//What should we do for proerty..?
				Scale            := ADataSet.Fields[24].AsInteger;
				//Class?
				Race             := ADataSet.Fields[26].AsInteger;
				MDef             := ADataSet.Fields[27].AsInteger;
				TamingItem       := ADataSet.Fields[28].AsInteger;
				FoodItem         := ADataSet.Fields[29].AsInteger;
			end;
		end;
	finally
		ADataSet.Free;
	end;
end;{Load}
//------------------------------------------------------------------------------
end.