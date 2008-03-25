//------------------------------------------------------------------------------
//CharacterQueries			                                                   UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Character related database routines
//
//	Changes -
//		February 12th, 2008
//
//------------------------------------------------------------------------------
unit CharacterQueries;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	{Project}
	Account,
	Character,
	QueryBase,
	CharaList,
	{3rd Party}
	ZSQLUpdate
	;


type

//------------------------------------------------------------------------------
//TCharacterQueries                                                       CLASS
//------------------------------------------------------------------------------
	TCharacterQueries = class(TQueryBase)

	protected


	public
		Function Exists(
			const ACharacter : TCharacter
		) : Boolean;

		Procedure Load(
			const ACharacter : TCharacter
		);

		Function GetName(
			const ACharacterID : LongWord
		) : String;

		Procedure LoadByAccount(
			const ACharacterList		: TCharacterList;
			const AnAccount					: TAccount
		);

		Procedure Save(
			const ACharacter : TCharacter
		);

		Procedure New(
			const ACharacter : TCharacter
		);

    Procedure Delete(
			const ACharacter : TCharacter
		);

		function GetVariable(
			const ACharacter	: TCharacter;
			const Key					: string
			) : string;

		procedure SetVariable(
			const ACharacter	: TCharacter;
			const Key					: string;
			const Value				: string
		);

	end;
//------------------------------------------------------------------------------


implementation


uses
	{RTL/VCL}
	SysUtils,
	Types,
	{Project}
	Main,
	Globals,
	GameConstants,
	{3rd Party}
	ZDataset,
	DB
	//none
	;

//------------------------------------------------------------------------------
//Load							                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Loads an Character
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TCharacterQueries.Load(
	const ACharacter : TCharacter
);

const
	AQuery =
		'SELECT `id`, `account_id`, `name`, `slot`, `job_id`, `base_level`, `job_level`, `base_exp`, '+
		'`job_exp`, `zeny`, `str`, `agi`, `vit`, `int`, `dex`, `luk`, `status_points`, `skill_points`, '+
		'`current_hp`, `current_sp`, `hair_style`, `hair_color`, `clothes_color`, `option`, `inventory_id`, '+
		'`storage_id`, `cart_inventory_id`, `righthand_item`, `lefthand_item`, `armor_item`, '+
		'`garment_item`, `shoes_item`, `accessory1_item`, `accessory2_item`, `head_top_item`, '+
		'`head_middle_item`, `head_bottom_item`, `last_map`, `last_map_x`, `last_map_y`, '+
		'`save_map`, `save_map_x`, `save_map_y`, `partner_id`, `parent1_id`, `parent2_id`, '+
		'`party_id`, `guild_id`, `is_online` '+
		'FROM characters';

var
	WhereClause : String;
	ADataSet		: TZQuery;
	AParam			: TParam;
	APoint			: TPoint;
begin
	if ACharacter.ID > 0 then
	begin
		WhereClause := ' WHERE id=:ID;';
	end else
	begin
		WhereClause := ' WHERE name=:Name;'
	end;

	ADataSet			:= TZQuery.Create(nil);
	//ID
	AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
	AParam.AsInteger := ACharacter.ID;
	ADataSet.Params.AddParam(
		AParam
	);
	//Name
	AParam := ADataset.Params.CreateParam(ftString, 'Name', ptInput);
	AParam.AsString := ACharacter.Name;
	ADataSet.Params.AddParam(
		AParam
	);
	//

	try
		Query(ADataSet, AQuery+WhereClause);
		ADataSet.First;
		if NOT ADataSet.Eof then
		begin
			//fill character data
			with ACharacter do
			begin
				ID            	 := ADataSet.Fields[0].AsInteger;
				AccountID				 := ADataSet.Fields[1].AsInteger;
				Name             := ADataSet.Fields[2].AsString;
				CharaNum         := ADataSet.Fields[3].AsInteger;
				JID              := ADataSet.Fields[4].AsInteger;
				BaseLV           := ADataSet.Fields[5].AsInteger;
				JobLV            := ADataSet.Fields[6].AsInteger;
				BaseEXP          := ADataSet.Fields[7].AsInteger;
				JobEXP           := ADataSet.Fields[8].AsInteger;
				Zeny             := ADataSet.Fields[9].AsInteger;
				ParamBase[STR]   := ADataSet.Fields[10].AsInteger;
				ParamBase[AGI]   := ADataSet.Fields[11].AsInteger;
				ParamBase[VIT]   := ADataSet.Fields[12].AsInteger;
				ParamBase[INT]   := ADataSet.Fields[13].AsInteger;
				ParamBase[DEX]   := ADataSet.Fields[14].AsInteger;
				ParamBase[LUK]   := ADataSet.Fields[15].AsInteger;
				StatusPts        := ADataSet.Fields[16].AsInteger;
				SkillPts         := ADataSet.Fields[17].AsInteger;
				HP               := ADataSet.Fields[18].AsInteger;
				SP               := ADataSet.Fields[19].AsInteger;
				Hair             := ADataSet.Fields[20].AsInteger;
				HairColor        := ADataSet.Fields[21].AsInteger;
				ClothesColor     := ADataSet.Fields[22].AsInteger;
				Option           := ADataSet.Fields[23].AsInteger;
				//InventoryID      := ADataSet.Fields[24].AsInteger;
				//StorageID				 := ADataSet.Fields[25].AsInteger;
				//CartInventoryID	 := ADataSet.Fields[26].AsInteger;
				RightHand        := ADataSet.Fields[27].AsInteger;
				LeftHand         := ADataSet.Fields[28].AsInteger;
				Armor        		 := ADataSet.Fields[29].AsInteger;
				Garment        	 := ADataSet.Fields[30].AsInteger;
				Shoes        		 := ADataSet.Fields[31].AsInteger;
				Accessory1       := ADataSet.Fields[32].AsInteger;
				Accessory2       := ADataSet.Fields[33].AsInteger;
				HeadTop          := ADataSet.Fields[34].AsInteger;
				HeadMid          := ADataSet.Fields[35].AsInteger;
				HeadBottom       := ADataSet.Fields[36].AsInteger;
				Map              := ADataSet.Fields[37].AsString;
					APoint.X       := ADataSet.Fields[38].AsInteger;
					APoint.Y       := ADataSet.Fields[39].AsInteger;
				Position         := APoint;
				SaveMap          := ADataSet.Fields[40].AsString;
					APoint.X       := ADataSet.Fields[41].AsInteger;
					APoint.Y       := ADataSet.Fields[42].AsInteger;
				SavePoint        := APoint;
				PartnerID        := ADataSet.Fields[43].AsInteger;
				ParentID1        := ADataSet.Fields[44].AsInteger;
				ParentID2        := ADataSet.Fields[45].AsInteger;
				PartyID          := ADataSet.Fields[46].AsInteger;
				GuildID          := ADataSet.Fields[47].AsInteger;
				Online           := ADataSet.Fields[48].AsInteger;

				CalcMaxWeight;
				CalcMaxHP;
				CalcMaxSP;
				CalcSpeed;
				CalcASpeed;
			end;
		end;

	finally
		ADataSet.Free;
	end;

	Inherited;
end;//Load
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetName								                                              FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a character's name by it's ID
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
Function TCharacterQueries.GetName(
	const ACharacterID			: LongWord
) : String;
const
	AQuery =
		'SELECT `name` FROM characters WHERE id=:ID;';

var
	ADataSet		: TZQuery;
	AParam			: TParam;

begin

	ADataSet			:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := ACharacterID;
		ADataSet.Params.AddParam(
			AParam
		);
		Query(ADataSet, AQuery);
		ADataSet.First;
		if NOT ADataSet.Eof then
		begin
			Result := ADataSet.Fields[0].AsString;
		end;
	finally
		ADataSet.Free;
	end;
end;//GetName
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadByAccountID				                                              PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Loads a list of characters by account id
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TCharacterQueries.LoadByAccount(
	const ACharacterList		: TCharacterList;
	const AnAccount					: TAccount
);
const
	AQuery =
		'SELECT `id` FROM characters WHERE account_id=:AccountID;';

var
	ADataSet		: TZQuery;
	AParam			: TParam;
	ACharacter	: TCharacter;
begin

	ADataSet			:= TZQuery.Create(nil);
	try
		//AccountID
		AParam := ADataset.Params.CreateParam(ftInteger, 'AccountID', ptInput);
		AParam.AsInteger := AnAccount.ID;
		ADataSet.Params.AddParam(
			AParam
		);
		Query(ADataSet, AQuery);
		ADataSet.First;
		while NOT ADataSet.Eof do
		begin
			ACharacter				:= TCharacter.Create(AnAccount.ClientInfo);
			ACharacter.ID			:= ADataSet.Fields[0].AsInteger;
			Load(ACharacter);
			ACharacterList.Add(ACharacter);
			ADataSet.Next;
		end;
	finally
		ADataSet.Free;
	end;
end;//LoadByAccountID
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Exists							                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Checks to see if a character exists
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
Function TCharacterQueries.Exists(
	const ACharacter : TCharacter
) : Boolean;

const
	AQuery =
		'SELECT `id` FROM characters';

var
	WhereClause : String;
	ADataSet		: TZQuery;
	AParam			: TParam;
begin
	Result := TRUE;

	if ACharacter.ID > 0 then
	begin
		WhereClause := ' WHERE id=:ID';
	end else
	begin
		WhereClause := ' WHERE name=:Name'
	end;

	if ACharacter.CharaNum <> 255 then
	begin
		WhereClause := WhereClause + ' AND Slot=:Slot;';
	end else
	begin
		WhereClause := WhereClause + ';';
  end;
	

	ADataSet			:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := ACharacter.ID;
		ADataSet.Params.AddParam(
			AParam
		);
		//Name
		AParam := ADataset.Params.CreateParam(ftString, 'Name', ptInput);
		AParam.AsString := ACharacter.Name;
		ADataSet.Params.AddParam(
			AParam
		);
		//Slot
		AParam := ADataset.Params.CreateParam(ftInteger, 'Slot', ptInput);
		AParam.AsInteger := ACharacter.CharaNum;
		ADataSet.Params.AddParam(
			AParam
		);


		Query(ADataSet, AQuery+WhereClause);
		ADataset.First;
		if ADataSet.Eof then
		begin
			Result := FALSE;
		end;

	finally
		ADataSet.Free;
	end;
end;//Exists
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Save				 						      																			 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Save a character.
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TCharacterQueries.Save(
	const
		ACharacter : TCharacter
	);
const

	AQuery =
		'UPDATE characters SET ' +
			'`slot`=:Slot, ' +
			'`name`=:Name, ' +
			'`job_id`=:JobID, ' +
			'`base_level`=:BaseLevel, ' +
			'`job_level`=:JobLevel, ' +
			'`base_exp`=:BaseExp, ' +
			'`job_exp`=:JobExp, ' +
			'`zeny`=:Zeny, ' +
			'`str`=:Str, ' +
			'`agi`=:Agi, ' +
			'`vit`=:Vit, ' +
			'`int`=:Int, ' +
			'`dex`=:Dex, ' +
			'`luk`=:Luk, ' +
			'`max_hp`=:MaxHP, ' +
			'`current_hp`=:CurrentHP, ' +
			'`max_sp`=:MaxSP, ' +
			'`current_sp`=:CurrentSP, ' +
			'`status_points`=:StatusPoints, ' +
			'`skill_points`=:SkillPoints, ' +
			'`option`=:Option, ' +
			//'inventory_id=:InventoryID, '+
			//'storage_id=:StorageID, '+
			//'cart_inventory_id=:CartInventoryID, '+
			'`party_id`=:PartyID, ' +
			'`guild_id`=:GuildID, ' +
			'`hair_style`=:HairStyle, ' +
			'`hair_color`=:HairColor, ' +
			'`clothes_color`=:ClothesColor, ' +
			'`righthand_item`=:RightHand, ' +
			'`lefthand_item`=:LeftHand, ' +
			'`armor_item`=:Armor, ' +
			'`garment_item`=:Garment, ' +
			'`shoes_item`=:Shoes, ' +
			'`accessory1_item`=:Accessory1, ' +
			'`accessory2_item`=:Accessory2, ' +
			'`head_top_item`=:HeadTop, ' +
			'`head_middle_item`=:HeadMiddle, ' +
			'`head_bottom_item`=:HeadBottom, ' +
			'`last_map`=:LastMap, ' +
			'`last_map_x`=:LastMapX, ' +
			'`last_map_y`=:LastMapY, ' +
			'`save_map`=:SaveMap, ' +
			'`save_map_x`=:SaveMapX, ' +
			'`save_map_y`=:SaveMapY, ' +
			'`partner_id`=:PartnerID, ' +
			'`parent1_id`=:Parent1ID, ' +
			'`parent2_id`=:Parent2ID, ' +
			'`is_online`=:Online ' +
			'WHERE id=:ID;';

var
	ADataSet		: TZQuery;
	AParam			: TParam;
begin
	ADataSet			:= TZQuery.Create(nil);

	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := ACharacter.ID;
		ADataSet.Params.AddParam(
			AParam
		);
		//Slot
		AParam := ADataset.Params.CreateParam(ftInteger, 'Slot', ptInput);
		AParam.AsInteger := ACharacter.CharaNum;
		ADataSet.Params.AddParam(
			AParam
		);
		//Name
		AParam := ADataset.Params.CreateParam(ftString, 'Name', ptInput);
		AParam.AsString := ACharacter.Name;
		ADataSet.Params.AddParam(
			AParam
		);
		//JobID
		AParam := ADataset.Params.CreateParam(ftInteger, 'JobID', ptInput);
		AParam.AsInteger := ACharacter.JID;
		ADataSet.Params.AddParam(
			AParam
		);
		//BaseLevel
		AParam := ADataset.Params.CreateParam(ftInteger, 'BaseLevel', ptInput);
		AParam.AsInteger := ACharacter.BaseLV;
		ADataSet.Params.AddParam(
			AParam
		);
		//JobLevel
		AParam := ADataset.Params.CreateParam(ftInteger, 'JobLevel', ptInput);
		AParam.AsInteger := ACharacter.JobLV;
		ADataSet.Params.AddParam(
			AParam
		);
		//BaseExp
		AParam := ADataset.Params.CreateParam(ftInteger, 'BaseExp', ptInput);
		AParam.AsInteger := ACharacter.BaseEXP;
		ADataSet.Params.AddParam(
			AParam
		);
		//JobExp
		AParam := ADataset.Params.CreateParam(ftInteger, 'JobExp', ptInput);
		AParam.AsInteger := ACharacter.JobEXP;
		ADataSet.Params.AddParam(
			AParam
		);
		//Zeny
		AParam := ADataset.Params.CreateParam(ftInteger, 'Zeny', ptInput);
		AParam.AsInteger := ACharacter.Zeny;
		ADataSet.Params.AddParam(
			AParam
		);
		//Luk
		AParam := ADataset.Params.CreateParam(ftInteger, 'Luk', ptInput);
		AParam.AsInteger := ACharacter.ParamBase[LUK];
		ADataSet.Params.AddParam(
			AParam
		);
		//Str
		AParam := ADataset.Params.CreateParam(ftInteger, 'Str', ptInput);
		AParam.AsInteger := ACharacter.ParamBase[STR];
		ADataSet.Params.AddParam(
			AParam
		);
		//Agi
		AParam := ADataset.Params.CreateParam(ftInteger, 'Agi', ptInput);
		AParam.AsInteger := ACharacter.ParamBase[AGI];
		ADataSet.Params.AddParam(
			AParam
		);
		//Dex
		AParam := ADataset.Params.CreateParam(ftInteger, 'Dex', ptInput);
		AParam.AsInteger := ACharacter.ParamBase[DEX];
		ADataSet.Params.AddParam(
			AParam
		);
		//Vit
		AParam := ADataset.Params.CreateParam(ftInteger, 'Vit', ptInput);
		AParam.AsInteger := ACharacter.ParamBase[VIT];
		ADataSet.Params.AddParam(
			AParam
		);
		//Int
		AParam := ADataset.Params.CreateParam(ftInteger, 'Int', ptInput);
		AParam.AsInteger := ACharacter.ParamBase[INT];
		ADataSet.Params.AddParam(
			AParam
		);
		//MaxHP
		AParam := ADataset.Params.CreateParam(ftInteger, 'MaxHP', ptInput);
		AParam.AsInteger := ACharacter.MaxHP;
		ADataSet.Params.AddParam(
			AParam
		);
		//MaxSP
		AParam := ADataset.Params.CreateParam(ftInteger, 'MaxSP', ptInput);
		AParam.AsInteger := ACharacter.MaxSP;
		ADataSet.Params.AddParam(
			AParam
		);
		//CurrentHP
		AParam := ADataset.Params.CreateParam(ftInteger, 'CurrentHP', ptInput);
		AParam.AsInteger := ACharacter.HP;
		ADataSet.Params.AddParam(
			AParam
		);
		//CurrentSP
		AParam := ADataset.Params.CreateParam(ftInteger, 'CurrentSP', ptInput);
		AParam.AsInteger := ACharacter.SP;
		ADataSet.Params.AddParam(
			AParam
		);
		//StatusPoints
		AParam := ADataset.Params.CreateParam(ftInteger, 'StatusPoints', ptInput);
		AParam.AsInteger := ACharacter.StatusPts;
		ADataSet.Params.AddParam(
			AParam
		);
		//SkillPoints
		AParam := ADataset.Params.CreateParam(ftInteger, 'SkillPoints', ptInput);
		AParam.AsInteger := ACharacter.SkillPts;
		ADataSet.Params.AddParam(
			AParam
		);
		//Option
		AParam := ADataset.Params.CreateParam(ftInteger, 'Option', ptInput);
		AParam.AsInteger := ACharacter.Option;
		ADataSet.Params.AddParam(
			AParam
		);
		//PartyID
		AParam := ADataset.Params.CreateParam(ftInteger, 'PartyID', ptInput);
		AParam.AsInteger := ACharacter.PartyID;
		ADataSet.Params.AddParam(
			AParam
		);
		//GuildID
		AParam := ADataset.Params.CreateParam(ftInteger, 'GuildID', ptInput);
		AParam.AsInteger := ACharacter.GuildID;
		ADataSet.Params.AddParam(
			AParam
		);
		//HairStyle
		AParam := ADataset.Params.CreateParam(ftInteger, 'HairStyle', ptInput);
		AParam.AsInteger := ACharacter.Hair;
		ADataSet.Params.AddParam(
			AParam
		);
		//HairColor
		AParam := ADataset.Params.CreateParam(ftInteger, 'HairColor', ptInput);
		AParam.AsInteger := ACharacter.HairColor;
		ADataSet.Params.AddParam(
			AParam
		);
		//ClothesColor
		AParam := ADataset.Params.CreateParam(ftInteger, 'ClothesColor', ptInput);
		AParam.AsInteger := ACharacter.ClothesColor;
		ADataSet.Params.AddParam(
			AParam
		);
		//RightHand
		AParam := ADataset.Params.CreateParam(ftInteger, 'RightHand', ptInput);
		AParam.AsInteger := ACharacter.RightHand;
		ADataSet.Params.AddParam(
			AParam
		);
		//LeftHand
		AParam := ADataset.Params.CreateParam(ftInteger, 'LeftHand', ptInput);
		AParam.AsInteger := ACharacter.LeftHand;
		ADataSet.Params.AddParam(
			AParam
		);
		//Armor
		AParam := ADataset.Params.CreateParam(ftInteger, 'Armor', ptInput);
		AParam.AsInteger := ACharacter.RightHand;
		ADataSet.Params.AddParam(
			AParam
		);
		//Garment
		AParam := ADataset.Params.CreateParam(ftInteger, 'Garment', ptInput);
		AParam.AsInteger := ACharacter.Garment;
		ADataSet.Params.AddParam(
			AParam
		);
		//Shoes
		AParam := ADataset.Params.CreateParam(ftInteger, 'Shoes', ptInput);
		AParam.AsInteger := ACharacter.Shoes;
		ADataSet.Params.AddParam(
			AParam
		);
		//Accessory1
		AParam := ADataset.Params.CreateParam(ftInteger, 'Accessory1', ptInput);
		AParam.AsInteger := ACharacter.Accessory1;
		ADataSet.Params.AddParam(
			AParam
		);
		//Accessory2
		AParam := ADataset.Params.CreateParam(ftInteger, 'Accessory2', ptInput);
		AParam.AsInteger := ACharacter.Accessory2;
		ADataSet.Params.AddParam(
			AParam
		);
		//HeadTop
		AParam := ADataset.Params.CreateParam(ftInteger, 'HeadTop', ptInput);
		AParam.AsInteger := ACharacter.HeadTop;
		ADataSet.Params.AddParam(
			AParam
		);
		//HeadMiddle
		AParam := ADataset.Params.CreateParam(ftInteger, 'HeadMiddle', ptInput);
		AParam.AsInteger := ACharacter.HeadMid;
		ADataSet.Params.AddParam(
			AParam
		);
		//HeadBottom
		AParam := ADataset.Params.CreateParam(ftInteger, 'HeadBottom', ptInput);
		AParam.AsInteger := ACharacter.HeadBottom;
		ADataSet.Params.AddParam(
			AParam
		);
		//LastMapID
		AParam := ADataset.Params.CreateParam(ftString, 'LastMap', ptInput);
		AParam.AsString := ACharacter.Map;
		ADataSet.Params.AddParam(
			AParam
		);
		//LastMapX
		AParam := ADataset.Params.CreateParam(ftInteger, 'LastMapX', ptInput);
		AParam.AsInteger := ACharacter.Position.X;
		ADataSet.Params.AddParam(
			AParam
		);
		//LastMapY
		AParam := ADataset.Params.CreateParam(ftInteger, 'LastMapY', ptInput);
		AParam.AsInteger := ACharacter.Position.Y;
		ADataSet.Params.AddParam(
			AParam
		);
		//SaveMapID
		AParam := ADataset.Params.CreateParam(ftString, 'SaveMap', ptInput);
		AParam.AsString := ACharacter.SaveMap;
		ADataSet.Params.AddParam(
			AParam
		);
		//SaveMapX
		AParam := ADataset.Params.CreateParam(ftInteger, 'SaveMapX', ptInput);
		AParam.AsInteger := ACharacter.SavePoint.X;
		ADataSet.Params.AddParam(
			AParam
		);
		//SaveMapY
		AParam := ADataset.Params.CreateParam(ftInteger, 'SaveMapY', ptInput);
		AParam.AsInteger := ACharacter.SavePoint.Y;
		ADataSet.Params.AddParam(
			AParam
		);
		//PartnerID
		AParam := ADataset.Params.CreateParam(ftInteger, 'PartnerID', ptInput);
		AParam.AsInteger := ACharacter.PartnerID;
		ADataSet.Params.AddParam(
			AParam
		);
		//Parent1ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'Parent1ID', ptInput);
		AParam.AsInteger := ACharacter.ParentID1;
		ADataSet.Params.AddParam(
			AParam
		);
		//Parent2ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'Parent2ID', ptInput);
		AParam.AsInteger := ACharacter.ParentID2;
		ADataSet.Params.AddParam(
			AParam
		);
		//Online
		AParam := ADataset.Params.CreateParam(ftBoolean, 'Online', ptInput);
		AParam.AsBoolean := Boolean(ACharacter.Online);
		ADataSet.Params.AddParam(
			AParam
		);

		QueryNoResult(ADataSet, AQuery);

	finally
		ADataSet.Free;
	end;

end;//Save
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//New				                                                         Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Creates an account.
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TCharacterQueries.New(
const
	ACharacter : TCharacter
);
const
	AQuery =
			'INSERT INTO characters '+
			'(name, slot, account_id) '+
			'VALUES(:Name, :Slot, :AccountID);';
var
	ADataSet		: TZQuery;
	AParam			: TParam;
begin

	ADataSet			:= TZQuery.Create(nil);
	try
		//Name
		AParam := ADataset.Params.CreateParam(ftString, 'Name', ptInput);
		AParam.AsString := ACharacter.Name;
		ADataSet.Params.AddParam(
			AParam
		);

		//Slot
		AParam := ADataset.Params.CreateParam(ftInteger, 'Slot', ptInput);
		AParam.AsInteger := ACharacter.CharaNum;
		ADataSet.Params.AddParam(
			AParam
		);
		//AccountID
		AParam := ADataset.Params.CreateParam(ftInteger, 'AccountID', ptInput);
		AParam.AsInteger := ACharacter.AccountID;
		ADataSet.Params.AddParam(
			AParam
		);

		QueryNoResult(ADataSet, AQuery);

	finally
		ADataSet.Free;
	end;

end;//New
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Delete							                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Deletes a Character
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TCharacterQueries.Delete(
	const ACharacter : TCharacter
);

const
	AQuery =
		'DELETE FROM characters WHERE id=:ID;';

var
	ADataSet		: TZQuery;
	AParam			: TParam;
begin

	ADataSet			:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := ACharacter.ID;
		ADataSet.Params.AddParam(
			AParam
		);

		QueryNoResult(ADataSet, AQuery);

	finally
		ADataSet.Free;
	end;
end;//Delete
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetVariable				                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Set variable
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TCharacterQueries.SetVariable(
	const ACharacter: TCharacter;
	const Key				: string;
	const Value			: string
);

const
	UpdateVariableQuery =
		'UPDATE charactervariables '+
		'SET `value`=:Value '+
		'WHERE character_id=:ID AND `key`=:Key;';

	InsertVariableQuery =
		'INSERT INTO charactervariables '+
		'(`character_id`, `key`, `value`) '+
		'VALUES(:ID, :Key, :Value);';

	CheckVariableQuery =
		'SELECT character_id FROM charactervariables WHERE '+
		'character_id=:ID AND `key`=:Key;';

var
	ADataSet		: TZQuery;
	ADataSet2		: TZQuery;
	AParam			: TParam;

begin

	ADataSet			:= TZQuery.Create(nil);
	ADataSet2			:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := ACharacter.ID;
		ADataSet.Params.AddParam(
			AParam
		);
		//Key
		AParam := ADataset.Params.CreateParam(ftString, 'Key', ptInput);
		AParam.AsString := Key;
		ADataSet.Params.AddParam(
			AParam
		);

		//ID
		AParam := ADataset2.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := ACharacter.ID;
		ADataSet2.Params.AddParam(
			AParam
		);
		//Key
		AParam := ADataset2.Params.CreateParam(ftString, 'Key', ptInput);
		AParam.AsString := Key;
		ADataSet2.Params.AddParam(
			AParam
		);
		//Value
		AParam := ADataset2.Params.CreateParam(ftString, 'Value', ptInput);
		AParam.AsString := Value;
		ADataSet2.Params.AddParam(
			AParam
		);

		Query(ADataSet, CheckVariableQuery);
		ADataset.First;
		if NOT ADataSet.Eof then
		begin
			QueryNoResult(ADataSet2, UpdateVariableQuery);
		end else
		begin
			QueryNoResult(ADataSet2, InsertVariableQuery);
    end;
	finally
		ADataSet.Free;
		ADataSet2.Free;
	end;
end;//SetVariable
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetVariable				                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Gets a variable
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
Function TCharacterQueries.GetVariable(
	const ACharacter: TCharacter;
	const Key				: string
) : String;

const
	AQuery =
		'SELECT `value` FROM charactervariables WHERE character_id=:ID AND `key`=:Key;';

var
	ADataSet		: TZQuery;
	AParam			: TParam;
begin
	Result := '';

	ADataSet			:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := ACharacter.ID;
		ADataSet.Params.AddParam(
			AParam
		);
		//Key
		AParam := ADataset.Params.CreateParam(ftString, 'Key', ptInput);
		AParam.AsString := Key;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		ADataset.First;
		if NOT ADataSet.Eof then
		begin
			Result := ADataset.Fields[0].AsString;
    end;

	finally
		ADataSet.Free;
	end;
end;//GetVariable
//------------------------------------------------------------------------------
end.