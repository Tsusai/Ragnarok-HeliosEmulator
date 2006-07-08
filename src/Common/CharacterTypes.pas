unit CharacterTypes;

interface
uses GameObjects;

type TCharacter = class(TBeing)
	private
		fName : String;
		fJob  : Cardinal;
		 
	public
	CID : Cardinal; //BASIC KEY, WE DO NOT MOD IDS !!!
	ID  : Cardinal;

	SP  : Word;
	BaseLV : Cardinal;
	JobLV  : Cardinal;
	BaseEXP : Cardinal;
	JobEXP  : Cardinal;
	MaxHP : Word;
	MaxSP : Word;
	Speed : Word;
	Zeny : Cardinal;

	Option : Cardinal;
	Karma  : Cardinal;
	Manner : Cardinal;

	StatusPoint : Word;
	SkillPoint  : Word;
	StatBase : array [1..6] of byte;
	StatBonus : array [1..6] of byte;

	Hair : Word;
	HairColor : Word;
	Head1 : Word;
	Head2 : Word;
	Head3 : Word;
	ClothesColor : Word;
	Weapon : Word;
	Shield : Word;
end;

implementation
uses
	SysUtils,
	Math,
	Globals;

	function GetCharaSQLDataString(
		CharaID : Cardinal;
		CharaParam : string;
		ExistingVal : String
	) : string;
	var
		Success : Boolean;
	begin
		Success := false;
		SQLQueryResult :=
			SQLConnection.query(
				Format('SELECT %s FROM char WHERE char_id = %d;',[CharaParam,CharaID]),
				true,Success
			);
		if Success and
			(SQLQueryResult.FieldsCount = 1) and
			(SQLQueryResult.RowsCount = 1) then
		begin
			Result := SQLQueryResult.FieldValue(0);
		end else
		begin
			Result := ExistingVal;
		end;
	end;

	function GetCharaSQLDataWord(
		CharaID : Cardinal;
		CharaParam : string;
		ExistingVal : Word
	) : Word;
	var
		Success : Boolean;
	begin
		Success := false;
		SQLQueryResult :=
			SQLConnection.query(
				Format('SELECT %s FROM char WHERE char_id = %d;',[CharaParam,CharaID]),
				true,Success
			);
		if Success and
			(SQLQueryResult.FieldsCount = 1) and
			(SQLQueryResult.RowsCount = 1) then
		begin
			Result := EnsureRange(StrToIntDef(SQLQueryResult.FieldValue(0),ExistingVal),Low(Word),High(Word));
		end else
		begin
			Result := ExistingVal;
		end;
	Result := EnsureRange(Result,Low(Word),High(Word));
	end;

	function GetCharaSQLDataCardinal(
		CharaID : Cardinal;
		CharaParam : string;
		ExistingVal : Cardinal
	) : Cardinal;
	var
		Success : Boolean;
	begin
		Success := false;
		SQLQueryResult :=
			SQLConnection.query(
				Format('SELECT %s FROM char WHERE char_id = %d;',[CharaParam,CharaID]),
				true,Success
			);
		if Success and
			(SQLQueryResult.FieldsCount = 1) and
			(SQLQueryResult.RowsCount = 1) then
		begin
			Result := EnsureRange(StrToIntDef(SQLQueryResult.FieldValue(0),ExistingVal),Low(Cardinal),High(Cardinal));
		end else
		begin
			Result := ExistingVal;
		end;
	Result := EnsureRange(Result,Low(Cardinal),High(Cardinal));
	end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

end.
