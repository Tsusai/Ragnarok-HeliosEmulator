//------------------------------------------------------------------------------
//MySQLDatabase		                                                        UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is one of our database objects which enabled Helios to use a MySQL
//    Database.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit MySQLDatabase;

interface
uses
  DatabaseTemplate,
	Character,
  Account;
type
//------------------------------------------------------------------------------
//TMySQLDatabase			                                                           CLASS
//------------------------------------------------------------------------------
//	What it does-
//			This is a child class for our database object system. It allows Helios
//    to communicate with a MySQL database and houses all routines for doing so.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
  TMySQLDatabase = class(TDatabaseTemplate)
  public
		function GetAccount(ID    : Cardinal) : TAccount;overload;override;
    function GetAccount(Name  : string) : TAccount;overload;override;

		function CreateChara(
			var ACharacter : TCharacter;
			AID : Cardinal;
			NName : string
		) : boolean;override;
		function LoadChara(CharaID : Cardinal) : TCharacter;override;
		function GetChara(CharaID : Cardinal) : TCharacter;override;
		function DeleteChara(var ACharacter : TCharacter) : boolean;override;

		procedure SaveAccount(AnAccount : TAccount);override;
		procedure SaveChara(AChara : TCharacter);override;


  end;
//------------------------------------------------------------------------------

implementation
	uses
		Types,
		GameGlobals,
		Globals,
    Console,
    SysUtils,
    Classes;

//------------------------------------------------------------------------------
//TMySQLDatabase.GetAccount()                               OVERLOADED FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			This function returns a TAccount type and is used for loading up an
//    account by ID.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
function TMySQLDatabase.GetAccount(ID: Cardinal) : TAccount;
var
	Success   : Boolean;
	AnAccount : TAccount;
  Index     : Integer;
begin
  Result := NIL;
  //Check Memory
	if not Assigned(AccountList) then Accountlist := TStringlist.Create;
	for Index := 0 to AccountList.Count -1 do begin
		if TAccount(AccountList.Objects[Index]).ID = ID then
		begin
			Result := TAccount(AccountList.Objects[Index]);
			exit;
		end;
	end;
  SQLQueryResult := SQLConnection.query('SELECT * FROM login WHERE account_id = "'+IntToStr(ID)+'";',true,Success);
	if Success and (SQLqueryResult.RowsCount = 1) then begin
		MainProc.Console('Account found in chara server');
		AnAccount := TAccount.Create;
		AnAccount.ID := StrToInt(SQLQueryResult.FieldValue(0));
		AnAccount.Username := SQlQueryResult.FieldValue(1);
		AnAccount.Password := SQlQueryResult.FieldValue(2);
		AnAccount.Gender   := SQLQueryResult.FieldValue(4)[0];
		AnAccount.EMail    := SQLQueryResult.FieldValue(6);
    AccountList.AddObject(AnAccount.Username, AnAccount);
		Result := AnAccount;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLDatabase.GetAccount()                               OVERLOADED FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			This function returns a TAccount type and is used for loading up an
//    account by Name.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
function TMySQLDatabase.GetAccount(Name : string) : TAccount;
var
	Success   : Boolean;
	AnAccount : TAccount;
  Index     : Integer;
begin
  Result := NIL;
  //Check Memory
	if not Assigned(AccountList) then Accountlist := TStringlist.Create;
	for Index := 0 to AccountList.Count -1 do begin
		if TAccount(AccountList.Objects[Index]).Username = Name then
		begin
			Result := TAccount(AccountList.Objects[Index]);
			exit;
		end;
	end;

  SQLQueryResult := SQLConnection.query('SELECT * FROM login WHERE userid = "'+Name+'";',true,Success);
	if Success and (SQLqueryResult.RowsCount = 1) then begin
		MainProc.Console('Account found in chara server');
		AnAccount := TAccount.Create;
		AnAccount.ID := StrToInt(SQLQueryResult.FieldValue(0));
		AnAccount.Username := SQlQueryResult.FieldValue(1);
		AnAccount.Password := SQlQueryResult.FieldValue(2);
		AnAccount.Gender   := SQLQueryResult.FieldValue(4)[0];
		AnAccount.EMail    := SQLQueryResult.FieldValue(6);
    AccountList.AddObject(AnAccount.Username, AnAccount);
		Result := AnAccount;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLDatabase.GetChara()                                           FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
function TMySQLDatabase.GetChara(CharaID : Cardinal) : TCharacter;
begin
	Result := NIL;
	if CharacterList.IndexOf(CharaID) > -1 then
	begin
		if TCharacter(CharacterList.IndexOfObject(CharaID)).CID = CharaID then
		begin
			Result := TCharacter(CharacterList.IndexOfObject(CharaID));
			Exit;
		end;
	end;
	if Result = NIL then
	begin
		Result := LoadChara(CharaID);
		if Assigned(Result) then
		begin
			CharacterList.AddObject(CharaID,Result);
		end;
	end else Result := NIL;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMySQLDatabase.SaveAccount()                                     Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TMySQLDatabase.SaveAccount(AnAccount: TAccount);
begin

end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLDatabase.SaveChara()                                     Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TMySQLDatabase.SaveChara(AChara : TCharacter);
var
	QueryString : string;
	Success : boolean;
begin
	with AChara do
	begin
		QueryString := Format('UPDATE `char` SET ' +
			'char_num=%d, ' +
			'name=''%s'', ' +
			'class=%d, ' +
			'base_level=%d, ' +
			'job_level=%d, ' +
			'base_exp=%d, ' +
			'job_exp=%d, ' +
			'zeny=%d, ' +
			'str=%d, ' +
			'agi=%d, ' +
			'vit=%d, ' +
			'`int`=%d, ' +  //needs to be in ` ` else MySQL thinks its an Integer type
			'dex=%d, ' +
			'luk=%d, ' +
			'max_hp=%d, ' +
			'hp=%d, ' +
			'max_sp=%d, ' +
			'sp=%d, ' +
			'status_point=%d, ' +
			'skill_point=%d, ' +
			'`option`=%d, ' +  // see INT above
			'karma=%d, ' +
			'manner=%d, ' +
			'party_id=%d, ' +
			'guild_id=%d, ' +
			'pet_id=%d, ' +
			'hair=%d, ' +
			'hair_color=%d, ' +
			'clothes_color=%d, ' +
			'weapon=%d, ' +
			'shield=%d, ' +
			'head_top=%d, ' +
			'head_mid=%d, ' +
			'head_bottom=%d, ' +
			'last_map=''%s'', ' +
			'last_x=%d, ' +
			'last_y=%d, ' +
			'save_map=''%s'', ' +
			'save_x=%d, ' +
			'save_y=%d, ' +
			'partner_id=%d, ' +
			'parent_id=%d, ' +
			'parent_id2=%d, ' +
			'baby_id=%d, ' +
			'online=%d, ' +
			'homun_id=%d ' +
			'WHERE char_id=%d;',
			[
			CharaNum,
			Name,
			JID,
			BaseLV,
			JobLV,
			BaseEXP,
			JobEXP,
			Zeny,
			ParamBase[STR],
			ParamBase[AGI],
			ParamBase[VIT],
			ParamBase[INT],
			ParamBase[DEX],
			ParamBase[LUK],
			MaxHP,
			HP,
			MaxSP,
			SP,
			StatusPts,
			SkillPts,
			Option,
			Karma,
			Manner,
			PartyID,
			GuildID,
			PetID,
			Hair,
			HairColor,
			ClothesColor,
			Weapon,
			Shield,
			HeadTop,
			HeadMid,
			HeadBottom,
			Map,
			Point.X,
			Point.Y,
			SaveMap,
			SavePoint.X,
			SavePoint.Y,
			PartnerID,
			ParentID1,
			ParentID2,
			BabyID,
			Online,
			HomunID,
			CID
			]);
	end;
	SQLConnection.Query(QueryString, FALSE, Success);
end;
//------------------------------------------------------------------------------

function TMySQLDatabase.CreateChara(
	var ACharacter : TCharacter;
	AID : Cardinal;
	NName : string
) : boolean;
var
	Success : boolean;
begin
	Result := FALSE;
	SQLConnection.query(
		Format('INSERT INTO `char` (account_id, name) VALUES(%d, "%s");',
		[AID,NName])
	,TRUE,Success);
	if Success then
	begin
		SQLQueryResult :=
				SQLConnection.query(
				Format('SELECT char_id FROM `char` WHERE account_id = %d AND name = "%s";',
				[AID,NName])
			,TRUE,Success);
		if (SQLQueryResult.RowsCount = 1) then
		begin
			ACharacter := GetChara(StrToInt(SQLQueryResult.FieldValue(0)));
			Result := Assigned(ACharacter);
		end;
	end;
end;

function TMySQLDatabase.LoadChara(CharaID : Cardinal) : TCharacter;
var
	Success : Boolean;
	APoint  : TPoint;
begin
	SQLQueryResult :=
		SQLConnection.query(
		Format('SELECT * FROM `char` WHERE char_id = %d;',
			[CharaID])
		,TRUE,Success);
	if (SQLQueryResult.RowsCount = 1) and (SQLQueryResult.FieldsCount = 48) then
	begin
		with Result do
		begin
			Result           := TCharacter.Create;
			CID              := StrToInt(SQLQueryResult.FieldValue(0));
			ID               := StrToInt(SQLQueryResult.FieldValue(1));
			Account          := GetAccount(ID);
			CharaNum         := StrToInt(SQLQueryResult.FieldValue(2));
			if CharaNum < 9 then
			begin
				//If its active, then attach to the player.
				Account.CharaID[CharaNum] := CID;
			end;
			Name            :=          SQLQueryResult.FieldValue(3);
			JID             := StrToInt(SQLQueryResult.FieldValue(4));
			BaseLV          := StrToInt(SQLQueryResult.FieldValue(5));
			JobLV           := StrToInt(SQLQueryResult.FieldValue(6));
			BaseEXP         := StrToInt(SQLQueryResult.FieldValue(7));
			JobEXP          := StrToInt(SQLQueryResult.FieldValue(8));
			Zeny            := StrToInt(SQLQueryResult.FieldValue(9));
			ParamBase[STR]  := StrToInt(SQLQueryResult.FieldValue(10));
			ParamBase[AGI]  := StrToInt(SQLQueryResult.FieldValue(11));
			ParamBase[VIT]  := StrToInt(SQLQueryResult.FieldValue(12));
			ParamBase[INT]  := StrToInt(SQLQueryResult.FieldValue(13));
			ParamBase[DEX]  := StrToInt(SQLQueryResult.FieldValue(14));
			ParamBase[LUK]  := StrToInt(SQLQueryResult.FieldValue(15));
			MaxHP           := StrToInt(SQLQueryResult.FieldValue(16));
			HP              := StrToInt(SQLQueryResult.FieldValue(17));
			MaxSP           := StrToInt(SQLQueryResult.FieldValue(18));
			SP              := StrToInt(SQLQueryResult.FieldValue(19));
			StatusPts       := StrToInt(SQLQueryResult.FieldValue(20));
			SkillPts        := StrToInt(SQLQueryResult.FieldValue(21));
			Option          := StrToInt(SQLQueryResult.FieldValue(22));
			Karma           := StrToInt(SQLQueryResult.FieldValue(23));
			Manner          := StrToInt(SQLQueryResult.FieldValue(24));
			PartyID         := StrToInt(SQLQueryResult.FieldValue(25));
			GuildID         := StrToInt(SQLQueryResult.FieldValue(26));
			PetID           := StrToInt(SQLQueryResult.FieldValue(27));
			Hair            := StrToInt(SQLQueryResult.FieldValue(28));
			HairColor       := StrToInt(SQLQueryResult.FieldValue(29));
			ClothesColor    := StrToInt(SQLQueryResult.FieldValue(30));
			Weapon          := StrToInt(SQLQueryResult.FieldValue(31));
			Shield          := StrToInt(SQLQueryResult.FieldValue(32));
			HeadTop         := StrToInt(SQLQueryResult.FieldValue(33));
			HeadMid         := StrToInt(SQLQueryResult.FieldValue(34));
			HeadBottom      := StrToInt(SQLQueryResult.FieldValue(35));
			Map             :=          SQLQueryResult.FieldValue(36) ;
				APoint.X      := StrToInt(SQLQueryResult.FieldValue(37));
				APoint.Y      := StrToInt(SQLQueryResult.FieldValue(38));
			Point           := APoint;
			SaveMap         :=          SQLQueryResult.FieldValue(39) ;
				APoint.X      := StrToInt(SQLQueryResult.FieldValue(40));
				APoint.Y      := StrToInt(SQLQueryResult.FieldValue(41));
			SavePoint       := APoint;
			PartnerID       := StrToInt(SQLQueryResult.FieldValue(42));
			ParentID1       := StrToInt(SQLQueryResult.FieldValue(43));
			ParentID2       := StrToInt(SQLQueryResult.FieldValue(44));
			BabyID          := StrToInt(SQLQueryResult.FieldValue(45));
			Online          := StrToInt(SQLQueryResult.FieldValue(46));
			HomunID         := StrToInt(SQLQueryResult.FieldValue(47));
			//Do not start the save timer caused by modifying everything else.
			DataChanged := false;
		end;
	end else Result := nil;
end;

function TMySQLDatabase.DeleteChara(var ACharacter : TCharacter) : boolean;
begin
	SQLConnection.query(
		Format('DELETE FROM `char` WHERE char_id=%d',[ACharacter.CID]),
		FALSE,Result
	);
	if Result then
	begin
		CharacterList.Delete(
			CharacterList.IndexOf(ACharacter.CID)
		);
		ACharacter.Account.CharaID[ACharacter.CharaNum] := 0;
		ACharacter.Free;
	end;
end;

{END MYSQLDATABASE}
end.
