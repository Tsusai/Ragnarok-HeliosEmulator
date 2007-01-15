//------------------------------------------------------------------------------
//MySQLGameDatabase		                                                        UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is one of our database objects which enabled Helios to use a MySQL
//    Database.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit MySQLGameDatabase;

interface
uses
	GameDatabaseTemplate,
	Character,
	CharaList,
	Account,
	uMysqlClient,
  Database;
type
//------------------------------------------------------------------------------
//TMySQLGameDatabase			                                                           CLASS
//------------------------------------------------------------------------------
//	What it does-
//			This is a child class for our database object system. It allows Helios
//    to communicate with a MySQL database and houses all routines for doing so.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
	TMySQLGameDatabase = class(TGameDatabaseTemplate)
	private
		Connection   : TMySQLClient;
    Parent : TDatabase;
	public


		Constructor Create(EnableGameDatabase : boolean; AParent : TDatabase); reintroduce; overload;
		Destructor Destroy();override;

		function CreateChara(
			var ACharacter : TCharacter;
			AID : Cardinal;
			NName : string
		) : boolean;override;

		function GetAccountCharas(AccountID : Cardinal) : TCharacterList;override;
		function LoadChara(CharaID : Cardinal) : TCharacter;override;

		function GetChara(
			CharaID : Cardinal;
			JanSQLClearTable : boolean = false
		) : TCharacter;override;

		function DeleteChara(var ACharacter : TCharacter) : boolean;override;
		function CharaExists(AccountID : Cardinal; Slot : Cardinal) : Boolean;overload;override;
		function CharaExists(Name : String) : Boolean;overload;override;

		procedure SaveChara(AChara : TCharacter);override;

	protected
		procedure Connect(); override;
		function SendQuery(
			const QString : string;
			StoreResult : boolean;
			var ExecutedOK : boolean
		) : TMySQLResult;
		procedure Disconnect();override;
	end;
//------------------------------------------------------------------------------

implementation
	uses
		Types,
		GameConstants,
		Globals,
		Console,
		SysUtils,
		Classes;
//------------------------------------------------------------------------------
//TMySQLGameDatabase.Create()                                          CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our connection object.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//		November 13th, 2006 - Tsusai - create inherit comes first.
//
//------------------------------------------------------------------------------
Constructor TMySQLGameDatabase.Create(EnableGameDatabase : boolean; AParent : TDatabase);
begin
	inherited Create;
  Parent := AParent;
  Connection := TMySQLClient.Create;
  if EnableGameDatabase then
  begin
	  Connect();
  end;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMySQLGameDatabase.Destroy()                                          DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys our connection object.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Destructor TMySQLGameDatabase.Destroy();
begin
	Disconnect;
  Connection.Free;
	inherited;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMySQLGameDatabase.Connect()                                            Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Initializes the MySQL Connection.
//
//	Changes -
//		October 5th, 2006 - RaX - Moved here from globals.
//
//------------------------------------------------------------------------------
Procedure TMySQLGameDatabase.Connect();
begin
	if NOT Connection.Connected then
	begin
    Connection.Host            := Parent.Options.GameHost;
    Connection.Port            := Parent.Options.GamePort;
    Connection.Db              := Parent.Options.GameDB;
    Connection.User            := Parent.Options.GameUser;
    Connection.Password        := Parent.Options.GamePass;
	end;

	Connection.ConnectTimeout  := 10;

	if NOT Connection.Connect then
	begin
		MainProc.Console('*****Could not connect to mySQL database server.');
	end;

end;
//------------------------------------------------------------------------------

function TMySQLGameDatabase.SendQuery(
	const QString : string;
	StoreResult : boolean;
	var ExecutedOK : boolean
) : TMySQLResult;
begin
	Result := Connection.query(QString,StoreResult,ExecutedOK);
	if not ExecutedOK then
	begin
		MainProc.Console('MySQL Query error: ' + QString);
	end;
end;

//------------------------------------------------------------------------------
//TMySQLGameDatabase.Disconnect()                                         Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Destroys the MySQL Connection.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TMySQLGameDatabase.Disconnect();
begin
	if Connection.Connected then
	begin
		Connection.close;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLGameDatabase.SetAccount()                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Builds a taccount object from a query result.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//		December 27th, 2006 - Tsusai - Fixed login key and gender char reading
//
//------------------------------------------------------------------------------
procedure SetAccount(
	var AnAccount : TAccount;
	var QueryResult : TMySQLResult
);
begin
	AnAccount := TAccount.Create;
	AnAccount.ID          := StrToInt(QueryResult.FieldValue(0));
	AnAccount.Username    := QueryResult.FieldValue(1);
	AnAccount.Password    := QueryResult.FieldValue(2);
	//Tsusai - For Gender, we need to return the first char, thats
	//why there is a [1]
	AnAccount.Gender       := QueryResult.FieldValue(4)[1];
	AnAccount.LoginCount   := StrToIntDef(QueryResult.FieldValue(5),0);
	AnAccount.EMail        := QueryResult.FieldValue(6);
	AnAccount.LoginKey[1]  := StrToIntDef(QueryResult.FieldValue(7),0);
	AnAccount.LoginKey[2]  := StrToIntDef(QueryResult.FieldValue(8),0);
	AnAccount.Level        := StrToIntDef(QueryResult.FieldValue(9),0);
	AnAccount.ConnectUntil := ConvertMySQLTime(QueryResult.FieldValue(11));
	AnAccount.LastIP       := QueryResult.FieldValue(12);
	AnAccount.Bantime      := ConvertMySQLTime(QueryResult.FieldValue(14));
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLGameDatabase.GetChara()                                           FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
function TMySQLGameDatabase.GetChara(
	CharaID : Cardinal;
	//JanSQLClearTable is never used.
	JanSQLClearTable : boolean = false
) : TCharacter;
var
  CharacterIndex : Integer;
begin
	Result := NIL;
  CharacterIndex := CharacterList.IndexOf(CharaID);
	if CharacterIndex > -1 then
	begin
		if CharacterList.Items[CharacterIndex].CID = CharaID then
		begin
			Result := CharacterList.Items[CharacterIndex];
			Exit;
		end;
	end;
	if Result = NIL then
	begin
		Result := LoadChara(CharaID);
		if Assigned(Result) then
		begin
			CharacterList.Add(Result);
		end;
	end else Result := NIL;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLGameDatabase.GetAccountCharas()                                   FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//		December 18th, 2006 - Tsusai - Freed result.
//
//------------------------------------------------------------------------------
function TMySQLGameDatabase.GetAccountCharas(AccountID : Cardinal) : TCharacterList;
var
	QueryResult     : TMySQLResult;
	Success         : Boolean;
	Index           : Integer;
begin
	Result := TCharacterList.Create(FALSE);
	QueryResult := SendQuery(
		Format('SELECT char_id FROM characters WHERE account_id = %d and char_num < 9',
		[AccountID]),TRUE,Success);
	if Success then
	begin
		if QueryResult.RowsCount > 0 then
		begin
			for Index := 0 to QueryResult.RowsCount - 1 do
			begin
				Result.Add(GetChara(StrToInt(QueryResult.FieldValue(0))));
				if Index < QueryResult.RowsCount then
				begin
					QueryResult.Next;
				end;
			end;
		end;
	end;
	if Assigned(QueryResult) then QueryResult.Free;
end;
//-----------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLGameDatabase.CharaExists()                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//		December 18th, 2006 - Tsusai - Simplified Result, freed Queryresult
//
//------------------------------------------------------------------------------
function TMySQLGameDatabase.CharaExists(AccountID : Cardinal; Slot : Cardinal) : Boolean;
var
	QueryResult : TMySQLResult;
	Success     : Boolean;
begin
	Result := false;
	QueryResult :=
			SendQuery(
			Format('SELECT char_id FROM characters WHERE char_num = %d and account_id = %d',[Slot, AccountID]),true, Success);
	if Success then
	begin
		Result := (QueryResult.RowsCount > 0);
	end;
	if Assigned(QueryResult) then QueryResult.Free;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLGameDatabase.CharaExists()                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//		December 18th, 2006 - Tsusai - Simplified Result, freed queryresult
//
//------------------------------------------------------------------------------
function TMySQLGameDatabase.CharaExists(Name : String) : Boolean;
var
	QueryResult : TMySQLResult;
	Success     : Boolean;
begin
	Result := false;
	QueryResult :=
			SendQuery(
			Format('SELECT char_id FROM characters WHERE name = ''%s''',[Name]),true,Success);
	if Success then
	begin
		Result := (QueryResult.RowsCount > 0);
	end;
	if Assigned(QueryResult) then QueryResult.Free;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLGameDatabase.SaveChara()                                     Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TMySQLGameDatabase.SaveChara(AChara : TCharacter);
var
	QueryString : string;
	Success : boolean;
begin
	with AChara do
	begin
		QueryString := Format('UPDATE characters SET ' +
			'char_num=%d, ' +
			'name=''%s'', ' +
			'class=%d, ' +
			'base_level=%d, ' +
			'job_level=%d, ' +
			'base_exp=%d, ' +
			'job_exp=%d, ' +
			'zeny=%d, ' +
			'p_str=%d, ' +
			'p_agi=%d, ' +
			'p_vit=%d, ' +
			'p_int=%d, ' +  //needs to be in ` ` else MySQL thinks its an Integer type
			'p_dex=%d, ' +
			'p_luk=%d, ' +
			'max_hp=%d, ' +
			'hp=%d, ' +
			'max_sp=%d, ' +
			'sp=%d, ' +
			'status_point=%d, ' +
			'skill_point=%d, ' +
			'options=%d, ' +  // see INT above
			'karma=%d, ' +
			'manner=%d, ' +
			'party_id=%d, ' +
			'guild_id=%d, ' +
			'pet_id=%d, ' +
			'hair=%d, ' +
			'hair_color=%d, ' +
			'clothes_color=%d, ' +
			'righthand=%d, ' +
			'lefthand=%d, ' +
      'armor=%d, ' +
      'garment=%d, ' +
      'shoes=%d, ' +
      'accessory1=%d, ' +
      'accessory2=%d, ' +
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
			RightHand,
			LeftHand,
      Armor,
      Garment,
      Shoes,
      Accessory1,
      Accessory2,
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
	SendQuery(QueryString, FALSE, Success);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLGameDatabase.CreateChara()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Create's a character in teh database.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TMySQLGameDatabase.CreateChara(
	var ACharacter : TCharacter;
	AID : Cardinal;
	NName : string
) : boolean;
var
	Success     : boolean;
	QueryResult : TMySQLResult;
begin
	Result := FALSE;
	SendQuery(
		Format('INSERT INTO characters (account_id, name) VALUES(%d, ''%s'')',
		[AID,NName])
	,TRUE,Success);
	if Success then
	begin
		QueryResult :=
				SendQuery(
				Format('SELECT char_id FROM characters WHERE account_id = %d AND name = ''%s''',
				[AID,NName])
			,TRUE,Success);
		if (QueryResult.RowsCount = 1) then
		begin
			ACharacter := GetChara(StrToInt(QueryResult.FieldValue(0)));
			Result := Assigned(ACharacter);
		end;
		if Assigned(QueryResult) then QueryResult.Free;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLGameDatabase.LoadChara()                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Gets a Character from the database.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TMySQLGameDatabase.LoadChara(CharaID : Cardinal) : TCharacter;
var
	Success     : Boolean;
	APoint      : TPoint;
	QueryResult : TMySQLResult;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT * FROM characters WHERE char_id = %d;',
			[CharaID])
		,TRUE,Success);
	if (QueryResult.RowsCount = 1) and (QueryResult.FieldsCount = 48) then
	begin
		with Result do
		begin
			Result           := TCharacter.Create;
			CID              := StrToInt(QueryResult.FieldValue(0));
			ID               := StrToInt(QueryResult.FieldValue(1));
			CharaNum         := StrToInt(QueryResult.FieldValue(2));
			Account					:= Parent.CommonData.GetAccount(ID);
			Account.CharaID[CharaNum] := CID;
			Name            :=          QueryResult.FieldValue(3);
			JID             := StrToInt(QueryResult.FieldValue(4));
			BaseLV          := StrToInt(QueryResult.FieldValue(5));
			JobLV           := StrToInt(QueryResult.FieldValue(6));
			BaseEXP         := StrToInt(QueryResult.FieldValue(7));
			JobEXP          := StrToInt(QueryResult.FieldValue(8));
			Zeny            := StrToInt(QueryResult.FieldValue(9));
			ParamBase[STR]  := StrToInt(QueryResult.FieldValue(10));
			ParamBase[AGI]  := StrToInt(QueryResult.FieldValue(11));
			ParamBase[VIT]  := StrToInt(QueryResult.FieldValue(12));
			ParamBase[INT]  := StrToInt(QueryResult.FieldValue(13));
			ParamBase[DEX]  := StrToInt(QueryResult.FieldValue(14));
			ParamBase[LUK]  := StrToInt(QueryResult.FieldValue(15));
			MaxHP           := StrToInt(QueryResult.FieldValue(16));
			HP              := StrToInt(QueryResult.FieldValue(17));
			MaxSP           := StrToInt(QueryResult.FieldValue(18));
			SP              := StrToInt(QueryResult.FieldValue(19));
			StatusPts       := StrToInt(QueryResult.FieldValue(20));
			SkillPts        := StrToInt(QueryResult.FieldValue(21));
			Option          := StrToInt(QueryResult.FieldValue(22));
			Karma           := StrToInt(QueryResult.FieldValue(23));
			Manner          := StrToInt(QueryResult.FieldValue(24));
			PartyID         := StrToInt(QueryResult.FieldValue(25));
			GuildID         := StrToInt(QueryResult.FieldValue(26));
			PetID           := StrToInt(QueryResult.FieldValue(27));
			Hair            := StrToInt(QueryResult.FieldValue(28));
			HairColor       := StrToInt(QueryResult.FieldValue(29));
			ClothesColor    := StrToInt(QueryResult.FieldValue(30));
			RightHand       := StrToInt(QueryResult.FieldValue(31));
			LeftHand        := StrToInt(QueryResult.FieldValue(32));
			HeadTop         := StrToInt(QueryResult.FieldValue(33));
			HeadMid         := StrToInt(QueryResult.FieldValue(34));
			HeadBottom      := StrToInt(QueryResult.FieldValue(35));
			Map             :=          QueryResult.FieldValue(36) ;
				APoint.X      := StrToInt(QueryResult.FieldValue(37));
				APoint.Y      := StrToInt(QueryResult.FieldValue(38));
			Point           := APoint;
			SaveMap         :=          QueryResult.FieldValue(39) ;
				APoint.X      := StrToInt(QueryResult.FieldValue(40));
				APoint.Y      := StrToInt(QueryResult.FieldValue(41));
			SavePoint       := APoint;
			PartnerID       := StrToInt(QueryResult.FieldValue(42));
			ParentID1       := StrToInt(QueryResult.FieldValue(43));
			ParentID2       := StrToInt(QueryResult.FieldValue(44));
			BabyID          := StrToInt(QueryResult.FieldValue(45));
			Online          := StrToInt(QueryResult.FieldValue(46));
			HomunID         := StrToInt(QueryResult.FieldValue(47));
			//Do not start the save timer caused by modifying everything else.
			DataChanged := false;
		end;
	end else Result := nil;
	if Assigned(QueryResult) then QueryResult.Free;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLGameDatabase.DeleteChara()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Deletes a character from the database.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TMySQLGameDatabase.DeleteChara(var ACharacter : TCharacter) : boolean;
begin
	SendQuery(
		Format('DELETE FROM characters WHERE char_id=%d',[ACharacter.CID]),
		FALSE,Result
	);

	if Result then
	begin
		CharacterList.Delete(
			CharacterList.IndexOf(ACharacter.CID)
		);
	end;
end;
//------------------------------------------------------------------------------


{END MySQLGameDatabase}
end.
