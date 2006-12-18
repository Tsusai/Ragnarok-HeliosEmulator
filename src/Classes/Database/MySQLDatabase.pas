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
	CharaList,
	Account,
	uMysqlClient;
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
	private
		Connection   : TMySQLClient;
	public


		Constructor Create(UseGameDatabase : boolean); reintroduce; overload;
		Destructor Destroy();override;

		function GetAccount(ID    : Cardinal) : TAccount;overload;override;
		function GetAccount(Name  : string) : TAccount;overload;override;

		procedure GetAccountBanAndConnectTime(var AnAccount : TAccount); override;

		function CreateChara(
			var ACharacter : TCharacter;
			AID : Cardinal;
			NName : string
		) : boolean;override;

		procedure CreateAccount(
			const Username : string;
			const Password : string;
			const GenderChar : char
		); override;

		function GetAccountCharas(AccountID : Cardinal) : TCharacterList;override;
		function LoadChara(CharaID : Cardinal) : TCharacter;override;
		function GetChara(CharaID : Cardinal) : TCharacter;override;
		function DeleteChara(var ACharacter : TCharacter) : boolean;override;
		function CharaExists(AccountID : Cardinal; Slot : Cardinal) : Boolean;overload;override;
		function CharaExists(Name : String) : Boolean;overload;override;
		function AccountExists(UserName : String) : Boolean;override;

		procedure SaveAccount(AnAccount : TAccount);override;
		procedure SaveChara(AChara : TCharacter);override;

		Function GetBaseHP(ACharacter : TCharacter) : Cardinal;override;
		Function GetBaseSP(ACharacter : TCharacter) : Cardinal;override;
	protected
		procedure Connect(UseGameDatabase : Boolean); reintroduce;overload;
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
//TMySQLDatabase.Create()                                          CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our connection object.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//		November 13th, 2006 - Tsusai - create inherit comes first.
//
//------------------------------------------------------------------------------
Constructor TMySQLDatabase.Create(UseGameDatabase : boolean);
begin
	inherited Create;
	Connect(UseGameDatabase);
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMySQLDatabase.Destroy()                                          DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys our connection object.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Destructor TMySQLDatabase.Destroy();
begin
	Disconnect;
	inherited;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMySQLDatabase.Connect()                                            Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Initializes the MySQL Connection.
//
//	Changes -
//		October 5th, 2006 - RaX - Moved here from globals.
//
//------------------------------------------------------------------------------
Procedure TMySQLDatabase.Connect(UseGameDatabase : boolean);
begin
	if Not Assigned(Connection) then
	begin
		Connection := TMySQLClient.Create;
	end;
	if NOT Connection.Connected then
	begin
		if Not UseGameDatabase then //Access the Common database
		begin
			Connection.Host            := ServerConfig.CommonHost;
			Connection.Port            := ServerConfig.CommonPort;
			Connection.Db              := ServerConfig.CommonDB;
			Connection.User            := ServerConfig.CommonUser;
			Connection.Password        := ServerConfig.CommonPass;
		end else //Access the Game database
		begin
			Connection.Host            := ServerConfig.GameHost;
			Connection.Port            := ServerConfig.GamePort;
			Connection.Db              := ServerConfig.GameDB;
			Connection.User            := ServerConfig.GameUser;
			Connection.Password        := ServerConfig.GamePass;
		end;
	end;

	Connection.ConnectTimeout  := 10;

	if NOT Connection.Connect then
	begin
		MainProc.Console('*****Could not connect to mySQL database server.');
	end;

end;
//------------------------------------------------------------------------------

function TMySQLDatabase.SendQuery(
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
//TMySQLDatabase.Disconnect()                                         Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Destroys the MySQL Connection.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TMySQLDatabase.Disconnect();
begin
	if Connection.Connected then
	begin
		Connection.close;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLDatabase.SetAccount()                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Builds a taccount object from a query result.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
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
	//why there is a [0]
	AnAccount.Gender      := QueryResult.FieldValue(4)[0];
	AnAccount.LoginCount  := StrToIntDef(QueryResult.FieldValue(5),0);
	AnAccount.EMail       := QueryResult.FieldValue(6);
	AnAccount.LoginKey[1] := StrToIntDef(QueryResult.FieldValue(7),0);
	AnAccount.LoginKey[1] := StrToIntDef(QueryResult.FieldValue(8),0);
	AnAccount.Level       := StrToIntDef(QueryResult.FieldValue(9),0);
	AnAccount.LastIP      := QueryResult.FieldValue(12);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLDatabase.GetAccount()                               OVERLOADED FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			This function returns a TAccount type and is used for loading up an
//    account by ID.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//		November 13th, 2005 - Tsusai - now calls a shared TAccount routine to
//													set the data
//
//------------------------------------------------------------------------------
function TMySQLDatabase.GetAccount(ID: Cardinal) : TAccount;
var
	Success     : Boolean;
	AnAccount   : TAccount;
	Index       : Integer;
	QueryResult : TMySQLResult;
begin
	Result := NIL;
	//Check Memory
	if not Assigned(AccountList) then Accountlist := TStringlist.Create;
	for Index := 0 to AccountList.Count -1 do begin
		if TAccount(AccountList.Objects[Index]).ID = ID then
		begin
			Result := TAccount(AccountList.Objects[Index]);
			break;
		end;
	end;

	QueryResult := SendQuery('SELECT * FROM accounts WHERE account_id = "'+IntToStr(ID)+'";',true,Success);
	if Success and (QueryResult.RowsCount = 1) then begin
		if Not Assigned(Result) then
		begin
			SetAccount(AnAccount,QueryResult);
			AccountList.AddObject(AnAccount.Username, AnAccount);
			Result := AnAccount;
		end;
		Result.ConnectUntil := ConvertMySQLTime(QueryResult.FieldValue(11));
		Result.Bantime      := ConvertMySQLTime(QueryResult.FieldValue(14));
	end;

	if Assigned(QueryResult) then QueryResult.Free;
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
//		November 13th, 2005 - Tsusai - now calls a shared TAccount routine to
//													set the data
//
//------------------------------------------------------------------------------
function TMySQLDatabase.GetAccount(Name : string) : TAccount;
var
	Success     : Boolean;
	AnAccount   : TAccount;
	Index       : Integer;
	QueryResult : TMySQLResult;
begin
	Result := NIL;
	//Check Memory
	if not Assigned(AccountList) then Accountlist := TStringlist.Create;
	for Index := 0 to AccountList.Count -1 do begin
		if TAccount(AccountList.Objects[Index]).Username = Name then
		begin
			Result := TAccount(AccountList.Objects[Index]);
			break;
		end;
	end;

	QueryResult := SendQuery('SELECT * FROM accounts WHERE userid = "'+Name+'";',true,Success);
	if Success and (QueryResult.RowsCount = 1) then begin
		if Not Assigned(Result) then
		begin
			SetAccount(AnAccount,QueryResult);
			AccountList.AddObject(AnAccount.Username, AnAccount);
			Result := AnAccount;
		end;
		Result.ConnectUntil := ConvertMySQLTime(QueryResult.FieldValue(11));
		Result.Bantime      := ConvertMySQLTime(QueryResult.FieldValue(14));
	end;

	if Assigned(QueryResult) then QueryResult.Free;
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
//TMySQLDatabase.GetAccountCharas()                                   FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
function TMySQLDatabase.GetAccountCharas(AccountID : Cardinal) : TCharacterList;
var
	QueryResult     : TMySQLResult;
	Success         : Boolean;
	ACharacterList  : TCharacterList;
	Index           : Integer;
begin
	ACharacterList := TCharacterList.Create;
	QueryResult := SendQuery(
		Format('SELECT char_id FROM characters WHERE account_id = %d and char_num < 9;',
		[AccountID]),TRUE,Success);
	if Success then
	begin
		if QueryResult.RowsCount > 0 then
		begin
			for Index := 0 to QueryResult.RowsCount - 1 do
			begin
				ACharacterList.Add(GetChara(StrToInt(QueryResult.FieldValue(0))));
				if Index < QueryResult.RowsCount then
				begin
					QueryResult.Next;
				end;
			end;
		end;
	end;
	Result := ACharacterList;
end;
//-----------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLDatabase.CharaExists()                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
function TMySQLDatabase.CharaExists(AccountID : Cardinal; Slot : Cardinal) : Boolean;
var
	QueryResult : TMySQLResult;
	Success     : Boolean;
begin
	QueryResult :=
			SendQuery(
			Format('SELECT * FROM characters WHERE char_num = %d and account_id = %d',[Slot, AccountID]),true, Success);
	if Success then
	begin
		if QueryResult.RowsCount > 0 then
		begin
			Result := TRUE;
		end else
		begin
			Result := FALSE;
		end;
	end else
	begin
		Result := FALSE;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLDatabase.CharaExists()                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
function TMySQLDatabase.CharaExists(Name : String) : Boolean;
var
	QueryResult : TMySQLResult;
	Success     : Boolean;
begin
	QueryResult :=
			SendQuery(
			Format('SELECT name FROM characters WHERE name = "%s"',[Name]),true,Success);
	if Success then
	begin
		if QueryResult.RowsCount > 0 then
		begin
			Result := TRUE;
		end else
		begin
			Result := FALSE;
		end;
	end else
	begin
		Result := FALSE;
	end;
end;
//------------------------------------------------------------------------------

function TMySQLDatabase.AccountExists(UserName : String) : Boolean;
var
	QueryResult : TMySQLResult;
	Success : boolean;
begin
	QueryResult :=
		SendQuery(
			Format('SELECT userid FROM accounts WHERE userid = "%s"',[UserName]),true,Success);
	if Success then
	begin
		if QueryResult.RowsCount > 0 then
		begin
			Result := TRUE;
		end else
		begin
			Result := FALSE;
		end;
	end else
	begin
		Result := FALSE;
	end;
end;

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
const
	BaseString =
		'UPDATE accounts SET '+
		'userid=''%s'', ' +
		'user_pass=''%s'', ' +
		'lastlogin=%d, ' +
		'sex=''%s'', ' +
		'logincount=%d, ' +
		'email=''%s'', ' +
		'loginkey1=%d, ' +
		'loginkey2=%d, ' +
		'connect_until=%d, ' +
		'ban_until=%d, ' +
		'last_ip=''%s'' ' +
		'WHERE account_id=%d;';
var
	Success : boolean;
	QueryString : string;
begin
	QueryString :=
		Format(BaseString,
			[AnAccount.Username,
			 AnAccount.Password,
			 StrToInt64(
				 FormatDateTime('yyyymmddhhmmss',AnAccount.LastLoginTime)),
			 AnAccount.Gender,
			 AnAccount.LoginCount,
			 AnAccount.EMail,
			 AnAccount.LoginKey[1],
			 AnAccount.LoginKey[2],
			 StrToInt64(
				 FormatDateTime('yyyymmddhhmmss',AnAccount.ConnectUntil)),
			 StrToInt64(
				 FormatDateTime('yyyymmddhhmmss',AnAccount.Bantime)),
			 AnAccount.LastIP,
			 AnAccount.ID]
		);
	SendQuery(QueryString, FALSE, Success);
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
	SendQuery(QueryString, FALSE, Success);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLDatabase.CreateChara()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Create's a character in teh database.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TMySQLDatabase.CreateChara(
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
		Format('INSERT INTO characters (account_id, name) VALUES(%d, "%s");',
		[AID,NName])
	,TRUE,Success);
	if Success then
	begin
		QueryResult :=
				SendQuery(
				Format('SELECT char_id FROM characters WHERE account_id = %d AND name = "%s";',
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

procedure TMySQLDatabase.CreateAccount(
	const Username : string;
	const Password : string;
	const GenderChar : char
);
var
	Success : boolean;
begin
	SendQuery(
		Format('INSERT INTO accounts (userid, user_pass, sex) VALUES("%s", "%s", "%s");',
		[Username,Password,GenderChar])
	,TRUE,Success);
end;

//------------------------------------------------------------------------------
//TMySQLDatabase.LoadChara()                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Gets a Character from the database.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TMySQLDatabase.LoadChara(CharaID : Cardinal) : TCharacter;
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
			Account          := MainProc.ACommonDatabase.AnInterface.GetAccount(ID);
			CharaNum         := StrToInt(QueryResult.FieldValue(2));
			if CharaNum < 9 then
			begin
				//If its active, then attach to the player.
				Account.CharaID[CharaNum] := CID;
			end;
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
			Weapon          := StrToInt(QueryResult.FieldValue(31));
			Shield          := StrToInt(QueryResult.FieldValue(32));
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
//TMySQLDatabase.DeleteChara()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Deletes a character from the database.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TMySQLDatabase.DeleteChara(var ACharacter : TCharacter) : boolean;
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
		ACharacter.Account.CharaID[ACharacter.CharaNum] := 0;
		ACharacter.Free;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLDatabase.GetAccountBanAndConnectTime()                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Gets an account's ban and connect time.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TMySQLDatabase.GetAccountBanAndConnectTime(var AnAccount : TAccount);
begin
	//Bleh...might as well RELOAD.
	AnAccount := Self.GetAccount(AnAccount.ID);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLDatabase.GetBaseHP()                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters basehp.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TMySQLDatabase.GetBaseHP(ACharacter : TCharacter) : Cardinal;
var
	Success     : Boolean;
	QueryResult : TMySQLResult;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT * FROM hp WHERE level = %d, job = %s ;',
			[ACharacter.BaseLV,ACharacter.JID])
		,TRUE,Success);
	if (QueryResult.RowsCount = 1) and (QueryResult.FieldsCount = 48) then
	begin
			Result              := StrToInt(QueryResult.FieldValue(0));
	end else Result := 0;
	if Assigned(QueryResult) then QueryResult.Free;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLDatabase.GetBaseSP()                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a characters basesp.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Function TMySQLDatabase.GetBaseSP(ACharacter : TCharacter) : Cardinal;
var
	Success     : Boolean;
	QueryResult : TMySQLResult;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT * FROM sp WHERE level = %d, job = %s ;',
			[ACharacter.BaseLV,ACharacter.JID])
		,TRUE,Success);
	if (QueryResult.RowsCount = 1) and (QueryResult.FieldsCount = 48) then
	begin
			Result              := StrToInt(QueryResult.FieldValue(0));
	end else Result := 0;
	if Assigned(QueryResult) then QueryResult.Free;
end;
//------------------------------------------------------------------------------

{END MYSQLDATABASE}
end.
