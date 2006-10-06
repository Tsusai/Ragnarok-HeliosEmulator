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
  public
    Connection   : TMySQLClient;

    Constructor Create();override;
    Destructor Destroy();override;

		function GetAccount(ID    : Cardinal) : TAccount;overload;override;
    function GetAccount(Name  : string) : TAccount;overload;override;

		function CreateChara(
			var ACharacter : TCharacter;
			AID : Cardinal;
			NName : string
		) : boolean;override;

    function GetAccountCharas(AccountID : Cardinal) : TCharacterList;override;
		function LoadChara(CharaID : Cardinal) : TCharacter;override;
		function GetChara(CharaID : Cardinal) : TCharacter;override;
		function DeleteChara(var ACharacter : TCharacter) : boolean;override;
    function CharaExists(AccountID : Cardinal; Slot : Cardinal) : Boolean;overload;override;
    function CharaExists(Name : String) : Boolean;overload;override;

		procedure SaveAccount(AnAccount : TAccount);override;
		procedure SaveChara(AChara : TCharacter);override;

  protected
    procedure Connect();override;
    procedure Disconnect();override;
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
//TMySQLDatabase.Create()                                          CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our connection object.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Constructor TMySQLDatabase.Create();
begin
  Connect;
  inherited;
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
Procedure TMySQLDatabase.Connect();
begin
	if Not Assigned(Connection) then
	begin
		Connection := TMySQLClient.Create;
	end;
  if NOT Connection.Connected then
  begin
           
	  Connection.Host            := ServerConfig.MySQLHost;
	  Connection.Port            := ServerConfig.MySQLPort;
	  Connection.Db              := ServerConfig.MySQLDB;
	  Connection.User            := ServerConfig.MySQLUser;
	  Connection.Password        := ServerConfig.MySQLPass;
	  Connection.ConnectTimeout  := 10;

	  if NOT Connection.Connect then
    begin
		  MainProc.Console('*****Could not connect to mySQL database server.');
	  end;
    
  end;
end;
//------------------------------------------------------------------------------

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
			exit;
		end;
	end;

  QueryResult := Connection.query('SELECT * FROM login WHERE account_id = "'+IntToStr(ID)+'";',true,Success);
	if Success and (QueryResult.RowsCount = 1) then begin
		MainProc.Console('Account found in chara server');
		AnAccount := TAccount.Create;
		AnAccount.ID := StrToInt(QueryResult.FieldValue(0));
		AnAccount.Username := QueryResult.FieldValue(1);
		AnAccount.Password := QueryResult.FieldValue(2);
		AnAccount.Gender   := QueryResult.FieldValue(4)[0];
		AnAccount.EMail    := QueryResult.FieldValue(6);
    AccountList.AddObject(AnAccount.Username, AnAccount);
		Result := AnAccount;
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
			exit;
		end;
	end;

  QueryResult := Connection.query('SELECT * FROM login WHERE userid = "'+Name+'";',true,Success);
	if Success and (QueryResult.RowsCount = 1) then begin
		MainProc.Console('Account found in chara server');
		AnAccount := TAccount.Create;
		AnAccount.ID := StrToInt(QueryResult.FieldValue(0));
		AnAccount.Username := QueryResult.FieldValue(1);
		AnAccount.Password := QueryResult.FieldValue(2);
		AnAccount.Gender   := QueryResult.FieldValue(4)[0];
		AnAccount.EMail    := QueryResult.FieldValue(6);
    AccountList.AddObject(AnAccount.Username, AnAccount);
		Result := AnAccount;
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
  Result := NIL;
  ACharacterList := TCharacterList.Create;
  QueryResult := Connection.query(
    Format('SELECT char_id FROM `char` WHERE account_id = %d and char_num < 9;',
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
      Result := ACharacterList;
    end;
  end;
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
			Connection.query(
			Format('SELECT * FROM `char` WHERE char_num = %d and account_id = %d',[Slot, AccountID]),true, Success);
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
			Connection.query(
			Format('SELECT name FROM `char` WHERE name = "%s"',[Name]),true,Success);
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
 {SQLQueryResult :=
		SQLConnection.query(
		}
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
	Connection.Query(QueryString, FALSE, Success);
end;
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
	Connection.query(
		Format('INSERT INTO `char` (account_id, name) VALUES(%d, "%s");',
		[AID,NName])
	,TRUE,Success);
	if Success then
	begin
		QueryResult :=
				Connection.query(
				Format('SELECT char_id FROM `char` WHERE account_id = %d AND name = "%s";',
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

function TMySQLDatabase.LoadChara(CharaID : Cardinal) : TCharacter;
var
	Success     : Boolean;
	APoint      : TPoint;
  QueryResult : TMySQLResult;
begin
	QueryResult :=
		Connection.query(
		Format('SELECT * FROM `char` WHERE char_id = %d;',
			[CharaID])
		,TRUE,Success);
	if (QueryResult.RowsCount = 1) and (QueryResult.FieldsCount = 48) then
	begin
		with Result do
		begin
			Result           := TCharacter.Create;
			CID              := StrToInt(QueryResult.FieldValue(0));
			ID               := StrToInt(QueryResult.FieldValue(1));
			Account          := GetAccount(ID);
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

function TMySQLDatabase.DeleteChara(var ACharacter : TCharacter) : boolean;
begin
	Connection.query(
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
