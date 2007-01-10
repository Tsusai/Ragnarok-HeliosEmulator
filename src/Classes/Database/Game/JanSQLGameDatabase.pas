//------------------------------------------------------------------------------
//JanSQLGameDatabase		                                                        UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is one of our database objects which enabled Helios to use a TEXT
//    Database.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit JanSQLGameDatabase;

interface
uses
	GameDatabaseTemplate,
	Character,
	CharaList,
	Account,
	janSQL,
  Database;

//------------------------------------------------------------------------------
//TJanSQLGameDatabase			                                                           CLASS
//------------------------------------------------------------------------------
//	What it does-
//			This is a child class for our database object system. It allows Helios
//    to communicate with a TEXT database and houses all routines for doing so.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
type
	TJanSQLGameDatabase = class(TGameDatabaseTemplate)
	private
		Database : TjanSQL;
    Parent  : TDatabase;
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
		function GetChara(CharaID : Cardinal) : TCharacter;override;
		function DeleteChara(var ACharacter : TCharacter) : boolean;override;
		function CharaExists(AccountID : Cardinal; Slot : Cardinal) : Boolean;overload;override;
		function CharaExists(Name : String) : Boolean;overload;override;

		procedure SaveChara(AChara : TCharacter);override;

	protected
		procedure Connect(); override;
		procedure Disconnect; override;
		function SendQuery(
			const QString : string
		) : Integer;
	end;
//------------------------------------------------------------------------------

implementation
	uses
		Types,
		GameConstants,
		Globals,
		Console,
		SysUtils,
		Classes,
    Math;


//------------------------------------------------------------------------------
//TJanSQLGameDatabase.Create()                                          CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our connection object.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//		November 13th, 2006 - Tsusai - create inherit comes first.
//
//------------------------------------------------------------------------------
Constructor TJanSQLGameDatabase.Create(EnableGameDatabase : boolean; AParent : TDatabase);
begin
	inherited Create;
  Parent := AParent;
  Database := TJanSQL.Create;
  if EnableGameDatabase then
  begin
	  Connect();
  end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLGameDatabase.Destroy()                                          DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys our connection object.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Destructor TJanSQLGameDatabase.Destroy();
begin
	Disconnect;
  Database.Free;
	inherited;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLGameDatabase.Disconnect()                                        Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Destroys the TEXT Connection.
//
//	Changes -
//		December 21st, 2006 - RaX - Created
//
//------------------------------------------------------------------------------
procedure TJanSQLGameDatabase.Disconnect;
begin

end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLGameDatabase.Connect()                                            Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Initializes the TEXT Connection.
//
//	Changes -
//		October 5th, 2006 - RaX - Moved here from globals.
//		December 18th, 2006 - Tsusai - Modified the connect to actually...connect
//			Also FileExists doesn't work for directories, its DirectoryExists
//
//------------------------------------------------------------------------------
Procedure TJanSQLGameDatabase.Connect();
var
	ResultIdentifier : Integer;
const ConnectQuery = 'Connect to ''%s''';
begin

	ResultIdentifier := 0;

  if DirectoryExists(Parent.Options.GameHost) then
  begin
    ResultIdentifier := Database.SQLDirect(Format(ConnectQuery,[Parent.Options.GameHost]));
  end else
  begin
    MainProc.Console('');
    MainProc.Console('The database at '+Parent.Options.GameHost+' does not exist!');
    MainProc.Console('Please ensure that you have correctly configured your ini file');
  end;

	if ResultIdentifier = 0 then
	begin
		MainProc.Console('*****Could not open text database. Error : ' + Database.Error);
		MainProc.Console(Parent.Options.GameHost);
	end else
  begin
    Database.ReleaseRecordset(ResultIdentifier);
  end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLGameDatabase.SendQuery()                                          Function
//------------------------------------------------------------------------------
//	What it does-
//			Sends a query to the jansql object.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//		December 18th, 2006 - Tsusai - Would not return a blank set if query failed
//			to return anything, only a nil pointer that would cause issues when read.
//
//------------------------------------------------------------------------------
function TJanSQLGameDatabase.SendQuery(
	const QString : string
) : Integer;
begin
	Result := Database.SQLDirect(QString);
	if (Result = 0) AND (Database.Error <> 'SELECT FROM: no records') then
	begin
		MainProc.Console('Text Query error: ' + QString);
    MainProc.Console(Database.Error);
	end;
end;//SendQuery
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLGameDatabase.SetAccount()                                        Procedure
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
	var QueryResult : TJanRecordSet
);
begin
	AnAccount := TAccount.Create;
	AnAccount.ID          := StrToInt(QueryResult.records[0].fields[0].value);
	AnAccount.Username     := QueryResult.records[0].fields[1].value;
	AnAccount.Password     := QueryResult.records[0].fields[2].value;
	//Tsusai - For Gender, we need to return the first char, thats
	//why there is a [1]
	AnAccount.Gender       := String(QueryResult.records[0].fields[4].value)[1];
	AnAccount.LoginCount   := StrToIntDef(QueryResult.records[0].fields[5].value,0);
	AnAccount.EMail        := QueryResult.records[0].fields[6].value;
	AnAccount.LoginKey[1]  := StrToIntDef(QueryResult.records[0].fields[7].value,0);
	AnAccount.LoginKey[2]  := StrToIntDef(QueryResult.records[0].fields[8].value,0);
	AnAccount.Level        := StrToIntDef(QueryResult.records[0].fields[9].value,0);
	AnAccount.ConnectUntil := ConvertMySQLTime(QueryResult.records[0].fields[11].value);
	AnAccount.LastIP       := QueryResult.records[0].fields[12].value;
	AnAccount.Bantime      := ConvertMySQLTime(QueryResult.records[0].fields[14].value);
end;//SetAccount
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLGameDatabase.GetChara()                                           FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
function TJanSQLGameDatabase.GetChara(CharaID : Cardinal) : TCharacter;
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
//TJanSQLGameDatabase.GetAccountCharas()                                   FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//		December 18th, 2006 - Tsusai - QueryResult now freed.
//
//------------------------------------------------------------------------------
function TJanSQLGameDatabase.GetAccountCharas(AccountID : Cardinal) : TCharacterList;
var
	QueryResult     : TJanRecordSet;
	Index           : Integer;
  ResultIdentifier : Integer;
begin
	Result := TCharacterList.Create(FALSE);
	ResultIdentifier := SendQuery(
		Format('SELECT char_id FROM characters WHERE account_id = %d and char_num < 9',
		[AccountID]));
  if ResultIdentifier > 0 then
  begin
    QueryResult := Database.RecordSets[ResultIdentifier];
	  if QueryResult.RecordCount > 0 then
    begin
      for Index := 0 to QueryResult.RecordCount - 1 do
      begin
			  Result.Add(GetChara(StrToInt(QueryResult.Records[Index].Fields[0].value)));
      end;
    end;
    SendQuery('RELEASE TABLE characters');
 	  Database.ReleaseRecordset(ResultIdentifier);
  end;
end;
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TJanSQLGameDatabase.CharaExists()                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//		December 18th, 2006 - Tsusai - Result simplified, freed queryresult.
//
//------------------------------------------------------------------------------
function TJanSQLGameDatabase.CharaExists(AccountID : Cardinal; Slot : Cardinal) : Boolean;
var
  ResultIdentifier : Integer;
begin
  Result := FALSE;
	ResultIdentifier :=
			SendQuery(
			Format('SELECT char_id FROM characters WHERE char_num = %d and account_id = %d',[Slot, AccountID]));
  if ResultIdentifier > 0 then
  begin
    if Database.RecordSets[ResultIdentifier].recordcount > 0 then
    begin
      Result := TRUE;
    end;
    SendQuery('RELEASE TABLE characters');
    Database.ReleaseRecordset(ResultIdentifier);
  end;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TJanSQLGameDatabase.CharaExists()                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//		December 18th, 2006 - Tsusai - Simplified Result, freed query result
//
//------------------------------------------------------------------------------
function TJanSQLGameDatabase.CharaExists(Name : String) : Boolean;
var
  ResultIdentifier : Integer;
begin
  Result := FALSE;
	ResultIdentifier :=
			SendQuery(
			Format('SELECT char_id FROM characters WHERE name = ''%s''',[Name]));
  if ResultIdentifier > 0 then
  begin
    if Database.RecordSets[ResultIdentifier].recordcount > 0 then
    begin
      Result := TRUE;
    end;
    SendQuery('RELEASE TABLE characters');
    Database.ReleaseRecordset(ResultIdentifier);
  end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLGameDatabase.SaveChara()                                        Procedure
//------------------------------------------------------------------------------
//	What it does-
//			saves a TCharacter to the database
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TJanSQLGameDatabase.SaveChara(AChara : TCharacter);
var
	QueryString : string;
	ResultIdentifier : Integer;
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
			'p_int=%d, ' +  //needs to be in ` ` else TEXT thinks its an Integer type
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
			'WHERE char_id=%d',
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
	ResultIdentifier := SendQuery(QueryString);
  SendQuery('SAVE TABLE characters');
  SendQuery('RELEASE TABLE characters');
  if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
end;//SaveChara
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLGameDatabase.CreateChara()                                       Function
//------------------------------------------------------------------------------
//	What it does-
//			Creates a character in the database. Also, adds it to the charalist.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//		December 18th, 2006 - Tsusai - Fixed query syntax
//
//------------------------------------------------------------------------------
function TJanSQLGameDatabase.CreateChara(
	var ACharacter : TCharacter;
	AID : Cardinal;
	NName : string
) : boolean;
var
	QueryResult : TJanRecordSet;
  ResultIdentifier : Integer;
  CharacterID : Integer;
  Index : Integer;
begin
	Result := FALSE;
  ResultIdentifier := SendQuery('SELECT char_id FROM characters');
  CharacterID := 1;
  if ResultIdentifier > 0 then
  begin
    QueryResult := Database.RecordSets[ResultIdentifier];
    for Index := 0 to QueryResult.recordcount - 1 do
    begin
      CharacterID := Max(CharacterID, QueryResult.records[Index].fields[0].value);
    end;
    inc(CharacterID);
  end;

	ResultIdentifier := SendQuery(
		Format('INSERT INTO characters (char_id, account_id, name) VALUES(%d, %d, ''%s'')',
		[CharacterID, AID,NName]));
  if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
	ResultIdentifier :=
		SendQuery(
		Format('SELECT char_id FROM characters WHERE account_id = %d AND name = ''%s''',
		[AID,NName]));
  QueryResult := Database.RecordSets[ResultIdentifier];
	if (QueryResult.RecordCount = 1) then
	begin
		ACharacter := GetChara(StrToInt(QueryResult.Records[0].Fields[0].value));
		Result := Assigned(ACharacter);
	end;
  SendQuery('SAVE TABLE characters');
  SendQuery('RELEASE TABLE characters');
	if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
end;//CreateChara
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLGameDatabase.LoadChara()                                        FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Loads a character from the database.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TJanSQLGameDatabase.LoadChara(CharaID : Cardinal) : TCharacter;
var
	APoint      : TPoint;
	QueryResult : TJanRecordSet;
  ResultIdentifier : Integer;
begin
	ResultIdentifier :=
		SendQuery(
		Format('SELECT * FROM characters WHERE char_id = %d',
			[CharaID]));
  QueryResult := Database.RecordSets[ResultIdentifier];
	if (QueryResult.RecordCount = 1) and (QueryResult.fieldcount = 48) then
	begin
		with Result do
		begin
			Result           := TCharacter.Create;
			CID              := StrToIntDef(QueryResult.Records[0].Fields[0].Value, 0);
			ID               := StrToIntDef(QueryResult.Records[0].Fields[1].Value, 0);
			CharaNum         := StrToIntDef(QueryResult.Records[0].Fields[2].Value, 0);
			Name            :=          QueryResult.Records[0].Fields[3].Value;
			JID             := StrToIntDef(QueryResult.Records[0].Fields[4].Value, 0);
			BaseLV          := StrToIntDef(QueryResult.Records[0].Fields[5].Value, 0);
			JobLV           := StrToIntDef(QueryResult.Records[0].Fields[6].Value, 0);
			BaseEXP         := StrToIntDef(QueryResult.Records[0].Fields[7].Value, 0);
			JobEXP          := StrToIntDef(QueryResult.Records[0].Fields[8].Value, 0);
			Zeny            := StrToIntDef(QueryResult.Records[0].Fields[9].Value, 0);
			ParamBase[STR]  := StrToIntDef(QueryResult.Records[0].Fields[10].Value, 0);
			ParamBase[AGI]  := StrToIntDef(QueryResult.Records[0].Fields[11].Value, 0);
			ParamBase[VIT]  := StrToIntDef(QueryResult.Records[0].Fields[12].Value, 0);
			ParamBase[INT]  := StrToIntDef(QueryResult.Records[0].Fields[13].Value, 0);
			ParamBase[DEX]  := StrToIntDef(QueryResult.Records[0].Fields[14].Value, 0);
			ParamBase[LUK]  := StrToIntDef(QueryResult.Records[0].Fields[15].Value, 0);
			MaxHP           := StrToIntDef(QueryResult.Records[0].Fields[16].Value, 0);
			HP              := StrToIntDef(QueryResult.Records[0].Fields[17].Value, 0);
			MaxSP           := StrToIntDef(QueryResult.Records[0].Fields[18].Value, 0);
			SP              := StrToIntDef(QueryResult.Records[0].Fields[19].Value, 0);
			StatusPts       := StrToIntDef(QueryResult.Records[0].Fields[20].Value, 0);
			SkillPts        := StrToIntDef(QueryResult.Records[0].Fields[21].Value, 0);
			Option          := StrToIntDef(QueryResult.Records[0].Fields[22].Value, 0);
			Karma           := StrToIntDef(QueryResult.Records[0].Fields[23].Value, 0);
			Manner          := StrToIntDef(QueryResult.Records[0].Fields[24].Value, 0);
			PartyID         := StrToIntDef(QueryResult.Records[0].Fields[25].Value, 0);
			GuildID         := StrToIntDef(QueryResult.Records[0].Fields[26].Value, 0);
			PetID           := StrToIntDef(QueryResult.Records[0].Fields[27].Value, 0);
			Hair            := StrToIntDef(QueryResult.Records[0].Fields[28].Value, 0);
			HairColor       := StrToIntDef(QueryResult.Records[0].Fields[29].Value, 0);
			ClothesColor    := StrToIntDef(QueryResult.Records[0].Fields[30].Value, 0);
			Weapon          := StrToIntDef(QueryResult.Records[0].Fields[31].Value, 0);
			Shield          := StrToIntDef(QueryResult.Records[0].Fields[32].Value, 0);
			HeadTop         := StrToIntDef(QueryResult.Records[0].Fields[33].Value, 0);
			HeadMid         := StrToIntDef(QueryResult.Records[0].Fields[34].Value, 0);
			HeadBottom      := StrToIntDef(QueryResult.Records[0].Fields[35].Value, 0);
			Map             :=          QueryResult.Records[0].Fields[36].Value ;
				APoint.X      := StrToIntDef(QueryResult.Records[0].Fields[37].Value, 0);
				APoint.Y      := StrToIntDef(QueryResult.Records[0].Fields[38].Value, 0);
			Point           := APoint;
			SaveMap         :=          QueryResult.Records[0].Fields[39].Value ;
				APoint.X      := StrToIntDef(QueryResult.Records[0].Fields[40].Value, 0);
				APoint.Y      := StrToIntDef(QueryResult.Records[0].Fields[41].Value, 0);
			SavePoint       := APoint;
			PartnerID       := StrToIntDef(QueryResult.Records[0].Fields[42].Value, 0);
			ParentID1       := StrToIntDef(QueryResult.Records[0].Fields[43].Value, 0);
			ParentID2       := StrToIntDef(QueryResult.Records[0].Fields[44].Value, 0);
			BabyID          := StrToIntDef(QueryResult.Records[0].Fields[45].Value, 0);
			Online          := StrToIntDef(QueryResult.Records[0].Fields[46].Value, 0);
			HomunID         := StrToIntDef(QueryResult.Records[0].Fields[47].Value, 0);
			//Do not start the save timer caused by modifying everything else.
			DataChanged := false;
		end;
	end else Result := nil;
  SendQuery('RELEASE TABLE characters');
	if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
end;//LoadChara
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TJanSQLGameDatabase.DeleteChara()                                        FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Deletes a character from the database.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TJanSQLGameDatabase.DeleteChara(var ACharacter : TCharacter) : boolean;
var
  ResultIdentifier : Integer;
begin
	ResultIdentifier := SendQuery(
		Format('DELETE FROM characters WHERE char_id=%d',[ACharacter.CID]));

		CharacterList.Delete(CharacterList.IndexOf(ACharacter.CID));
    Result := TRUE;
    SendQuery('SAVE TABLE characters');
    SendQuery('RELEASE TABLE characters');
  if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
end;//DeleteChara
//------------------------------------------------------------------------------

{END JanSQLGameDatabase}
end.
