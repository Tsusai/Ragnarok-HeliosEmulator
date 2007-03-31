//------------------------------------------------------------------------------
//MySQLGameDatabase		                                            UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is one of our database objects which enabled Helios to use a MySQL
//    Database.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//	[2007/03/30] CR - Used Icarus to clean up uses clauses.
//
//------------------------------------------------------------------------------
unit MySQLGameDatabase;


interface


uses
	{RTL/VCL}
	//none
	{Project}
	GameDatabaseTemplate,
	Character,
	CharaList,
	Database,
	{Third Party}
	uMysqlClient
	;


type
//------------------------------------------------------------------------------
//TMySQLGameDatabase			                                   CLASS
//------------------------------------------------------------------------------
//	What it does-
//			This is a child class for our database object system. It allows Helios
//    to communicate with a MySQL database and houses all routines for doing so.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//		January 20th, 2007 - Tsusai - Connect is now a bool function
//			Create holds connection result
//
//------------------------------------------------------------------------------
	TMySQLGameDatabase = class(TGameDatabaseTemplate)
	private
		Connection   : TMySQLClient;
    Parent : TDatabase;
	public


		Constructor Create(
			AParent : TDatabase
		); reintroduce; overload;
		Destructor Destroy();override;

		function CreateChara(
			var ACharacter : TCharacter;
			AID : LongWord;
			NName : string;
			CharaNum : Integer
		) : boolean;override;

		function GetAccountCharas(AccountID : LongWord) : TCharacterList;override;
		function LoadChara(CharaID : LongWord) : TCharacter;override;

		function GetChara(
			CharaID : LongWord;
			JanSQLClearTable : boolean = false
		) : TCharacter;override;

		function DeleteChara(var ACharacter : TCharacter) : boolean;override;
		function CharaExists(AccountID : LongWord; Slot : Word) : Boolean;overload;override;
		function CharaExists(Name : String) : Boolean;overload;override;

		procedure SaveChara(AChara : TCharacter);override;

		function Connect() : boolean; override;
		procedure Disconnect();override;
		
	protected
		function SendQuery(
			const QString : string;
			StoreResult : boolean;
			var ExecutedOK : boolean
		) : TMySQLResult;
	end;
//------------------------------------------------------------------------------


implementation


uses
	SysUtils,
	Types,
	{Project}
	GameConstants,
	Globals,
	Main
	{Third Party}
	//none
	;


//------------------------------------------------------------------------------
//TMySQLGameDatabase.Create()                                        CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our connection object.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//		November 13th, 2006 - Tsusai - create inherit comes first.
//		January 20th, 2007 - Tsusai - Create holds connection result
//
//------------------------------------------------------------------------------
Constructor TMySQLGameDatabase.Create(
	AParent : TDatabase
);
begin
	inherited Create;
	Parent := AParent;
	Connection := TMySQLClient.Create;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMySQLGameDatabase.Destroy()                                        DESTRUCTOR
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
//TMySQLGameDatabase.Connect()                               PROTECTED PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Initializes the MySQL Connection.
//
//	Changes -
//		October 5th, 2006 - RaX - Moved here from globals.
//		January 20th, 2007 - Tsusai - Connect is now a bool function
//
//------------------------------------------------------------------------------
function TMySQLGameDatabase.Connect() : boolean;
begin
	Result := true;
	if NOT Connection.Connected then
	begin
		Connection.Host            := MainProc.DatabaseOptions.GameHost;
		Connection.Port            := MainProc.DatabaseOptions.GamePort;
		Connection.Db              := MainProc.DatabaseOptions.GameDB;
		Connection.User            := MainProc.DatabaseOptions.GameUser;
		Connection.Password        := MainProc.DatabaseOptions.GamePass;
	end;

	Connection.ConnectTimeout  := 10;

	if NOT Connection.Connect then
	begin
		Console.WriteLn('*****Could not connect to mySQL database server.');
		Result := false;
	end;

end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendQuery			                              PROTECTED FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//                  Send SQL Query to Mysql database and return with TMySQLResult
//
//	Changes -
//		March 12th, 2007 - Aeomin - Created Header
//
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
		Console.Message('MySQL Query error: ' + QString + '- '+Connection.LastError, 'Game Database' , MS_ERROR);
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLGameDatabase.Disconnect()                            PROTECTED PROCEDURE
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
//TMySQLGameDatabase.GetChara()                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Fetch Character based on Character ID and return with TCharacter
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//		March 12th, 2007 - Aeomin - Modify Header
//
//------------------------------------------------------------------------------
function TMySQLGameDatabase.GetChara(
	CharaID : LongWord;
	//JanSQLClearTable is never used.
	JanSQLClearTable : boolean = false
) : TCharacter;
begin
	Result := LoadChara(CharaID);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLGameDatabase.GetAccountCharas()                                 FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Fetch Character List of an account using that account ID
//			and return with TCharacterList
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//		December 18th, 2006 - Tsusai - Freed result.
//		March 12th, 2007 - Aeomin - Modify Header
//
//------------------------------------------------------------------------------
function TMySQLGameDatabase.GetAccountCharas(AccountID : LongWord) : TCharacterList;
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
//TMySQLGameDatabase.CharaExists()                                      FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Checking whether character is exist or not based on
//        Account ID and Slot Number, return as BOOLEAN
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//		December 18th, 2006 - Tsusai - Simplified Result, freed Queryresult
//		March 12th, 2007 - Aeomin - Modify Header
//
//------------------------------------------------------------------------------
function TMySQLGameDatabase.CharaExists(AccountID : LongWord; Slot : Word) : Boolean;
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
//TMySQLGameDatabase.CharaExists()                                      FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Checking whether character is exist or not based on
//        Character Name, return as BOOLEAN
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//		December 18th, 2006 - Tsusai - Simplified Result, freed queryresult
//		March 12th, 2007 - Aeomin - Modify Header
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
//TMySQLGameDatabase.SaveChara()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Save character data to database use data from TCharacter
//        Character Name, return as BOOLEAN
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//		March 12th, 2007 - Aeomin - Modify Header
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
			Position.X,
			Position.Y,
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
//TMySQLGameDatabase.CreateChara()                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Creates a character in the database.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TMySQLGameDatabase.CreateChara(
	var ACharacter : TCharacter;
	AID : LongWord;
	NName : string;
	CharaNum : Integer
) : boolean;
var
	Success     : boolean;
	QueryResult : TMySQLResult;
begin
	Result := FALSE;
	SendQuery(
		Format('INSERT INTO characters (account_id, char_num, name) VALUES(%d, %d, ''%s'')',
		[AID,CharaNum, NName])
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
//TMySQLGameDatabase.LoadChara()                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Gets a Character from the database.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TMySQLGameDatabase.LoadChara(CharaID : LongWord) : TCharacter;
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
	if (QueryResult.RowsCount = 1) then
	begin
		with Result do
		begin
			Result           := TCharacter.Create(Parent.ClientInfo);
			CID              := StrToInt(QueryResult.FieldValue(0));
			ID               := StrToInt(QueryResult.FieldValue(1));
			CharaNum         := StrToInt(QueryResult.FieldValue(2));
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
			HP              := StrToInt(QueryResult.FieldValue(17));
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
			Armor        		:= StrToInt(QueryResult.FieldValue(33));
			Garment        	:= StrToInt(QueryResult.FieldValue(34));
			Shoes        		:= StrToInt(QueryResult.FieldValue(35));
			Accessory1      := StrToInt(QueryResult.FieldValue(36));
			Accessory2      := StrToInt(QueryResult.FieldValue(37));
			HeadTop         := StrToInt(QueryResult.FieldValue(38));
			HeadMid         := StrToInt(QueryResult.FieldValue(39));
			HeadBottom      := StrToInt(QueryResult.FieldValue(40));
			Map             :=          QueryResult.FieldValue(41) ;
				APoint.X      := StrToInt(QueryResult.FieldValue(42));
				APoint.Y      := StrToInt(QueryResult.FieldValue(43));
			Position        := APoint;
			SaveMap         :=          QueryResult.FieldValue(44) ;
				APoint.X      := StrToInt(QueryResult.FieldValue(45));
				APoint.Y      := StrToInt(QueryResult.FieldValue(46));
			SavePoint       := APoint;
			PartnerID       := StrToInt(QueryResult.FieldValue(47));
			ParentID1       := StrToInt(QueryResult.FieldValue(48));
			ParentID2       := StrToInt(QueryResult.FieldValue(49));
			BabyID          := StrToInt(QueryResult.FieldValue(50));
			Online          := StrToInt(QueryResult.FieldValue(51));
			HomunID         := StrToInt(QueryResult.FieldValue(52));

			CalcMaxHP;
			CalcMaxSP;
			CalcMaxWeight;
			CalcSpeed;

			//Do not start the save timer caused by modifying everything else.
			DataChanged := false;
		end;
	end else Result := nil;
	if Assigned(QueryResult) then QueryResult.Free;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLGameDatabase.DeleteChara()                                     PROCEDURE
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
end;
//------------------------------------------------------------------------------


{END MySQLGameDatabase}
end.
