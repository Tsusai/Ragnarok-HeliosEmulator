(*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

Unit
SQLiteGameDatabase

*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

[2007/06/18] Helios - Tsusai

================================================================================
License:  (FreeBSD, plus commercial with written permission clause.)
================================================================================

Project Helios - Copyright (c) 2005-2007

All rights reserved.

Please refer to Helios.dpr for full license terms.

================================================================================
Overview:
================================================================================

	This unit enables Helios to use SQLite files for the Game (Character) Database.

================================================================================
Revisions:
================================================================================
(Format: [yyyy/mm/dd] <Author> - <Desc of Changes>)
[2007/06/18] Tsusai - Created Unit.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)


unit SQLiteGameDatabase;


interface


uses
	{RTL/VCL}
	//none
	{Project}
	Character,
	CharaList,
	Database,
	GameDatabaseTemplate,
	{3rd Party}
	SQLiteTable3
	;


(*= CLASS =====================================================================*
TSQLiteGameDatabase

[2007/04/06] ChrstphrR

*------------------------------------------------------------------------------*
Overview:
*------------------------------------------------------------------------------*


*------------------------------------------------------------------------------*
Revisions:
*------------------------------------------------------------------------------*
(Format: [yyyy/mm/dd] <Author> - <Description of Change>)
[2007/06/18] Tsusai - Created.
[2007/06/30] Tsusai - Added Get/SetCharaVariable
*=============================================================================*)
Type
TSQLiteGameDatabase = class(TGameDatabaseTemplate)
protected
	fDatabase : TSQLiteDatabase;
	fParent  : TDatabase;

	function SendQuery(
		const
			QString : String
		) : TSQLiteTable;

public

	Constructor Create(
		const
			AParent : TDatabase
		);

	Destructor Destroy; override;

	function  CreateChara(
		var
			ACharacter : TCharacter;
		const
			AID        : LongWord;
		const
			Name      : String;
		const
			CharaNum   : Integer
		) : Boolean; override;

	function  GetAccountCharas(
		const
			AccountID : LongWord
		) : TCharacterList; override;

	function  LoadChara(
		const
			CharaID : LongWord
		) : TCharacter; overload; override;

	function  LoadChara(
		const
			CharaName : String
		) : TCharacter; overload; override;

	function  GetChara(
		const
				CharaID          : LongWord
		) : TCharacter; overload; override;

	function  GetChara(
		const
				CharaName          : String
		) : TCharacter; overload; override;

	function  DeleteChara(
		var
			ACharacter : TCharacter
		) : Boolean; override;

	function  CharaExists(
		const
			AccountID : LongWord;
		const
			Slot      : Word
		) : Boolean; overload; override;

	function  CharaExists(
		const
			Name : String
		) : Boolean; overload; override;

	procedure SaveChara(
		const
			AChara : TCharacter
		); override;
	
	function GetCharaVariable(
		const 
			AChara : TCharacter; 
		const 
			Key : string
		) : integer; override;
		
	procedure SetCharaVariable(
		const 
			AChara : TCharacter; 
		const 
			Key : string;
		const 
			Value : integer
		); override;

	function GetCharaName(
	const
		CharID    : LongWord
		):String;override;

	function  GetFriendList(
		const
			CharID : LongWord
		) : TCharacterList; override;

	function DeleteFriend(
		const ReqID     : LongWord;
		const AccountID : LongWord;
		const CharID    : LongWord
		) : Boolean; override;

	procedure AddFriend(
		const OrigID   : LongWord;
		const AccID    : LongWord;
		const CharID   : LongWord;
		const CharName : String
		); override;

  function IsFriend(
		const CharID   : LongWord;
		const TargetAID: LongWord;
		const TargetID : LongWord
		):Boolean; override;

	function  Connect : Boolean; override;
	procedure Disconnect; override;

	property Database : TSQLiteDatabase
		read  fDatabase;
	property Parent : TDatabase
		read  fParent;

End;(* TSQLiteGameDatabase
*== CLASS ====================================================================*)


implementation


uses
	{RTL/VCL}
	Math,
	SysUtils,
	Types,
	{Project}
	GameConstants,
	Globals,
	Main,
  SQLExtendedRoutines
	{3rd Party}
	//none
	;


//------------------------------------------------------------------------------
//TSQLiteGameDatabase.Create                                         CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our connection object.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//------------------------------------------------------------------------------
Constructor TSQLiteGameDatabase.Create(
	const
		AParent : TDatabase
	);
begin
	inherited Create;
	fParent := AParent;
	fDatabase := TSQLiteDatabase.Create;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteGameDatabase.Destroy                                         DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys our connection object.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
Destructor TSQLiteGameDatabase.Destroy();
begin
	Disconnect;
	Database.Free;
	inherited;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteGameDatabase.Disconnect()                                      Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Destroys the SQLite Connection.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
procedure TSQLiteGameDatabase.Disconnect;
begin

end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteGameDatabase.Connect()                                        Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Initializes the SQLite Connection.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//------------------------------------------------------------------------------
function TSQLiteGameDatabase.Connect() : boolean;
begin

	if DirectoryExists(MainProc.DatabaseOptions.GameHost) then
	begin
		Result :=
			Database.Connect(
				ExpandFileName(MainProc.DatabaseOptions.GameHost)
				 + '/'+MainProc.DatabaseOptions.GameDB+'.db');
	end else
	begin
		Console.WriteLn('');
		Console.WriteLn('The database at '+MainProc.DatabaseOptions.GameHost+' does not exist!');
		Console.WriteLn('Please ensure that you have correctly configured your ini file');
		Result := false;
	end;

	if not Result then
	begin
		Console.WriteLn('*****Could not open text database.');
		Console.WriteLn(MainProc.DatabaseOptions.GameHost);
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteGameDatabase.SendQuery()                                       Function
//------------------------------------------------------------------------------
//	What it does-
//			Sends a query to the SQLite object.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteGameDatabase.SendQuery(
	const QString : string
) : TSQLiteTable;
begin
	Result := Database.GetTable(QString);
	{if (Result = 0) AND (Database.Error <> 'SELECT FROM: no records') then
	begin
		Console.Message('Text Query Error: ' + QString + '- ' + Database.Error, 'Game Database', MS_ERROR);
		Console.WriteLn(Database.Error);
	end;}
end;//SendQuery
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteGameDatabase.GetChara()                                        FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Fetch Character based on Character ID, return as TCharacter
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteGameDatabase.GetChara(
	const
		CharaID          : LongWord
	) : TCharacter;
begin
	Result := LoadChara(CharaID);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteGameDatabase.GetChara()                                        FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Fetch Character based on Character Name, return as TCharacter
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteGameDatabase.GetChara(
	const
		CharaName          : String
	) : TCharacter;
begin
	Result := LoadChara(CharaName);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteGameDatabase.GetAccountCharas()                                FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Fetch Character List of an account using that account ID.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteGameDatabase.GetAccountCharas(
	const
		AccountID : LongWord
	) : TCharacterList;
var
	QueryResult     : TSQLiteTable;
	Index           : Integer;
begin
	Result := TCharacterList.Create(FALSE);
	QueryResult := SendQuery(
		Format('SELECT char_id FROM characters WHERE account_id = %d and char_num < 9',
		[AccountID]));
	if QueryResult.RowCount > 0 then
	begin
		for Index := 0 to QueryResult.RowCount - 1 do
		begin
			Result.Add(GetChara(QueryResult.FieldAsInteger(0)));
			QueryResult.Next;
		end;
	end;
	if Assigned(QueryResult) then QueryResult.Free;
end;
//-----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TSQLiteGameDatabase.CharaExists()                                     FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Checking whether character is exist or not based on
//        Account ID and Slot Number, return as BOOLEAN
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteGameDatabase.CharaExists(
	const
		AccountID : LongWord;
	const
		Slot      : Word
	) : Boolean;
var
	QueryResult : TSQLiteTable;
begin
	QueryResult :=
			SendQuery(
			Format('SELECT char_id FROM characters WHERE char_num = %d and account_id = %d',[Slot, AccountID]));
	Result := (QueryResult.Count > 0);
	if Assigned(QueryResult) then QueryResult.Free;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TSQLiteGameDatabase.CharaExists()                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			checking whether character is exist or not based on
//        Character Name, return as BOOLEAN
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteGameDatabase.CharaExists(
	const
		Name : String
	) : Boolean;
var
	QueryResult : TSQLiteTable;
begin
	QueryResult :=
			SendQuery(
			Format('SELECT char_id FROM characters WHERE name = ''%s''',[SQLEscapeString(Name)]));
	Result := (QueryResult.Count > 0);
	if Assigned(QueryResult) then QueryResult.Free;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteGameDatabase.SaveChara()                                        Procedure
//------------------------------------------------------------------------------
//	What it does-
//			saves a TCharacter to the database
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//		[2007/06/19] Tsusai - SQLite makes a return table anyways, so we need to free it.
//
//------------------------------------------------------------------------------
procedure TSQLiteGameDatabase.SaveChara(
	const
		AChara : TCharacter
	);
var
	QueryString : string;
	QueryResult : TSQLiteTable;
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
			'WHERE char_id=%d',
			[
			CharaNum,
			SQLEscapeString(Name),
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
	QueryResult := SendQuery(QueryString);
	if Assigned(QueryResult) then QueryResult.Free;
end;//SaveChara
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteGameDatabase.CreateChara()                                       Function
//------------------------------------------------------------------------------
//	What it does-
//			Creates a character in the database. Also, adds it to the charalist.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteGameDatabase.CreateChara(
	var
		ACharacter : TCharacter;
	const
		AID : LongWord;
	const
		Name : string;
	const
		CharaNum : Integer
	) : Boolean;
var
	QueryResult      : TSQLiteTable;
begin
	Result := false;
	SendQuery(
		Format(
			'INSERT INTO characters (account_id, char_num, name) VALUES(%d, %d, ''%s'')',
			[AID, CharaNum, SQLEscapeString(Name)]
			)
		);

	QueryResult := SendQuery(
		Format(
			'SELECT char_id FROM characters WHERE account_id = %d AND name = ''%s''',
			[AID,SQLEscapeString(Name)]
			)
		);

	if (QueryResult.Count = 1) then
	begin
		//Call GetChara
		ACharacter := GetChara(QueryResult.FieldAsInteger(0));
		Result := Assigned(ACharacter);
	end;
	
	if Assigned(QueryResult) then QueryResult.Free;
end;//CreateChara
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteGameDatabase.LoadChara()                                        FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Loads a character from the database.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteGameDatabase.LoadChara(
	const
		CharaID : LongWord
	) : TCharacter;
var
	APoint      : TPoint;
	QueryResult : TSQLiteTable;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT * FROM characters WHERE char_id = %d',
			[CharaID]));
	if (QueryResult.Count = 1) then
	begin
		with Result do
		begin
			Result           := TCharacter.Create(Parent.ClientInfo);
			CID              := QueryResult.FieldAsInteger(0);
			ID               := QueryResult.FieldAsInteger(1);
			CharaNum         := QueryResult.FieldAsInteger(2);
			Name             := QueryResult.Fields[3];
			JID              := QueryResult.FieldAsInteger(4);
			BaseLV           := QueryResult.FieldAsInteger(5);
			JobLV            := QueryResult.FieldAsInteger(6);
			BaseEXP          := QueryResult.FieldAsInteger(7);
			JobEXP           := QueryResult.FieldAsInteger(8);
			Zeny             := QueryResult.FieldAsInteger(9);
			ParamBase[STR]   := QueryResult.FieldAsInteger(10);
			ParamBase[AGI]   := QueryResult.FieldAsInteger(11);
			ParamBase[VIT]   := QueryResult.FieldAsInteger(12);
			ParamBase[INT]   := QueryResult.FieldAsInteger(13);
			ParamBase[DEX]   := QueryResult.FieldAsInteger(14);
			ParamBase[LUK]   := QueryResult.FieldAsInteger(15);
			HP               := QueryResult.FieldAsInteger(17);
			SP               := QueryResult.FieldAsInteger(19);
			StatusPts        := QueryResult.FieldAsInteger(20);
			SkillPts         := QueryResult.FieldAsInteger(21);
			Option           := QueryResult.FieldAsInteger(22);
			Karma            := QueryResult.FieldAsInteger(23);
			Manner           := QueryResult.FieldAsInteger(24);
			PartyID          := QueryResult.FieldAsInteger(25);
			GuildID          := QueryResult.FieldAsInteger(26);
			PetID            := QueryResult.FieldAsInteger(27);
			Hair             := QueryResult.FieldAsInteger(28);
			HairColor        := QueryResult.FieldAsInteger(29);
			ClothesColor     := QueryResult.FieldAsInteger(30);
			RightHand        := QueryResult.FieldAsInteger(31);
			LeftHand         := QueryResult.FieldAsInteger(32);
			Armor        		 := QueryResult.FieldAsInteger(33);
			Garment        	 := QueryResult.FieldAsInteger(34);
			Shoes        		 := QueryResult.FieldAsInteger(35);
			Accessory1       := QueryResult.FieldAsInteger(36);
			Accessory2       := QueryResult.FieldAsInteger(37);
			HeadTop          := QueryResult.FieldAsInteger(38);
			HeadMid          := QueryResult.FieldAsInteger(39);
			HeadBottom       := QueryResult.FieldAsInteger(40);
			Map              := QueryResult.Fields[41] ;
				APoint.X       := QueryResult.FieldAsInteger(42);
				APoint.Y       := QueryResult.FieldAsInteger(43);
			Position         := APoint;
			SaveMap          := QueryResult.Fields[44] ;
				APoint.X       := QueryResult.FieldAsInteger(45);
				APoint.Y       := QueryResult.FieldAsInteger(46);
			SavePoint        := APoint;
			PartnerID        := QueryResult.FieldAsInteger(47);
			ParentID1        := QueryResult.FieldAsInteger(48);
			ParentID2        := QueryResult.FieldAsInteger(49);
			BabyID           := QueryResult.FieldAsInteger(50);
			Online           := QueryResult.FieldAsInteger(51);
			HomunID          := QueryResult.FieldAsInteger(52);
			//Do not start the save timer caused by modifying everything else.
			DataChanged := false;
			CalcMaxWeight;
			CalcMaxHP;
			CalcMaxSP;
			CalcSpeed;
		end;
	end else Result := nil;
	if Assigned(QueryResult) then QueryResult.Free;
end;//LoadChara
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteGameDatabase.LoadChara()                                        FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Loads a character from the database.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteGameDatabase.LoadChara(
	const
		CharaName : String
	) : TCharacter;
var
	APoint      : TPoint;
	QueryResult : TSQLiteTable;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT * FROM characters WHERE name = ''%s''',
			[SQLEscapeString(CharaName)]));
	if (QueryResult.Count = 1) then
	begin
		with Result do
		begin
			Result          := TCharacter.Create(Parent.ClientInfo);
			CID             := QueryResult.FieldAsInteger(0);
			ID              := QueryResult.FieldAsInteger(1);
			CharaNum        := QueryResult.FieldAsInteger(2);
			Name            := QueryResult.Fields[3];
			JID             := QueryResult.FieldAsInteger(4);
			BaseLV          := QueryResult.FieldAsInteger(5);
			JobLV           := QueryResult.FieldAsInteger(6);
			BaseEXP         := QueryResult.FieldAsInteger(7);
			JobEXP          := QueryResult.FieldAsInteger(8);
			Zeny            := QueryResult.FieldAsInteger(9);
			ParamBase[STR]  := QueryResult.FieldAsInteger(10);
			ParamBase[AGI]  := QueryResult.FieldAsInteger(11);
			ParamBase[VIT]  := QueryResult.FieldAsInteger(12);
			ParamBase[INT]  := QueryResult.FieldAsInteger(13);
			ParamBase[DEX]  := QueryResult.FieldAsInteger(14);
			ParamBase[LUK]  := QueryResult.FieldAsInteger(15);
			HP              := QueryResult.FieldAsInteger(17);
			SP              := QueryResult.FieldAsInteger(19);
			StatusPts       := QueryResult.FieldAsInteger(20);
			SkillPts        := QueryResult.FieldAsInteger(21);
			Option          := QueryResult.FieldAsInteger(22);
			Karma           := QueryResult.FieldAsInteger(23);
			Manner          := QueryResult.FieldAsInteger(24);
			PartyID         := QueryResult.FieldAsInteger(25);
			GuildID         := QueryResult.FieldAsInteger(26);
			PetID           := QueryResult.FieldAsInteger(27);
			Hair            := QueryResult.FieldAsInteger(28);
			HairColor       := QueryResult.FieldAsInteger(29);
			ClothesColor    := QueryResult.FieldAsInteger(30);
			RightHand       := QueryResult.FieldAsInteger(31);
			LeftHand        := QueryResult.FieldAsInteger(32);
			Armor        		:= QueryResult.FieldAsInteger(33);
			Garment        	:= QueryResult.FieldAsInteger(34);
			Shoes        		:= QueryResult.FieldAsInteger(35);
			Accessory1      := QueryResult.FieldAsInteger(36);
			Accessory2      := QueryResult.FieldAsInteger(37);
			HeadTop         := QueryResult.FieldAsInteger(38);
			HeadMid         := QueryResult.FieldAsInteger(39);
			HeadBottom      := QueryResult.FieldAsInteger(40);
			Map             := QueryResult.Fields[41];
				APoint.X      := QueryResult.FieldAsInteger(42);
				APoint.Y      := QueryResult.FieldAsInteger(43);
			Position        := APoint;
			SaveMap         := QueryResult.Fields[44];
				APoint.X      := QueryResult.FieldAsInteger(45);
				APoint.Y      := QueryResult.FieldAsInteger(46);
			SavePoint       := APoint;
			PartnerID       := QueryResult.FieldAsInteger(47);
			ParentID1       := QueryResult.FieldAsInteger(48);
			ParentID2       := QueryResult.FieldAsInteger(49);
			BabyID          := QueryResult.FieldAsInteger(50);
			Online          := QueryResult.FieldAsInteger(51);
			HomunID         := QueryResult.FieldAsInteger(52);
			//Do not start the save timer caused by modifying everything else.
			DataChanged := false;
			CalcMaxWeight;
			CalcMaxHP;
			CalcMaxSP;
			CalcSpeed;
		end;
	end else Result := nil;
	if Assigned(QueryResult) then QueryResult.Free;
end;//LoadChara
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteGameDatabase.DeleteChara()                                     FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Deletes a character from the database.
//
//	Changes -
//		[2007/06/18] Tsusai - Created.
//		[2007/06/19] Tsusai - SQLite makes a return table anyways, so we need to free it.
//
//------------------------------------------------------------------------------
function TSQLiteGameDatabase.DeleteChara(var ACharacter : TCharacter) : boolean;
var
	QueryResult : TSQLiteTable;
begin
	QueryResult := SendQuery(
		Format('DELETE FROM characters WHERE char_id=%d',[ACharacter.CID]));
	Result := true;
	if Assigned(QueryResult) then QueryResult.Free;
end;//DeleteChara
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TSQLiteGameDatabase.GetCharaFlag()                                  FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a character variable from the database
//
//	Changes -
//		[2007/06/30] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteGameDatabase.GetCharaVariable(
	const AChara : TCharacter;
	const Key : string
) : integer;
var
	QueryResult : TSQLiteTable;
begin
	Result := 0;
	QueryResult := 
		SendQuery(
			Format('Select value FROM character_vars WHERE char_id = %d and key = ''%s''',[AChara.CID,SQLEscapeString(Key)])
		);
	if QueryResult.Count = 1 then
	begin
		Result := QueryResult.FieldAsInteger(0);
	end;
	if Assigned(QueryResult) then QueryResult.Free;
end;//GetCharaVariable
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TSQLiteGameDatabase.SaveCharaVariable()                                     FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Saves a character variable.
//
//	Changes -
//		[2007/06/30] Tsusai - Created.
//
//------------------------------------------------------------------------------
procedure TSQLiteGameDatabase.SetCharaVariable(
	const AChara : TCharacter;
	const Key : string;
	const Value : integer
);
var
	QueryResult : TSQLiteTable;
	Existing : integer;
begin
	Existing := GetCharaVariable(AChara,Key);
	//Check to see if its actually changing.
	if Existing <> Value then
	begin
		if Value <> 0 then
		begin
			//Update
			QueryResult :=
				SendQuery(
					Format('INSERT OR REPLACE INTO character_vars (char_id, key, value) VALUES (%d,''%s'',%d)',[AChara.CID,SQLEscapeString(Key),Value])
				);
		end else
		begin
			//Delete the key.  Value is 0
			QueryResult :=
				SendQuery(
					Format('DELETE FROM character_vars WHERE char_id = %d and key = ''%s''',[AChara.CID,SQLEscapeString(Key)])
				);
		end;
		if Assigned(QueryResult) then QueryResult.Free;
	end;
end;//SetCharaVariable
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetCharaName                                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Get character name based on char id.
//
//	Changes -
//		[2007/08/09] Aeomin - Created.
//
//------------------------------------------------------------------------------
function TSQLiteGameDatabase.GetCharaName(
	const
		CharID    : LongWord
):String;
var
	QueryResult : TSQLiteTable;
begin
	QueryResult :=
		SendQuery(
		Format('SELECT * FROM characters WHERE char_id = %d',
			[CharID]));
	if (QueryResult.Count = 1) then
	begin
		Result := QueryResult.Fields[3];
	end else
	begin
		Result := '';
	end;
	if Assigned(QueryResult) then QueryResult.Free;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetFriendList                                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Get list of friends.
//
//	Changes -
//		[2007/12/05] Aeomin - Created.
//
//------------------------------------------------------------------------------
function  TSQLiteGameDatabase.GetFriendList(
		const
			CharID : LongWord
		) : TCharacterList;
var
	QueryResult     : TSQLiteTable;
	Index           : Byte;
	Char            : TCharacter;
begin
	Result := TCharacterList.Create(TRUE);
	QueryResult := SendQuery(
		Format(
			'SELECT * FROM friend WHERE char_id = %d LIMIT 0,%d',
			[CharID,  MAX_FRIENDS]
			)
		);
	if QueryResult.Count > 0 then
	begin
		for Index := 0 to QueryResult.Count - 1 do
		begin
			Char      := TCharacter.Create(Parent.ClientInfo);
			Char.ID   := QueryResult.FieldAsInteger(1);
			Char.CID  := QueryResult.FieldAsInteger(2);
			Char.Name := QueryResult.Fields[3];
			Result.Add(Char);
			if Index < QueryResult.Count then
			begin
				QueryResult.Next;
			end;
		end;
	end;
	if Assigned(QueryResult) then QueryResult.Free;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//DeleteFriend                                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Delete a friend, return false is friend not even exists
//
//	Changes -
//		[2007/12/06] Aeomin - Created.
//
//------------------------------------------------------------------------------
function TSQLiteGameDatabase.DeleteFriend(
		const ReqID     : LongWord;
		const AccountID : LongWord;
		const CharID    : LongWord
		) : Boolean;
var
	QueryResult     : TSQLiteTable;
begin
	Result := False;
	QueryResult := SendQuery(
		Format(
			'SELECT * FROM friend WHERE char_id = %d AND id1 = %d AND id2 = %d',
			[ReqID, AccountID, CharID]
			)
		);

	if QueryResult.Count > 0 then
	begin
		SendQuery(
		Format(
			'DELETE FROM friend WHERE char_id = %d AND id1 = %d AND id2 = %d',
			[ReqID, AccountID, CharID]
			)
		);
	end;

	if Assigned(QueryResult) then QueryResult.Free;
end;{DeleteFriend}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//AddFriend                                                             FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Add a friend, just insert to database
//
//	Changes -
//		[2007/12/08] Aeomin - Created.
//
//------------------------------------------------------------------------------
procedure TSQLiteGameDatabase.AddFriend(
	const OrigID   : LongWord;
	const AccID    : LongWord;
	const CharID   : LongWord;
	const CharName : String
);
begin
	SendQuery(
		Format(
			'INSERT INTO `friend` (`char_id`,`id1`,`id2`,`name`) VALUE (%d, %d, %d, ''%s'')',
			[OrigID, AccID, CharID, SQLEscapeString(CharName)]
			)
		);
end;{AddFriend}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//IsFriend                                                              FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Check if TargetID is friend of CharID
//
//	Changes -
//		[2007/12/08] Aeomin - Created.
//		[2007/12/22] RaX - Copied to SQLite.
//------------------------------------------------------------------------------
function TSQLiteGameDatabase.IsFriend(
	const CharID   : LongWord;
	const TargetAID: LongWord;
	const TargetID : LongWord
):Boolean;
var
	QueryResult     : TSQLiteTable;
begin
	Result := False;

	QueryResult := SendQuery(
		Format(
			'SELECT * FROM `friend` WHERE `char_id` = %d AND `id1` = %d AND `id2` = %d',
			[CharID, TargetAID, TargetID]
			)
		);

  if QueryResult.RowCount > 0 then
  begin
    Result := True;
  end;

	if Assigned(QueryResult) then QueryResult.Free;
end;{IsFriend}
//------------------------------------------------------------------------------
{END SQLiteGameDatabase}
end.
