(*------------------------------------------------------------------------------
Class Character
Tsusai July 2006

Description:
 Basic character object.

[2006/07/08] RaX - Moved into the 'Classes' Directory and renamed to
 'Character.pas'. One class per file is the name of the game.
------------------------------------------------------------------------------*)
unit Character;

interface
uses
	//IDE
	Types,
	//Helios
	Account,
	GameGlobals;

	type TCharacter = class
	private
		fCharacterNumber  : Byte;
		fName             : String;
		fJID              : Word;
		fBaseLV           : Byte;
		fJobLV            : Byte;
		fBaseEXP          : Byte;
		fJobEXP           : Byte;
		fZeny             : Cardinal;
		fParamBase        : Array[STR..LUK] of Byte;
		fMaxHP            : Word;
		fHP               : Word;
		fMaxSP            : Word;
		fSP               : Word;
		fStatusPts        : Word;
		fSkillPts         : Word;
		fOption           : Word;
		fKarma            : Word;
		fManner           : Word;
		fPartyID          : Cardinal;
		fGuildID          : Cardinal;
		fPetID            : Cardinal;
		fHair             : Word;
		fHairColor        : Word;
		fClothesColor     : Word;
		fWeapon           : Word;
		fShield           : Word;
		fHeadTop          : Word;
		fHeadMid          : Word;
		fHeadBottom       : Word;
		fMap              : String;
		fMapPt            : TPoint;
		fSaveMap          : String;
		fSaveMapPt        : TPoint;
		fPartnerID        : Cardinal;
		fParentID1        : Cardinal;
		fParentID2        : Cardinal;
		fBabyID           : Cardinal;
		fOnline           : Byte;
		fHomunID          : Cardinal;

		fDataChanged      : Boolean; //For timed save procedure to activate.
		fTimeToSave       : TDateTime;

		procedure SetSaveTime(Value : boolean);


		procedure SetCharaNum(Value : byte);
		procedure SetName(Value : string);
		procedure SetClass(Value : word);
		procedure SetBaseLV(Value : byte);
		procedure SetJobLV(Value : byte);
		procedure SetBaseEXP(Value : byte);
		procedure SetJobEXP(Value : byte);
		procedure SetZeny(Value : Cardinal);
		function  GetBaseStats(Index : Byte) : byte;
		procedure SetBaseStats(Index: byte; Value: byte);
		procedure SetMaxHP(Value : word);
		procedure SetHP(Value : word);
		procedure SetMaxSP(Value : word);
		procedure SetSP(Value : word);
		Procedure SetOption(Value : word);
		Procedure SetKarma(Value : word);
		Procedure SetManner(Value : word);
		Procedure SetPartyID(Value : cardinal);
		Procedure SetGuildID(Value : cardinal);
		Procedure SetPetID(Value : cardinal);
		Procedure SetHair(Value : word);
		Procedure SetHairColor(Value : word);
		Procedure SetClothesColor(Value : word);
		Procedure SetWeapon(Value : word);
		Procedure SetShield(Value : word);
		Procedure SetHeadTop(Value : word);
		Procedure SetHeadMid(Value : word);
		Procedure SetHeadBottom(Value : word);
		procedure SetStatusPts(Value : word);
		procedure SetSkillPts(Value : word);
		procedure SetMap(Value : string);
		procedure SetMapPt(Value : TPoint);
		procedure SetSMap(Value : string);
		procedure SetSMapPt(Value : TPoint);
		procedure SetPartnerID(Value : Cardinal);
		procedure SetParentID1(Value : Cardinal);
		procedure SetParentID2(Value : Cardinal);
		procedure SetBabyID(Value : Cardinal);
		procedure SetOnline(Value : Byte);
		procedure SetHomunID(Value : Cardinal);

	public

		CID : Cardinal;
		ID  : Cardinal; //Account ID
		Speed : word; //Not in MySQL...odd...
		Account : TAccount;

		ClientVersion : byte;

		property DataChanged : boolean  read fDataChanged write SetSaveTime;
		//For timed save procedure to activate.

		property CharaNum  : Byte       read fCharacterNumber write SetCharaNum;
		property Name      : string     read fName write SetName;
		property JID       : Word       read fJID write SetClass;
		property BaseLV    : Byte       read fBaseLV write SetBaseLV;
		property JobLV     : Byte       read fJobLV write SetJobLV;
		property BaseEXP   : Byte       read fBaseEXP write SetBaseEXP;
		property JobEXP    : Byte       read fJobEXP write SetJobEXP;
		property Zeny      : Cardinal   read fZeny write SetZeny;
		property ParamBase[Index : byte] : byte read GetBaseStats write SetBaseStats;
		property MaxHP     : Word       read fMaxHP write SetMaxHP;
		property HP        : Word       read fHP write SetHP;
		property MaxSP     : Word       read fMaxSP write SetMaxSP;
		property SP        : Word       read fSP write SetSP;
		property StatusPts : Word       read fStatusPts write SetStatusPts;
		property SkillPts  : Word       read fSkillPts write SetSkillPts;
		property Option    : Word       read fOption write SetOption;
		property Karma     : Word       read fKarma write SetKarma;
		property Manner    : Word       read fManner write SetManner;
		property PartyID   : Cardinal   read fPartyID write SetPartyID;
		property GuildID   : Cardinal   read fGuildID write SetGuildID;
		property PetID     : Cardinal   read fPetID write SetPetID;
		property Hair      : Word       read fHair write SetHair;
		property HairColor : Word       read fHairColor write SetHairColor;
		property ClothesColor: Word     read fClothesColor write SetClothesColor;
		property Weapon    : Word       read fWeapon write SetWeapon;
		property Shield    : Word       read fShield write SetShield;
		property HeadTop   : Word       read fHeadTop write SetHeadTop;
		property HeadMid   : Word       read fHeadMid write SetHeadMid;
		property HeadBottom: Word       read fHeadBottom write SetHeadBottom;
		property Map       : string     read fMap write SetMap;
		property Point     : TPoint     read fMapPt write SetMapPt;
		property SaveMap   : string     read fSaveMap write SetSMap;
		property SavePoint : TPoint     read fSaveMapPt write SetSMapPt;
		property PartnerID : Cardinal   read fPartnerID write SetPartnerID;
		property ParentID1 : Cardinal   read fParentID1 write SetParentID1;
		property ParentID2 : Cardinal   read fParentID2 write SetParentID2;
		property BabyID    : Cardinal   read fBabyID write SetBabyID;
		property Online    : Byte       read fOnline write SetOnline;
		property HomunID   : Cardinal   read fHomunID write SetHomunID;

		function CreateInSQL(AID : Cardinal; NName : string) : boolean;
		function LoadFromSQL(CharaID : Cardinal) : boolean;
		procedure SaveToSQL;
		function RemoveFromSQL : boolean;
	end;

	function GetCharacters(CharID : Cardinal) : TCharacter;

implementation
uses
	//IDE
	SysUtils,
	//Helios
  Database,
	Globals;

procedure TCharacter.SetSaveTime(Value : boolean);
Const
	HoursPerDay   = 24;
	MinsPerHour   = 60;
	MinsPerDay    = HoursPerDay * MinsPerHour;
	MinInterval   = 5; //Time set at 5 min
begin
	if Value and not fDataChanged then begin
		fDataChanged  := TRUE;
		fTimeToSave   := ((Now * MinsPerDay) + MinInterval) / MinsPerDay;
	end;
end;

procedure TCharacter.SetCharaNum(Value : byte);
begin
	DataChanged       := TRUE;
	fCharacterNumber  := Value;
end;

procedure TCharacter.SetName(Value : string);
begin
	DataChanged := TRUE;
	fName       := Value;
end;

procedure TCharacter.SetClass(Value : Word);
begin
	DataChanged := TRUE;
	fJID        := Value;
end;

procedure TCharacter.SetBaseLV(Value : byte);
begin
	DataChanged := TRUE;
	fBaseLV     := Value;
end;

procedure TCharacter.SetJobLV(Value : byte);
begin
	DataChanged := TRUE;
	fJobLV      := Value;
end;

procedure TCharacter.SetBaseEXP(Value : byte);
begin
	DataChanged := TRUE;
	fBaseEXP    := Value;
end;

procedure TCharacter.SetJobEXP(Value : byte);
begin
	DataChanged := TRUE;
	fJobEXP     := Value;
end;

procedure TCharacter.SetZeny(Value : Cardinal);
begin
	DataChanged := TRUE;
	fZeny       := Value;
end;

function TCharacter.GetBaseStats(Index : Byte) : Byte;
begin
	Result := fParamBase[Index];
end;

procedure TCharacter.SetBaseStats(Index, Value: Byte);
begin
	DataChanged       := TRUE;
	fParamBase[Index] := Value;
end;

procedure TCharacter.SetMaxHP(Value : word);
begin
	DataChanged := TRUE;
	fMaxHP      := Value;
end;

procedure TCharacter.SetHP(Value : word);
begin
	DataChanged := TRUE;
	fHP         := Value;
end;

procedure TCharacter.SetMaxSP(Value : word);
begin
	DataChanged := TRUE;
	fMaxSP      := Value;
end;

procedure TCharacter.SetSP(Value : word);
begin
	DataChanged := TRUE;
	fSP         := Value;
end;

procedure TCharacter.SetStatusPts(Value : Word);
begin
	DataChanged := TRUE;
	fStatusPts  := Value;
end;

procedure TCharacter.SetSkillPts(Value : Word);
begin
	DataChanged := TRUE;
	fSkillPts   := Value;
end;

procedure TCharacter.SetOption(Value : word);
begin
	DataChanged := TRUE;
	fOption     := Value;
end;

procedure TCharacter.SetKarma(Value : word);
begin
	DataChanged := TRUE;
	fKarma      := Value;
end;

procedure TCharacter.SetManner(Value : word);
begin
	DataChanged := TRUE;
	fManner     := Value;
end;

procedure TCharacter.SetPartyID(Value : cardinal);
begin
	DataChanged := TRUE;
	fPartyID    := Value;
end;

procedure TCharacter.SetGuildID(Value : cardinal);
begin
	DataChanged := TRUE;
	fGuildID    := Value;
end;

procedure TCharacter.SetPetID(Value : cardinal);
begin
	DataChanged := TRUE;
	fPetID      := Value;
end;

procedure TCharacter.SetHair(Value : word);
begin
	DataChanged := TRUE;
	fHair       := Value;
end;

procedure TCharacter.SetHairColor(Value : word);
begin
	DataChanged := TRUE;
	fHairColor  := Value;
end;

procedure TCharacter.SetClothesColor(Value : word);
begin
	DataChanged   := TRUE;
	fClothesColor := Value;
end;

procedure TCharacter.SetWeapon(Value : word);
begin
	DataChanged := TRUE;
	fWeapon     := Value;
end;

procedure TCharacter.SetShield(Value : word);
begin
	DataChanged := TRUE;
	fShield     := Value;
end;

procedure TCharacter.SetHeadTop(Value : word);
begin
	DataChanged := TRUE;
	fHeadTop    := Value;
end;

procedure TCharacter.SetHeadMid(Value : word);
begin
	DataChanged := TRUE;
	fHeadMid    := Value;
end;

procedure TCharacter.SetHeadBottom(Value : word);
begin
	DataChanged := TRUE;
	fHeadBottom := Value;
end;

procedure TCharacter.SetMap(Value : string);
begin
	DataChanged := TRUE;
	fMap        := Value;
end;

procedure TCharacter.SetMapPt(Value : TPoint);
begin
	DataChanged := TRUE;
	fMapPt      := Value;
end;

procedure TCharacter.SetSMap(Value : string);
begin
	DataChanged := TRUE;
	fSaveMap    := Value;
end;

procedure TCharacter.SetSMapPt(Value : TPoint);
begin
	DataChanged := TRUE;
	fSaveMapPt  := Value;
end;

procedure TCharacter.SetPartnerID(Value : Cardinal);
begin
	DataChanged := TRUE;
	fPartnerID  := Value;
end;

procedure TCharacter.SetParentID1(Value : Cardinal);
begin
	DataChanged := TRUE;
	fParentID1  := Value;
end;

procedure TCharacter.SetParentID2(Value : Cardinal);
begin
	DataChanged := TRUE;
	fParentID2  := Value;
end;

procedure TCharacter.SetBabyID(Value : Cardinal);
begin
	DataChanged := TRUE;
	fBabyID     := Value;
end;

procedure TCharacter.SetOnline(Value : Byte);
begin
	DataChanged := TRUE;
	fOnline     := Value;
end;

procedure TCharacter.SetHomunID(Value : Cardinal);
begin
	DataChanged := TRUE;
	fHomunID    := Value;
end;

function TCharacter.CreateInSQL(AID : Cardinal; NName : string) : boolean;
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
			Result := TRUE;
			CID := StrToInt(SQLQueryResult.FieldValue(0));
			LoadFromSQL(CID);
		end;
	end;
end;

procedure TCharacter.SaveToSQL;
var
	QueryString : string;
	Success : boolean;
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
		fCharacterNumber,
		fName,
		fJID,
		fBaseLV,
		fJobLV,
		fBaseEXP,
		fJobEXP,
		fZeny,
		fParamBase[STR],
		fParamBase[AGI],
		fParamBase[VIT],
		fParamBase[INT],
		fParamBase[DEX],
		fParamBase[LUK],
		fMaxHP,
		fHP,
		fMaxSP,
		fSP,
		fStatusPts,
		fSkillPts,
		fOption,
		fKarma,
		fManner,
		fPartyID,
		fGuildID,
		fPetID,
		fHair,
		fHairColor,
		fClothesColor,
		fWeapon,
		fShield,
		fHeadTop,
		fHeadMid,
		fHeadBottom,
		fMap,
		fMapPt.X,
		fMapPt.Y,
		fSaveMap,
		fSaveMapPt.X,
		fSaveMapPt.Y,
		fPartnerID,
		fParentID1,
		fParentID2,
		fBabyID,
		fOnline,
		fHomunID,
		CID
		]);
	SQLConnection.Query(QueryString, FALSE, Success);
end;

function TCharacter.LoadFromSQL(CharaID : Cardinal) : boolean;
var
	Success : Boolean;
  ADatabase : TDatabase;
begin
	Result := FALSE;
  ADatabase := TDatabase.Create();
	SQLQueryResult :=
		SQLConnection.query(
		Format('SELECT * FROM `char` WHERE char_id = %d;',
			[CharaID])
		,TRUE,Success);
	if (SQLQueryResult.RowsCount = 1) and (SQLQueryResult.FieldsCount = 48) then
	begin
		Result := TRUE;
		CID              := StrToInt(SQLQueryResult.FieldValue(0));
		ID               := StrToInt(SQLQueryResult.FieldValue(1));
		Account          := ADatabase.AnInterface.GetAccount(ID);
		fCharacterNumber := StrToInt(SQLQueryResult.FieldValue(2));
		if fCharacterNumber < 9 then
		begin
			//If its active, then attach to the player.
			Account.CharaID[fCharacterNumber] := CID;
		end;
		fName            :=          SQLQueryResult.FieldValue(3);
		fJID             := StrToInt(SQLQueryResult.FieldValue(4));
		fBaseLV          := StrToInt(SQLQueryResult.FieldValue(5));
		fJobLV           := StrToInt(SQLQueryResult.FieldValue(6));
		fBaseEXP         := StrToInt(SQLQueryResult.FieldValue(7));
		fJobEXP          := StrToInt(SQLQueryResult.FieldValue(8));
		fZeny            := StrToInt(SQLQueryResult.FieldValue(9));
		fParamBase[STR]  := StrToInt(SQLQueryResult.FieldValue(10));
		fParamBase[AGI]  := StrToInt(SQLQueryResult.FieldValue(11));
		fParamBase[VIT]  := StrToInt(SQLQueryResult.FieldValue(12));
		fParamBase[INT]  := StrToInt(SQLQueryResult.FieldValue(13));
		fParamBase[DEX]  := StrToInt(SQLQueryResult.FieldValue(14));
		fParamBase[LUK]  := StrToInt(SQLQueryResult.FieldValue(15));
		fMaxHP           := StrToInt(SQLQueryResult.FieldValue(16));
		fHP              := StrToInt(SQLQueryResult.FieldValue(17));
		fMaxSP           := StrToInt(SQLQueryResult.FieldValue(18));
		fSP              := StrToInt(SQLQueryResult.FieldValue(19));
		fStatusPts       := StrToInt(SQLQueryResult.FieldValue(20));
		fSkillPts        := StrToInt(SQLQueryResult.FieldValue(21));
		fOption          := StrToInt(SQLQueryResult.FieldValue(22));
		fKarma           := StrToInt(SQLQueryResult.FieldValue(23));
		fManner          := StrToInt(SQLQueryResult.FieldValue(24));
		fPartyID         := StrToInt(SQLQueryResult.FieldValue(25));
		fGuildID         := StrToInt(SQLQueryResult.FieldValue(26));
		fPetID           := StrToInt(SQLQueryResult.FieldValue(27));
		fHair            := StrToInt(SQLQueryResult.FieldValue(28));
		fHairColor       := StrToInt(SQLQueryResult.FieldValue(29));
		fClothesColor    := StrToInt(SQLQueryResult.FieldValue(30));
		fWeapon          := StrToInt(SQLQueryResult.FieldValue(31));
		fShield          := StrToInt(SQLQueryResult.FieldValue(32));
		fHeadTop         := StrToInt(SQLQueryResult.FieldValue(33));
		fHeadMid         := StrToInt(SQLQueryResult.FieldValue(34));
		fHeadBottom      := StrToInt(SQLQueryResult.FieldValue(35));
		fMap             :=          SQLQueryResult.FieldValue(36) ;
		fMapPt.X         := StrToInt(SQLQueryResult.FieldValue(37));
		fMapPt.Y         := StrToInt(SQLQueryResult.FieldValue(38));
		fSaveMap         :=          SQLQueryResult.FieldValue(39) ;
		fSaveMapPt.X     := StrToInt(SQLQueryResult.FieldValue(40));
		fSaveMapPt.Y     := StrToInt(SQLQueryResult.FieldValue(41));
		fPartnerID       := StrToInt(SQLQueryResult.FieldValue(42));
		fParentID1       := StrToInt(SQLQueryResult.FieldValue(43));
		fParentID2       := StrToInt(SQLQueryResult.FieldValue(44));
		fBabyID          := StrToInt(SQLQueryResult.FieldValue(45));
		fOnline          := StrToInt(SQLQueryResult.FieldValue(46));
		fHomunID         := StrToInt(SQLQueryResult.FieldValue(47));
	end;
  FreeAndNil(ADatabase);
end;

function TCharacter.RemoveFromSQL : boolean;
begin
	SQLConnection.query(
		Format('DELETE FROM `char` WHERE char_id=%d',[CID]),
	FALSE,Result);
end;

function GetCharacters(CharID : Cardinal) : TCharacter;
begin
	Result := NIL;
	if CharacterList.IndexOf(CharID) > -1 then
	begin
		if TCharacter(CharacterList.IndexOfObject(CharID)).CID = CharID then
		begin
			Result := TCharacter(CharacterList.IndexOfObject(CharID));
			Exit;
		end;
	end;
	if Result = NIL then
	begin
		Result := TCharacter.Create;
		if Result.LoadFromSQL(CharID) then
		begin
			CharacterList.AddObject(CharID,Result);
		end;
	end else Result := NIL;
end;

end.
