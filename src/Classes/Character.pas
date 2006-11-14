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
	GameConstants,
	//Third Party
	IdContext;

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

	protected
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
		ClientInfo : TIdContext;

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

		procedure CalcMaxHP;

	end;

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

procedure TCharacter.CalcMaxHP;
begin
end;

end.
