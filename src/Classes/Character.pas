//------------------------------------------------------------------------------
//Character                                                                UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Holds our TCharacter Class
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
//TCharacter                                                          PROCEDURE
//------------------------------------------------------------------------------
	type TCharacter = class
	private
		fCharacterNumber  : Byte;
		fName             : String;
		fJID              : Word;
    fJOB              : String;
		fBaseLV           : Byte;
		fJobLV            : Byte;
		fBaseEXP          : Cardinal;
		fJobEXP           : Cardinal;
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
		fRightHand        : Word;
		fLeftHand         : Word;
    fArmor            : Word;
    fGarment          : Word;
    fShoes            : Word;
    fAccessory1       : Word;
    fAccessory2       : Word;
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

		fJobName          : string;

	protected
		procedure SetSaveTime(Value : boolean);

		procedure SetCharaNum(Value : byte);
		procedure SetName(Value : string);
		procedure SetClass(Value : word);
		procedure SetBaseLV(Value : byte);
		procedure SetJobLV(Value : byte);
		procedure SetBaseEXP(Value : Cardinal);
		procedure SetJobEXP(Value : Cardinal);
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
		Procedure SetRightHand(Value : word);
		Procedure SetLeftHand(Value : word);
    Procedure SetArmor(Value : Word);
    Procedure SetGarment(Value : Word);
    Procedure SetShoes(Value : Word);
    Procedure SetAccessory1(Value : Word);
    Procedure SetAccessory2(Value : Word);
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
		Direction : byte;

		BaseNextEXP  : Cardinal;
		JobNextEXP   : Cardinal;
		Weight       : Cardinal;
		MaxWeight    : Cardinal;

		ParamUP : array [STR..LUK] of byte;
		ParamBonus : array [STR..LUK] of byte;

		MATK2 : word;
		MATK1 : word;
		DEF1 : word;
		DEF2 : word;
		MDEF1 : word;
		MDEF2 : word;
		HIT : word;
		FLEE1 : word;
		Lucky : word;
		Critical : word;
		ASpeed : word;

		ClientVersion : Integer;
		ClientInfo : TIdContext;

		property DataChanged : boolean  read fDataChanged write SetSaveTime;
		//For timed save procedure to activate.

		property CharaNum  : Byte       read fCharacterNumber write SetCharaNum;
		property Name      : string     read fName write SetName;
		property JID       : Word       read fJID write SetClass;
		property BaseLV    : Byte       read fBaseLV write SetBaseLV;
		property JobLV     : Byte       read fJobLV write SetJobLV;
		property BaseEXP   : Cardinal   read fBaseEXP write SetBaseEXP;
		property JobEXP    : Cardinal   read fJobEXP write SetJobEXP;
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
		property RightHand : Word       read fRightHand write SetRightHand;
		property LeftHand  : Word       read fLeftHand write SetLeftHand;
    property Armor     : Word       read fArmor write SetArmor;
    property Garment   : Word       read fGarment write SetGarment;
    property Shoes     : Word       read fShoes write SetShoes;
    property Accessory1: Word       read fAccessory1 write SetAccessory1;
    property Accessory2: Word       read fAccessory1 write SetAccessory2;
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

		property JobName   : string     read fJobName;

		procedure CalcMaxHP;
    procedure CalcMaxSP;
    procedure CalcSpeed;

	end;{TCharacter}
//------------------------------------------------------------------------------

implementation
uses
	//IDE
	SysUtils,
  Math,
	//Helios
	BufferIO,
	Globals,
	PacketTypes
	;

//------------------------------------------------------------------------------
//SetSaveTime                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the last time the character was saved to the database.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetSaveTime(Value : boolean);
begin
	if Value and not fDataChanged then begin
		fDataChanged  := TRUE;
		fTimeToSave   := IncMinute(Now,5);
	end;
end;{SetSaveTime}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetCharaNum                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the CharaNum to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetCharaNum(Value : byte);
begin
	DataChanged       := TRUE;
	fCharacterNumber  := Value;
end;{SetCharaNum}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetName                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Name to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetName(Value : string);
begin
	DataChanged := TRUE;
	fName       := Value;
end;{SetName}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetClass                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Class to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetClass(Value : Word);
begin
	DataChanged := TRUE;
	fJID        := Value;

	case fJID of
		JOB_NOVICE     :  fJobName := 'Novice';
		JOB_SWORDSMAN  :  fJobName := 'Swordsman';
		JOB_MAGE       :  fJobName := 'Magician';
		JOB_ARCHER     :  fJobName := 'Archer';
		JOB_ACOLYTE    :  fJobName := 'Acolyte';
		JOB_MERCHANT   :  fJobName := 'Merchant';
		JOB_THIEF      :  fJobName := 'Thief';

		JOB_KNIGHT     :  fJobName := 'Knight';
		JOB_PRIEST     :  fJobName := 'Priest';
		JOB_WIZARD     :  fJobName := 'Wizard';
		JOB_BLACKSMITH :  fJobName := 'Blacksmith';
		JOB_HUNTER     :  fJobName := 'Hunter';
		JOB_ASSASSIN   :  fJobName := 'Assassin';

		JOB_CRUSADER   :  fJobName := 'Crusader';
		JOB_MONK       :  fJobName := 'Monk';
		JOB_SAGE       :  fJobName := 'Sage';
		JOB_ROGUE      :  fJobName := 'Rogue';
		JOB_ALCHEMIST  :  fJobName := 'Alchemist';
		JOB_BARD       :  fJobName := 'Bard';
		JOB_DANCER     :  fJobName := 'Dancer';

		JOB_SNOVICE    :  fJobName := 'Super_Novice';

		JOB_GUNSLINGER :  fJobName := 'Gunslinger';
		JOB_NINJA      :  fJobName := 'Ninja';

		HJOB_HIGH_NOVICE     : fJobName := 'High_Novice';
		HJOB_HIGH_SWORDSMAN  : fJobName := 'High_Swordsman';
		HJOB_HIGH_MAGE       : fJobName := 'High_Magician';
		HJOB_HIGH_ARCHER     : fJobName := 'High_Archer';
		HJOB_HIGH_ACOLYTE    : fJobName := 'High_Acolyte';
		HJOB_HIGH_MERCHANT   : fJobName := 'High_Merchant';
		HJOB_HIGH_THIEF      : fJobName := 'High_Thief';

		HJOB_LORD_KNIGHT     : fJobName := 'Lord_Knight';
		HJOB_HIGH_PRIEST     : fJobName := 'High_Priest';
		HJOB_HIGH_WIZARD     : fJobName := 'High_Wizard';
		HJOB_WHITESMITH      : fJobName := 'Whitesmith';
		HJOB_SNIPER          : fJobName := 'Sniper';
		HJOB_ASSASSIN_CROSS  : fJobName := 'Assassin_Cross';

		HJOB_PALADIN         : fJobName := 'Paladin';
		HJOB_CHAMPION        : fJobName := 'Champion';
		HJOB_PROFESSOR       : fJobName := 'Scholar';
		HJOB_STALKER         : fJobName := 'Stalker';
		HJOB_CREATOR         : fJobName := 'Biochemist';
		HJOB_CLOWN           : fJobName := 'Clown';
		HJOB_GYPSY           : fJobName := 'Gypsy';

		HJOB_BABY             : fJobName := 'Baby_Novice';
		HJOB_BABY_SWORDSMAN   : fJobName := 'Baby_Swordsman';
		HJOB_BABY_MAGE        : fJobName := 'Baby_Magician';
		HJOB_BABY_ARCHER      : fJobName := 'Baby_Archer';
		HJOB_BABY_ACOLYTE     : fJobName := 'Baby_Acolyte';
		HJOB_BABY_MERCHANT    : fJobName := 'Baby_Merchant';
		HJOB_BABY_THIEF       : fJobName := 'Baby_Thief';

		HJOB_BABY_KNIGHT      : fJobName := 'Baby_Knight';
		HJOB_BABY_PRIEST      : fJobName := 'Baby_Priest';
		HJOB_BABY_WIZARD      : fJobName := 'Baby_Wizard';
		HJOB_BABY_BLACKSMITH  : fJobName := 'Baby_Blacksmith';
		HJOB_BABY_HUNTER      : fJobName := 'Baby_Hunter';
		HJOB_BABY_ASSASSIN    : fJobName := 'Baby_Assassin';

		HJOB_BABY_CRUSADER    : fJobName := 'Baby_Crusader';
		HJOB_BABY_MONK        : fJobName := 'Baby_Monk';
		HJOB_BABY_SAGE        : fJobName := 'Baby_Sage';
		HJOB_BABY_ROGUE       : fJobName := 'Baby_Rogue';
		HJOB_BABY_ALCHEMIST   : fJobName := 'Baby_Alchemist';
		HJOB_BABY_BARD        : fJobName := 'Baby_Bard';
		HJOB_BABY_DANCER      : fJobName := 'Baby_Dancer';

		HJOB_BABY_SNOVICE     : fJobName := 'Baby_Super_Novice';

		HJOB_EXPANDED_TAEKWON              : fJobName := 'Taekwon';
		HJOB_EXPANDED_STAR_GLADIATOR       : fJobName := 'Star_Gladiator';
		HJOB_EXPANDED_STAR_GLADIATOR_2     : fJobName := 'Star_Gladiator)';
		HJOB_EXPANDED_SOUL_LINKER          : fJobName := 'Soul_Linker';
	end;

end;{SetClass}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetBaseLV                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the BaseLV  to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetBaseLV(Value : byte);
begin
	DataChanged := TRUE;
	fBaseLV     := Value;
end;{SetBaseLV}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetJobLV                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Name to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetJobLV(Value : byte);
begin
	DataChanged := TRUE;
	fJobLV      := Value;
end;{SetJobLV}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetBaseEXP                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the BaseEXP to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetBaseEXP(Value : Cardinal);
begin
	DataChanged := TRUE;
	fBaseEXP    := Value;
end;{SetBaseEXP}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetJobEXP                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets JobEXP to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetJobEXP(Value : Cardinal);
begin
	DataChanged := TRUE;
	fJobEXP     := Value;
end;{SetJobEXP}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetZeny                                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Zeny to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetZeny(Value : Cardinal);
begin
	DataChanged := TRUE;
	fZeny       := Value;
end;{SetZeny}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetBaseStats                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Gets the base stat at Index.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function TCharacter.GetBaseStats(Index : Byte) : Byte;
begin
	Result := fParamBase[Index];
end;{GetBaseStats}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetBaseStats                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets Base Stat at Index to Value.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetBaseStats(Index, Value: Byte);
begin
	DataChanged       := TRUE;
	fParamBase[Index] := Value;
end;{SetBaseStats}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetMaxHP                                                            PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the MaxHP to Value.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetMaxHP(Value : word);
begin
	DataChanged := TRUE;
	fMaxHP      := Value;
end;{SetMaxHP}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//SetHP                                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the HP to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetHP(Value : word);
begin
	DataChanged := TRUE;
	fHP         := Value;
end;{SetHP}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetMaxSP                                                            PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the MaxSP to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetMaxSP(Value : word);
begin
	DataChanged := TRUE;
	fMaxSP      := Value;
end;{SetMaxSP}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetName                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the SP to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetSP(Value : word);
begin
	DataChanged := TRUE;
	fSP         := Value;
end;{SetSP}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetStatusPts                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the StatusPoints to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetStatusPts(Value : Word);
begin
	DataChanged := TRUE;
	fStatusPts  := Value;
end;{SetStatusPts}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetSkillPts                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the SkillPoints to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetSkillPts(Value : Word);
begin
	DataChanged := TRUE;
	fSkillPts   := Value;
end;{SetSkillPts}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetOption                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Option to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetOption(Value : word);
begin
	DataChanged := TRUE;
	fOption     := Value;
end;{SetOption}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetKarma                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Karma to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetKarma(Value : word);
begin
	DataChanged := TRUE;
	fKarma      := Value;
end;{SetName}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetManner                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Manner to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetManner(Value : word);
begin
	DataChanged := TRUE;
	fManner     := Value;
end;{SetManner}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetPartyID                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the PartyID to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetPartyID(Value : cardinal);
begin
	DataChanged := TRUE;
	fPartyID    := Value;
end;{SetPartyID}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetGuildID                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the GuildID to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetGuildID(Value : cardinal);
begin
	DataChanged := TRUE;
	fGuildID    := Value;
end;{SetGuildID}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetPetID                                                            PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the PetID to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetPetID(Value : cardinal);
begin
	DataChanged := TRUE;
	fPetID      := Value;
end;{SetPetID}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetHair                                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Hair to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetHair(Value : word);
begin
	DataChanged := TRUE;
	fHair       := Value;
end;{SetHair}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetHairColor                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the HairColor to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetHairColor(Value : word);
begin
	DataChanged := TRUE;
	fHairColor  := Value;
end;{SetHairColor}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetClothesColor                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the ClothesColor to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetClothesColor(Value : word);
begin
	DataChanged   := TRUE;
	fClothesColor := Value;
end;{SetClothesColor}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetRightHand                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Right Hand to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetRightHand(Value : word);
begin
	DataChanged := TRUE;
	fRightHand  := Value;
end;{SetWeapon}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetLeftHand                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Shield to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetLeftHand(Value : word);
begin
	DataChanged := TRUE;
	fLeftHand   := Value;
end;{SetShield}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetArmor                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Armor to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		January 15th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetArmor(Value : word);
begin
	DataChanged := TRUE;
	fArmor   := Value;
end;{SetArmor}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetGarment                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Garment to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		January 15th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetGarment(Value : word);
begin
	DataChanged := TRUE;
	fGarment   := Value;
end;{SetGarment}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetShoes                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Shoes to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		January 15th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetShoes(Value : word);
begin
	DataChanged := TRUE;
	fShoes   := Value;
end;{SetShoes}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetAccessory1                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Accessory1 to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		January 15th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetAccessory1(Value : word);
begin
	DataChanged := TRUE;
	fAccessory1   := Value;
end;{SetAccessory1}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetAccessory2                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Accessory2 to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		January 15th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetAccessory2(Value : word);
begin
	DataChanged := TRUE;
	fAccessory2   := Value;
end;{SetAccessory2}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetHeadTop                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the HeadTop to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetHeadTop(Value : word);
begin
	DataChanged := TRUE;
	fHeadTop    := Value;
end;{SetHeadTop}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetHeadMid                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the HeadMid to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetHeadMid(Value : word);
begin
	DataChanged := TRUE;
	fHeadMid    := Value;
end;{SetHeadMid}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetHeadBottom                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the HeadBottom to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetHeadBottom(Value : word);
begin
	DataChanged := TRUE;
	fHeadBottom := Value;
end;{SetHeadBottom}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetMap                                                              PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Map to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetMap(Value : string);
begin
	DataChanged := TRUE;
	fMap        := Value;
end;{SetMap}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetMapPt                                                            PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the MapPt to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetMapPt(Value : TPoint);
begin
	DataChanged := TRUE;
	fMapPt      := Value;
end;{SetMapPt}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetSMap                                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the SMap(SaveMap) to Value. Also, lets our object know that data
//    has changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetSMap(Value : string);
begin
	DataChanged := TRUE;
	fSaveMap    := Value;
end;{SetSMap}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetSMapPt                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the SMapPt to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetSMapPt(Value : TPoint);
begin
	DataChanged := TRUE;
	fSaveMapPt  := Value;
end;{SetSMapPt}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetPartnerID                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the PartnerID to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetPartnerID(Value : Cardinal);
begin
	DataChanged := TRUE;
	fPartnerID  := Value;
end;{SetPartnerID}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetParentID                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the ParentID to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetParentID1(Value : Cardinal);
begin
	DataChanged := TRUE;
	fParentID1  := Value;
end;{SetParentID}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetParentID2                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the ParentID2 to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetParentID2(Value : Cardinal);
begin
	DataChanged := TRUE;
	fParentID2  := Value;
end;{SetParentID2}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetBabyID                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the BabyID to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetBabyID(Value : Cardinal);
begin
	DataChanged := TRUE;
	fBabyID     := Value;
end;{SetBabyID}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetOnline                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the Online to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetOnline(Value : Byte);
begin
	DataChanged := TRUE;
	fOnline     := Value;
end;{SetOnline}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetHomunID                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the HomunID to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetHomunID(Value : Cardinal);
begin
	DataChanged := TRUE;
	fHomunID    := Value;
end;{SetHomunID}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//CalcMaxHP                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Calculates the character's Maximum HP.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.CalcMaxHP;
begin
  MAXHP := EnsureRange(
    (MAXHP + (35 + BaseLV * 5 + ((1 + BaseLV) * BaseLV div 2) *
      ADatabase.StaticData.GetBaseHP(self) div 100) * (100 + ParamBase[VIT]) div 100)
      ,1
      ,High(MAXHP)
 		);
end;{CalcMaxHP}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//CalcMaxSP                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Calculates the character's Maximum SP.
//
//	Changes -
//		January 17th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TCharacter.CalcMaxSP;
begin
  MAXSP := MAXSP + BaseLV * ADatabase.StaticData.GetBaseSP(self) * (100 + ParamBase[INT]) div 100;
end;{CalcMaxSP}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//CalcSpeed                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Calculates the character's Speed.
//
//	Changes -
//		January 17th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TCharacter.CalcSpeed;
begin
  Speed := 150;
end;{CalcSpeed}
//------------------------------------------------------------------------------


end.
