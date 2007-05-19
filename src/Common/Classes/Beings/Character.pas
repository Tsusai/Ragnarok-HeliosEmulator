//------------------------------------------------------------------------------
//Character                                                                UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Holds our TCharacter Class
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//		[2007/03/28] CR - Cleaned up uses clauses with help of Icarus.
//		[2007/04/23] Tsusai - Changed lua filename
//
//------------------------------------------------------------------------------
unit Character;


interface


uses
	{RTL/VCL}
	Types,
	{Project}
	Being,
	GameConstants,
	LuaCoreRoutines,
	{Third Party}
	IdContext,
	List32
	;


type


TCharaScriptStatus = (
		SCRIPT_PAUSED,
		SCRIPT_RUNNING,
		SCRIPT_NOTRUNNING
	);

TCharaState = (
		charaDead,
		charaPlayDead,
		charaSitting,
		charaStanding,
		charaWalking
	);


(*= CLASS =====================================================================*
TCharacter

*------------------------------------------------------------------------------*
Overview:
*------------------------------------------------------------------------------*

	This class represents a Character.  It descends from TBeing, and handles all
Character specific properties not already defined and handled in TBeing.

	Parts of TCharacter that may look like reduplication are actually not:
since we must flag when data is altered, some of the TBeing Set* methods for
properties have to be overriden to properly flag when the object's state has
changed, and thus, must be saved.

*------------------------------------------------------------------------------*
Revisions:
*------------------------------------------------------------------------------*
(Format: [yyyy/mm/dd] <Author> - <Description of Change>)
[2007/04/28] CR - Modified Class header.  Eliminated private section - all
	internal fields used for properties are now protected, instead.  Removed
	GetBaseStat property method (unneeded).  Altered SetBaseStat to model the
	changes I made in TBeing last commit.  Eliminated public variables that were
	IDENTICAL to those in TBeing (to avoid a repeat of problems like we had with
	fZeny in an earlier commit).  Changed ParamUP and ParamBonus so they use the
	ByteStatArray type like TBeing uses for ParamBase.
*=============================================================================*)
TCharacter = class(TBeing)
protected
	fCharacterNumber  : Byte;
	fStatusPts        : Word;
	fSkillPts         : Word;
	fKarma            : Word;
	fManner           : Word;
	fPartyID          : LongWord;
	fGuildID          : LongWord;
	fPetID            : LongWord;
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
	fSaveMap          : String;
	fSaveMapPt        : TPoint;
	fPartnerID        : LongWord;
	fParentID1        : LongWord;
	fParentID2        : LongWord;
	fBabyID           : LongWord;
	fOnline           : Byte;
	fHomunID          : LongWord;

	fDataChanged      : Boolean; //For timed save procedure to activate.
	fTimeToSave       : TDateTime;

	fJobName          : String;
	fCharaState       : TCharaState;

	procedure SetSaveTime(Value : Boolean);
	procedure SetCharaState(Value : TCharaState);
	procedure SetCharaNum(Value : Byte);

	Procedure SetName(
		const
			Value : String
		); override;

	procedure SetClass(Value : Word); override;
	procedure SetBaseLV(Value : Byte); override;
	procedure SetJobLV(Value : Byte); override;
	procedure SetBaseEXP(Value : LongWord); override;
	procedure SetJobEXP(Value : LongWord); override;
	procedure SetZeny(Value : LongWord); override;

	Procedure SetBaseStats(
		const
			Index: Byte;
		const
			Value: Byte
		); override;

	procedure SetMaxHP(Value : Word); override;
	procedure SetHP(Value : Word); override;
	procedure SetMaxSP(Value : Word); override;
	procedure SetSP(Value : Word); override;
	Procedure SetOption(Value : Word); override;
	procedure SetMap(Value : String); override;
	procedure SetPosition(Value : TPoint); override;

	Procedure SetKarma(Value : Word);
	Procedure SetManner(Value : Word);
	Procedure SetPartyID(Value : LongWord);
	Procedure SetGuildID(Value : LongWord);
	Procedure SetPetID(Value : LongWord);
	Procedure SetHair(Value : Word);
	Procedure SetHairColor(Value : Word);
	Procedure SetClothesColor(Value : Word);
	Procedure SetRightHand(Value : Word);
	Procedure SetLeftHand(Value : Word);
	Procedure SetArmor(Value : Word);
	Procedure SetGarment(Value : Word);
	Procedure SetShoes(Value : Word);
	Procedure SetAccessory1(Value : Word);
	Procedure SetAccessory2(Value : Word);
	Procedure SetHeadTop(Value : Word);
	Procedure SetHeadMid(Value : Word);
	Procedure SetHeadBottom(Value : Word);
	procedure SetStatusPts(Value : Word);
	procedure SetSkillPts(Value : Word);
	procedure SetSMap(Value : String);
	procedure SetSMapPt(Value : TPoint);
	procedure SetPartnerID(Value : LongWord);
	procedure SetParentID1(Value : LongWord);
	procedure SetParentID2(Value : LongWord);

	Procedure SetBabyID(
		const
			Value : LongWord
		);

	Procedure SetOnline(
		const
			Value : Byte
		);

	Procedure SetHomunID(
		const
			Value : LongWord
		);

public
	CID : LongWord;

	DcAndKeepData : Boolean;

	LuaInfo : TLuaInfo; //personal lua "thread"
	ScriptStatus : TCharaScriptStatus; //lets us know what is going on with lua,
	// if its paused or running

	BaseNextEXP  : LongWord;
	JobNextEXP   : LongWord;
	Weight       : LongWord;
	MaxWeight    : LongWord;

	ClientInfo   : TIdContext;

	ParamUP      : ByteStatArray;
	ParamBonus   : ByteStatArray;

	//Stat Calculations should fill these in
	//Maybe a record type for this crap for shared info between mobs and chars
	//Hell...maybe..properties? o_O

	AttackRange : Word;
	//No idea what 0..5 is from.  Stats?
	ATK : array[R_HAND..L_HAND] of array[0..5] of Word; // Displayed ATK power

	ClientVersion : Integer;

	OnTouchIDs : TIntList32;


	procedure CalcMaxHP; override;
	procedure CalcMaxSP; override;
	procedure CalcSpeed; override;
	procedure CalcMaxWeight;

	Procedure SendSubStat(
		const
			Mode     : Word;
		const
			DataType : Word;
		const
			Value    : LongWord
		);

	procedure SendCharacterStats(UpdateView : boolean = false);

	Constructor Create(AClient : TIdContext);
	Destructor  Destroy; override;


	property DataChanged : Boolean
		read  fDataChanged
		write SetSaveTime;

	property CharaState  : TCharaState read fCharaState write SetCharaState;
	//For timed save procedure to activate.
	property BaseEXP   : LongWord    read fBaseEXP write SetBaseEXP;
	property JobEXP    : LongWord    read fJobEXP write SetJobEXP;
	property Zeny      : LongWord    read fZeny write SetZeny;
	property CharaNum  : Byte       read fCharacterNumber write SetCharaNum;
	property StatusPts : Word       read fStatusPts write SetStatusPts;
	property SkillPts  : Word       read fSkillPts write SetSkillPts;
	property Karma     : Word       read fKarma write SetKarma;
	property Manner    : Word       read fManner write SetManner;
	property PartyID   : LongWord    read fPartyID write SetPartyID;
	property GuildID   : LongWord    read fGuildID write SetGuildID;
	property PetID     : LongWord    read fPetID write SetPetID;
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
	property SaveMap   : String
		read  fSaveMap
		write SetSMap;
	property SavePoint : TPoint     read fSaveMapPt write SetSMapPt;
	property PartnerID : LongWord    read fPartnerID write SetPartnerID;
	property ParentID1 : LongWord    read fParentID1 write SetParentID1;
	property ParentID2 : LongWord    read fParentID2 write SetParentID2;
	property BabyID    : LongWord    read fBabyID write SetBabyID;
	property Online    : Byte       read fOnline write SetOnline;
	property HomunID   : LongWord    read fHomunID write SetHomunID;

	Property JobName   : String
		read  fJobName;
End;(* TCharacter
*== CLASS ====================================================================*)


implementation


uses
	{RTL/VCL}
	Math,
	SysUtils,
	{Project}
	BufferIO,
	Globals,
	PacketTypes,
	TCPServerRoutines
	{Third Party}
	//none
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
procedure TCharacter.SetSaveTime(Value : Boolean);
begin
	if Value and not fDataChanged then
	begin
		fDataChanged  := TRUE;
		fTimeToSave   := IncMinute(Now,5);
	end;
end;{SetSaveTime}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetCharaState                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets Character State
//
//	Changes -
//		March 12th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
procedure TCharacter.SetCharaState(Value : TCharaState);
begin
	//Need to add easy to get to codes (STANCE_MOVE from prometheus)
	//usually used for packets
	fCharaState := Value;
end;
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


(*- Procedure -----------------------------------------------------------------*
TCharacter.SetName
--------------------------------------------------------------------------------
Overview:
--
	Sets the Name to Value.
	Also, lets our object know that data has changed.

--
Pre:
	TODO
Post:
	TODO

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2006/22/06] RaX - Created Header.
[2007/04/28] CR - Changed Header, made parameter constant (efficient use of
	strings).
*-----------------------------------------------------------------------------*)
Procedure TCharacter.SetName(
	const
		Value : String
	);
Begin
	DataChanged := TRUE;
	fName := Value;
End; (* Proc TCharacter.SetName
*-----------------------------------------------------------------------------*)


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
	fJID := Value;

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
		HJOB_EXPANDED_STAR_GLADIATOR_2     : fJobName := 'Star_Gladiator';
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
	Inherited;
	DataChanged := TRUE;
end;{SetBaseLV}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetJobLV                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the JobLV to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//		March 12th, 2007 - Aeomin - Fix Header Typo
//
//------------------------------------------------------------------------------
procedure TCharacter.SetJobLV(Value : byte);
begin
	Inherited;
	DataChanged := TRUE;
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
procedure TCharacter.SetBaseEXP(Value : LongWord);
begin
	Inherited;
	DataChanged := TRUE;
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
procedure TCharacter.SetJobEXP(Value : LongWord);
begin
	Inherited;
	DataChanged := TRUE;
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
procedure TCharacter.SetZeny(Value : LongWord);
begin
	Inherited;
	DataChanged := TRUE;
end;{SetZeny}
//------------------------------------------------------------------------------


(*- Procedure -----------------------------------------------------------------*
TCharacter.SetBaseStats
--------------------------------------------------------------------------------
Overview:
--

	Sets Base Stat at Index to Value.

	Passes off to the ancestor routine which does range checking via Asserts,
	and then flags DataChanged for saving the Character Data when that event is
	next triggered.

--
Pre:
	Index must be between STR..LUK (Checked by the inherited method)
Post:
	DataChanged is True

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2006/12/22] RaX - Created Header.
[2007/04/28] CR - Altered Comment Header, improved description, noted Pre and
	Post conditions.  Altered parameters to match TBeing.
*-----------------------------------------------------------------------------*)
Procedure TCharacter.SetBaseStats(
	const
		Index : Byte;
	const
		Value : Byte
	);
Begin
	Inherited;
	DataChanged := TRUE;
End; (* Proc TCharacter.SetBaseStats
*-----------------------------------------------------------------------------*)

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
	Inherited;
	DataChanged := TRUE;
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
	Inherited;
	DataChanged := TRUE;
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
	Inherited;
	DataChanged := TRUE;
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
	Inherited;
	DataChanged := TRUE;
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
	Inherited;
	DataChanged := TRUE;
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
procedure TCharacter.SetPartyID(Value : LongWord);
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
procedure TCharacter.SetGuildID(Value : LongWord);
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
procedure TCharacter.SetPetID(Value : LongWord);
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
	Inherited;
	DataChanged := TRUE;
end;{SetMap}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetPosition                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the MapPt to Value. Also, lets our object know that data has
//    changed.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharacter.SetPosition(Value : TPoint);
begin
	Inherited;
	DataChanged := TRUE;
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
	fSaveMap := Value;
	DataChanged := TRUE;
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
	fSaveMapPt := Value;
	DataChanged := TRUE;
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
procedure TCharacter.SetPartnerID(Value : LongWord);
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
procedure TCharacter.SetParentID1(Value : LongWord);
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
procedure TCharacter.SetParentID2(Value : LongWord);
begin
	DataChanged := TRUE;
	fParentID2  := Value;
end;{SetParentID2}
//------------------------------------------------------------------------------


(*- Procedure -----------------------------------------------------------------*
TCharacter.SetBabyID
--------------------------------------------------------------------------------
Overview:
--

	Sets the Online flag to Value, and flags DataChanged, if and only if Value
differs.

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2006/12/22] RaX - Created Header.
[2007/05/06] CR - Altered routine and description, to minimize data saves.
	Value parameter made constant.
*-----------------------------------------------------------------------------*)
Procedure TCharacter.SetBabyID(
	const
		Value : LongWord
	);
Begin
	if (Value <> fBabyID) then
	begin
		DataChanged := TRUE;
		fBabyID     := Value;
	end;
End; (* Proc TCharacter.SetBabyID
*-----------------------------------------------------------------------------*)


(*- Procedure -----------------------------------------------------------------*
TCharacter.SetOnline
--------------------------------------------------------------------------------
Overview:
--

	Sets the Online flag to Value, and flags DataChanged, if and only if Value
differs.

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2006/12/22] RaX - Created Header.
[2007/05/06] CR - Altered routine and description, to minimize data saves.
	Value parameter made constant.
*-----------------------------------------------------------------------------*)
Procedure TCharacter.SetOnline(
	const
		Value : Byte
	);
Begin
	if (Value <> fOnline) then
	begin
		DataChanged := TRUE;
		fOnline     := Value;
	end;
End; (* Proc TCharacter.SetOnline
*-----------------------------------------------------------------------------*)


(*- Procedure -----------------------------------------------------------------*
TCharacter.SetHomunID
--------------------------------------------------------------------------------
Overview:
--

	Sets the HomunID to Value and flags DataChanged, if and only if Value is
different.

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2006/12/22] RaX - Created Header.
[2007/05/06] CR - Make Set only change if the Value is different. Altered
	comment header.
*-----------------------------------------------------------------------------*)
Procedure TCharacter.SetHomunID(
	const
		Value : LongWord
	);
Begin
	if (Value <> fHomunID) then
	begin
		DataChanged := TRUE;
		fHomunID    := Value;
	end;
End; (* Proc TCharacter.SetHomunID
*-----------------------------------------------------------------------------*)


(*- Procedure -----------------------------------------------------------------*
TCharacter.CalcMaxHP
--------------------------------------------------------------------------------
Overview:
--

	Calculates the character's Maximum HP.

	[2007/04/28] CR - This routine appears to be used as an initialization.
	Repeatedly calling this routine looks like it will be wasteful.  AND...
	This routine IS repeatedly called.

	Nota Bene:  MaxHP values do not depend on the HP the character currently
	has AT ALL!
	However...  MaxHP DOES depend on the following properties:
	JobName/JID, (via the GetBaseMaxHP call)
	BaseLV,
	VIT

--
Pre:
	TODO
Post:
	TODO

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2006/12/22] RaX - Created Header.
[2007/04/28] CR - Altered Comment Header, added further description of the
	routine.  Used an internal variable to simplify/shorten the span where the
	Database connection is open to retrieve the BaseMaxHP value.  Reindented the
	formula for initializing MaxHP.  Boy is it clearer to read without that long
	abomination of a method name to retrieve "GetBaseMaxHP"! :P~~
*-----------------------------------------------------------------------------*)
Procedure TCharacter.CalcMaxHP;
Var
	BaseMaxHP : Word;
Begin
	TThreadLink(ClientInfo.Data).DatabaseLink.StaticData.Connect;
	try
		BaseMaxHP :=
			TThreadLink(ClientInfo.Data).DatabaseLink.StaticData.GetBaseMaxHP(Self);
	finally
		TThreadLink(ClientInfo.Data).DatabaseLink.StaticData.Disconnect;
	end;

	fMaxHP := EnsureRange(
		(
			(35 + BaseLV * 5 + (
				(1 + BaseLV) * BaseLV div 2) *
				BaseMaxHP div 100
			) * (100 + ParamBase[VIT]) div 100
		),
		1,
		High(fMaxHP)
	);

	if (fHP > fMaxHP) then
	begin
		fHP := fMaxHP;
	end;
End; (* Proc TCharacter.CalcMaxHP
*-----------------------------------------------------------------------------*)


(*-----------------------------------------------------------------------------*
Proc TCharacter.SendSubStat

--
Overview:
--
Send sub state defined by Mode(speed, Def,MDef etc...),
send party info, and Recalculate Weight

Parameters:
Mode: either 0 or 1, since its $00b0 or $00b1 (Its $00b0 + Mode)
[2007/03/19] CR - Should Mode be a WordBool instead, if it's only 0 or 1?


--
Pre:
	TODO
Post:
	TODO

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment> )
[2007/03/12] Aeomin - Added Comment Header
[2007/03/24] CR - Parameters passed are not altered, thus all parameters are
	now explicitly constant.
[2007/03/24] CR - Moved first section of this routine into local procedure
	Send_00b0.  Moved the non-implemented blocks into local routines as well.
	Made all parameters constant, which self-documents these, and makes calling
	the routine more efficient.
*-----------------------------------------------------------------------------*)
Procedure TCharacter.SendSubStat(
	const
		Mode     : Word;
	const
		DataType : Word;
	const
		Value    : LongWord
	);
Var
	OutBuffer : TBuffer;


	(*- Local Procedure .................*
	Send_00b0

	--
	[2007/03/24] CR - Extracted verbatim
		from main body.
	*...................................*)
	procedure Send_00b0;
	begin
		WriteBufferWord(0, $00b0 + Mode, OutBuffer);
		WriteBufferWord(2, DataType, OutBuffer);
		WriteBufferLongWord(4, Value, OutBuffer);

		if (Online <> 0) then
		begin
			SendBuffer(
				ClientInfo,
				OutBuffer,
				GetPacketLength($00b0 + Mode, ClientVersion)
			);
		end;
	end;(* Send_00b0
	*...................................*)

	(*- Local Procedure .................*
	PartyInfo

	Not yet Implemented.
	--
	[2007/03/24] CR - Extracted
		from main body.
	*...................................*)
	procedure PartyInfo;
	begin
		{[2007/03/24] CR -  already disabled, no comment about this routine}
		//Party Info from prometheus
		{
		if (tc.PartyName <> '') and (Mode = 0) and ((DType = 5) or (DType = 6)) then
		begin
			WriteBufferWord( 0, $0106);
			WriteBufferLongWord( 2, tc.ID);
			WriteBufferWord( 6, tc.HP);
			WriteBufferWord( 8, tc.MAXHP);
			SendPCmd(tc, OutBuffer, 10, True, True);
		end;
		}

	end;(* PartyInfo
	*...................................*)

	(*- Local Procedure .................*
	OverweightTest

	Not yet Implemented.
	--
	[2007/03/24] CR - Extracted
		from main body.
	*...................................*)
	procedure OverweightTest;
	{
	var
		WeightPercent  : Integer;
	}
	begin
		{[2007/03/24] CR -  already disabled, no comment about this routine}
		//Party Info from prometheus
		{
		if (tc.PartyName <> '') and (Mode = 0) and ((DType = 5) or (DType = 6)) then
		begin
			WriteBufferWord( 0, $0106);
			WriteBufferLongWord( 2, tc.ID);
			WriteBufferWord( 6, tc.HP);
			WriteBufferWord( 8, tc.MAXHP);
			SendPCmd(tc, OutBuffer, 10, True, True);
		end;
		}

	end;(* OverweightTest
	*...................................*)

Begin
	Send_00b0;

	{[2007/03/24] CR - These are "empty" - not yet implemented. }
	PartyInfo;
	OverweightTest;
End; (* Proc TCharacter.SendSubStats
*-----------------------------------------------------------------------------*)


//------------------------------------------------------------------------------
//SendCharacterStats                                                   PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Send chatacter stats, Such speed,hp(max),sp(max).. and tons of it
//
//	Changes -
//		March 12th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
procedure TCharacter.SendCharacterStats(UpdateView : boolean = false);
Var
	idx :integer;
	OutBuffer : TBuffer;
Begin
	//Speed
	SendSubStat(0, 0, Speed);
	//HPSP
	SendSubStat(0, 5, HP);
	SendSubStat(0, 6, MAXHP);

	SendSubStat(0, 7, SP);
	SendSubStat(0, 8, MAXSP);

	// Update status points and points needed to level up.
	WriteBufferWord( 0, $00bd, OutBuffer);
	WriteBufferWord( 2, Self.StatusPts, OutBuffer);
	for idx := STR to LUK do
	begin
		WriteBufferByte((idx)*2+4, ParamBase[idx], OutBuffer);
		WriteBufferByte((idx)*2+5, ParamUp[idx], OutBuffer);
	end;
	WriteBufferWord(16, ATK[0][0], OutBuffer);
	WriteBufferWord(18, ATK[1][0] + ATK[0][4], OutBuffer);
	WriteBufferWord(20, MATK2, OutBuffer);
	WriteBufferWord(22, MATK1, OutBuffer);
	WriteBufferWord(24, DEF1, OutBuffer);
	WriteBufferWord(26, DEF2, OutBuffer);
	WriteBufferWord(28, MDEF1, OutBuffer);
	WriteBufferWord(30, MDEF2, OutBuffer);
	WriteBufferWord(32, HIT, OutBuffer);
	WriteBufferWord(34, FLEE1, OutBuffer);
	WriteBufferWord(36, Lucky, OutBuffer);
	WriteBufferWord(38, Critical, OutBuffer);
	WriteBufferWord(40, ASpeed, OutBuffer);
	WriteBufferWord(42, 0, OutBuffer);
	SendBuffer(ClientInfo, OutBuffer, GetPacketLength($00bd,ClientVersion));

	// Update base XP
	SendSubStat(1, 1, BaseEXP);
	SendSubStat(1, $0016, BaseNextEXP);

	// Update job XP
	SendSubStat(1, 2, JobEXP);
	SendSubStat(1, $0017, JobNextEXP);

	// Update Zeny
	SendSubStat(1, $0014, Zeny);

	// Update weight
	SendSubStat(0, $0018, Weight);
	SendSubStat(0, $0019, MaxWeight);

	// Send status points.
	for idx := 0 to 5 do
	begin
		WriteBufferWord( 0, $0141, OutBuffer);
		WriteBufferLongWord( 2, 13+idx, OutBuffer);
		WriteBufferLongWord( 6, ParamBase[idx+1], OutBuffer);
		WriteBufferLongWord(10, ParamBonus[idx+1], OutBuffer);
		SendBuffer(ClientInfo, OutBuffer, GetPacketLength($0141,ClientVersion));
	end;
	// Send attack range.
	WriteBufferWord(0, $013a, OutBuffer);
	WriteBufferWord(2, AttackRange, OutBuffer);
	SendBuffer(ClientInfo, OutBuffer, GetPacketLength($013a,ClientVersion));

	// Update the character's view packets if necessary.
	if UpdateView then
	begin
		{tc.UpdateLook(3, tc.Head3, 0, True);
		tc.UpdateLook(4, tc.Head1, 0, True);
		tc.UpdateLook(5, tc.Head2, 0, True);
		if (tc.Shield > 0) then
		begin
			tc.UpdateLook(2, tc.WeaponSprite[0], tc.Shield);
		end else begin
			tc.UpdateLook(2, tc.WeaponSprite[0], tc.WeaponSprite[1]);
		end;}
	end;
End;
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
	TThreadLink(ClientInfo.Data).DatabaseLink.StaticData.Connect;
	MAXSP := MAXSP + BaseLV * TThreadLink(ClientInfo.Data).DatabaseLink.StaticData.GetBaseMaxSP(self) * (100 + ParamBase[INT]) div 100;
	TThreadLink(ClientInfo.Data).DatabaseLink.StaticData.Disconnect;
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


//------------------------------------------------------------------------------
//CalcMaxWeight                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Calculates the character's Maximum weight.
//
//	Changes -
//		January 24th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TCharacter.CalcMaxWeight;
begin
	TThreadLink(ClientInfo.Data).DatabaseLink.StaticData.Connect;
	MaxWeight  := LongWord((ParamBase[STR] - ParamBonus[STR]) * 300) +
                TThreadLink(ClientInfo.Data).DatabaseLink.StaticData.GetBaseMaxWeight(self);
	TThreadLink(ClientInfo.Data).DatabaseLink.StaticData.Disconnect;
end;{CalcMaxWeight}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Create				                                                     CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Creates our character
//
//	Changes -
//		January 24th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Constructor TCharacter.Create(AClient : TIdContext);
begin
	inherited Create;
	ClientInfo := AClient;
	OnTouchIDs := TIntList32.Create;
	ScriptStatus := SCRIPT_NOTRUNNING;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy				                                                     DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys our character
//
//	Changes -
//		January 24th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Destructor TCharacter.Destroy;
begin
	OnTouchIDs.Free;
	TerminateLuaThread(LuaInfo);
	inherited;
end;
//------------------------------------------------------------------------------
end.