unit GameConstants;
//------------------------------------------------------------------------------
//GameConstants			                                                       UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This unit houses game related global variables and constants.
//
//	Changes -
//		September 21st, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
interface
const

	CHAR_CLICKAREA = 16;//the distance away a character can click from itself
											//CHAR_CLICKAREA used in Map.pas - Pathing.

//------------------------------------------------------------------------------
//                             Character Stat Constants
//------------------------------------------------------------------------------
	STR = 1;
	AGI = 2;
	VIT = 3;
	INT = 4;
	DEX = 5;
	LUK = 6;

//------------------------------------------------------------------------------
//                             Character Stat Constants
//------------------------------------------------------------------------------
	JOB_NOVICE     =  0;  //Novice
	JOB_SWORDSMAN  =  1;  //Swordsman
	JOB_MAGE       =  2;  //Magician
	JOB_ARCHER     =  3;  //Archer
	JOB_ACOLYTE    =  4;  //Acolyte
	JOB_MERCHANT   =  5;  //Merchant
	JOB_THIEF      =  6;  //Thief

	JOB_KNIGHT     =  7;  //Knight
	JOB_PRIEST     =  8;  //Priest
	JOB_WIZARD     =  9;  //Wizard
	JOB_BLACKSMITH = 10;  //Blacksmith
	JOB_HUNTER     = 11;  //Hunter
	JOB_ASSASSIN   = 12;  //Assassin
	JOB_KNIGHT_2   = 13;  //Mounted Knight(on peco)

	JOB_CRUSADER   = 14;  //Crusader
	JOB_MONK       = 15;  //Monk
	JOB_SAGE       = 16;  //Sage
	JOB_ROGUE      = 17;  //Rogue
	JOB_ALCHEMIST  = 18;  //Alchemist
	JOB_BARD       = 19;  //Bard
	JOB_DANCER     = 20;  //Dancer
	JOB_CRUSADER_2 = 21;  //Mounted Crusader(on peco)

	JOB_WEDDING    = 22;  //Wedding
	JOB_SNOVICE    = 23;  //Super Novice
	//
	JOB_MAX    = 23;

	JOB_HIGH_OFFSET    = 24;

	JOB_HIGH_NOVICE     = JOB_HIGH_OFFSET + JOB_NOVICE; //24 High Novice
	JOB_HIGH_SWORDSMAN  = JOB_HIGH_OFFSET + JOB_SWORDSMAN;  //25 High Swordsman
	JOB_HIGH_MAGE       = JOB_HIGH_OFFSET + JOB_MAGE; //26 High Magician
	JOB_HIGH_ARCHER     = JOB_HIGH_OFFSET + JOB_ARCHER; //27 High Archer
	JOB_HIGH_ACOLYTE    = JOB_HIGH_OFFSET + JOB_ACOLYTE;  //28 High Acolyte
	JOB_HIGH_MERCHANT   = JOB_HIGH_OFFSET + JOB_MERCHANT; //29 High Merchant
	JOB_HIGH_THIEF      = JOB_HIGH_OFFSET + JOB_THIEF;  //30 High Thief

	JOB_LORD_KNIGHT     = JOB_HIGH_OFFSET + JOB_KNIGHT; //31 Lord Knight
	JOB_HIGH_PRIEST     = JOB_HIGH_OFFSET + JOB_PRIEST; //32 High Priest
	JOB_HIGH_WIZARD     = JOB_HIGH_OFFSET + JOB_WIZARD; //33 High Wizard
	JOB_WHITESMITH      = JOB_HIGH_OFFSET + JOB_BLACKSMITH; //34 Whitesmith
	JOB_SNIPER          = JOB_HIGH_OFFSET + JOB_HUNTER; //35 Sniper
	JOB_ASSASSIN_CROSS  = JOB_HIGH_OFFSET + JOB_ASSASSIN; //36 Assassin Cross
	JOB_PECO_KNIGHT     = JOB_HIGH_OFFSET + JOB_KNIGHT_2; //37 Mounted Lord Knight(on peco)

	JOB_PALADIN         = JOB_HIGH_OFFSET + JOB_CRUSADER; //38 Paladin
	JOB_CHAMPION        = JOB_HIGH_OFFSET + JOB_MONK; //39 Champion
	JOB_PROFESSOR       = JOB_HIGH_OFFSET + JOB_SAGE; //40 Professor
	JOB_STALKER         = JOB_HIGH_OFFSET + JOB_ROGUE; //41 Stalker
	JOB_CREATOR         = JOB_HIGH_OFFSET + JOB_ALCHEMIST; //42 Creator
	JOB_CLOWN           = JOB_HIGH_OFFSET + JOB_BARD; //43 Clown
	JOB_GYPSY           = JOB_HIGH_OFFSET + JOB_DANCER; //44 Gypsy
	JOB_PECO_PALADIN    = JOB_HIGH_OFFSET + JOB_CRUSADER_2; //45 Mounted Paladin(on peco)

	//
	JOB_BABY_OFFSET    = 46;

	JOB_BABY            = JOB_BABY_OFFSET + JOB_NOVICE; //46 Baby Novice
	JOB_BABY_SWORDSMAN  = JOB_BABY_OFFSET + JOB_SWORDSMAN;  //47 Baby Swordsman
	JOB_BABY_MAGE       = JOB_BABY_OFFSET + JOB_MAGE; //48 Baby Magician
	JOB_BABY_ARCHER     = JOB_BABY_OFFSET + JOB_ARCHER; //49 Baby Archer
	JOB_BABY_ACOLYTE    = JOB_BABY_OFFSET + JOB_ACOLYTE;  //50 Baby Acolyte
	JOB_BABY_MERCHANT   = JOB_BABY_OFFSET + JOB_MERCHANT; //51 Baby Merchant
	JOB_BABY_THIEF      = JOB_BABY_OFFSET + JOB_THIEF;  //52 Baby Thief

	JOB_BABY_KNIGHT      = JOB_BABY_OFFSET + JOB_KNIGHT;  //53 Baby Knight
	JOB_BABY_PRIEST      = JOB_BABY_OFFSET + JOB_PRIEST;  //54 Baby Priest
	JOB_BABY_WIZARD      = JOB_BABY_OFFSET + JOB_WIZARD;  //55 Baby Wizard
	JOB_BABY_BLACKSMITH  = JOB_BABY_OFFSET + JOB_BLACKSMITH;  //56 Baby Blacksmith
	JOB_BABY_HUNTER      = JOB_BABY_OFFSET + JOB_HUNTER;  //57 Baby Hunter
	JOB_BABY_ASSASSIN    = JOB_BABY_OFFSET + JOB_ASSASSIN;  //58 Baby Assassin
	JOB_BABY_KNIGHT_2    = JOB_BABY_OFFSET + JOB_KNIGHT_2;  //59 Baby Peco Knight

	JOB_BABY_CRUSADER    = JOB_BABY_OFFSET + JOB_CRUSADER;  //60 Baby Cruaser
	JOB_BABY_MONK        = JOB_BABY_OFFSET + JOB_MONK;  //61 Baby Monk
	JOB_BABY_SAGE        = JOB_BABY_OFFSET + JOB_SAGE;  //62 Baby Sage
	JOB_BABY_ROGUE       = JOB_BABY_OFFSET + JOB_ROGUE; //63 Baby Rogue
	JOB_BABY_ALCHEMIST   = JOB_BABY_OFFSET + JOB_ALCHEMIST; //64 Baby Alchemist
	JOB_BABY_BARD        = JOB_BABY_OFFSET + JOB_BARD;  //65 Baby Bard
	JOB_BABY_DANCER      = JOB_BABY_OFFSET + JOB_DANCER;  //66 Baby Dancer
	JOB_BABY_CRUSADER_2  = JOB_BABY_OFFSET + JOB_CRUSADER_2;  //67 Mounted Baby Crusader(on peco)
	JOB_BABY_SNOVICE     = 68;  //Baby Super Novice

	JOB_EXPANDED_OFFSET    = 69;

	JOB_EXPANDED_TAEKWON              = JOB_EXPANDED_OFFSET + JOB_NOVICE; //69 TaeKwon Boy/Girl
	JOB_EXPANDED_STAR_GLADIATOR       = JOB_EXPANDED_OFFSET + JOB_SWORDSMAN; //70 Star Gladiator?
	JOB_EXPANDED_STAR_GLADIATOR_2     = JOB_EXPANDED_OFFSET + JOB_MAGE; //71 Star Gladiator Peco?
	JOB_EXPANDED_SOUL_LINKER          = JOB_EXPANDED_OFFSET + JOB_ARCHER;  //72 Soul Linker

	//-
	//High (4001+ Job classes)
	HJOB_OFFSET    = 4001;
	HJOB_HIGH_NOVICE     = HJOB_OFFSET + JOB_NOVICE; //4001 High Novice
	HJOB_HIGH_SWORDSMAN  = HJOB_OFFSET + JOB_SWORDSMAN;  //4002 High Swordsman
	HJOB_HIGH_MAGE       = HJOB_OFFSET + JOB_MAGE; //4003 High Magician
	HJOB_HIGH_ARCHER     = HJOB_OFFSET + JOB_ARCHER; //4004 High Archer
	HJOB_HIGH_ACOLYTE    = HJOB_OFFSET + JOB_ACOLYTE;  //4005 High Acolyte
	HJOB_HIGH_MERCHANT   = HJOB_OFFSET + JOB_MERCHANT; //4006 High Merchant
	HJOB_HIGH_THIEF      = HJOB_OFFSET + JOB_THIEF;  //4007 High Thief

	HJOB_LORD_KNIGHT     = HJOB_OFFSET + JOB_KNIGHT; //4008 Lord Knight
	HJOB_HIGH_PRIEST     = HJOB_OFFSET + JOB_PRIEST; //4009 High Priest
	HJOB_HIGH_WIZARD     = HJOB_OFFSET + JOB_WIZARD; //4010 High Wizard
	HJOB_WHITESMITH      = HJOB_OFFSET + JOB_BLACKSMITH; //4011 Whitesmith
	HJOB_SNIPER          = HJOB_OFFSET + JOB_HUNTER; //4012 Sniper
	HJOB_ASSASSIN_CROSS  = HJOB_OFFSET + JOB_ASSASSIN; //4013 Assassin Cross
	HJOB_PECO_KNIGHT     = HJOB_OFFSET + JOB_KNIGHT_2; //4014 Mounted Lord Knight(on peco)

	HJOB_PALADIN         = HJOB_OFFSET + JOB_CRUSADER; //4015 Paladin
	HJOB_CHAMPION        = HJOB_OFFSET + JOB_MONK; //4016 Champion
	HJOB_PROFESSOR       = HJOB_OFFSET + JOB_SAGE; //4017 Professor
	HJOB_STALKER         = HJOB_OFFSET + JOB_ROGUE; //4018 Stalker
	HJOB_CREATOR         = HJOB_OFFSET + JOB_ALCHEMIST; //4019 Creator
	HJOB_CLOWN           = HJOB_OFFSET + JOB_BARD; //4020 Clown
	HJOB_GYPSY           = HJOB_OFFSET + JOB_DANCER; //4021 Gypsy
	HJOB_PECO_PALADIN    = HJOB_OFFSET + JOB_CRUSADER_2; //4022 Mounted Paladin(on peco)

	//
	HJOB_BABY_OFFSET    = 4023;

	HJOB_BABY            = HJOB_BABY_OFFSET + JOB_NOVICE; //4023 Baby Novice
	HJOB_BABY_SWORDSMAN  = HJOB_BABY_OFFSET + JOB_SWORDSMAN;  //4024 Baby Swordsman
	HJOB_BABY_MAGE       = HJOB_BABY_OFFSET + JOB_MAGE; //4025 Baby Magician
	HJOB_BABY_ARCHER     = HJOB_BABY_OFFSET + JOB_ARCHER; //4026 Baby Archer
	HJOB_BABY_ACOLYTE    = HJOB_BABY_OFFSET + JOB_ACOLYTE;  //4027 Baby Acolyte
	HJOB_BABY_MERCHANT   = HJOB_BABY_OFFSET + JOB_MERCHANT; //4028 Baby Merchant
	HJOB_BABY_THIEF      = HJOB_BABY_OFFSET + JOB_THIEF;  //4029 Baby Thief

	HJOB_BABY_KNIGHT      = HJOB_BABY_OFFSET + JOB_KNIGHT;  //4030 Baby Knight
	HJOB_BABY_PRIEST      = HJOB_BABY_OFFSET + JOB_PRIEST;  //4031 Baby Priest
	HJOB_BABY_WIZARD      = HJOB_BABY_OFFSET + JOB_WIZARD;  //4032 Baby Wizard
	HJOB_BABY_BLACKSMITH  = HJOB_BABY_OFFSET + JOB_BLACKSMITH;  //4033 Baby Blacksmith
	HJOB_BABY_HUNTER      = HJOB_BABY_OFFSET + JOB_HUNTER;  //4034 Baby Hunter
	HJOB_BABY_ASSASSIN    = HJOB_BABY_OFFSET + JOB_ASSASSIN;  //4035 Baby Assassin
	HJOB_BABY_KNIGHT_2    = HJOB_BABY_OFFSET + JOB_KNIGHT_2;  //4036 Baby Peco Knight

	HJOB_BABY_CRUSADER    = HJOB_BABY_OFFSET + JOB_CRUSADER;  //4037 Baby Cruaser
	HJOB_BABY_MONK        = HJOB_BABY_OFFSET + JOB_MONK;  //4038 Baby Monk
	HJOB_BABY_SAGE        = HJOB_BABY_OFFSET + JOB_SAGE;  //4039 Baby Sage
	HJOB_BABY_ROGUE       = HJOB_BABY_OFFSET + JOB_ROGUE; //4040 Baby Rogue
	HJOB_BABY_ALCHEMIST   = HJOB_BABY_OFFSET + JOB_ALCHEMIST; //4041 Baby Alchemist
	HJOB_BABY_BARD        = HJOB_BABY_OFFSET + JOB_BARD;  //4042 Baby Bard
	HJOB_BABY_DANCER      = HJOB_BABY_OFFSET + JOB_DANCER;  //4043 Baby Dancer
	HJOB_BABY_CRUSADER_2  = HJOB_BABY_OFFSET + JOB_CRUSADER_2;  //4044 Mounted Baby Crusader(on peco)
	HJOB_BABY_SNOVICE     = 4045;  //Baby Super Novice

	HJOB_EXPANDED_OFFSET    = 4046;

	HJOB_EXPANDED_TAEKWON              = HJOB_EXPANDED_OFFSET + JOB_NOVICE; //4046 TaeKwon Boy/Girl
	HJOB_EXPANDED_STAR_GLADIATOR       = HJOB_EXPANDED_OFFSET + JOB_SWORDSMAN; //4047 Star Gladiator?
	HJOB_EXPANDED_STAR_GLADIATOR_2     = HJOB_EXPANDED_OFFSET + JOB_MAGE; //4048 Star Gladiator Peco?
	HJOB_EXPANDED_SOUL_LINKER          = HJOB_EXPANDED_OFFSET + JOB_ARCHER;  //4049 Soul Linker

	HJOB_MAX = 4049;


implementation

end.
