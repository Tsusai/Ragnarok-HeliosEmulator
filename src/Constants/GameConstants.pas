(*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

Unit
GameConstants

*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

[2006/??/??] Helios - Author unstated...

================================================================================
License:  (FreeBSD, plus commercial with written permission clause.)
================================================================================

Project Helios - Copyright (c) 2005-2007

All rights reserved.

Please refer to Helios.dpr for full license terms.

================================================================================
Overview:
================================================================================

This unit houses game related global variables and constants.

================================================================================
Revisions:
================================================================================
(Format: [yyyy/mm/dd] <Author> - <Desc of Changes>)
[2006/09/21] RaX - Created Header.
[2006/12/23] Muad_Dib - Added new jobs (nin, gs, xmas).
[2007/04/28] CR - Altered header.  Rearranged Direction constants to reuse
	NORTH and NORTHEAST as the bounds for the constant Directions array.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)
unit GameConstants;


interface


uses
	Types;


const
  CHAR_BLEVEL_MAX = 9999;
  CHAR_JLEVEL_MAX = 9999;
  CHAR_STAT_MAX = 9999;
  CHAR_STATPOINT_MAX = 1000000;
  CHAR_SKILLPOINT_MAX = 1000000;

	NPC_WARPSPRITE = 45;
	NPC_INVISIBLE = 32767;

	L_HAND = 1;
	R_HAND = 0;

//------------------------------------------------------------------------------
//                             Character Stat Constants
//------------------------------------------------------------------------------
	STR = 0; {Strength - Increases attack strength and weight capacity }
	AGI = 1; {Agility - Increases attack speed and dodge rate. }
	VIT = 2; {Vitality - Increases defense and vitality, as well as HP recovery. }
	INT = 3; {Intelligence - Increases magic attack and magic defense. }
	DEX = 4; {Dexterity - Increases accuracy and stabilizes the amount of damage
			done by a weapon. As well as lowers the cast time to some skills/spells. }
	LUK = 5; {Luck - Increases Critical hits and perfect dodge rate, and brings
			about lots of luck. }


//------------------------------------------------------------------------------
//                             Movement Constants
//------------------------------------------------------------------------------
	NORTH			= 0;
	NORTHWEST	= 1;
	WEST			= 2;
	SOUTHWEST = 3;
	SOUTH			= 4;
	SOUTHEAST	= 5;
	EAST			= 6;
	NORTHEAST	= 7;

	{ Unit direction vectors with North and West being positive Y, and X,
	respectively. }
	Directions : array[NORTH..NORTHEAST] of TPoint = (
		(X:0;Y:1),   //North
		(X:-1;Y:1),  //NorthWest
		(X:-1;Y:0),  //West
		(X:-1;Y:-1), //SouthWest
		(X:0;Y:-1),  //South
		(X:1;Y:-1),  //SouthEast
		(X:1;Y:0),   //East
		(X:1;Y:1)    //NorthEast
	);

	Diagonals = [NORTHEAST, NORTHWEST, SOUTHEAST, SOUTHWEST];



//------------------------------------------------------------------------------
//                             Character Job Constants
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

	JOB_GUNSLINGER = 24;  //Gunslinger
	JOB_NINJA      = 25;  //Ninja

	JOB_XMAS       = 26;  //Xmas Suit

	//High (4001+ Job classes)
	HJOB_OFFSET    = 4001;

	HJOB_HIGH_NOVICE     = HJOB_OFFSET + JOB_NOVICE;      //4001 High Novice
	HJOB_HIGH_SWORDSMAN  = HJOB_OFFSET + JOB_SWORDSMAN;   //4002 High Swordsman
	HJOB_HIGH_MAGE       = HJOB_OFFSET + JOB_MAGE;        //4003 High Magician
	HJOB_HIGH_ARCHER     = HJOB_OFFSET + JOB_ARCHER;      //4004 High Archer
	HJOB_HIGH_ACOLYTE    = HJOB_OFFSET + JOB_ACOLYTE;     //4005 High Acolyte
	HJOB_HIGH_MERCHANT   = HJOB_OFFSET + JOB_MERCHANT;    //4006 High Merchant
	HJOB_HIGH_THIEF      = HJOB_OFFSET + JOB_THIEF;       //4007 High Thief

	HJOB_LORD_KNIGHT     = HJOB_OFFSET + JOB_KNIGHT;      //4008 Lord Knight
	HJOB_HIGH_PRIEST     = HJOB_OFFSET + JOB_PRIEST;      //4009 High Priest
	HJOB_HIGH_WIZARD     = HJOB_OFFSET + JOB_WIZARD;      //4010 High Wizard
	HJOB_WHITESMITH      = HJOB_OFFSET + JOB_BLACKSMITH;  //4011 Master Smith
	HJOB_SNIPER          = HJOB_OFFSET + JOB_HUNTER;      //4012 Sniper
	HJOB_ASSASSIN_CROSS  = HJOB_OFFSET + JOB_ASSASSIN;    //4013 Assassin Cross
	HJOB_PECO_KNIGHT     = HJOB_OFFSET + JOB_KNIGHT_2;    //4014 Mounted Lord Knight(on peco)

	HJOB_PALADIN         = HJOB_OFFSET + JOB_CRUSADER;    //4015 Paladin
	HJOB_CHAMPION        = HJOB_OFFSET + JOB_MONK;        //4016 Champion
	HJOB_PROFESSOR       = HJOB_OFFSET + JOB_SAGE;        //4017 Scholar
	HJOB_STALKER         = HJOB_OFFSET + JOB_ROGUE;       //4018 Stalker
	HJOB_CREATOR         = HJOB_OFFSET + JOB_ALCHEMIST;   //4019 Biochemist
	HJOB_CLOWN           = HJOB_OFFSET + JOB_BARD;        //4020 Minstrel
	HJOB_GYPSY           = HJOB_OFFSET + JOB_DANCER;      //4021 Gypsy
	HJOB_PECO_PALADIN    = HJOB_OFFSET + JOB_CRUSADER_2;  //4022 Mounted Paladin(on peco)

	//Baby (4023+ Job classes)
	HJOB_BABY_OFFSET    = 4023;

	HJOB_BABY            = HJOB_BABY_OFFSET + JOB_NOVICE;       //4023 Baby Novice
	HJOB_BABY_SWORDSMAN  = HJOB_BABY_OFFSET + JOB_SWORDSMAN;    //4024 Baby Swordsman
	HJOB_BABY_MAGE       = HJOB_BABY_OFFSET + JOB_MAGE;         //4025 Baby Magician
	HJOB_BABY_ARCHER     = HJOB_BABY_OFFSET + JOB_ARCHER;       //4026 Baby Archer
	HJOB_BABY_ACOLYTE    = HJOB_BABY_OFFSET + JOB_ACOLYTE;      //4027 Baby Acolyte
	HJOB_BABY_MERCHANT   = HJOB_BABY_OFFSET + JOB_MERCHANT;     //4028 Baby Merchant
	HJOB_BABY_THIEF      = HJOB_BABY_OFFSET + JOB_THIEF;        //4029 Baby Thief

	HJOB_BABY_KNIGHT      = HJOB_BABY_OFFSET + JOB_KNIGHT;      //4030 Baby Knight
	HJOB_BABY_PRIEST      = HJOB_BABY_OFFSET + JOB_PRIEST;      //4031 Baby Priest
	HJOB_BABY_WIZARD      = HJOB_BABY_OFFSET + JOB_WIZARD;      //4032 Baby Wizard
	HJOB_BABY_BLACKSMITH  = HJOB_BABY_OFFSET + JOB_BLACKSMITH;  //4033 Baby Blacksmith
	HJOB_BABY_HUNTER      = HJOB_BABY_OFFSET + JOB_HUNTER;      //4034 Baby Hunter
	HJOB_BABY_ASSASSIN    = HJOB_BABY_OFFSET + JOB_ASSASSIN;    //4035 Baby Assassin
	HJOB_BABY_KNIGHT_2    = HJOB_BABY_OFFSET + JOB_KNIGHT_2;    //4036 Baby Peco Knight

	HJOB_BABY_CRUSADER    = HJOB_BABY_OFFSET + JOB_CRUSADER;    //4037 Baby Cruaser
	HJOB_BABY_MONK        = HJOB_BABY_OFFSET + JOB_MONK;        //4038 Baby Monk
	HJOB_BABY_SAGE        = HJOB_BABY_OFFSET + JOB_SAGE;        //4039 Baby Sage
	HJOB_BABY_ROGUE       = HJOB_BABY_OFFSET + JOB_ROGUE;       //4040 Baby Rogue
	HJOB_BABY_ALCHEMIST   = HJOB_BABY_OFFSET + JOB_ALCHEMIST;   //4041 Baby Alchemist
	HJOB_BABY_BARD        = HJOB_BABY_OFFSET + JOB_BARD;        //4042 Baby Bard
	HJOB_BABY_DANCER      = HJOB_BABY_OFFSET + JOB_DANCER;      //4043 Baby Dancer
	HJOB_BABY_CRUSADER_2  = HJOB_BABY_OFFSET + JOB_CRUSADER_2;  //4044 Mounted Baby Crusader(on peco)
	HJOB_BABY_SNOVICE     = 4045;  //Baby Super Novice

	//Expanded (4046+ Job classes)
	HJOB_EXPANDED_OFFSET    = 4046;

	HJOB_EXPANDED_TAEKWON              = HJOB_EXPANDED_OFFSET + JOB_NOVICE;     //4046 Taekwon Boy/Girl
	HJOB_EXPANDED_STAR_GLADIATOR       = HJOB_EXPANDED_OFFSET + JOB_SWORDSMAN;  //4047 Taekwon Master
	HJOB_EXPANDED_STAR_GLADIATOR_2     = HJOB_EXPANDED_OFFSET + JOB_MAGE;       //4048 Taekwon Master(Flying)
	HJOB_EXPANDED_SOUL_LINKER          = HJOB_EXPANDED_OFFSET + JOB_ARCHER;     //4049 Soul Linker

	HJOB_MAX = 4049;


implementation

end.
