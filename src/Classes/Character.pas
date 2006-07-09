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
uses GameObjects;

type TCharacter = class(TBeing)
	private
		//fID   : Cardinal;
		//fName : String;
		//fJob  : Cardinal;

	public
	CID : Cardinal; //BASIC KEY, WE DO NOT MOD IDS !!!

	SP  : Word;
	BaseLV : Cardinal;
	JobLV  : Cardinal;
	BaseEXP : Cardinal;
	JobEXP  : Cardinal;
	MaxHP : Word;
	MaxSP : Word;
	Speed : Word;
	Zeny : Cardinal;

	Option : Cardinal;
	Karma  : Cardinal;
	Manner : Cardinal;

	StatusPoint : Word;
	SkillPoint  : Word;
	StatBase : array [1..6] of byte;
	StatBonus : array [1..6] of byte;

	Hair : Word;
	HairColor : Word;
	Head1 : Word;
	Head2 : Word;
	Head3 : Word;
	ClothesColor : Word;
	Weapon : Word;
	Shield : Word;
end;

implementation

end.
