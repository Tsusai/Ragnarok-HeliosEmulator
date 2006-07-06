unit CharacterTypes;

interface
uses GameObjects;

type TCharacter = class(TBeing)
	public
	CID : Cardinal;
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
 