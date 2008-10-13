//------------------------------------------------------------------------------
//ItemTypes                                                                 UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Contains Item related Types
//
//	Changes -
//		February 25th, 2008 - RaX - Created
//
//------------------------------------------------------------------------------

unit ItemTypes;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	{Project}
	GameTypes
	;
	{Third Party}

type

	TItemType = (EQUIPMENT,USEABLE,MISC);

	TEquipLocations =
	(
		HEADUPPER,
		HEADMID,
		HEADLOWER,
		RIGHTHAND,
		LEFTHAND,
		CAPE,
		FEET,
		BODY,
		ACCESSORY1,
		ACCESSORY2
	);//1 byte

	TEquipTypes =
	(
		HEADGEAR,			//1
		SHIELD,				//2
		BODYARMOR,			//3
		SHOES,				//4
		GARMENT,			//5
		ACCESSORY,			//6
		SWORD,				//7
		TWOHANDEDSWORD,			//8
		DAGGER,				//9
		MACE,				//10
		TWOHANDEDMACE,			//11
		BOW,				//12
		AXE,				//13
		TWOHANDEDAXE,			//14
		SPEAR,				//15
		TWOHANDEDSPEAR,			//16
		STAFF,				//17
		BOOK,				//18
		FIST,				//19
		KATAR,				//20
		INSTRUMENT,			//21
		WHIP,				//22
		GATLINGGUN,			//23
		GRENADELAUNCHER,		//24
		REVOLVER,			//25
		RIFLE,				//26
		SHOTGUN				//27
	);//1 byte

function ItemTypeToByte(const AnItemType : TItemType) : Byte;
function ByteToItemType(AByte : Byte) : TItemType;

function EquipLocationsToByte(const AnEquipLocation : TEquipLocations) : Word;
function ByteToEquipLocations(AWord : Word) : TEquipLocations;

function EquipTypeToByte(const AnEquipType : TEquipTypes) : Byte;
function ByteToEquipType(AByte : Byte) : TEquipTypes;

function EquipLocationToLookType(const AnEquipLocation : TEquipLocations):TLookTypes;


implementation

function ItemTypeToByte(const AnItemType : TItemType) : Byte;
begin
	case AnItemType of
		EQUIPMENT :
			begin
				Result := 4;
			end;
		USEABLE :
			begin
				Result := 2;
			end;
		else
		begin
			Result := 3;
		end;
	end;
end;{ItemTypeToByte}


function ByteToItemType(AByte : Byte) : TItemType;
begin
	case AByte of
		4 :
			begin
				Result := EQUIPMENT;
			end;
		2 :
			begin
				Result := USEABLE;
			end;
		else
		begin
			Result := MISC;
		end;
	end;
end;{ByteToItemType}

function EquipLocationsToByte(const AnEquipLocation : TEquipLocations) : Word;
begin
	case AnEquipLocation of
		HEADLOWER		: Result := 1;
		RIGHTHAND		: Result := 2;
		CAPE			: Result := 4;
		ACCESSORY1		: Result := 8;
		BODY			: Result := 16;
		LEFTHAND		: Result := 32;
		FEET			: Result := 64;
		ACCESSORY2		: Result := 128;
		HEADUPPER		: Result := 256;
		HEADMID			: Result := 512;
		else
			Result := 1;
	end;
end;{EquipLocationsToByte}

function ByteToEquipLocations(AWord : Word) : TEquipLocations;
begin
	case AWord of
		1		: Result := HEADLOWER;
		2		: Result := RIGHTHAND;
		4		: Result := CAPE;
		8		: Result := ACCESSORY1;
		16		: Result := BODY;
		32		: Result := LEFTHAND;
		64		: Result := FEET;
		128		: Result := ACCESSORY2;
		256		: Result := HEADUPPER;
		512		: Result := HEADMID;
		else
			Result := HEADUPPER;
	end;
end;{ByteToEquipLocations}

function EquipTypeToByte(const AnEquipType : TEquipTypes) : Byte;
begin
	case AnEquipType of
		HEADGEAR		: Result := 1;
		SHIELD			: Result := 2;
		BODYARMOR		: Result := 3;
		SHOES			: Result := 4;
		GARMENT			: Result := 5;
		ACCESSORY		: Result := 6;
		SWORD			: Result := 7;
		TWOHANDEDSWORD		: Result := 8;
		DAGGER			: Result := 9;
		MACE			: Result := 10;
		TWOHANDEDMACE		: Result := 11;
		BOW			: Result := 12;
		AXE			: Result := 13;
		TWOHANDEDAXE		: Result := 14;
		SPEAR			: Result := 15;
		TWOHANDEDSPEAR		: Result := 16;
		STAFF			: Result := 17;
		BOOK			: Result := 18;
		FIST			: Result := 19;
		KATAR			: Result := 20;
		INSTRUMENT		: Result := 21;
		WHIP			: Result := 22;
		GATLINGGUN		: Result := 23;
		GRENADELAUNCHER		: Result := 24;
		REVOLVER		: Result := 25;
		RIFLE			: Result := 26;
		SHOTGUN			: Result := 27;
		else
			Result := 1;
	end;
end;{EquipTypeToByte}

function ByteToEquipType(AByte : Byte) : TEquipTypes;
begin
	case AByte of
		1		: Result := HEADGEAR;
		2		: Result := SHIELD;
		3		: Result := BODYARMOR;
		4		: Result := SHOES;
		5		: Result := GARMENT;
		6		: Result := ACCESSORY;
		7		: Result := SWORD;
		8		: Result := TWOHANDEDSWORD;
		9		: Result := DAGGER;
		10		: Result := MACE;
		11		: Result := TWOHANDEDMACE;
		12		: Result := BOW;
		13		: Result := AXE;
		14		: Result := TWOHANDEDAXE;
		15		: Result := SPEAR;
		16		: Result := TWOHANDEDSPEAR;
		17		: Result := STAFF;
		18		: Result := BOOK;
		19		: Result := FIST;
		20		: Result := KATAR;
		21		: Result := INSTRUMENT;
		22		: Result := WHIP;
		23		: Result := GATLINGGUN;
		24		: Result := GRENADELAUNCHER;
		25		: Result := REVOLVER;
		26		: Result := RIFLE;
		27		: Result := SHOTGUN;
		else
			Result := HEADGEAR;
	end;
end;{ByteToEquipType}

function EquipLocationToLookType(const AnEquipLocation : TEquipLocations):TLookTypes;
begin
	case AnEquipLocation of
		HEADUPPER:Result := LOOK_HEAD_TOP;
		HEADMID:  Result := LOOK_HEAD_MID;
		HEADLOWER:Result := LOOK_HEAD_BOTTOM;
		RIGHTHAND:Result := LOOK_WEAPON;
		LEFTHAND: Result := LOOK_SHIELD;
		FEET:     Result := LOOK_SHOES;
		else
			Result := TLookTypes(0);
	end;
end;{EquipLocationToLookType}
end.
