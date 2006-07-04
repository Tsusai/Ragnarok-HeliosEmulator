unit AccountTypes;

interface
	uses
		GameObjects;

	type TAccount = class
		ID : cardinal;
		//Unicode
		Username : string[24];
		Password : string[24];
		EMail    : string[24];
		Gender : byte;
		Banned : boolean;
		UnBanDateTime : string;
		LastIP : string[15];
		LoginKey : array [1..2] of cardinal;
		Characters : array [1..9] of TCharacter;
	end;

implementation

end.
 