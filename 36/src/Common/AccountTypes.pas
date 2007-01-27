unit AccountTypes;

interface

	type TAccount = class
		ID : cardinal;
		//Unicode
		Username : widestring;
		Password : widestring;
		EMail    : string;
		Gender : byte;
		Banned : boolean;
		UnBanDateTime : string;
		LastIP : string[15];
		LoginKey : array [1..2] of cardinal;
	end;

implementation

end.
 