unit AccountTypes;

interface
	uses
		GameObjects;

	type TAccount = class
	private
		fGender : Char;
		procedure SetGender(Value : Char);
	public
		ID : cardinal;
		//Unicode
		Username : string[24];
		Password : string[24];
		EMail    : string[24];
		GenderNum : Byte;
		Banned : boolean;
		UnBanDateTime : string;
		LastIP : string[15];
		LoginKey : array [1..2] of cardinal;
		Characters : array [1..9] of TCharacter;

		property Gender : Char read fGender write SetGender;

	end;

implementation

procedure TAccount.SetGender(Value : Char);
begin
	case Value of
		'M': GenderNum := 1;
		'F': Gendernum := 0;
		else begin
			GenderNum := 0;
			Value := 'F';
		end;
	end;
	fGender := Value;
end;

end.
 