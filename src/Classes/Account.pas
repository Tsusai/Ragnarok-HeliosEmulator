(*------------------------------------------------------------------------------
Account
Tsusai July 2006

Description:
 The TAccount object and needed properties.

[2006/07/08] Tsusai - TCharacter is now in CharacterTypes, adjusted units as
 needed.
[2006/07/08] RaX - Moved to 'Classes' Directory and renamed to 'Account.pas'.
 One class per file is the name of the game.
------------------------------------------------------------------------------*)
unit Account;

interface

	type TAccount = class
	private
		fGender       : Char;
		procedure SetGender(Value : Char);
	public
		ID : cardinal;
		//Unicode
		Username        : string[24];
		Password        : string[24];
		EMail           : string[24];
		GenderNum       : Byte; //0 or 1 for packet (F/M respectively)
		Banned          : boolean;
		UnBanDateTime   : string;
		LastIP          : string[15]; { TODO -oTsusai -cTAccount : Change to property }
		LoginKey        : array [1..2] of cardinal;
		CharaID         : array [0..8] of Cardinal;
		LoginCount      : Integer;
		LastLoginTime   : TDateTime;
		Level           : Byte;

		property Gender : Char  read  fGender write SetGender;

	end;

implementation

(*------------------------------------------------------------------------------
TAccount.SetGender

Takes the Char from the SQL table (M or F) and figures out what to set GenderNum
 at.
------------------------------------------------------------------------------*)
procedure TAccount.SetGender(Value : Char);
begin
	case Value of
		'M': GenderNum := 1;
		'F': Gendernum := 0;
		else begin
			GenderNum :=  0;
			Value     := 'F';
		end;
	end;
	fGender := Value;
end; (* proc TAccount.SetGender
------------------------------------------------------------------------------*)

end.
 