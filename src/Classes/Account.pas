//------------------------------------------------------------------------------
//Account                                                                  UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This holds the Account class.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit Account;

interface

//------------------------------------------------------------------------------
//TACCOUNT                                                                CLASS
//------------------------------------------------------------------------------
	type TAccount = class
	private
		fGender       : Char;
		procedure SetGender(Value : Char);
		function  GetBanned : boolean;
		function  GetConnectUntilTime : boolean;
	public
		ID : cardinal;
		//Unicode
		Username        : string[24];
		Password        : string[24];
		EMail           : string[24];
		GenderNum       : Byte; //0 or 1 for packet (F/M respectively)
		Bantime         : TDateTime;
		UnBanDateTime   : string;
		LastIP          : string[15];
		LoginKey        : array [1..2] of cardinal;
		CharaID         : array [0..8] of Cardinal;
		LoginCount      : Integer;
		LastLoginTime   : TDateTime;
		Level           : Byte;
		ConnectUntil    : TDateTime;

		property Gender : Char  read  fGender write SetGender;
		property IsBanned : boolean read GetBanned;
		property GameTimeUp  : boolean read GetConnectUntilTime;

		procedure SetBannedTime(TimeString : string);
		procedure SetConnectUntilTime(TimeString : string);

	end;{TAccount}
//------------------------------------------------------------------------------


implementation
uses
	SysUtils,
	Console,
	Globals;

//------------------------------------------------------------------------------
//SetGender                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Takes the Char from the SQL table (M or F) and figures out what to set
//    GenderNum at.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
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
end; {SetGender}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetBanned                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Checks to see if the account is banned.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//		December 27th, 2006 - Tsusai - Removed SQL call, simplified
//
//------------------------------------------------------------------------------
function TAccount.GetBanned : boolean;
begin
	Result := (BanTime > Now);
end;{GetBanned}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetBannedTime                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets the time that a user will be banned until
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TAccount.SetBannedTime(TimeString : string);
begin
	Self.Bantime := ConvertMySQLTime(TimeString);
	ADatabase.CommonData.SaveAccount(self);
end;{SetBannedTime}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetConnectUntilTime                                                  FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets the connectuntil time from the database.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//		December 27th, 2006 - Tsusai - Removed SQL call, simplified
//
//------------------------------------------------------------------------------
function TAccount.GetConnectUntilTime : boolean;
begin
	Result := (Now > ConnectUntil);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetConnectUntilTime                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Saves the connectuntil time to the database.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TAccount.SetConnectUntilTime(TimeString : string);
begin
	Self.ConnectUntil := ConvertMySQLTime(TimeString);
	ADatabase.CommonData.SaveAccount(self);
end;{SetConnectUntilTime}
//------------------------------------------------------------------------------


end.
