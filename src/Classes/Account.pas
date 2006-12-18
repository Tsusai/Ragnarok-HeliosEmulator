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

	end;

implementation
uses
	SysUtils,
  Console,
	Database,
	Globals;

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

function TAccount.GetBanned : boolean;
begin
	MainProc.ACommonDatabase.AnInterface.GetAccountBanAndConnectTime(Self);
	if Now > BanTime then
	begin
		Result := false;
	end else
	begin
		Result := true;
	end;
end;

procedure TAccount.SetBannedTime(TimeString : string);
begin
	Self.Bantime := ConvertMySQLTime(TimeString);
	MainProc.ACommonDatabase.AnInterface.SaveAccount(self);
end;

function TAccount.GetConnectUntilTime : boolean;
begin
	MainProc.ACommonDatabase.AnInterface.GetAccountBanAndConnectTime(Self);
	if Now > ConnectUntil then
	begin
		Result := true;
	end else
	begin
		Result := false;
	end;
end;

procedure TAccount.SetConnectUntilTime(TimeString : string);
begin
	Self.Bantime := ConvertMySQLTime(TimeString);
	MainProc.ACommonDatabase.AnInterface.SaveAccount(self);
end;


end.
