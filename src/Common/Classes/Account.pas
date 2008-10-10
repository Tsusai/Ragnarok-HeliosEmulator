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

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
uses
	IdContext
	;
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
		ID : LongWord;
		//Unicode
		Name        		: string[24];
		Password        : string[32];
		EMail           : string[24];
		GenderNum       : Byte; //0 or 1 for packet (F/M respectively)
		BannedUntil     : TDateTime;
		LastIP          : string[15];
		LoginKey        : array [1..2] of LongWord;
		CharaID         : array [0..8] of LongWord;
		LoginCount      : Integer;
		LastLoginTime   : TDateTime;
		Level           : Byte;
		StorageID	: LongWord;
		ConnectUntil    : TDateTime;
		State           : Byte;

		ClientInfo			: TIdContext;
		ZoneServerID : Word;
		InGame : Boolean;
		UnderKickQuery : Boolean;
		property Gender : Char  read  fGender write SetGender;
		property IsBanned : boolean read GetBanned;
		property GameTimeUp  : boolean read GetConnectUntilTime;

		procedure SetBannedTime(TimeString : string);
		procedure SetConnectUntilTime(TimeString : string);
		procedure TemporaryBan(Seconds:Integer);
		procedure PermanantBan();
		function GetBanUntilTimeString:String;
		Constructor Create(AClient : TIdContext);
	end;{TAccount}
//------------------------------------------------------------------------------


implementation
uses
	SysUtils,
	Globals,
	PacketTypes,
	DateUtils;

//------------------------------------------------------------------------------
//SetGender                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Set fGender in local Class (M = 1 , F = 0)
//	GenderNum at.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//		March 12th, 2007 - Aeomin - Modify Header.
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
	Result := (BannedUntil > Now);
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
	Self.BannedUntil := ConvertMySQLTime(TimeString);
	TThreadLink(ClientInfo.Data).DatabaseLink.Account.Save(self);
end;{SetBannedTime}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetBannedTime                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Set temperary ban
//
//	Changes -
//		April 10th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
procedure TAccount.TemporaryBan(Seconds:Integer);
begin
	Self.BannedUntil := UnixToDateTime(DateTimeToUnix(Now) + Seconds);
	TThreadLink(ClientInfo.Data).DatabaseLink.Account.Save(self);
end;{SetBannedTime}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetPermBan                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Set Permanent ban
//
//	Changes -
//		October 9th, 2008 - Spre - Created
//
//------------------------------------------------------------------------------
procedure TAccount.PermanantBan();
begin
	SetBannedTime('9999-12-31 23:59:59');
end;{SetPermBan}
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
	TThreadLink(ClientInfo.Data).DatabaseLink.Account.Save(self);
end;{SetConnectUntilTime}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetBanUntilTimeString                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Generate Temperary ban's remaining time in STRING
//
//	Changes -
//		April 12th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
function TAccount.GetBanUntilTimeString:String;
var
	Years		:Word;
	Months		:Word;
	Days		:Word;
	Hours		:Word;
	Minutes		:Word;
	Seconds		:Word;
	MillionSeconds   :Word;
	Glue		:String;
	//Add "s" to end if Count > 1

	function Plural(Const Count:Word; Const Name:String):String;
	begin
		if Count > 1 then
			Result := Name +'s'
		else
			Result := Name
	end;

begin
	//Decode the remaining time to Years,Months,Days,Hours,Minutes,Seconds, and MillionSeconds
	DecodeDateTime(UnixToDateTime(DateTimeToUnix(BannedUntil)-DateTimeToUnix(Now)), Years, Months, Days, Hours, Minutes, Seconds, MillionSeconds);
	//Need to do something related to UNIX TIME
	Dec(Years, 1970);
	Dec(Months);
	Dec(Days);
	//Glue up between 2 unit of time
	Glue := ' ';
	Result := '';
		//Onlys shows Years, because months is 0
		if (Years > 0) and (Months = 0) then
		begin
			Result := Result + InttoStr(Years) + ' ' + Plural(Years, 'year');
		//Shows both years and months
		end else if (Years > 0) and (Months > 0) then
		begin
			Result := Result + InttoStr(Years) + ' ' + Plural(Years, 'year') + Glue + InttoStr(Months) + ' ' + Plural(Months, 'month');
		//Shows only months
		end else if (Months > 0) and (Days = 0) then
		begin
			Result := Result + InttoStr(Months) + ' ' + Plural(Months, 'month');
		//Shows months and days
		end else if (Months > 0) and (Days > 0) then
		begin
			Result := Result + InttoStr(Months) + ' ' + Plural(Months, 'month') + Glue + InttoStr(Days) + ' ' + Plural(Days, 'day');
		//Shows days only
		end else if (Days > 0) and (Hours = 0) then
		begin
			Result := Result + InttoStr(Days) + ' ' + Plural(Days, 'day');
		//Shows days and hours
		end else if (Days > 0) and (Hours > 0) then
		begin
			Result := Result + InttoStr(Days) + ' ' + Plural(Days, 'day') + Glue + InttoStr(Hours) + ' ' + Plural(Hours, 'hour');
		//Shows hours online
		end else if (Hours > 0) and (Minutes = 0) then
		begin
			Result := Result + InttoStr(Hours) + ' ' + Plural(Hours, 'hour');
		//Shows hours and minutes
		end else if (Hours > 0) and (Minutes > 0) then
		begin
			Result := Result + InttoStr(Hours) + ' ' + Plural(Hours, 'hour') + Glue + InttoStr(Minutes) + ' ' + Plural(Minutes, 'minute');
		//Shows only minutes
		end else if (Minutes > 0) and (Seconds = 0) then
		begin
			Result := Result + InttoStr(Minutes) + ' ' + Plural(Minutes, 'minute');
		//Shows minutes and seconds
		end else if (Minutes > 0) and (Seconds > 0) then
		begin
			Result := Result + InttoStr(Minutes) + ' ' + Plural(Minutes, 'minute') + Glue + InttoStr(Seconds) + ' ' + Plural(Seconds, 'second');
		//And.. only seconds left
		end else if Seconds > 0 then
		begin
			Result := Result + InttoStr(Seconds) + ' ' + Plural(Seconds, 'second');
		end;
		//Not sure how this going to be trigger, but incase it does, just show 1 second...
		if Result='' then
			Result := '1 second';
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Create                                                             CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Creates our account and sets up the clientinfo
//
//	Changes -
//		March 27th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Constructor TAccount.Create(AClient : TIdContext);
begin
	inherited Create;
	ClientInfo := AClient;
end;{SetConnectUntilTime}
//------------------------------------------------------------------------------


end.
