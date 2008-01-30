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
	IdContext;
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
		Username        : string[24];
		Password        : string[24];
		EMail           : string[24];
		GenderNum       : Byte; //0 or 1 for packet (F/M respectively)
		Bantime         : TDateTime;
		UnBanDateTime   : string;
		LastIP          : string[15];
		LoginKey        : array [1..2] of LongWord;
		CharaID         : array [0..8] of LongWord;
		LoginCount      : Integer;
		LastLoginTime   : TDateTime;
		Level           : Byte;
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
		procedure TemperaryBan(Seconds:Integer);
		function GetBanUntilTimeString:String;
		//2 functions bellow is made by Thomas Greiner
		//http://www.swissdelphicenter.ch/torry/showcode.php?id=844
		function DateTimeToUnix(ConvDate: TDateTime): Longint;
		function UnixToDateTime(USec: Longint): TDateTime;
		Constructor Create(AClient : TIdContext);
	end;{TAccount}
//------------------------------------------------------------------------------


implementation
uses
	SysUtils,
	Globals,
	PacketTypes,
	DateUtils;

const
	// Sets UnixStartDate to TDateTime of 01/01/1970
	UnixStartDate: TDateTime = 25569.0;
//------------------------------------------------------------------------------
//SetGender                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Set fGender in local Class (M = 1 , F = 0)
//    GenderNum at.
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
	TThreadLink(ClientInfo.Data).DatabaseLink.CommonData.SaveAccount(self);
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
procedure TAccount.TemperaryBan(Seconds:Integer);
begin
	Self.Bantime := UnixToDateTime(DateTimeToUnix(Now) + Seconds);
	TThreadLink(ClientInfo.Data).DatabaseLink.CommonData.SaveAccount(self);
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
	TThreadLink(ClientInfo.Data).DatabaseLink.CommonData.SaveAccount(self);
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
	DecodeDateTime(UnixToDateTime(DateTimeToUnix(BanTime)-DateTimeToUnix(Now)), Years, Months, Days, Hours, Minutes, Seconds, MillionSeconds);
	//Need to do something related to UNIX TIME
	Dec(Years, 1970);
	Dec(Months);
	Dec(Days);
	//Glue up between 2 time
	Glue := ' ';
        Result := '';
		//Onlys shows Years, because months is 0
		if (Years > 0) and (Months = 0) then
		begin
			Result := Result + InttoStr(Years) + ' ' + Plural(Years, 'Year');
		//Shows both years and months
		end else if (Years > 0) and (Months > 0) then
		begin
			Result := Result + InttoStr(Years) + ' ' + Plural(Years, 'Year') + Glue + InttoStr(Months) + ' ' + Plural(Months, 'Month');
		//Shows only months
		end else if (Months > 0) and (Days = 0) then
		begin
			Result := Result + InttoStr(Months) + ' ' + Plural(Months, 'Month');
		//Shows months and days
		end else if (Months > 0) and (Days > 0) then
		begin
			Result := Result + InttoStr(Months) + ' ' + Plural(Months, 'Month') + Glue + InttoStr(Days) + ' ' + Plural(Days, 'Day');
		//Shows days only
		end else if (Days > 0) and (Hours = 0) then
		begin
			Result := Result + InttoStr(Days) + ' ' + Plural(Days, 'Day');
		//Shows days and hours
		end else if (Days > 0) and (Hours > 0) then
		begin
			Result := Result + InttoStr(Days) + ' ' + Plural(Days, 'Day') + Glue + InttoStr(Hours) + ' ' + Plural(Hours, 'Hour');
		//Shows hours online
		end else if (Hours > 0) and (Minutes = 0) then
		begin
			Result := Result + InttoStr(Hours) + ' ' + Plural(Hours, 'Hour');
		//Shows hours and minutes
		end else if (Hours > 0) and (Minutes > 0) then
		begin
			Result := Result + InttoStr(Hours) + ' ' + Plural(Hours, 'Hour') + Glue + InttoStr(Minutes) + ' ' + Plural(Minutes, 'Minute');
		//Shows only minutes
		end else if (Minutes > 0) and (Seconds = 0) then
		begin
			Result := Result + InttoStr(Minutes) + ' ' + Plural(Minutes, 'Minute');
		//Shows minutes and seconds
		end else if (Minutes > 0) and (Seconds > 0) then
		begin
			Result := Result + InttoStr(Minutes) + ' ' + Plural(Minutes, 'Minute') + Glue + InttoStr(Seconds) + ' ' + Plural(Seconds, 'Second');
		//And.. only seconds left
		end else if Seconds > 0 then
		begin
			Result := Result + InttoStr(Seconds) + ' ' + Plural(Seconds, 'Second');
		end;
		//Not sure how this going to be trigger, but incase it does, just show 1 second...
		if Result='' then
			Result := '1 Second';
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//DateTimeToUnix                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Convert TDateTime to Unix Time
//
//	Changes -
//		April 12th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
function TAccount.DateTimeToUnix(ConvDate: TDateTime): Longint;
begin
	//example: DateTimeToUnix(now);
	Result := Round((ConvDate - UnixStartDate) * 86400);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//UnixToDateTime                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Convert Unix Time to TDateTime
//
//	Changes -
//		April 12th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
function TAccount.UnixToDateTime(USec: Longint): TDateTime;
begin
	//Example: UnixToDateTime(1003187418);
	Result := (Usec / 86400) + UnixStartDate;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Create			                                                 		CONSTRUCTOR
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
