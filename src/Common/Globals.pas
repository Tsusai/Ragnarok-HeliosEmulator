//------------------------------------------------------------------------------
//Globals                                                                  UNIT
//------------------------------------------------------------------------------
//
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit Globals;

interface
uses
	//IDE
	Classes,
	//Helios
	Commands,
	ServerOptions,
  CharaList
  ;


	procedure InitGlobals;
	procedure DestroyGlobals;
	procedure TerminateApplication;

	function  GetMD5(const Input : string) : string;
	function  MakeRNDString(Count: Integer): string;
	function  ConvertMySQLTime(DateString: string) : TDateTime;
	function IncSecond(const AValue: TDateTime; const ANumberOfSeconds: Int64): TDateTime;
	function IncMinute(const AValue: TDateTime; const ANumberOfMinutes: Int64): TDateTime;

//------------------------------------------------------------------------------
//                              Global Variables
//------------------------------------------------------------------------------
var
	Command         : TCommands;
	AppPath         : String;

	ServerConfig    : TServerOptions;

	AccountList     : TStringList;
	CharacterList   : TCharacterList;

	LastAccountID   : Integer;
//------------------------------------------------------------------------------


implementation
	uses
		//IDE
		DateUtils,
		SysUtils,
		//Helios
		WinLinux,
		Console,
		//3rd Party
		IdHashMessageDigest;

Const
	HoursPerDay   = 24;
	MinsPerHour   = 60;
	MinsPerDay    = HoursPerDay * MinsPerHour;

//------------------------------------------------------------------------------
//GetMD5                                                               FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Creates a MD5 Hash from a string.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function GetMD5(const Input : string) : String;
var
	MD5Hash : TIdHashMessageDigest5;
begin
	MD5Hash := TIdHashMessageDigest5.Create;
	Result := MD5Hash.AsHex(MD5Hash.HashValue(Input));
end;{GetMD5}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//InitGlobals                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our global variables.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure InitGlobals;
begin
	AccountList     := TStringList.Create;
	CharacterList   := TCharacterList.Create(True);
	if ParamCount = 0 then
	begin
		ServerConfig    := TServerOptions.Create('./ServerOptions.ini');
	end else
	if ParamCount = 1 then
	begin
		MainProc.Console('USING REMOTE INI : ' + ParamStr(1));
		ServerConfig    := TServerOptions.Create('./' + ParamStr(1));
	end;
	ServerConfig.Load;
end; {InitGlobals}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//DestroyGlobals                                                      PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Free's up our Globals variables.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure DestroyGlobals;
begin
	AccountList.Free;
	CharacterList.Free;
	ServerConfig.Save;
	ServerConfig.Free;
end;{DestroyGlobals}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TerminateApplication                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Kills our main process and free's it.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TerminateApplication;
begin
	KillTerminationCapturing;
	Command.Free;
	MainProc.Shutdown;
	FreeAndNil(MainProc);
	KillProcess;
end;{TerminateApplication}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//MakeRNDString                                                       FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Makes a random string of length Count.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function MakeRNDString(Count: Integer): string;
const
	chars = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/';
var
	i, x: integer;
begin
	Result := '';
	for i := 1 to Count do
	begin
		x := Length(chars) - Random(Length(chars));
		Result := Result + chars[x];
	end;
end;{MakeRNDString}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ConvertMySQLTime                                                     FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Converts a MySQL Formatted Time into a TDateTime.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function ConvertMySQLTime(DateString: string) : TDateTime;
var
	Year, Month, Day, Hour, Min, Sec : word;
	Code : integer;
begin
	Result := 2;   //01 Jan 1900
	if Length(DateString) = 19 then
	begin
		if not (DateString = '0000-00-00 00:00:00') then
		begin
			Val(copy(DateString, 1, 4), Year, Code);
			Val(copy(DateString, 6, 2), Month, Code);
			Val(copy(DateString, 9, 2), Day, Code);
			Val(copy(DateString, 12, 2), Hour, Code);
			Val(copy(DateString, 15, 2), Min, Code);
			Val(copy(DateString, 18, 2), Sec, Code);
			Result := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0);
		end;
	end;
end;{ConvertMySQLTime}
//------------------------------------------------------------------------------

function IncSecond(const AValue: TDateTime;
	const ANumberOfSeconds: Int64): TDateTime;
begin
	Result := ((AValue * SecsPerDay) + ANumberOfSeconds) / SecsPerDay;
end;

function IncMinute(const AValue: TDateTime;
	const ANumberOfMinutes: Int64): TDateTime;
begin
  Result := ((AValue * MinsPerDay) + ANumberOfMinutes) / MinsPerDay;
end;

end.
