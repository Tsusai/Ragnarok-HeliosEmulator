(*------------------------------------------------------------------------------
Globals
Tsusai July 2006

Description:
 Contains almost all Global variables and lists for the entire project
------------------------------------------------------------------------------*)
unit Globals;

interface
uses
	//IDE
	Classes,
	//Helios
	Commands,
	ServerOptions,
	//3rd Party
	List32;

	procedure InitGlobals;
	procedure DestroyGlobals;
	procedure TerminateApplication;

	function  GetMD5(const Input : UTF8string) : UTF8String;
	function  MakeRNDString(Count: Integer): string;
	function  ConvertMySQLTime(DateString: string) : TDateTime;

var
	Command         : TCommands;
	CharaServerList : TStringList;
	AppPath         : String;

	ServerConfig    : TServerOptions;

	AccountList     : TStringList;
	CharacterList   : TIntList32;

	LastAccountID   : Integer;

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

(*------------------------------------------------------------------------------
GetMD5

Hash a string

[2006/09/24] Tsusai - Added
------------------------------------------------------------------------------*)
function GetMD5(const Input : UTF8string) : UTF8String;
var
	MD5Hash : TIdHashMessageDigest5;
begin
	MD5Hash := TIdHashMessageDigest5.Create;
	Result := MD5Hash.AsHex(MD5Hash.HashValue(Input));
end;

(*------------------------------------------------------------------------------
InitGlobals

Creates all needed global objects

------------------------------------------------------------------------------*)
procedure InitGlobals;
begin
	AccountList     := TStringList.Create;
	CharacterList   := TIntList32.Create;
	CharaServerList := TStringList.Create;
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
end; (* proc InitGlobals
------------------------------------------------------------------------------*)

(*------------------------------------------------------------------------------
DestroyGlobals

Frees all global objects
------------------------------------------------------------------------------*)
procedure DestroyGlobals;
begin
	AccountList.Free;
	CharacterList.Free;
	ServerConfig.Save;
	ServerConfig.Free;
	CharaServerList.Free;
end; (* proc DestroyGlobals
------------------------------------------------------------------------------*)

procedure TerminateApplication;
begin
	KillTerminationCapturing;
	Command.Free;
	MainProc.Shutdown;
	FreeAndNil(MainProc);
	KillProcess;
end;

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
end;

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
end;

end.
