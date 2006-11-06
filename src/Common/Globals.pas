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
	List32,
	uMysqlClient;

	procedure InitGlobals;
	procedure DestroyGlobals;
	procedure TerminateApplication;

	function  GetMD5(const Input : UTF8string) : UTF8String;

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
	ServerConfig    := TServerOptions.Create('./ServerOptions.ini');
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

end.
