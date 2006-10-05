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
  
	function  ConnectToMySQL : Boolean;
	function  GetMD5(const Input : UTF8string) : UTF8String;

var
	HeliosVersion   : String = 'Helios Ragnarok Server Version 0.0.0.31';
	Command         : TCommands;
	SQLConnection   : TMySQLClient;
	SQLQueryResult  : TMySQLResult;
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
ConnectToMySQL

Connects the server to the MySQL table, creating the two main objects,
 SQLConnection and SQLQueryResult.

[2006/07/06] Tsusai - Reanmed to ConnectToMySQL.
------------------------------------------------------------------------------*)
{ TODO 1 -cMySQL : Replace default values with INI loaded ones }
function ConnectToMySQL : boolean;
var
	Success : Boolean;
begin
	Result  := FALSE;
	Success := FALSE;
	if Not Assigned(SQLConnection) then
	begin
		SQLConnection := TMySQLClient.Create;
	end;
	SQLConnection.Host            := ServerConfig.MySQLHost;
	SQLConnection.Port            := ServerConfig.MySQLPort;
	SQLConnection.Db              := ServerConfig.MySQLDB;
	SQLConnection.User            := ServerConfig.MySQLUser;
	SQLConnection.Password        := ServerConfig.MySQLPass;
	SQLConnection.ConnectTimeout  := 10;

	MainProc.Console(Format('  - Connecting to mySQL server.  Will abort after %d seconds',[SQLConnection.ConnectTimeout]));
	if SQLConnection.Connect then
	begin
		MainProc.Console(Format('  - SQL Connected to %s at port %d', [SQLConnection.Host, SQLConnection.Port]));
		SQLQueryResult := SQLConnection.query('SELECT * FROM `char` c;',true,Success);
		if Success then
		begin
			MainProc.Console('    - Test Query Success');
			MainProc.Console('      - Returned Rows : ' + IntToStr(SQLQueryResult.RowsCount)); //total rows of info
			MainProc.Console('      - Field Count   : ' + IntToStr(SQLQueryResult.FieldsCount)); //total fields
			MainProc.Console('      - Chara Name    : ' + SQLQueryResult.FieldValue(3)); // gives name (even if blank)
			Result := true;
		end;
	end else begin
		MainProc.Console('*****Could not connect to mySQL database server.');
		MainProc.Console('*****All incoming client connections will be refused.');
	end;
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
	if MainProc.SQLConnected then
	begin
		SQLConnection.Close;
	end;
	SQLConnection.Free;
	SQLQueryResult.Free;
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
