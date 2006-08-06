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
	ServerOptions,
	//3rd Party
	List32,
	uMysqlClient;

	procedure InitGlobals;
	procedure DestroyGlobals;

var
	SQLConnection : TMySQLClient;
	SQLQueryResult : TMySQLResult;
	CharaServerList : TStringList;
	AppPath : String;

	ServerConfig : TServerOptions;

	AccountList : TStringList;
	CharacterList : TIntList32;

	LastAccountID : Integer;

implementation
	uses
		//IDE
		SysUtils,
		//Helios
		Console;

(*------------------------------------------------------------------------------
ConnectToMySQL

Connects the server to the MySQL table, creating the two main objects,
 SQLConnection and SQLQueryResult.

[2006/07/06] Tsusai - Reanmed to ConnectToMySQL.
------------------------------------------------------------------------------*)
{ TODO 1 -cMySQL : Replace default values with INI loaded ones }
procedure ConnectToMySQL;
var
	Success : boolean;
begin
	Success := false;
	if Not Assigned(SQLConnection) then
	begin
		SQLConnection := TMysqlClient.create;
	end;
	SQLConnection.Host := ServerConfig.MySQLHost;
	SQLConnection.Port := ServerConfig.MySQLPort;
	SQLConnection.Db   := ServerConfig.MySQLDB;
	SQLConnection.User := ServerConfig.MySQLUser;
	SQLConnection.Password := ServerConfig.MySQLPass;
	SQLConnection.ConnectTimeout := 60;

	MainProc.Console('  - Connecting to mySQL server.  Will abort after 60 seconds');
	if SQLConnection.Connect then
	begin
		MainProc.Console(Format('  - SQL Connected to %s at port %d', [SQLConnection.Host, SQLConnection.Port]));
		MainProc.Console('    Accounts   : ' + IntToStr(SQLConnection.query('SELECT * FROM `login`',true,Success).RowsCount));
		MainProc.Console('    Characters : ' + IntToStr(SQLConnection.query('SELECT * FROM `char`',true,Success).RowsCount));
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
	AccountList := TStringList.Create;
	CharacterList := TIntList32.Create;
	CharaServerList := TStringList.Create;
	ServerConfig := TServerOptions.Create('./ServerOptions.ini');
	ServerConfig.Load;
	ConnectToMySQL; //takes care of SQLConnection and SQLQueryResult
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
	SQLConnection.close;
	SQLConnection.Free;
	SQLQueryResult.Free;
end; (* proc DestroyGlobals
------------------------------------------------------------------------------*)

end.
