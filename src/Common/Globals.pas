unit Globals;

interface
uses
	Classes,
	ServerOptions,
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
	CharacterList : TStringList;

	LastAccountID : Integer;

implementation
	uses
		SysUtils,
		Console;

procedure ConnectToSQL;
var
	Success : boolean;
begin
	Success := false;
	SQLConnection := TMysqlClient.create;
	SQLConnection.Host := 'localhost';
	SQLConnection.User := 'root';
	SQLConnection.Password := 'helios';
	SQLConnection.Db := 'Helios';
	SQLConnection.ConnectTimeout := 60;

	MainProc.Console('  - Connecting to mySQL server.  Will abort after 60 seconds');
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
		end;
	end else begin
		MainProc.Console('*****Could not connect to mySQL database server.');
		MainProc.Console('*****All incoming client connections will be refused.');
	end;
end;


procedure InitGlobals;
begin
	AccountList := TStringList.Create;
	CharacterList := TStringList.Create;
	CharaServerList := TStringList.Create;
	ServerConfig := TServerOptions.Create('./ServerOptions.ini');
	ServerConfig.Load;
	ConnectToSQL;
end;

procedure DestroyGlobals;
begin
	AccountList.Free;
	CharacterList.Free;
	ServerConfig.Save;
	ServerConfig.Free;
	CharaServerList.Free;
	SQLConnection.Free;
	SQLQueryResult.Free;
end;

end.
