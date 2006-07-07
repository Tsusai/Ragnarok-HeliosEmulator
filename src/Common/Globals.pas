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

	LastAccountID : Integer;

implementation
	uses
		SysUtils;

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

	Console('  - Connecting to mySQL server.  Will abort after 60 seconds');
	if SQLConnection.Connect then
	begin
		Console(Format('  - SQL Connected to %s at port %d', [SQLConnection.Host, SQLConnection.Port]));
		SQLQueryResult := SQLConnection.query('SELECT * FROM `char` c;',true,Success);
		if Success then
		begin
			Console('    - Test Query Success');
			Console('      - Returned Rows : ' + IntToStr(SQLQueryResult.RowsCount)); //total rows of info
			Console('      - Field Count   : ' + IntToStr(SQLQueryResult.FieldsCount)); //total fields
			Console('      - Chara Name    : ' + SQLQueryResult.FieldValue(3)); // gives name (even if blank)
		end;
	end else begin
		Console('*****Could not connect to mySQL database server.');
		Console('*****All incoming client connections will be refused.');
	end;
end;


procedure InitGlobals;
begin
	AccountList := TStringList.Create;
	CharaServerList := TStringList.Create;
	ServerConfig := TServerOptions.Create('./ServerOptions.ini');
	ServerConfig.Load;
	ConnectToSQL;
end;

procedure DestroyGlobals;
begin
	AccountList.Free;
	ServerConfig.Save;
	ServerConfig.Free;
	CharaServerList.Free;
	SQLConnection.Free;
	SQLQueryResult.Free;
end;

end.
