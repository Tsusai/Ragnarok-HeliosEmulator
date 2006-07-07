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

	WriteLn('  - Connecting to mySQL server.  Will abort after 60 seconds');
	if SQLConnection.Connect then
	begin
		writeln(Format('  - SQL Connected to %s at port %d', [SQLConnection.Host, SQLConnection.Port]));
		SQLQueryResult := SQLConnection.query('SELECT * FROM `char` c;',true,Success);
		if Success then
		begin
			Writeln('    - Test Query Success');
			WriteLn('      - Returned Rows : ' + IntToStr(SQLQueryResult.RowsCount)); //total rows of info
			WriteLn('      - Field Count   : ' + IntToStr(SQLQueryResult.FieldsCount)); //total fields
			WriteLn('      - Chara Name    : ' + SQLQueryResult.FieldValue(3)); // gives name (even if blank)
		end;
	end else begin
		WriteLn('*****Could not connect to mySQL database server.');
		Writeln('*****All incoming client connections will be refused.');
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
