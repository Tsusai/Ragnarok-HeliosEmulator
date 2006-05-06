unit Globals;

interface
uses
	Classes,
	Database,
	ServerOptions;

	procedure InitGlobals;
	procedure DestroyGlobals;

var
	AccountList : TStringList;
	CharaServerList : TStringList;
	AppPath : String;
	ADatabase : TDatabase;

	ServerConfig : TServerOptions;

implementation
	uses
		SysUtils;

procedure InitGlobals;
begin
	AccountList := TStringList.Create;
	CharaServerList := TStringList.Create;
	ADatabase := TDatabase.Create;
	ServerConfig := TServerOptions.Create('./ServerOptions.ini');
	ServerConfig.Load;
end;

procedure DestroyGlobals;
begin
	ServerConfig.Save;
	ServerConfig.Free;
	AccountList.Free;
	CharaServerList.Free;
	ADatabase.Free;
end;

end.
