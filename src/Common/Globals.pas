unit Globals;

interface
uses
	Classes,
	Database,
	ServerOptions;

	procedure InitGlobals;
	procedure DestroyGlobals;

var
	CharaServerList : TStringList;
	AppPath : String;
	ADatabase : TDatabase;

	ServerConfig : TServerOptions;

implementation
	uses
		SysUtils;

procedure InitGlobals;
begin

	CharaServerList := TStringList.Create;
	ADatabase := TDatabase.Create;
	ServerConfig := TServerOptions.Create('./ServerOptions.ini');
	ServerConfig.Load;
end;

procedure DestroyGlobals;
begin
	ServerConfig.Save;
	ServerConfig.Free;
	CharaServerList.Free;
	ADatabase.Free;
end;

end.
