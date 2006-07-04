unit Globals;

interface
uses
	Classes,
	ServerOptions;

	procedure InitGlobals;
	procedure DestroyGlobals;

var
	CharaServerList : TStringList;
	AppPath : String;

	ServerConfig : TServerOptions;

	AccountList : TStringList;

	LastAccountID : Integer;

implementation
	uses
		SysUtils;

procedure InitGlobals;
begin
	AccountList := TStringList.Create;
	CharaServerList := TStringList.Create;
	ServerConfig := TServerOptions.Create('./ServerOptions.ini');
	ServerConfig.Load;
end;

procedure DestroyGlobals;
begin
	AccountList.Free;
	ServerConfig.Save;
	ServerConfig.Free;
	CharaServerList.Free;
end;

end.
