unit Globals;

interface
uses
	Classes;

	procedure InitGlobals;
	procedure DestroyGlobals;

var
	AccountList : TStringList;
	CharaServerList : TStringList;
	AppPath : String;

implementation

procedure InitGlobals;
begin
	AccountList := TStringList.Create;
	CharaServerList := TStringList.Create;
end;

procedure DestroyGlobals;
begin
	AccountList.Free;
	CharaServerList.Free;
end;

end.
