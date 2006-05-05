unit Globals;

interface
uses
	Classes,
	Database;

	procedure InitGlobals;
	procedure DestroyGlobals;

var
	AccountList : TStringList;
	CharaServerList : TStringList;
	AppPath : String;
	ADatabase : TDatabase;

implementation

procedure InitGlobals;
begin
	AccountList := TStringList.Create;
	CharaServerList := TStringList.Create;
	ADatabase := TDatabase.Create;
end;

procedure DestroyGlobals;
begin
	AccountList.Free;
	CharaServerList.Free;
	ADatabase.Free;
end;

end.
