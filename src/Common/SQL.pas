unit SQL;

interface

	function GetSQLDataStr(
		Table : string;
		ID : Cardinal;
		Param : string
	) : string;

	function GetSQLDataInt(
		Table : string;
		ID : Cardinal;
		Param : string
	) : Cardinal;

	procedure SetSQLDataStr(
		Table : string;
		ID : Cardinal;
		Param : string;
		NewValue : string
	);

	procedure SetSQLDataInt(
		Table : string;
		ID : Cardinal;
		Param : string;
		NewValue : Cardinal
	);

implementation
uses
	//IDE
	SysUtils,
	Math,
	//Helios
	Globals;

function GetSQLDataStr(
	Table : string;
	ID : Cardinal;
	Param : string
) : string;
var
	Success : Boolean;
begin
	Result := '';
	SQLQueryResult :=
		SQLConnection.query(
			Format('SELECT %s FROM `%s` WHERE char_id = %d;',[Param,Table,ID]),
			true,Success
		);
	if Success and
		(SQLQueryResult.FieldsCount = 1) and
		(SQLQueryResult.RowsCount = 1) then
	begin
		Result := SQLQueryResult.FieldValue(0);
	end;
end;

function GetSQLDataInt(
	Table : string;
	ID : Cardinal;
	Param : string
) : Cardinal;
var
	Success : Boolean;
begin
	Result := 0;
	SQLQueryResult :=
		SQLConnection.query(
			Format('SELECT %s FROM `%s` WHERE char_id = %d;',[Param,Table,ID]),
			true,Success
		);
	if Success and
		(SQLQueryResult.FieldsCount = 1) and
		(SQLQueryResult.RowsCount = 1) then
	begin
		Result := EnsureRange(StrToIntDef(SQLQueryResult.FieldValue(0),0),Low(Cardinal),High(Cardinal));
	end;
	Result := EnsureRange(Result,Low(Cardinal),High(Cardinal));
end;

procedure SetSQLDataStr(
	Table : string;
	ID : Cardinal;
	Param : string;
	NewValue : string
);
var
	Success : Boolean;
begin
	SQLConnection.query(
		Format('UPDATE `%s` SET %s="%s" WHERE char_id = %d;',[Table,Param,NewValue,ID]),
		true,Success);
end;

procedure SetSQLDataInt(
	Table : string;
	ID : Cardinal;
	Param : string;
	NewValue : Cardinal
);
var
	Success : Boolean;
begin
	SQLConnection.query(
		Format('UPDATE `%s` SET %s=%d WHERE char_id = %d;',[Table,Param,NewValue,ID]),
		true,Success);
end;

end.