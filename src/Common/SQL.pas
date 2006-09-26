unit SQL;

interface

	function GetSQLDataStr(
		Table     : String;
		ID        : Cardinal;
		Param     : String
	) : String;

	function GetSQLDataInt(
		Table     : String;
		ID        : Cardinal;
		Param     : String
	) : Cardinal;

	procedure SetSQLDataStr(
		Table     : String;
		ID        : Cardinal;
		Param     : String;
		NewValue  : String
	);

	procedure SetSQLDataInt(
		Table     : String;
		ID        : Cardinal;
		Param     : String;
		NewValue  : Cardinal
	);

implementation
uses
	//IDE
	SysUtils,
	Math,
	//Helios
	Globals;

function GetSQLDataStr(
	Table : String;
	ID    : Cardinal;
	Param : String
) : String;
var
	Success : Boolean;
begin
	Result := '';
	SQLQueryResult :=
		SQLConnection.query(
			Format('SELECT %s FROM `%s` WHERE char_id = %d;',[Param,Table,ID]),
			TRUE,Success
		);
	if Success and
		(SQLQueryResult.FieldsCount = 1) and
		(SQLQueryResult.RowsCount = 1) then
	begin
		Result := SQLQueryResult.FieldValue(0);
	end;
end;

function GetSQLDataInt(
	Table : String;
	ID    : Cardinal;
	Param : String
) : Cardinal;
var
	Success : Boolean;
begin
	Result := 0;
	SQLQueryResult :=
		SQLConnection.query(
			Format('SELECT %s FROM `%s` WHERE char_id = %d;',[Param,Table,ID]),
			TRUE,Success
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
	Table     : String;
	ID        : Cardinal;
	Param     : String;
	NewValue  : String
);
var
	Success   : Boolean;
begin
	SQLConnection.query(
		Format('UPDATE `%s` SET %s="%s" WHERE char_id = %d;',[Table,Param,NewValue,ID]),
		true,Success);
end;

procedure SetSQLDataInt(
	Table     : String;
	ID        : Cardinal;
	Param     : String;
	NewValue  : Cardinal
);
var
	Success : Boolean;
begin
	SQLConnection.query(
		Format('UPDATE `%s` SET %s=%d WHERE char_id = %d;',[Table,Param,NewValue,ID]),
		true,Success);
end;

end.