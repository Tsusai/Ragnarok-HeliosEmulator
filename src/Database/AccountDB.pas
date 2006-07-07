unit AccountDB;

interface
	uses
		GameObjects,
		AccountTypes,
		Classes;

		function FindAccount(Name : string) : TAccount;
		procedure GetLastAccountIDAndCount;

implementation
uses
	SysUtils,
	StrUtils,
	Globals,
	Math,
	Console;

function FindAccount(Name : string) : TAccount;
var
	idx : integer;
	Success : boolean;
	AnAccount : TAccount;
begin
	Result := nil;
	//Check Memory
	if not Assigned(AccountList) then Accountlist := Tstringlist.Create;
	if AccountList.IndexOfName(Name) > -1 then
	begin
		Result := TAccount(AccountList.Objects[AccountList.IndexOfName(Name)]);
		exit;
	end;

	SQLQueryResult := SQLConnection.query('SELECT * FROM login WHERE account_id = 100100;',true,Success);
	if Success then begin
		if SQLqueryResult.RowsCount = 1 then
		begin
			WriteLn('Account found in chara server');
			AnAccount := TAccount.Create;
			AnAccount.ID := StrToInt(SQLQueryResult.FieldValue(0));
			AnAccount.Username := SQlQueryResult.FieldValue(1);
			AnAccount.Password := SQlQueryResult.FieldValue(2);
			AnAccount.Gender   := SQLQueryResult.FieldValue(4)[0];
			AnAccount.EMail    := SQLQueryResult.FieldValue(6);
			AccountList.AddObject(AnAccount.Username, AnAccount);
			Result := AnAccount;
		end;
	end;
end;

procedure GetLastAccountIDAndCount;
var
	searchResult : TSearchRec;
	Count : integer;
	MaxID : Cardinal;
begin
	Count := 0;
	MaxID := 0;
		MainProc.Console('  - Account Count Loading');
	if not DirectoryExists(AppPath + 'save') then
	begin
		if not CreateDir(AppPath + 'save') then
		begin
			MainProc.Console(' Saved folder could not be created.  Exiting.');
			exit;
		end;
	end;
	if not DirectoryExists(AppPath + 'save/accounts') then
	begin
		if not CreateDir(AppPath + 'save/accounts') then
		begin
			MainProc.Console(' Saved accounts folder could not be created.  Exiting.');
			exit;
		end;
	end;
	begin
		if FindFirst(AppPath + 'save/accounts/*.txt', faAnyFile, searchResult) = 0 then
		begin
			repeat
				Inc(Count);
				MaxID := Max(MaxID,
					StrToIntDef(
					 Copy(searchResult.Name, 0, AnsiPos('.', searchResult.Name)-1)
					 ,0));
			until FindNext(searchResult) <> 0;
			// Must free up resources used by these successful finds
			FindClose(searchResult);
		end;
		LastAccountID := MaxID;
		MainProc.Console(Format('    - Account load success: %d accounts loaded successfully.', [Count]));
		MainProc.Console('');
		 //Added to display accounts loaded on statusbar. - RaX
	end;
end;

end.



