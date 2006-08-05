(*------------------------------------------------------------------------------
AccountDB
Tsusai July 2006

Description:
 Should handle all account finding procedures
------------------------------------------------------------------------------*)
unit AccountDB;

interface
	uses
		Account;

		function FindAccountByName(Name : string) : TAccount;
		function FindAccountByID(ID : Cardinal) : TAccount;
		procedure GetLastAccountIDAndCount;

implementation
uses
	Classes,
	SysUtils,
	Globals,
	Math,
	Console;

(*------------------------------------------------------------------------------
FindAccountByName

Locates an account and returns a TAccount object.
Tries to find the object in memory first, then procedes to load basics from SQL.

[2006/07/06] Tsusai - Fixed bug of not finding it in memory, with IndexofName
 not working for an odd reason....
------------------------------------------------------------------------------*)
function FindAccountByName(Name : string) : TAccount;
var
	Success : boolean;
	AnAccount : TAccount;
	idx : integer;
begin
	Result := nil;
	//Check Memory
	if not Assigned(AccountList) then Accountlist := Tstringlist.Create;
	for idx := 0 to AccountList.Count -1 do begin
		if TAccount(AccountList.Objects[idx]).Username = Name then
		begin
			Result := TAccount(AccountList.Objects[idx]);
			exit;
		end;
	end;

	SQLQueryResult := SQLConnection.query('SELECT * FROM login WHERE userid = "'+Name+'";',true,Success);
	if Success and (SQLqueryResult.RowsCount = 1) then begin
		MainProc.Console('Account found in chara server');
		AnAccount := TAccount.Create;
		AnAccount.ID := StrToInt(SQLQueryResult.FieldValue(0));
		AnAccount.Username := SQlQueryResult.FieldValue(1);
		AnAccount.Password := SQlQueryResult.FieldValue(2);
		AnAccount.Gender   := SQLQueryResult.FieldValue(4)[0];
		AnAccount.EMail    := SQLQueryResult.FieldValue(6);
		AccountList.AddObject(AnAccount.Username, AnAccount);
		Result := AnAccount;
	end;
end; (* func FindAccount
------------------------------------------------------------------------------*)

(*------------------------------------------------------------------------------
FindAccountByID

Locates an account and returns a TAccount object.
Tries to find the object in memory first, then procedes to load basics from SQL.
-----------------------------------------------------------------------------*)
function FindAccountByID(ID : Cardinal) : TAccount;
var
	Success : boolean;
	AnAccount : TAccount;
	idx : integer;
begin
	Result := nil;
	//Check Memory
	if not Assigned(AccountList) then Accountlist := Tstringlist.Create;
	for idx := 0 to AccountList.Count -1 do begin
		if TAccount(AccountList.Objects[idx]).ID = ID then
		begin
			Result := TAccount(AccountList.Objects[idx]);
			exit;
		end;
	end;

	SQLQueryResult := SQLConnection.query('SELECT * FROM login WHERE account_id = "'+IntToStr(ID)+'";',true,Success);
	if Success and (SQLqueryResult.RowsCount = 1) then begin
		MainProc.Console('Account found in chara server');
		AnAccount := TAccount.Create;
		AnAccount.ID := StrToInt(SQLQueryResult.FieldValue(0));
		AnAccount.Username := SQlQueryResult.FieldValue(1);
		AnAccount.Password := SQlQueryResult.FieldValue(2);
		AnAccount.Gender   := SQLQueryResult.FieldValue(4)[0];
		AnAccount.EMail    := SQLQueryResult.FieldValue(6);
		AccountList.AddObject(AnAccount.Username, AnAccount);
		Result := AnAccount;
	end;
end; (* func FindAccount
------------------------------------------------------------------------------*)

(*------------------------------------------------------------------------------
GetLastAccountIDAndCount

TXT Legacy.  Placeholder till i can figure out how to retrieve the largest
ID from SQL (i'm no query master)
------------------------------------------------------------------------------*)
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
end;(* proc GetLastAccountIDAndCount
------------------------------------------------------------------------------*)

end.



