unit AccountDB;

interface
	uses
		GameObjects,
		AccountTypes,
		Classes;

		function FindAccount(Name : string) : TAccount; //txt searching, no massive load
		procedure GetLastAccountIDAndCount;

implementation
uses
	SysUtils,
	StrUtils,
	Globals,
	Math,
	Console;

function LoadAccountInfo(accountfile : string; Name : String) : TAccount;
var
	AccFile : TStringlist;
	AnAccount : TAccount;
begin
	Result := nil;
	AccFile := TStringList.Create;

	AccFile.LoadFromFile(accountfile);
	if Name = AccFile.Values['UserName'] then begin
		AnAccount := TAccount.Create;
		AnAccount.Username := AccFile.Values['UserName'];
		AnAccount.ID := StrToIntDef(AccFile.Values['ID'],0);
		AnAccount.Username := AccFile.Values['UserName'];
		AnAccount.Password := AccFile.Values['Password'];
		AnAccount.Banned :=  StrToBoolDef(AccFile.Values['Banned'],false);
		AnAccount.EMail := AccFile.Values['Email'];

		if ((LowerCase(AccFile.Values['Gender']) <> 'male') and
			(LowerCase(AccFile.Values['Gender']) <> 'female')) or
			(AnAccount.ID = 0) then begin
			MainProc.Console('       ****Error with account file ' + accountfile + #13);
			Result := nil;
			AnAccount.Free;
		end else
		begin
			if (LowerCase(AccFile.Values['Gender']) = 'male') then
				AnAccount.Gender := 1
			else AnAccount.Gender := 0;
			Result := AnAccount;
			AccountList.AddObject(IntToStr(AnAccount.ID),AnAccount);
		end;
	end;
	AccFile.Free;
end;

function SearchAccountFiles(Name : String) : TAccount;
var
	searchResult : TSearchRec;
begin
	Result := nil;
	if FindFirst(AppPath + 'save/accounts/*.txt', faAnyFile, searchResult) = 0 then
	begin
		repeat
			Result := LoadAccountInfo(AppPath + 'save/accounts/' + searchResult.Name, Name);
			if Assigned(Result) then break;
		until FindNext(searchResult) <> 0;
		// Must free up resources used by these successful finds
		FindClose(searchResult);
	end;
end;

function FindAccount(Name : string) : TAccount;
var
	idx : integer;
begin
	//Check Memory
	if not Assigned(AccountList) then Accountlist := Tstringlist.Create;
	for idx := 0 to AccountList.Count -1 do
	begin
		if TAccount(AccountList.Objects[idx]).Username = Name then
		begin
			Result := TAccount(AccountList.Objects[idx]);
			exit;
		end;
	end;
	//Attempt to search txt files.  Could do a SQL query here if needed and load
	//into memory perhaps.
	Result := SearchAccountFiles(Name);
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



