unit Database;

interface

type
	TDatabase = Class
 public
		function LoadAccounts : boolean;
		function Reload : string;
	private
		function LoadAccountInfo(accountfile : string) : boolean;
		function ReadAccountFiles : boolean;
end;

implementation
uses
	SysUtils,
	Globals,
	Classes,
	AccountTypes,
	Console;

function TDatabase.LoadAccountInfo(accountfile : string) : boolean;
var
	AccFile : TStringlist;
	AnAccount : TAccount;
begin
	Result := TRUE;
	AccFile := TStringList.Create;
	AnAccount := TAccount.Create;

	AccFile.LoadFromFile(accountfile);
	AnAccount.ID := StrToIntDef(AccFile.Values['ID'],0);
	AnAccount.Username := AccFile.Values['UserName'];
	AnAccount.Password := AccFile.Values['Password'];
	AnAccount.Banned :=  StrToBoolDef(AccFile.Values['Banned'],false);
	AnAccount.EMail := AccFile.Values['Email'];

	if ((LowerCase(AccFile.Values['Gender']) <> 'male') and
		(LowerCase(AccFile.Values['Gender']) <> 'female')) or
		(AnAccount.ID = 0) then begin
		MainProc.Console('       ****Error with account file ' + accountfile + #13);
		Result := FALSE;
		AnAccount.Free;
	end else
	begin
		if (LowerCase(AccFile.Values['Gender']) = 'male') then
			AnAccount.Gender := 1
		else AnAccount.Gender := 0;
		AccountList.AddObject(IntToStr(AnAccount.ID),AnAccount);
	end;
	AccFile.Free;
end;

function TDatabase.ReadAccountFiles : boolean;
var
	searchResult : TSearchRec;
begin
	Result := true;
	if FindFirst(AppPath + 'save/accounts/*.txt', faAnyFile, searchResult) = 0 then
	begin
		repeat
			if not LoadAccountInfo(AppPath + 'save/accounts/' + searchResult.Name) then
			begin
				Result := false;
				break;
			end;
		until FindNext(searchResult) <> 0;
		// Must free up resources used by these successful finds
		FindClose(searchResult);
	end;
end;

function TDatabase.LoadAccounts : boolean;
begin
	MainProc.Console('  - Accounts Loading');
	if not DirectoryExists(AppPath + 'save') then
	begin
		if not CreateDir(AppPath + 'save') then
		begin
			MainProc.Console(' Saved folder could not be created.  Exiting.');
			Result := false;
			exit;
		end;
	end;
	if not DirectoryExists(AppPath + 'save/accounts') then
	begin
		if not CreateDir(AppPath + 'save/accounts') then
		begin
			MainProc.Console(' Saved accounts folder could not be created.  Exiting.');
			Result := false;
			exit;
		end;
	end;
	Result := ReadAccountFiles;
	if not Result then
	begin
		MainProc.Console('    - Account load failed: Either Database is severely corrupted or does not exist.');
	end else
	begin
		MainProc.Console(Format('    - Account load success: %d accounts loaded successfully.', [AccountList.Count]));
    MainProc.Console('');
		 //Added to display accounts loaded on statusbar. - RaX
	end;
end;

function TDatabase.Reload() : String;
	begin
		Result := '';
		AccountList.Free;//Changed from AccountList.Clear() to Free to fix apparent Memory Leak - RaX
		AccountList := TStringList.Create;
		if not LoadAccounts then begin
			Result := 'Failed loading accounts, check your gamedata';
		end;
	end;

end.



