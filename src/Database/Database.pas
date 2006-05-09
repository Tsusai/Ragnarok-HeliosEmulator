unit Database;

interface
	uses
		Classes;
type
	TChara = record
	//
	end;

	TDatabase = Class
	constructor Create();
	public
		Loaded : Boolean;

		AccountList : TStringList;
		CharaList : array of TChara;

		procedure LoadAccounts;

		function Reload : string;
	private
		function LoadAccountInfo(accountfile : string) : boolean;
		function ReadAccountFiles : boolean;
end;

implementation
uses
	SysUtils,
	Globals,
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

procedure TDatabase.LoadAccounts;
begin
	Loaded := FALSE;//RaX - Assume Failure.
	MainProc.Console('  - Accounts Loading');
	if not DirectoryExists(AppPath + 'save') then
	begin
		if not CreateDir(AppPath + 'save') then
		begin
			MainProc.Console(' Saved folder could not be created.  Exiting.');
			Loaded := false;
			exit;
		end;
	end;
	if not DirectoryExists(AppPath + 'save/accounts') then
	begin
		if not CreateDir(AppPath + 'save/accounts') then
		begin
			MainProc.Console(' Saved accounts folder could not be created.  Exiting.');
			Loaded := false;
			exit;
		end;
	end;
	Loaded := ReadAccountFiles;
	if not Loaded then
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

		//Reload account list
		AccountList.Free;
		AccountList := TStringList.Create;
		LoadAccounts;
		if not Loaded then begin
			Result := 'Failed loading accounts, check your gamedata';
			Exit;
		end;
		//End Reload AccountList

		//Reload Character List.

		//End Reload Character List.
	end;

constructor TDatabase.Create();
begin
	AccountList := TStringList.Create;
	LoadAccounts();
	//Add additional loads (IE::LoadCharacters) here.
end;
end.



