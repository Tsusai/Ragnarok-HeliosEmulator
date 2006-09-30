//------------------------------------------------------------------------------
//MySQLDatabase		                                                        UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is one of our database objects which enabled Helios to use a MySQL
//    Database.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit MySQLDatabase;

interface
uses
  Database,
  Character,
  Account;
type
//------------------------------------------------------------------------------
//TMySQLDatabase			                                                           CLASS
//------------------------------------------------------------------------------
//	What it does-
//			This is a child class for our database object system. It allows Helios
//    to communicate with a MySQL database and houses all routines for doing so.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
  TMySQLDatabase = class(TDatabase)
  public
    function GetAccount(ID    : Cardinal) : TAccount;overload;override;
    function GetAccount(Name  : string) : TAccount;overload;override;
    function GetChara(  ID    : Integer)   : TCharacter;override;

    procedure SaveAccount(AnAccount : TAccount);
    procedure SaveChara(AChara : TCharacter);


  end;
//------------------------------------------------------------------------------

implementation
  uses
    Globals,
    Console,
    SysUtils,
    Classes;

//------------------------------------------------------------------------------
//TMySQLDatabase.GetAccount()                               OVERLOADED FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			This function returns a TAccount type and is used for loading up an
//    account by ID.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
function TMySQLDatabase.GetAccount(ID: Cardinal) : TAccount;
var
	Success   : Boolean;
	AnAccount : TAccount;
  Index     : Integer;
begin
  Result := NIL;
  //Check Memory
	if not Assigned(AccountList) then Accountlist := TStringlist.Create;
	for Index := 0 to AccountList.Count -1 do begin
		if TAccount(AccountList.Objects[Index]).ID = ID then
		begin
			Result := TAccount(AccountList.Objects[Index]);
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
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLDatabase.GetAccount()                               OVERLOADED FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			This function returns a TAccount type and is used for loading up an
//    account by Name.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
function TMySQLDatabase.GetAccount(Name : string) : TAccount;
var
	Success   : Boolean;
	AnAccount : TAccount;
  Index     : Integer;
begin
  Result := NIL;
  //Check Memory
	if not Assigned(AccountList) then Accountlist := TStringlist.Create;
	for Index := 0 to AccountList.Count -1 do begin
		if TAccount(AccountList.Objects[Index]).Username = Name then
		begin
			Result := TAccount(AccountList.Objects[Index]);
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
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLDatabase.GetChara()                                           FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
function TMySQLDatabase.GetChara(ID : Integer) : TCharacter;
begin
  Result := NIL;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TMySQLDatabase.SaveAccount()                                     Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TMySQLDatabase.SaveAccount(AnAccount: TAccount);
begin

end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TMySQLDatabase.SaveChara()                                     Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Doesn't do anything yet.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TMySQLDatabase.SaveChara(AChara : TCharacter);
begin
end;
//------------------------------------------------------------------------------


{END MYSQLDATABASE}
end.
