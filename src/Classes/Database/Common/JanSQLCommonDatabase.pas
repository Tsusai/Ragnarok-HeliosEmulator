//------------------------------------------------------------------------------
//JanSQLCommonDatabase		                                            UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is one of our database objects which enabled Helios to use a TEXT
//    Database.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit JanSQLCommonDatabase;

interface
uses
	CommonDatabaseTemplate,
	Account,
	janSQL,
  Database;

//------------------------------------------------------------------------------
//TJanSQLCommonDatabase			                                   CLASS
//------------------------------------------------------------------------------
//	What it does-
//			This is a child class for our database object system. It allows Helios
//    to communicate with a TEXT database and houses all routines for doing so.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//		January 20th, 2007 - Tsusai - Connect is now a bool function
//			Create holds connection result
//
//------------------------------------------------------------------------------
type
	TJanSQLCommonDatabase = class(TCommonDatabaseTemplate)
	private
		Database : TjanSQL;
    Parent  : TDatabase;
	public

		Constructor Create(
			EnableCommonDatabase : boolean;
			var LoadedOK : boolean;
			AParent : TDatabase
		); reintroduce; overload;
		Destructor Destroy();override;

		function GetAccount(ID    : LongWord) : TAccount;overload;override;
		function GetAccount(Name  : string) : TAccount;overload;override;

		procedure RefreshAccountData(var AnAccount : TAccount); override;

		procedure CreateAccount(
			const Username : string;
			const Password : string;
			const GenderChar : char
		); override;

		function AccountExists(UserName : String) : Boolean;override;

		procedure SaveAccount(AnAccount : TAccount);override;

	protected
		function Connect() : boolean; override;
		procedure Disconnect; override;
		function SendQuery(
			const QString : string
		) : Integer;
	end;
//------------------------------------------------------------------------------

implementation
	uses
		Classes,
		Globals,
		SysUtils;


//------------------------------------------------------------------------------
//Create()                 					     CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our connection object.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//		November 13th, 2006 - Tsusai - create inherit comes first.
//		January 20th, 2007 - Tsusai - Create holds connection result
//
//------------------------------------------------------------------------------
Constructor TJanSQLCommonDatabase.Create(
	EnableCommonDatabase : boolean;
	var LoadedOK : boolean;
	AParent : TDatabase
);
begin
	inherited Create(EnableCommonDatabase);
	Parent := AParent;
	Database := TJanSQL.Create;
	if EnableCommonDatabase then
	begin
		LoadedOK := Connect();
	end;
end;//Create
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy()							      DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys our connection object.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Destructor TJanSQLCommonDatabase.Destroy();
begin
	Database.Free;
	Disconnect;
	inherited;
end;//Destroy
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Disconnect()							       Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Destroys the TEXT Connection.
//
//	Changes -
//		December 21st, 2006 - RaX - Created
//
//------------------------------------------------------------------------------
procedure TJanSQLCommonDatabase.Disconnect;
begin

end;//Disconnect
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Connect() 							       Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Initializes the TEXT Connection.
//
//	Changes -
//		October 5th, 2006 - RaX - Moved here from globals.
//		December 18th, 2006 - Tsusai - Modified the connect to actually...connect
//			Also FileExists doesn't work for directories, its DirectoryExists
//		January 20th, 2007 - Tsusai - Connect is now a bool function
//
//------------------------------------------------------------------------------
function TJanSQLCommonDatabase.Connect() : boolean;
var
	ResultIdentifier : Integer;
const ConnectQuery = 'Connect to ''%s''';
begin
	Result := true;
	ResultIdentifier := 0;

	if DirectoryExists(Parent.Options.CommonHost) then
	begin
		ResultIdentifier := Database.SQLDirect(Format(ConnectQuery,[Parent.Options.CommonHost]));
	end else
	begin
		Console.WriteLn('');
		Console.WriteLn('The database at '+Parent.Options.CommonHost+' does not exist!');
		Console.WriteLn('Please ensure that you have correctly configured your ini file');
		Result := false;
	end;

	if ResultIdentifier = 0 then
	begin
		Console.WriteLn('*****Could not open text database. Error : ' + Database.Error);
		Console.WriteLn(Parent.Options.CommonHost);
		Result := false;
	end else
	begin
		Database.ReleaseRecordset(ResultIdentifier);
	end;
end;//Connect
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendQuery()							        Function
//------------------------------------------------------------------------------
//	What it does-
//			Sends a query to the jansql object.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//		December 18th, 2006 - Tsusai - Would not return a blank set if query failed
//			to return anything, only a nil pointer that would cause issues when read.
//
//------------------------------------------------------------------------------
function TJanSQLCommonDatabase.SendQuery(
	const QString : string
) : Integer;
begin
	Result := Database.SQLDirect(QString);
	if (Result = 0) AND (Database.Error <> 'SELECT FROM: no records') then
	begin
	Console.Message('Text Query error: ' + QString, 'Common Database', MS_ERROR);
		Console.WriteLn(Database.Error);
	end;
end;//SendQuery
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetAccount()							       Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Builds a taccount object from a query result.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//		December 27th, 2006 - Tsusai - Fixed login key and gender char reading
//
//------------------------------------------------------------------------------
procedure SetAccount(
	var AnAccount : TAccount;
	var QueryResult : TJanRecordSet
);
begin
	AnAccount := TAccount.Create;
	AnAccount.ID          := StrToInt(QueryResult.records[0].fields[0].value);
	AnAccount.Username     := QueryResult.records[0].fields[1].value;
	AnAccount.Password     := QueryResult.records[0].fields[2].value;
	//Tsusai - For Gender, we need to return the first char, thats
	//why there is a [1]
	AnAccount.Gender       := String(QueryResult.records[0].fields[4].value)[1];
	AnAccount.LoginCount   := StrToIntDef(QueryResult.records[0].fields[5].value,0);
	AnAccount.EMail        := QueryResult.records[0].fields[6].value;
	AnAccount.LoginKey[1]  := StrToIntDef(QueryResult.records[0].fields[7].value,0);
	AnAccount.LoginKey[2]  := StrToIntDef(QueryResult.records[0].fields[8].value,0);
	AnAccount.Level        := StrToIntDef(QueryResult.records[0].fields[9].value,0);
	AnAccount.ConnectUntil := ConvertMySQLTime(QueryResult.records[0].fields[11].value);
	AnAccount.LastIP       := QueryResult.records[0].fields[12].value;
	AnAccount.Bantime      := ConvertMySQLTime(QueryResult.records[0].fields[14].value);
end;//SetAccount
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetAccount()					             OVERLOADED FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			This function returns a TAccount type and is used for loading up an
//    account by ID.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//		November 13th, 2005 - Tsusai - now calls a shared TAccount routine to
//					       set the data
//		December 18th, 2006 - Tsusai - Corrected query string syntax
//		December 27th, 2006 - Tsusai - Reorganized
//
//------------------------------------------------------------------------------
function TJanSQLCommonDatabase.GetAccount(ID: LongWord) : TAccount;
var
	AnAccount   : TAccount;
	Index       : Integer;
	QueryResult : TJanRecordSet;
	ResultIdentifier : Integer;
begin
	Result := NIL;
	//Check Memory
	if not Assigned(AccountList) then Accountlist := TStringlist.Create;
	for Index := 0 to AccountList.Count -1 do begin
		if TAccount(AccountList.Objects[Index]).ID = ID then
		begin
			Result := TAccount(AccountList.Objects[Index]);
			break;
		end;
	end;

	if Assigned(Result) then
	begin
		RefreshAccountData(Result);
	end else
	begin
		ResultIdentifier := SendQuery(
		Format('SELECT * FROM accounts WHERE account_id = %d', [ID]));
		QueryResult := Database.RecordSets[ResultIdentifier];
		if (QueryResult.recordcount = 1) then begin
			if Not Assigned(Result) then
			begin
				SetAccount(AnAccount,QueryResult);
				AccountList.AddObject(AnAccount.Username, AnAccount);
				Result := AnAccount;
			end;
		end;
		SendQuery('RELEASE TABLE accounts');
		if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
	end;
end;//GetAccount
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetAccount()						     OVERLOADED FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			This function returns a TAccount type and is used for loading up an
//    account by Name.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//		November 13th, 2005 - Tsusai - now calls a shared TAccount routine to
//					       set the data
//		December 18th, 2006 - Tsusai - Corrected query syntax
//		December 27th, 2006 - Tsusai - Reorganized
//
//------------------------------------------------------------------------------
function TJanSQLCommonDatabase.GetAccount(Name : string) : TAccount;
var
	AnAccount   : TAccount;
	Index       : Integer;
	QueryResult : TJanRecordSet;
	ResultIdentifier : Integer;
begin
	Result := NIL;
	//Check Memory
	if not Assigned(AccountList) then Accountlist := TStringlist.Create;
	for Index := 0 to AccountList.Count -1 do begin
		if TAccount(AccountList.Objects[Index]).Username = Name then
		begin
			Result := TAccount(AccountList.Objects[Index]);
			break;
		end;
	end;

	if Assigned(Result) then
	begin
		RefreshAccountData(Result);
	end else
	begin
		ResultIdentifier := SendQuery('SELECT * FROM accounts WHERE userid = '+Name);
		QueryResult := Database.RecordSets[ResultIdentifier];
		if (QueryResult.recordcount = 1) then begin
			if Not Assigned(Result) then
			begin
				SetAccount(AnAccount,QueryResult);
				AccountList.AddObject(AnAccount.Username, AnAccount);
				Result := AnAccount;
			end;
		end;
		SendQuery('RELEASE TABLE accounts');
		if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
	end;

end;//GetAccount
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//AccountExists()						        FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Checks to see if an account exists in the database.
//
//	Changes -
//		January 11th, 2007 - RaX - Created header.
//
//------------------------------------------------------------------------------
function TJanSQLCommonDatabase.AccountExists(UserName : String) : Boolean;
var
	QueryResult : TJanRecordSet;
	ResultIdentifier : Integer;
begin
	ResultIdentifier :=
		SendQuery(
			Format('SELECT userid FROM accounts WHERE userid = ''%s''',[UserName]));
	QueryResult := Database.RecordSets[ResultIdentifier];
	Result := (QueryResult.recordcount > 0);
	SendQuery('RELEASE TABLE accounts');
	if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
end;//AccountExists
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SaveAccount() 						       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Save an account based on TAccount, Generate SQL Query and send
//            to SQL via SendQuery.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//		March 12th, 2007 - Aeomin - Modify Header
//
//------------------------------------------------------------------------------
procedure TJanSQLCommonDatabase.SaveAccount(AnAccount: TAccount);
const
	BaseString =
		'UPDATE accounts SET '+
		'userid=''%s'', ' +
		'user_pass=''%s'', ' +
		'lastlogin=''%s'', ' +
		'sex=''%s'', ' +
		'logincount=%d, ' +
		'email=''%s'', ' +
		'loginkey1=%d, ' +
		'loginkey2=%d, ' +
		'connect_until=''%s'', ' +
		'ban_until=''%s'', ' +
		'last_ip=''%s'' ' +
		'WHERE account_id=%d';
var
	QueryString : string;
	ResultIdentifier : Integer;
begin
	QueryString :=
		Format(BaseString,
			[AnAccount.Username,
			 AnAccount.Password,
			 FormatDateTime('yyyy-mm-dd hh:mm:ss',AnAccount.LastLoginTime),
			 AnAccount.Gender,
			 AnAccount.LoginCount,
			 AnAccount.EMail,
			 AnAccount.LoginKey[1],
			 AnAccount.LoginKey[2],
			 FormatDateTime('yyyy-mm-dd hh:mm:ss',AnAccount.ConnectUntil),
			 FormatDateTime('yyyy-mm-dd hh:mm:ss',AnAccount.Bantime),
			 AnAccount.LastIP,
			 AnAccount.ID]
		);
	ResultIdentifier := SendQuery(QueryString);
	SendQuery('SAVE TABLE accounts');
	SendQuery('RELEASE TABLE accounts');
	if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
end;//SaveAccount
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//CreateAccount                                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Creates an account.
//
//	Changes -
//		January 11th, 2007 - RaX - Created header.
//
//------------------------------------------------------------------------------
procedure TJanSQLCommonDatabase.CreateAccount(
	const Username : string;
	const Password : string;
	const GenderChar : char
);
var
	ResultIdentifier : Integer;
begin

	ResultIdentifier := SendQuery(
		Format('INSERT INTO accounts (userid, user_pass, sex) VALUES(''%s'', ''%s'', ''%s'')',
		[Username,Password,GenderChar]));
	SendQuery('SAVE TABLE accounts');
	SendQuery('RELEASE TABLE accounts');
	if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
end;//CreateAccount
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RefreshAccountData()					               Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Retrieves all needed data regardless of if its in memory or not.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//		December 27th, 2006 - Tsusai - Remade, now gets all needed account data
//			regardless
//
//------------------------------------------------------------------------------
procedure TJanSQLCommonDatabase.RefreshAccountData(var AnAccount : TAccount);
var
	ResultIdentifier : Integer;
	QueryResult : TJanRecordSet;
begin
	ResultIdentifier := SendQuery(
		Format('SELECT loginkey1, loginkey2, connect_until, ban_until FROM accounts WHERE account_id=%d',[AnAccount.ID]));
	QueryResult := Database.RecordSets[ResultIdentifier];

	AnAccount.LoginKey[1]  := StrToIntDef(QueryResult.records[0].fields[0].Value,0);
	AnAccount.LoginKey[2]  := StrToIntDef(QueryResult.records[0].fields[1].Value,0);
	AnAccount.ConnectUntil := ConvertMySQLTime(QueryResult.records[0].fields[2].Value);
	AnAccount.Bantime := ConvertMySQLTime(QueryResult.records[0].fields[3].Value);
  SendQuery('RELEASE TABLE accounts');
	if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
end;//GetAccountBanAndConnectTime
//------------------------------------------------------------------------------


{END JanSQLCommonDatabase}
end.
