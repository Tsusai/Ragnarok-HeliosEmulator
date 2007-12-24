(*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

Unit
SQLiteCommonDatabase

*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

[2007/06/14] Helios - Tsusai

================================================================================
License:  (FreeBSD, plus commercial with written permission clause.)
================================================================================

Project Helios - Copyright (c) 2005-2007

All rights reserved.

Please refer to Helios.dpr for full license terms.

================================================================================
Overview:
================================================================================

	This unit enables Helios to use SQLite files for the Common (Account)
	Database.

================================================================================
Revisions:
================================================================================
(Format: [yyyy/mm/dd] <Author> - <Desc of Changes>)
[2007/06/14] Tsusai - Created.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)


unit SQLiteCommonDatabase;


interface


uses
	{RTL/VCL}
	Classes,
	{Project}
	Account,
	CommonDatabaseTemplate,
	Database,
	{3rd Party}
	SQLiteTable3
	;


(*= CLASS =====================================================================*
TSQLiteCommonDatabase

*------------------------------------------------------------------------------*
Overview:
*------------------------------------------------------------------------------*

	This is a child class for our Common database system. It allows Helios'
Servers to communicate with a SQLite database and implements all the necessary
routines defined by the Common Database Template.

*------------------------------------------------------------------------------*
Revisions:
*------------------------------------------------------------------------------*
(Format: [yyyy/mm/dd] <Author> - <Description of Change>)
[2007/06/14] Tsusai - Created.
*=============================================================================*)
Type
TSQLiteCommonDatabase = class(TCommonDatabaseTemplate)
protected

	fDatabase : TSQLiteDatabase;
	fParent   : TDatabase;

	procedure SetAccount(
		out
			AnAccount   : TAccount;
		const
			QueryResult : TSQLiteTable
		);

	function  SendQuery(
		const
			QString : String
		) : TSQLiteTable;

public

	Constructor Create(
		const
			AParent : TDatabase
		);

	Destructor Destroy; override;

	function GetAccount(
		const
			ID    : LongWord
		) : TAccount; overload; override;

	function GetAccount(
		const
			Name  : String
		) : TAccount; overload; override;

	procedure RefreshAccountData(
		var
			AnAccount : TAccount
		); override;

	procedure CreateAccount(
		const
			Username : String;
		const
			Password : String;
		const
			GenderChar : Char
	); override;

	function AccountExists(
		const
			UserName : String
		) : Boolean; override;

	procedure SaveAccount(
		const
			AnAccount : TAccount
		); override;

	function  Connect : Boolean; override;

	procedure Disconnect; override;

	property Database : TSQLiteDatabase
		read  fDatabase;
	property Parent : TDatabase
		read  fParent;

End;(* TSQLiteCommonDatabase
*== CLASS ====================================================================*)


implementation


uses
	{RTL/VCL}
	SysUtils,
	{Project}
	Globals,
	Main,
  SQLExtendedRoutines
	{3rd Party}
	//none
	;


//------------------------------------------------------------------------------
//Create                   					     CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our connection object.
//
//	Changes -
//		[2007/06/14] Tsusai - Created.
//
//------------------------------------------------------------------------------
Constructor TSQLiteCommonDatabase.Create(
	const
		AParent : TDatabase
	);
begin
	inherited Create;

	fParent   := AParent;
	fDatabase := TSQLiteDatabase.Create;
end;//Create
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy  							      DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys our connection object.
//
//	Changes -
//		[2007/06/14] Tsusai - Created.
//
//------------------------------------------------------------------------------
Destructor TSQLiteCommonDatabase.Destroy;
begin
	Database.Free;
	Disconnect;

	inherited;
end;//Destroy
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Disconnect							       Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Nothing.
//
//	Changes -
//		[2007/06/14] Tsusai - Created.
//
//------------------------------------------------------------------------------
procedure TSQLiteCommonDatabase.Disconnect;
begin

end;//Disconnect
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Connect 							       Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Makes the SQLite open the database file
//
//	Changes -
//		[2007/06/14] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteCommonDatabase.Connect : Boolean;
begin
	if DirectoryExists(MainProc.DatabaseOptions.CommonHost) then
	begin
		Result :=
			Database.Connect(
				ExpandFileName(MainProc.DatabaseOptions.CommonHost)
				 + '/'+MainProc.DatabaseOptions.CommonDB+'.db');
	end else
	begin
		Console.WriteLn('');
		Console.WriteLn('The database at '+MainProc.DatabaseOptions.CommonHost+' does not exist!');
		Console.WriteLn('Please ensure that you have correctly configured your ini file');
		Result := false;
	end;

	if not Result then
	begin
		Console.WriteLn('*****Could not open text database.');
		Console.WriteLn(MainProc.DatabaseOptions.CommonHost);
	end;
end;//Connect
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SendQuery()							        Function
//------------------------------------------------------------------------------
//	What it does-
//			Sends a query to the SQLite object.
//
//	Changes -
//		[2007/06/14] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteCommonDatabase.SendQuery(
	const
		QString : String
	) : TSQLiteTable;
begin
	Result := Database.GetTable(QString);

	{if (Database.Query(QString,Result)) and (Database.LastError <> 0) then
	begin
		Console.Message('Text Query error: ' + QString + ' : ' +  Database.LastErrorMessage, 'Common Database', MS_ERROR);
		Console.WriteLn(Database.LastErrorMessage);
	end;}
end;//SendQuery
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetAccount()							       Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Builds a taccount object from a query result.
//
//	Changes -
//		[2007/06/14] Tsusai - Created.
//
//------------------------------------------------------------------------------
procedure TSQLiteCommonDatabase.SetAccount(
	out
		AnAccount : TAccount;
	const
		QueryResult : TSQLiteTable
	);
begin
	AnAccount := TAccount.Create(Parent.ClientInfo);
	AnAccount.ID           := QueryResult.FieldAsInteger(0);
	AnAccount.Username     := QueryResult.Fields[1];
	AnAccount.Password     := QueryResult.Fields[2];
	//Tsusai - For Gender, we need to return the first char, thats
	//why there is a [1]
	AnAccount.Gender       := (QueryResult.Fields[4])[1];
	AnAccount.LoginCount   := QueryResult.FieldAsInteger(5);
	AnAccount.EMail        := QueryResult.Fields[6];
	AnAccount.LoginKey[1]  := QueryResult.FieldAsInteger(7);
	AnAccount.LoginKey[2]  := QueryResult.FieldAsInteger(8);
	AnAccount.Level        := QueryResult.FieldAsInteger(9);
	AnAccount.ConnectUntil := ConvertMySQLTime(QueryResult.Fields[11]);
	AnAccount.LastIP       := QueryResult.Fields[12];
	AnAccount.Bantime      := ConvertMySQLTime(QueryResult.Fields[14]);
	AnAccount.State        := QueryResult.FieldAsInteger(15);
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
//		[2007/06/14] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteCommonDatabase.GetAccount(
	const
		ID: LongWord
	) : TAccount;
var
	AnAccount   : TAccount;
	QueryResult : TSQLiteTable;
begin
	QueryResult := SendQuery(Format('SELECT * FROM accounts WHERE account_id = %d', [ID]));
	if (QueryResult.Count = 1) then
	begin
		SetAccount(AnAccount,QueryResult);
		Result := AnAccount;
	end else
	begin
		Result := NIL;
	end;
	if Assigned(QueryResult) then QueryResult.Free;
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
//		[2007/06/14] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteCommonDatabase.GetAccount(
	const
		Name : String
	) : TAccount;
var
	AnAccount   : TAccount;
	QueryResult : TSQLiteTable;
begin
	QueryResult := SendQuery(Format('SELECT * FROM accounts WHERE userid = "%s"', [SQLEscapeString(Name)]));
	if QueryResult.Count = 1 then
	begin
		SetAccount(AnAccount,QueryResult);
		Result := AnAccount;
	end else
	begin
		Result := NIL;
	end;
	if Assigned(QueryResult) then QueryResult.Free;
end;//GetAccount
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//AccountExists()						        FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Checks to see if an account exists in the database.
//
//	Changes -
//		[2007/06/14] Tsusai - Created.
//
//------------------------------------------------------------------------------
function TSQLiteCommonDatabase.AccountExists(
	const
		UserName : String
	) : Boolean;
var
	QueryResult : TSQLiteTable;
begin
	QueryResult :=
		SendQuery(
			Format('SELECT userid FROM accounts WHERE userid = ''%s''',[SQLEscapeString(UserName)]));
	Result := (QueryResult.Count > 0);
	if Assigned(QueryResult) then QueryResult.Free;
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
//		[2007/06/14] Tsusai - Created.
//
//------------------------------------------------------------------------------
procedure TSQLiteCommonDatabase.SaveAccount(
	const
		AnAccount : TAccount
	);
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
		'last_ip=''%s'', ' +
		'state=%d ' +
		'WHERE account_id=%d';
var
	QueryString : String;
	QueryResult : TSQLiteTable;
begin
	QueryString :=
		Format(BaseString,
			[SQLEscapeString(AnAccount.Username),
			 SQLEscapeString(AnAccount.Password),
			 FormatDateTime('yyyy-mm-dd hh:mm:ss',AnAccount.LastLoginTime),
			 AnAccount.Gender,
			 AnAccount.LoginCount,
			 AnAccount.EMail,
			 AnAccount.LoginKey[1],
			 AnAccount.LoginKey[2],
			 FormatDateTime('yyyy-mm-dd hh:mm:ss',AnAccount.ConnectUntil),
			 FormatDateTime('yyyy-mm-dd hh:mm:ss',AnAccount.Bantime),
			 AnAccount.LastIP,
			 AnAccount.State,
			 AnAccount.ID]
		);
	QueryResult := SendQuery(QueryString);
	if Assigned(QueryResult) then QueryResult.Free;
end;//SaveAccount
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//CreateAccount                                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Creates an account.
//
//	Changes -
//		[2007/06/14] Tsusai - Created.
//		[2007/06/19] Tsusai - Added QueryResult so that any data can be freed.
//
//------------------------------------------------------------------------------
procedure TSQLiteCommonDatabase.CreateAccount(
	const Username : string;
	const Password : string;
	const GenderChar : char
);
var
	QueryResult : TSQLiteTable;
begin
	QueryResult := SendQuery(
		Format('INSERT INTO accounts (userid, user_pass, sex) VALUES(''%s'', ''%s'', ''%s'')',
		[SQLEscapeString(Username),SQLEscapeString(Password),GenderChar]));
	if Assigned(QueryResult) then QueryResult.Free;
end;//CreateAccount
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RefreshAccountData()					               Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Retrieves all needed data regardless of if its in memory or not.
//
//	Changes -
//		[2007/06/14] Tsusai - Created.
//
//------------------------------------------------------------------------------
procedure TSQLiteCommonDatabase.RefreshAccountData(
	var
		AnAccount : TAccount
	);
var

	QueryResult : TSQLiteTable;
begin
	QueryResult := SendQuery(
		Format('SELECT loginkey1, loginkey2, connect_until, ban_until FROM accounts WHERE account_id=%d',[AnAccount.ID]));

	AnAccount.LoginKey[1]  := QueryResult.FieldAsInteger(0);
	AnAccount.LoginKey[2]  := QueryResult.FieldAsInteger(1);
	AnAccount.ConnectUntil := ConvertMySQLTime(QueryResult.Fields[2]);
	AnAccount.Bantime := ConvertMySQLTime(QueryResult.Fields[3]);
	if Assigned(QueryResult) then QueryResult.Free;
end;//GetAccountBanAndConnectTime
//------------------------------------------------------------------------------


{END SQLiteCommonDatabase}
end.
