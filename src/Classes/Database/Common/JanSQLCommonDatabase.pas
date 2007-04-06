(*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

Unit
JanSQLCommonDatabase

*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

[2006/09/29] Helios - RaX

================================================================================
License:  (FreeBSD, plus commercial with written permission clause.)
================================================================================

Project Helios - Copyright (c) 2005-2007

All rights reserved.

Please refer to Helios.dpr for full license terms.

================================================================================
Overview:
================================================================================

	This unit enables Helios to use TEXT files for the Common (Account) Database.

================================================================================
Revisions:
================================================================================
(Format: [yyyy/mm/dd] <Author> - <Desc of Changes>)
[2006/09/29] RaX - Created.
[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
[2007/04/06] CR - Altered Header, Changes to TJanSQLCommonDatabase to match
	template changes for parameters, and cleaning up the Create/Destroy routines.
	Changed the description for clarity.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)


unit JanSQLCommonDatabase;


interface


uses
	{RTL/VCL}
	//none
	{Project}
	Account,
	CommonDatabaseTemplate,
	Database,
	{3rd Party}
	janSQL
	;


(*= CLASS =====================================================================*
TJanSQLCommonDatabase

[2006/09/29] RaXChrstphrR

*------------------------------------------------------------------------------*
Overview:
*------------------------------------------------------------------------------*

	This is a child class for our Common database system. It allows Helios'
Servers to communicate with a TEXT database and implements all the necessary
routines defined by the Common Database Template.

*------------------------------------------------------------------------------*
Revisions:
*------------------------------------------------------------------------------*
(Format: [yyyy/mm/dd] <Author> - <Description of Change>)
[2006/09/29] RaX - Created.
[2007/01/20] Tsusai - Connect is now a bool function
	Create holds connection result
[2007/04/06] CR - Made all parameters var/const/in/out.  Create doesn't
	override the constructor in the template (due to changes to the template
	model).  Parent and Database made into read-only properties, with f* internal
	fields to store them.  Converted all private fields and methods to protected.
*=============================================================================*)
Type
TJanSQLCommonDatabase = class(TCommonDatabaseTemplate)
protected

	fDatabase : TjanSQL;
	fParent   : TDatabase;

	procedure SetAccount(
		out
			AnAccount   : TAccount;
		const
			QueryResult : TJanRecordSet
		);

	function  SendQuery(
		const
			QString : String
		) : Integer;

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

	property Database : TjanSQL
		read  fDatabase;
	property Parent : TDatabase
		read  fParent;

End;(* TJanSQLCommonDatabase
*== CLASS ====================================================================*)


implementation


uses
	{RTL/VCL}
	SysUtils,
	{Project}
	Globals,
	Main
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
//		October 5th, 2006 - RaX - Created.
//		November 13th, 2006 - Tsusai - create inherit comes first.
//		January 20th, 2007 - Tsusai - Create holds connection result
//
//------------------------------------------------------------------------------
Constructor TJanSQLCommonDatabase.Create(
	const
		AParent : TDatabase
	);
begin
	inherited Create;

	fParent   := AParent;
	fDatabase := TJanSQL.Create;
end;//Create
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy  							      DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys our connection object.
//
//	Changes -
//		October 5th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Destructor TJanSQLCommonDatabase.Destroy;
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
//Connect 							       Procedure
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
function TJanSQLCommonDatabase.Connect : Boolean;
var
	ResultIdentifier : Integer;
const ConnectQuery = 'Connect to ''%s''';
begin
	Result := true;
	ResultIdentifier := 0;

	if DirectoryExists(MainProc.DatabaseOptions.CommonHost) then
	begin
		ResultIdentifier := Database.SQLDirect(Format(ConnectQuery,[MainProc.DatabaseOptions.CommonHost]));
	end else
	begin
		Console.WriteLn('');
		Console.WriteLn('The database at '+MainProc.DatabaseOptions.CommonHost+' does not exist!');
		Console.WriteLn('Please ensure that you have correctly configured your ini file');
		Result := false;
	end;

	if ResultIdentifier = 0 then
	begin
		Console.WriteLn('*****Could not open text database. Error : ' + Database.Error);
		Console.WriteLn(MainProc.DatabaseOptions.CommonHost);
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
	const
		QString : String
	) : Integer;
begin
	Result := Database.SQLDirect(QString);
	if (Result = 0) AND (Database.Error <> 'SELECT FROM: no records') then
	begin
		Console.Message('Text Query error: ' + QString + ' : ' +  Database.Error, 'Common Database', MS_ERROR);
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
procedure TJanSQLCommonDatabase.SetAccount(
	out
		AnAccount : TAccount;
	const
		QueryResult : TJanRecordSet
	);
begin
	AnAccount := TAccount.Create(Parent.ClientInfo);
	AnAccount.ID           := StrToIntDef(QueryResult.records[0].fields[0].value, 0);
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
function TJanSQLCommonDatabase.GetAccount(
	const
		ID: LongWord
	) : TAccount;
var
	AnAccount   : TAccount;
	QueryResult : TJanRecordSet;
	ResultIdentifier : Integer;
begin
	ResultIdentifier := SendQuery(
	Format('SELECT * FROM accounts WHERE account_id = %d', [ID]));
	QueryResult := Database.RecordSets[ResultIdentifier];
	if (QueryResult.recordcount = 1) then begin
		SetAccount(AnAccount,QueryResult);
		Result := AnAccount;
	end else
	begin
		Result := NIL;
	end;
	SendQuery('RELEASE TABLE accounts');
	if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
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
function TJanSQLCommonDatabase.GetAccount(
	const
		Name : String
	) : TAccount;
var
	AnAccount   : TAccount;
	QueryResult : TJanRecordSet;
	ResultIdentifier : Integer;
begin
	ResultIdentifier := SendQuery('SELECT * FROM accounts WHERE userid = '+Name);
	QueryResult := Database.RecordSets[ResultIdentifier];
	if (QueryResult.recordcount = 1) then begin
		SetAccount(AnAccount,QueryResult);
		Result := AnAccount;
	end else
	begin
		Result := NIL;
	end;
	SendQuery('RELEASE TABLE accounts');
	if ResultIdentifier > 0 then Database.ReleaseRecordset(ResultIdentifier);
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
function TJanSQLCommonDatabase.AccountExists(
	const
		UserName : String
	) : Boolean;
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
procedure TJanSQLCommonDatabase.SaveAccount(
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
procedure TJanSQLCommonDatabase.RefreshAccountData(
	var
		AnAccount : TAccount
	);
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
	if (ResultIdentifier > 0) then
	begin
		Database.ReleaseRecordset(ResultIdentifier);
	end;
end;//GetAccountBanAndConnectTime
//------------------------------------------------------------------------------


{END JanSQLCommonDatabase}
end.
