//------------------------------------------------------------------------------
//Database			                                                           UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is the parent class of our other database objects, it also includes
//    a function for choosing which database we're using based on a
//    configuration variable. It contains all database interface routines.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit CommonDatabaseTemplate;

interface
uses
	Account;
type
//------------------------------------------------------------------------------
//TCommonDatabaseTemplate			                                                           CLASS
//------------------------------------------------------------------------------
//	What it does-
//			This is our parent class for database interfacing. Contains all database
//    routines.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//		January 20th, 2007 - Tsusai - Connect is now a bool function
//
//------------------------------------------------------------------------------
	TCommonDatabaseTemplate = class(TObject)
	public
		Constructor Create(EnableCommonDatabase : Boolean); virtual;
		Destructor Destroy();override;
		function GetAccount(ID    : Cardinal) : TAccount;overload;virtual;
		function GetAccount(Name  : string) : TAccount;overload;virtual;
		procedure RefreshAccountData(var AnAccount : TAccount);virtual;

		procedure CreateAccount(
			const Username : string;
			const Password : string;
			const GenderChar : char
		); virtual;

		function AccountExists(UserName : String) : Boolean;virtual;

		procedure SaveAccount(AnAccount : TAccount);virtual;

	protected
		function Connect() : boolean; virtual;
		procedure Disconnect();virtual;
	end;
//------------------------------------------------------------------------------
	//function CreateDatabase() : TDatabase;

implementation

//------------------------------------------------------------------------------
//TDatabase	routines                                             Routine stubs
//------------------------------------------------------------------------------
//	What it does-
//			These are simple placeholders for routines which will override these.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Constructor TCommonDatabaseTemplate.Create(EnableCommonDatabase : Boolean);
begin
	inherited Create();
end;

Destructor TCommonDatabaseTemplate.Destroy();
begin
	inherited;
end;

function TCommonDatabaseTemplate.Connect() : boolean;
begin
	Result := false;
end;

procedure TCommonDatabaseTemplate.Disconnect();
begin
end;

function TCommonDatabaseTemplate.GetAccount(ID: Cardinal) : TAccount;
begin
	Result := NIL;
end;

function TCommonDatabaseTemplate.GetAccount(Name : string) : TAccount;
begin
	Result := NIL;
end;

function TCommonDatabaseTemplate.AccountExists(UserName : String) : Boolean;
begin
	Result := FALSE;
end;

procedure TCommonDatabaseTemplate.SaveAccount(AnAccount: TAccount);
begin
end;

procedure TCommonDatabaseTemplate.CreateAccount(
	const Username : string;
	const Password : string;
	const GenderChar : char
);
begin
end;

procedure TCommonDatabaseTemplate.RefreshAccountData(var AnAccount : TAccount);
begin
	//No change.
end;
//------------------------------------------------------------------------------
{END TCommonDatabaseTemplate}
end.
