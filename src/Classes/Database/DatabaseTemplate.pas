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
unit DatabaseTemplate;

interface
uses
	Character,
	Account,
	CharaList;
type
//------------------------------------------------------------------------------
//TDatabaseTemplate			                                                           CLASS
//------------------------------------------------------------------------------
//	What it does-
//			This is our parent class for database interfacing. Contains all database
//    routines.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
	TDatabaseTemplate = class(TObject)
	public
		Constructor Create(); virtual;
		Destructor Destroy();override;
		function GetAccount(ID    : Cardinal) : TAccount;overload;virtual;
		function GetAccount(Name  : string) : TAccount;overload;virtual;
		procedure GetAccountBanAndConnectTime(var AnAccount : TAccount);virtual;
		function CreateChara(
			var ACharacter : TCharacter;
			AID : Cardinal;
			NName : string
		) : boolean;virtual;

		procedure CreateAccount(
			const Username : string;
			const Password : string;
			const GenderChar : char
		); virtual;

		function GetAccountCharas(AccountID : Cardinal) : TCharacterList;virtual;
		function LoadChara(CharaID : Cardinal) : TCharacter;virtual;
		function GetChara( CharaID    : Cardinal)   : TCharacter;virtual;
		function DeleteChara(var ACharacter : TCharacter) : boolean;virtual;
		function CharaExists(AccountID : Cardinal; Slot : Cardinal) : Boolean;overload;virtual;
		function CharaExists(Name : String) : Boolean;overload;virtual;
		function AccountExists(UserName : String) : Boolean;virtual;

		procedure SaveAccount(AnAccount : TAccount);virtual;
		procedure SaveChara(AChara : TCharacter);virtual;

		Function GetBaseHP(ACharacter : TCharacter) : Cardinal;virtual;
		Function GetBaseSP(ACharacter : TCharacter) : Cardinal;virtual;

	protected
		procedure Connect();virtual;
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
Constructor TDatabaseTemplate.Create();
begin
	inherited;
end;

Destructor TDatabaseTemplate.Destroy();
begin
	inherited;
end;

procedure TDatabaseTemplate.Connect();
begin
end;

procedure TDatabaseTemplate.Disconnect();
begin
end;

function TDatabaseTemplate.GetAccount(ID: Cardinal) : TAccount;
begin
	Result := NIL;
end;

function TDatabaseTemplate.GetAccountCharas(AccountID : Cardinal) : TCharacterList;
begin
	Result := NIL;
end;

function TDatabaseTemplate.GetAccount(Name : string) : TAccount;
begin
	Result := NIL;
end;


function TDatabaseTemplate.GetChara(CharaID : Cardinal) : TCharacter;
begin
	Result := NIL;
end;

function TDatabaseTemplate.CharaExists(AccountID : Cardinal; Slot : Cardinal) : Boolean;
begin
	Result := FALSE;
end;

function TDatabaseTemplate.CharaExists(Name : String) : Boolean;
begin
	Result := FALSE;
end;

function TDatabaseTemplate.AccountExists(UserName : String) : Boolean;
begin
	Result := FALSE;
end;

procedure TDatabaseTemplate.SaveAccount(AnAccount: TAccount);
begin
end;

procedure TDatabaseTemplate.SaveChara(AChara : TCharacter);
begin
end;

function TDatabaseTemplate.CreateChara(
	var ACharacter : TCharacter;
	AID : Cardinal;
	NName : string
) : boolean;
begin
	Result := false;
end;

procedure TDatabaseTemplate.CreateAccount(
	const Username : string;
	const Password : string;
	const GenderChar : char
);
begin
end;

function TDatabaseTemplate.LoadChara(CharaID : Cardinal) : TCharacter;
begin
	Result := nil;
end;

function TDatabaseTemplate.DeleteChara(var ACharacter : TCharacter) : boolean;
begin
	Result := false;
end;

procedure TDatabaseTemplate.GetAccountBanAndConnectTime(var AnAccount : TAccount);
begin
	//No change.
end;

Function TDatabaseTemplate.GetBaseHP(ACharacter : TCharacter) : Cardinal;
begin
	Result := 0;
end;

Function TDatabaseTemplate.GetBaseSP(ACharacter : TCharacter) : Cardinal;
begin
	Result := 0;
end;
//------------------------------------------------------------------------------
{END TDATABASETEMPLATE}
end.
