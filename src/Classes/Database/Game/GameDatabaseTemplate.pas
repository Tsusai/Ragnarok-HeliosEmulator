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
unit GameDatabaseTemplate;

interface
uses
	Character,
	Account,
	CharaList;
type
//------------------------------------------------------------------------------
//TGameDatabaseTemplate			                                                           CLASS
//------------------------------------------------------------------------------
//	What it does-
//			This is our parent class for database interfacing. Contains all database
//    routines.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//		January 20th, 2007 - Tsusai - Connect is now a bool function
//			Create holds connection result
//
//------------------------------------------------------------------------------
	TGameDatabaseTemplate = class(TObject)
	public
		Constructor Create(); virtual;
		Destructor Destroy();override;

		function CreateChara(
			var ACharacter : TCharacter;
			AID : Cardinal;
			NName : string;
			CharaNum : Integer
		) : boolean;virtual;

		function GetAccountCharas(AccountID : Cardinal) : TCharacterList;virtual;
		function LoadChara(CharaID : Cardinal) : TCharacter;virtual;
		function GetChara(
			CharaID : Cardinal;
			JanSQLClearTable : boolean = false
		) : TCharacter;virtual;
		function DeleteChara(var ACharacter : TCharacter) : boolean;virtual;
		function CharaExists(AccountID : Cardinal; Slot : Cardinal) : Boolean;overload;virtual;
		function CharaExists(Name : String) : Boolean;overload;virtual;

		procedure SaveChara(AChara : TCharacter);virtual;

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
Constructor TGameDatabaseTemplate.Create();
begin
	inherited;
end;

Destructor TGameDatabaseTemplate.Destroy();
begin
	inherited;
end;

function TGameDatabaseTemplate.Connect() : boolean; 
begin
end;

procedure TGameDatabaseTemplate.Disconnect();
begin
end;

function TGameDatabaseTemplate.GetAccountCharas(AccountID : Cardinal) : TCharacterList;
begin
	Result := NIL;
end;

function TGameDatabaseTemplate.GetChara(
	CharaID : Cardinal;
	JanSQLClearTable : boolean = false
) : TCharacter;
begin
	Result := NIL;
end;

function TGameDatabaseTemplate.CharaExists(AccountID : Cardinal; Slot : Cardinal) : Boolean;
begin
	Result := FALSE;
end;

function TGameDatabaseTemplate.CharaExists(Name : String) : Boolean;
begin
	Result := FALSE;
end;

procedure TGameDatabaseTemplate.SaveChara(AChara : TCharacter);
begin
end;

function TGameDatabaseTemplate.CreateChara(
	var ACharacter : TCharacter;
	AID : Cardinal;
	NName : string;
	CharaNum : Integer
) : boolean;
begin
	Result := false;
end;

function TGameDatabaseTemplate.LoadChara(CharaID : Cardinal) : TCharacter;
begin
	Result := nil;
end;

function TGameDatabaseTemplate.DeleteChara(var ACharacter : TCharacter) : boolean;
begin
	Result := false;
end;
//------------------------------------------------------------------------------
{END TGameDatabaseTemplate}
end.
