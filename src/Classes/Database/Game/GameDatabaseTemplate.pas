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
			AID : LongWord;
			NName : string;
			CharaNum : Integer
		) : boolean;virtual;

		function GetAccountCharas(AccountID : LongWord) : TCharacterList;virtual;
		function LoadChara(CharaID : LongWord) : TCharacter;virtual;
		function GetChara(
			CharaID : LongWord;
			JanSQLClearTable : boolean = false
		) : TCharacter;virtual;
		function DeleteChara(var ACharacter : TCharacter) : boolean;virtual;
		function CharaExists(AccountID : LongWord; Slot : Word) : Boolean;overload;virtual;
		function CharaExists(Name : String) : Boolean;overload;virtual;

		procedure SaveChara(AChara : TCharacter);virtual;

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
  Result := FALSE;
end;

procedure TGameDatabaseTemplate.Disconnect();
begin
end;

function TGameDatabaseTemplate.GetAccountCharas(AccountID : LongWord) : TCharacterList;
begin
	Result := NIL;
end;

function TGameDatabaseTemplate.GetChara(
	CharaID : LongWord;
	JanSQLClearTable : boolean = false
) : TCharacter;
begin
	Result := NIL;
end;

function TGameDatabaseTemplate.CharaExists(AccountID : LongWord; Slot : Word) : Boolean;
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
	AID : LongWord;
	NName : string;
	CharaNum : Integer
) : boolean;
begin
	Result := false;
end;

function TGameDatabaseTemplate.LoadChara(CharaID : LongWord) : TCharacter;
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
