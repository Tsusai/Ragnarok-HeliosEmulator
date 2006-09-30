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
unit Database;

interface
uses
  Character,
  Account,
  Classes;
type
//------------------------------------------------------------------------------
//TDatabase			                                                           CLASS
//------------------------------------------------------------------------------
//	What it does-
//			This is our parent class for database interfacing. Contains all database
//    routines.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
  TDatabase = class(TObject)
  public
    Constructor Create();
    function GetAccount(ID    : Cardinal) : TAccount;overload;virtual;
    function GetAccount(Name  : string) : TAccount;overload;virtual;
    function GetChara(  ID    : Integer)   : TCharacter;virtual;

    procedure SaveAccount(AnAccount : TAccount);
    procedure SaveChara(AChara : TCharacter);


  end;
//------------------------------------------------------------------------------
  function CreateDatabase() : TDatabase;

implementation
uses
  DatabaseConstants,
  Globals,
  MySQLDatabase;

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
Constructor TDatabase.Create();
begin
  inherited;
  writeln('You have not yet configured your database');
  writeln(' type or have mis-configured it.');
  writeln(' HELIOS WILL NOT FUNCTION!!!');
end;

function TDatabase.GetAccount(ID: Cardinal) : TAccount;
begin
  Result := NIL;
end;

function TDatabase.GetAccount(Name : string) : TAccount;
begin
  Result := NIL;
end;


function TDatabase.GetChara(ID : Integer) : TCharacter;
begin
  Result := NIL;
end;

procedure TDatabase.SaveAccount(AnAccount: TAccount);
begin

end;
procedure TDatabase.SaveChara(AChara : TCharacter);
begin
end;
//------------------------------------------------------------------------------
{END TDATABASE}

//------------------------------------------------------------------------------
//CreateDatabase                                                      Function
//------------------------------------------------------------------------------
//	What it does-
//			Figures out which database to create.
//
//	Changes -
//		September 29th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
function CreateDatabase() : TDatabase;
begin
  Result := TDatabase.Create;
  case ServerConfig.DatabaseType of
    TEXT  : Writeln('Text Databasing is not implemented.');
    MYSQL : Result := TMySQLDatabase.Create();
  end;
end;
//------------------------------------------------------------------------------
{END CREATEDATABASE}
end.
