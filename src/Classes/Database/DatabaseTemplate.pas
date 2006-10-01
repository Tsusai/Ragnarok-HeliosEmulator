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
  Classes;
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
    Constructor Create();
    function GetAccount(ID    : Cardinal) : TAccount;overload;virtual;
    function GetAccount(Name  : string) : TAccount;overload;virtual;
    function GetChara(  ID    : Integer)   : TCharacter;virtual;

    procedure SaveAccount(AnAccount : TAccount);
    procedure SaveChara(AChara : TCharacter);


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

function TDatabaseTemplate.GetAccount(ID: Cardinal) : TAccount;
begin
  Result := NIL;
end;

function TDatabaseTemplate.GetAccount(Name : string) : TAccount;
begin
  Result := NIL;
end;


function TDatabaseTemplate.GetChara(ID : Integer) : TCharacter;
begin
  Result := NIL;
end;

procedure TDatabaseTemplate.SaveAccount(AnAccount: TAccount);
begin

end;
procedure TDatabaseTemplate.SaveChara(AChara : TCharacter);
begin
end;
//------------------------------------------------------------------------------
{END TDATABASETEMPLATE}
end.
