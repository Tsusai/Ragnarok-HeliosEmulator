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
unit StaticDatabaseTemplate;

interface
uses
	Character,
  MapTypes,
  Classes;
type
//------------------------------------------------------------------------------
//TStaticDatabaseTemplate			                                                           CLASS
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
	TStaticDatabaseTemplate = class(TObject)
	public
		Constructor Create(); virtual;
		Destructor Destroy();override;

		Function GetBaseMaxHP(ACharacter : TCharacter) : Word;virtual;
		Function GetBaseMaxSP(ACharacter : TCharacter) : Word;virtual;
		Function GetBaseMaxWeight(ACharacter : TCharacter) : LongWord;virtual;

		Function GetMapCannotSave(MapName : String) : Boolean;virtual;
		Function GetMapZoneID(MapName : String) : Integer;virtual;
    Function GetMapFlags(MapName : String) : TFlags;virtual;
		Function GetMapsForZone(ZoneID : LongWord)  : TStringList;virtual;

		function Connect() : boolean; virtual;
		procedure Disconnect();virtual;
	end;
//------------------------------------------------------------------------------


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
Constructor TStaticDatabaseTemplate.Create();
begin
	inherited Create;
end;

Destructor TStaticDatabaseTemplate.Destroy();
begin
	inherited;
end;

function TStaticDatabaseTemplate.Connect() : boolean;
begin
	Result := false;
end;

procedure TStaticDatabaseTemplate.Disconnect();
begin
end;

Function TStaticDatabaseTemplate.GetBaseMaxHP(ACharacter : TCharacter) : Word;
begin
	Result := 0;
end;

Function TStaticDatabaseTemplate.GetBaseMaxSP(ACharacter : TCharacter) : Word;
begin
	Result := 0;
end;

Function TStaticDatabaseTemplate.GetBaseMaxWeight(ACharacter : TCharacter) : LongWord;
begin
	Result := 0;
end;

Function TStaticDatabaseTemplate.GetMapCannotSave(MapName : String) : Boolean;
begin
	Result := FALSE;
end;

Function TStaticDatabaseTemplate.GetMapZoneID(MapName : String) : Integer;
begin
	Result := 0;
end;

Function TStaticDatabaseTemplate.GetMapFlags(MapName : String) : TFlags;
var
  Flags : TFlags;
begin
  Result := Flags;
end;

Function TStaticDatabaseTemplate.GetMapsForZone(ZoneID : LongWord)  : TStringList;
begin
  Result := TStringList.Create;
end;
//------------------------------------------------------------------------------
{END TStaticDatabaseTemplate}
end.
