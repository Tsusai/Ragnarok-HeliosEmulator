//------------------------------------------------------------------------------
//CharacterList                                                            UNIT
//------------------------------------------------------------------------------
//  What it does -
//      A list of TCharacters
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
unit CharaList;

interface
uses
	Character,
	ContNrs;

type
	PCharacter = ^TCharacter;

//------------------------------------------------------------------------------
//TCharacterList                                                          CLASS
//------------------------------------------------------------------------------
	TCharacterList = Class(TObject)

	Private
		fList : TObjectList;

		Function GetValue(Index : Integer) : TCharacter;
		Procedure SetValue(Index : Integer; Value : TCharacter);
		Function GetCount : Integer;

	Public
		Constructor Create(OwnsCharacters : Boolean);
		Destructor Destroy; override;
		Property Items[Index : Integer] : TCharacter
		read GetValue write SetValue;default;
		Property Count : Integer read GetCount;

		Procedure Add(const ACharacter : TCharacter);
		Procedure Insert(const ACharacter : TCharacter; Index : Integer);
		Procedure Delete(Index : Integer);
		Procedure Clear();

		Function IndexOf(const CID : LongWord) : Integer;
		Function IndexOfAID(const AID : LongWord) : Integer;
	end;
//------------------------------------------------------------------------------


implementation

//------------------------------------------------------------------------------
//Create                                                            CONSTRUCTOR
//------------------------------------------------------------------------------
//  What it does -
//      Initializes our characterlist. Creates a storage area.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
constructor TCharacterList.Create(OwnsCharacters : Boolean);
begin
	inherited Create;
	fList := TObjectList.Create(OwnsCharacters);
end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy                                                            DESTRUCTOR
//------------------------------------------------------------------------------
//  What it does -
//      Destroys our list and frees any memory used.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
destructor TCharacterList.Destroy;
begin
	fList.Free;
	// Call TObject destructor
	inherited;
end;{Destroy}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Add                                                                 PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Adds a TCharacter to the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TCharacterList.Add(const ACharacter : TCharacter);
begin
	fList.Add(ACharacter);
end;{Add}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Insert                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Inserts a TCharacter at Index Position.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TCharacterList.Insert(const ACharacter : TCharacter; Index: Integer);
begin
	fList.Insert(Index, ACharacter);
end;{Insert}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Delete                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Removes a TCharacter at Index from the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TCharacterList.Delete(Index : Integer);
begin
	fList.Delete(Index);
end;{Delete}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//IndexOf                                                              FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Returns the index in the list of the TCharacter;
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
function TCharacterList.IndexOf(const CID: LongWord): Integer;
var
	Index : Integer;
begin
	Index := fList.Count-1;
	Result := -1;
	while (Index >= 0) do
	begin
		if CID = Items[Index].CID then
		begin
			Result := Index;
			Exit;
		end;
		dec(Index,  1);
	end;
end;{IndexOf}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//IndexOfAID                                                           FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Returns the index in the list of the TCharacter;
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
function TCharacterList.IndexOfAID(const AID: LongWord): Integer;
var
	Index : Integer;
begin
	Index := fList.Count-1;
	Result := -1;
	while (Index >= 0) do
	begin
		if AID = Items[Index].ID then
		begin
			Result := Index;
			Exit;
		end;
		dec(Index,  1);
	end;
end;{IndexOfAID}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Clear                                                               PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Clears the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TCharacterList.Clear;
begin
	fList.Clear;
end;{Clear}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetValue                                                             FUNCTION
//------------------------------------------------------------------------------
//  What it does -
//      Returns a TCharacter at the index.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
function TCharacterList.GetValue(Index : Integer): TCharacter;
begin
	Result := TCharacter(fList.Items[Index]);
end;{GetValue}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetValue                                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sets a TCharacter into the list at Index.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TCharacterList.SetValue(Index : Integer; Value : TCharacter);
begin
	fList.Items[Index] := Value;
end;{SetValue}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetCount                                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Gets the count from the fList object
//
//  Changes -
//    October 30th, 2007 - RaX - Created.
//------------------------------------------------------------------------------
Function TCharacterList.GetCount : Integer;
begin
	Result := fList.Count;
end;{GetCount}
//------------------------------------------------------------------------------
end.

