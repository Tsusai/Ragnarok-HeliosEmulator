//------------------------------------------------------------------------------
//BeingList                                                                 UNIT
//------------------------------------------------------------------------------
//	What it does -
//		A list of TCharacters
//
//	Changes -
//		[2008/12/17] Aeomin- Changed to BeingList from CharaList
//		December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
unit BeingList;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
uses
	Being,
	ContNrs;

type
	PBeing = ^TBeing;

//------------------------------------------------------------------------------
//TCharacterList                                                          CLASS
//------------------------------------------------------------------------------
	TBeingList = Class(TObject)
	Private
		fList : TObjectList;
		fOwnsObject : Boolean;

		Function GetValue(Index : Integer) : TBeing;
		Procedure SetValue(Index : Integer; Value : TBeing);
		Function GetCount : Integer;

	Public
		Constructor Create(OwnsBeings : Boolean);
		Destructor Destroy; override;
		Property Items[Index : Integer] : TBeing
		read GetValue write SetValue;default;
		Property Count : Integer read GetCount;

		Procedure Add(const ABeing : TBeing);
		Procedure Insert(const ABeing : TBeing; Index : Integer);
		Procedure Delete(Index : Integer);
		Procedure Clear();

		Function IndexOf(const ID : LongWord) : Integer;
		Function IndexOfAID(const AID : LongWord) : Integer;
		// DO NOT USE THIS UNLESS MUST
		function IndexOfName(const Name : String):Integer;
	end;
//------------------------------------------------------------------------------


implementation

uses
	Character
	;

//------------------------------------------------------------------------------
//Create                                                            CONSTRUCTOR
//------------------------------------------------------------------------------
//  What it does -
//      Initializes our characterlist. Creates a storage area.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
constructor TBeingList.Create(OwnsBeings : Boolean);
begin
	inherited Create;
	fOwnsObject := OwnsBeings;
	fList := TObjectList.Create(FALSE);
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
destructor TBeingList.Destroy;
var
	Index : Integer;
begin
	if fOwnsObject AND (fList.Count >0) then
	begin
		for Index := fList.Count -1 downto 0 do
			fList.Items[Index].Free;
	end;
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
procedure TBeingList.Add(const ABeing : TBeing);
begin
	fList.Add(ABeing);
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
procedure TBeingList.Insert(const ABeing : TBeing; Index: Integer);
begin
	fList.Insert(Index, ABeing);
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
procedure TBeingList.Delete(Index : Integer);
begin
	if fOwnsObject then
		fList.Items[Index].Free;
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
function TBeingList.IndexOf(const ID: LongWord): Integer;
var
	Index : Integer;
begin
	Index := fList.Count-1;
	Result := -1;
	while (Index >= 0) do
	begin
		if ID = Items[Index].ID then
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
function TBeingList.IndexOfAID(const AID: LongWord): Integer;
var
	Index : Integer;
begin
	Index := fList.Count-1;
	Result := -1;
	while (Index >= 0) do
	begin
		if (Items[Index] is TCharacter)AND(AID = TCharacter(Items[Index]).AccountID) then
		begin
			Result := Index;
			Break;
		end;
		Dec(Index);
	end;
end;{IndexOfAID}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//IndexOfName                                                           FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Find index by name, DO NOT USE THIS UNLESS MUST
//
//	Changes -
//		[2009/06/27] Aeomin - Created.
//
//------------------------------------------------------------------------------
function TBeingList.IndexOfName(const Name : String):Integer;
var
	Index : Integer;
begin
	Index := fList.Count-1;
	Result := -1;
	while (Index >= 0) do
	begin
		if (Items[Index] is TCharacter)AND(Name = TCharacter(Items[Index]).Name) then
		begin
			Result := Index;
			Break;
		end;
		Dec(Index);
	end;
end;
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
procedure TBeingList.Clear;
var
	Index : Integer;
begin
	if fOwnsObject AND (fList.Count >0) then
	begin
		for Index := fList.Count -1 downto 0 do
			fList.Items[Index].Free;
	end;
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
function TBeingList.GetValue(Index : Integer): TBeing;
begin
	Result := TBeing(fList.Items[Index]);
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
procedure TBeingList.SetValue(Index : Integer; Value : TBeing);
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
Function TBeingList.GetCount : Integer;
begin
	Result := fList.Count;
end;{GetCount}
//------------------------------------------------------------------------------
end.

