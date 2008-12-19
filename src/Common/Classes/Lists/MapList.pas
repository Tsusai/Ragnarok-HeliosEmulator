//------------------------------------------------------------------------------
//MapList                                                            UNIT
//------------------------------------------------------------------------------
//  What it does -
//      A list of TMaps
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
unit MapList;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
uses
	Map,
	ContNrs;

type

//------------------------------------------------------------------------------
//TMapList                                                          CLASS
//------------------------------------------------------------------------------
	TMapList = Class(TObject)

	Private
		fList : TObjectList;
		fOwnsObject : Boolean;

		Function GetValue(Index : Integer) : TMap;
		Procedure SetValue(Index : Integer; Value : TMap);
		Function GetCount : Integer;
	Public
		Constructor Create(OwnsMaps : Boolean);
		Destructor Destroy; override;
		Property Items[Index : Integer] : TMap
		read GetValue write SetValue;default;


		Procedure Add(const AMap : TMap);
		Procedure Insert(const AMap : TMap; Index : Integer);
		Procedure Delete(Index : Integer);
		Procedure Clear();
		Function IndexOf(const MapName : String) : Integer;

		Property Count : Integer
		read GetCount;
	end;
//------------------------------------------------------------------------------


implementation
//------------------------------------------------------------------------------
//Create                                                            CONSTRUCTOR
//------------------------------------------------------------------------------
//  What it does -
//      Initializes our Maplist. Creates a storage area.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
constructor TMapList.Create(OwnsMaps : Boolean);
begin
	inherited Create;
	fOwnsObject := OwnsMaps;
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
destructor TMapList.Destroy;
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
//      Adds a TMap to the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TMapList.Add(const AMap : TMap);
begin
	fList.Add(AMap);
end;{Add}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Insert                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Inserts a TMap at Index Position.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TMapList.Insert(const AMap : TMap; Index: Integer);
begin
	fList.Insert(Index, AMap);
end;{Insert}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Delete                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Removes a TMap at Index from the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TMapList.Delete(Index : Integer);
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
//      Returns the index in the list of the TMap;
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
function TMapList.IndexOf(const MapName : String): Integer;
var
	Index : Integer;
begin
	Index := fList.Count-1;
	Result := -1;
	while (Index >= 0) do
	begin
		if MapName = Items[Index].Name then
		begin
			Result := Index;
			Exit;
		end;
		dec(Index,  1);
	end;
end;{IndexOf}
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
procedure TMapList.Clear;
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
//      Returns a TMap at the index.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
function TMapList.GetValue(Index : Integer): TMap;
begin
	Result := TMap(fList.Items[Index]);
end;{GetValue}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetValue                                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Sets a TMap into the list at Index.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TMapList.SetValue(Index : Integer; Value : TMap);
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
Function TMapList.GetCount : Integer;
begin
	Result := fList.Count;
end;{GetCount}
//------------------------------------------------------------------------------
end.

