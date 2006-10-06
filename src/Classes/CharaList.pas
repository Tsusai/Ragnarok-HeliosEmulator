unit CharaList;

interface
uses
  Character;

type
    TCharacterList = class(TObject)
      List : array of TCharacter;
      Count : Integer;
      
      Constructor Create();
      Destructor Destroy();override;

      Function Add(ACharacter : TCharacter) : Integer;
      Function Exists(Index : Integer)  : Boolean;
      Function Delete(Index : Integer)  : Boolean;overload;
      Function Delete(Name  : String)   : Boolean;overload;
    end;

implementation
uses
  SysUtils;
Constructor TCharacterList.Create();
begin
  Count := 0;
end;

Destructor TCharacterList.Destroy();
var
  Index : Integer;
begin
  for Index := 0 to Count-1 do begin
    List[Index].Free;
  end;
  inherited;
end;

Function TCharacterList.Add(ACharacter : TCharacter) : Integer;
begin
  SetLength(List,Count+1);
  List[Count] := ACharacter;
  Result := Count;
  Inc(Count);
end;

Function TCharacterList.Exists(Index : Integer) : Boolean;
begin
  Result := FALSE;
  if (Index >= 0) AND (Index < Count) then begin
    Result := TRUE;
  end;
end;

Function TCharacterList.Delete(Index : Integer) : Boolean;
begin
  if Exists(Index) then begin
    List[Index].Free;
    for Index := Index to Count-1 do begin
      List[Index] := List[Index+1];
    end;
    dec(Count);
    SetLength(List,Count);
    Result := TRUE;
  end else begin
    Result := FALSE;
  end;
end;

Function TCharacterList.Delete(Name : String) : Boolean;
var
  Index: Integer;
begin
  Result := FALSE;
  for Index := 0 to Count - 1 do
  begin
    if List[Index].Name = Name then
    begin
      Result := Delete(Index);
    end;
  end;
end;

end.

