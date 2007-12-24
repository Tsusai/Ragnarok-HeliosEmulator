unit SQLExtendedRoutines;

interface
  function SQLEscapeString(const AString	: String): String;
implementation
  //This routine takes a sql string variable and escapes potentially
  //dangerous characters.
  function SQLEscapeString(const AString	: String): String;
  var
    Index : Integer;
  begin
    Result := '';
    for Index := 1 to Length(AString) do
    begin
      if AString[Index] in [ '''', '\', '"', ';'] then
      begin
        Result := Result + '\' + AString[Index];
      end else
      begin
        Result := Result + AString[Index];
      end;
    end;
  end;
end.