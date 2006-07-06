//special ifdef unit....keeps rest of code clean
unit WinLinux;

interface

	function GetCardinalFromIPString(IPString : string) : Cardinal;

implementation
uses
	{$IFDEF MSWINDOWS}
	Winsock;
	{$ENDIF}
	{$IFDEF LINUX}
	Libc;
	{$ENDIF}

	function GetCardinalFromIPString(IPString : string) : Cardinal;
	begin
		Result := Cardinal(inet_addr(PChar(IPString)));
	end;

end.
