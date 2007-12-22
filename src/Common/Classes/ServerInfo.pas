unit ServerInfo;

interface
uses
	IdContext;
type
	TServerInfo = class
	protected
		fWANIPCard  : LongWord;
		fLANIPCard  : LongWord;

		fWAN        : String;
		fLAN        : String;

		fLANPartial : String;

		procedure SetWANLongWord(const Value : String);
		procedure SetLANLongWord(const Value : String);

	public
		Port    : Word;
		Connection: TIdContext;
    
		property WAN       : String   read fWAN write SetWANLongWord;
		property LAN       : String   read fLAN write SetLANLongWord;

		function Address(const ClientIP : string) : LongWord;
	end;

implementation
uses
	StrUtils,
	WinLinux;

procedure TServerInfo.SetWANLongWord(const Value : string);
begin
	fWAN       := Value;
	fWANIPCard := GetLongWordFromIPString(GetIPStringFromHostname(Value).Full);
end;

procedure TServerInfo.SetLANLongWord(const Value : string);
var
	ReturnedIPs : TIPSet;
begin
	fLAN        := Value;
	ReturnedIPs := GetIPStringFromHostname(Value);
	fLANPartial := ReturnedIPs.Partial;
	fLANIPCard  := GetLongWordFromIPString(ReturnedIPs.Full);
end;

function TServerInfo.Address(const ClientIP : string) : LongWord;
begin
	if AnsiStartsText('127.0.0.', ClientIP) then
  begin
		Result := GetLongWordFromIPString('127.0.0.1');
	end else
	if AnsiStartsText(fLANPartial, ClientIP) then
	begin
		Result := fLANIPCard;
	end else
	begin
		Result := fWANIPCard;
	end;
end;

end.
