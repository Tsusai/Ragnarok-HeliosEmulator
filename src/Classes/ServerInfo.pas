unit ServerInfo;

interface

type
	TServerInfo = class
	protected
		fWANIPCard  : LongWord;
		fLANIPCard  : LongWord;

		fWAN        : String;
		fLAN        : String;

		fLANPartial : String;

		procedure SetWANLongWord(Value : String);
		procedure SetLANLongWord(Value : String);

	public
		Port    : Word;

	published
		property WAN       : String   read fWAN write SetWANLongWord;
		property LAN       : String   read fLAN write SetLANLongWord;

		function Address(ClientIP : string) : LongWord;
	end;

implementation
uses
	StrUtils,
	WinLinux;

procedure TServerInfo.SetWANLongWord(Value : string);
begin
	fWAN       := Value;
	fWANIPCard := GetLongWordFromIPString(GetIPStringFromHostname(Value).Full);
end;

procedure TServerInfo.SetLANLongWord(Value : string);
var
	ReturnedIPs : TIPSet;
begin
	fLAN        := Value;
	ReturnedIPs := GetIPStringFromHostname(Value);
	fLANPartial := ReturnedIPs.Partial;
	fLANIPCard  := GetLongWordFromIPString(ReturnedIPs.Full);
end;

function TServerInfo.Address(ClientIP : string) : LongWord;
begin
	if AnsiStartsText(fLANPartial, ClientIP) then
	begin
		Result := fLANIPCard;
	end else
	begin
		Result := fWANIPCard;
	end;
end;

end.
