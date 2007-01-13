unit ServerInfo;

interface

type
	TServerInfo = class
	protected
		fWANIPCard  : Cardinal;
		fLANIPCard  : Cardinal;

		fWAN        : String;
		fLAN        : String;

		fLANPartial : String;

		procedure SetWANCardinal(Value : String);
		procedure SetLANCardinal(Value : String);

	public
		Port    : Word;

	published
		property WAN       : String   read fWAN write SetWANCardinal;
		property LAN       : String   read fLAN write SetLANCardinal;

		function Address(ClientIP : string) : Cardinal;
	end;

implementation
uses
	StrUtils,
	WinLinux;

procedure TServerInfo.SetWANCardinal(Value : string);
begin
	fWAN       := Value;
	fWANIPCard := GetCardinalFromIPString(GetIPStringFromHostname(Value).Full);
end;

procedure TServerInfo.SetLANCardinal(Value : string);
var
	ReturnedIPs : TIPSet;
begin
	fLAN        := Value;
	ReturnedIPs := GetIPStringFromHostname(Value);
	fLANPartial := ReturnedIPs.Partial;
	fLANIPCard  := GetCardinalFromIPString(ReturnedIPs.Full);
end;

function TServerInfo.Address(ClientIP : string) : Cardinal;
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
