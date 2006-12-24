unit CharacterServerInfo;

interface

type
	TCharaServerInfo = class
	protected
		fWANIPCard : Cardinal;
		fLANIPCard : Cardinal;
		fWANIP     : String;
		fLANIP     : String;

		procedure SetWANIPCardinal(Value : String);
		procedure SetLANIPCardinal(Value : String);

	public
		WANPort    : Word;
		ServerName : String;
		OnlineUsers : Word;
	published
		property WANCardinal : Cardinal read fWANIPCard;
		property LANCardinal : Cardinal read fLANIPCard;
		property WANIP       : String   read fWANIP write SetWANIPCardinal;
		property LANIP       : String   read fLANIP write SetLANIPCardinal;
	end;

implementation
uses
	WinLinux;

procedure TCharaServerInfo.SetWANIPCardinal(Value : string);
begin
	fWANIP         := GetIPStringFromHostname(Value);
	fWANIPCard     := GetCardinalFromIPString(fWANIP);
end;

procedure TCharaServerInfo.SetLANIPCardinal(Value : string);
begin
	fLANIP         := GetIPStringFromHostname(Value);
	fLANIPCard     := GetCardinalFromIPString(fLANIP);
end;

end.
