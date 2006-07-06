unit CharaServerTypes;

interface
type
	TCharaServ = class
	private
		fIP : widestring;
		procedure SetIPCardinal(Value : widestring);
	public
		IPCardinal : cardinal;
		Port : word;
		ServerName : string;
		OnlineUsers : word;
		InternalServer : boolean;

		property IP : widestring read fIP write SetIPCardinal;
	end;

implementation
uses
	WinLinux;

procedure TCharaServ.SetIPCardinal(Value : widestring);
begin
	IPCardinal := GetCardinalFromIPString(Value);
end;

end.

