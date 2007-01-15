unit ZoneServerInfo;

interface
uses
	ServerInfo;

type
	TZoneServerInfo = class(TServerInfo)
	public
		ZoneID : cardinal;
	end;

implementation

end.
