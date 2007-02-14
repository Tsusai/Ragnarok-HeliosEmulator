unit ZoneServerInfo;

interface
uses
	ServerInfo;

type
	TZoneServerInfo = class(TServerInfo)
	public
		ZoneID : LongWord;
	end;

implementation

end.
