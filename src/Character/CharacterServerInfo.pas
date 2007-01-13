unit CharacterServerInfo;

interface
uses
	ServerInfo;

type
	TCharaServerInfo = class(TServerInfo)
	public
		ServerName : String;
		OnlineUsers : Word;
	end;

implementation

end.
