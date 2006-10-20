unit TCPServerRoutines;

interface
uses
	IdTCPServer;

	function ActivateServer(Name : string; var Server : TIdTCPServer) : Boolean;
	procedure DeActivateServer(var AServer : TIdTCPServer);


implementation
uses
	SysUtils,
	Classes,
	Console,
	IdContext,
	IdException;

function ActivateServer(Name : string; var Server : TIdTCPServer) : boolean;
Const
	PORT_ERR =
		'The %s port (%d) is in use.  Please correct and restart.';
begin
	Result := true;
	try
		Server.Active := True;
	except
		on EIdCouldNotBindSocket do
		begin
			MainProc.Console(Format(PORT_ERR, [Name, Server.DefaultPort]));
			Result := false;
			Exit;
		end;
	end;
end;

procedure DeActivateServer(var AServer : TIdTCPServer);
var
	List : TList;
	idx : integer;
begin
	if AServer.Active then
	begin
		List := AServer.Contexts.LockList;
		try
			for idx := 0 to List.Count - 1 do
			begin
				try
					TIdContext(List.Items[idx]).Connection.Disconnect;
				except
					on E: Exception do
					begin
					end;
				end;
			end;
		finally
			AServer.Contexts.UnlockList;
		end;
		//Sleep is needed else d/c Timeout exceptions will occur (multithreading)
		Sleep(500);
		{[2006/09/08] Tsusai - Reduced to 500 miliseconds, from 5 whole ones
			it does work on my server, not sure on a whole bunch of them}
		AServer.Active := false;
	end;
	AServer.Bindings.Clear;
end;

end.
