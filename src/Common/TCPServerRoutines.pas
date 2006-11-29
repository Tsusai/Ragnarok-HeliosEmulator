unit TCPServerRoutines;

interface
uses
	IdTCPServer,
	IdTCPClient;

	function ActivateServer(Name : string; var AServer : TIdTCPServer) : Boolean;
	procedure DeActivateServer(var AServer : TIdTCPServer);

	function ActivateClient(var AClient : TIdTCPClient) : Boolean;
	procedure DeActivateClient(var AClient : TIdTCPClient);

implementation
uses
	SysUtils,
	Classes,
	Console,
	IdContext,
	IdException;

function ActivateServer(Name : string; var AServer : TIdTCPServer) : boolean;
Const
	PORT_ERR =
		'The %s port (%d) is in use.  Please correct and restart.';
begin
	if not Assigned(AServer) then
	begin
		AServer := TIdTCPServer.Create;
	end;
	Result := true;
	try
		AServer.Active := True;
	except
		on EIdCouldNotBindSocket do
		begin
			MainProc.Console(Format(PORT_ERR, [Name, AServer.DefaultPort]));
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
	If Assigned(AServer) then
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
end;

function ActivateClient(var AClient : TIdTCPClient) : boolean;
Const
	PORT_ERR =
		'The %s port (%d) is in use.  Please correct and restart.';
begin
	if not Assigned(AClient) then
	begin
		AClient := TIdTCPClient.Create;
	end;
	Result := true;
end;

procedure DeActivateClient(var AClient : TIdTCPClient);
begin
	If Assigned(AClient) then
	begin
		if AClient.Connected then
		begin
			AClient.Disconnect;
		end;
	end;
end;
end.
