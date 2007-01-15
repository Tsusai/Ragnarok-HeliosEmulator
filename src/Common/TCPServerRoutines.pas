//------------------------------------------------------------------------------
//TCPServerRoutines                                                        UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Holds our Common Server Routines.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit TCPServerRoutines;

interface
uses
	IdTCPServer,
	CommClient;

	function ActivateServer(Name : string; var AServer : TIdTCPServer) : Boolean;
	procedure DeActivateServer(const Name : String; var AServer : TIdTCPServer);

	function ActivateClient(var AClient : TInterClient) : Boolean;
	procedure DeActivateClient(var AClient : TInterClient);

  Function GetPacketLength(ID : Word; Version : Integer = 0) : Integer;
  
implementation
uses
	SysUtils,
	StrUtils,
	IdException,
	IdContext,
	Classes,
	Console,
	PacketDB;

//------------------------------------------------------------------------------
//ActivateServer                                                       FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Activates a TCP server.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function ActivateServer(Name : string; var AServer : TIdTCPServer) : boolean;
Const
	PORT_ERR =
		'The %s port (%d) is in use.  Please correct and restart.';
	LOADING =
		'  **Activating %s Server Component';
	LOADED =
		'  --%s Server Component Now Running';
begin
	if not Assigned(AServer) then
	begin
		AServer := TIdTCPServer.Create;
	end;
	Result := true;
	MainProc.Console(Format(LOADING, [Name]));
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
	MainProc.Console(Format(LOADED, [Name]));
end;{ActivateServer}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//DeActivateServer                                                    PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Deactivates a server.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//		January 3rd, 2006 - Tsusai - Added console strings.
//
//------------------------------------------------------------------------------
procedure DeActivateServer(const Name : String; var AServer : TIdTCPServer);
const
	TERMINATING =
		'  **Deactivating %s Server Component';
	TERMINATED =
		'  --%s Server Component Offline';
begin

	If Assigned(AServer) then
	begin
		if AServer.Active then
		begin
			MainProc.Console(Format(TERMINATING, [Name]));
			AServer.Active := false;
		end;
		AServer.Bindings.Clear;
		MainProc.Console(Format(TERMINATED, [Name]));
	end;
end;{DeActivateServer}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//ActivateClient                                                       FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Activates a client.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//		January 14th, 2007 - Tsusai - Updated the Create call
//
//------------------------------------------------------------------------------
function ActivateClient(var AClient : TInterClient) : boolean;
begin
	if not Assigned(AClient) then
	begin
		AClient := TInterClient.Create('Unknown','Unknown');
	end;
	AClient.Active := true;
	Result := true;
end;{ActivateClient}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//DeactivateClient                                                     FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Deactivates a client.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure DeActivateClient(var AClient : TInterClient);
begin
	If Assigned(AClient) then
	begin
		AClient.Active := false;
	end;
end;{DeActivateClient}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetPacketLength                                                      FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets the length of a packet specified by ID for version Version
//
//	Changes -
//		December 22nd, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Function GetPacketLength(ID : Word; Version : Integer = 0) : Integer;
var
  Index           : Integer;
	CodebaseLength  : Integer;
	Found : boolean;
begin
	Result := 0;
	Found := false;
	CodebaseLength := Length(Codebase[Version].Packet);
	for Index := 0 to CodebaseLength - 1 do
	begin
		if Codebase[Version].Packet[Index].ID = ID then
		begin
			Result := Codebase[Version].Packet[Index].PLength;
			Found := true;
			break;
		end;
	end;
	if not Found and not (Version = 0) then
	begin
		CodebaseLength := Length(Codebase[0].Packet);
		for Index := 0 to CodebaseLength - 1 do
		begin
			if Codebase[0].Packet[Index].ID = ID then
			begin
				Result := Codebase[0].Packet[Index].PLength;
				break;
			end;
		end;
	end;
end;{GetPacketLength}
//------------------------------------------------------------------------------
end.
