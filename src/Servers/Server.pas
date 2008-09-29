//------------------------------------------------------------------------------
//Server			                                                        UNIT
//------------------------------------------------------------------------------
//	What it does-
//      Base class of all servers
//
//	Changes -
//		January 26th, 2008 - RaX - Created
//
//------------------------------------------------------------------------------
unit Server;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	SysUtils,
	{Project}
	Database,
	{3rd Party}
	IdTCPServer,
	IdContext
	;


type
	TServer = class
	protected
		fPort            : Word;

		TCPServer        : TIdTCPServer;

		Procedure OnExecute(AConnection: TIdContext);virtual;abstract;
		Procedure OnConnect(AConnection: TIdContext);virtual;abstract;
		Procedure OnDisconnect(AConnection: TIdContext);virtual;abstract;
		Procedure OnException(AConnection: TIdContext;
			AException: Exception);virtual;abstract;

		Procedure SetPort(Value : Word);
		Function GetStarted : Boolean;

	public
		WANIP : string;
		LANIP : string;

		Database : TDatabase;

		property Started : Boolean read GetStarted;
		property Port : Word read fPort write SetPort;

		Constructor Create;
		Destructor  Destroy;Override;

		Procedure   Start();virtual;
		Procedure   Stop;virtual;abstract;
	end;


implementation


//------------------------------------------------------------------------------
//Create  ()                                                        CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our server
//
//	Changes -
//
//------------------------------------------------------------------------------
Constructor TServer.Create;
begin
	inherited;
	TCPServer := TIdTCPServer.Create();
	TCPServer.OnConnect := OnConnect;
	TCPServer.OnDisconnect := OnDisconnect;
	TCPServer.OnExecute := OnExecute;
	TCPServer.OnException := OnException;

	fPort := 0;
	Database := TDatabase.Create();
end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy()                                                        DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys our server
//
//	Changes -
//
//------------------------------------------------------------------------------
Destructor TServer.Destroy;
Begin
	TCPServer.Free;
	Database.Disconnect();
	Database.Free;
	inherited;
End; (* Dest TZoneServer.Destroy
*-----------------------------------------------------------------------------*)


//------------------------------------------------------------------------------
//GetStarted                                                          FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Checks to see if the internal TCP server is active, if it is it returns
//    true.
//
//	Changes -
//
//------------------------------------------------------------------------------
Function TServer.GetStarted() : Boolean;
begin
	Result := TCPServer.Active;
end;{GetStarted}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//SetPort                                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//
//------------------------------------------------------------------------------
Procedure TServer.SetPort(Value : Word);
begin
	fPort := Value;
	TCPServer.DefaultPort := fPort;
end;{GetStarted}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Start			                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//
//	Changes -
//
//------------------------------------------------------------------------------
Procedure TServer.Start();
begin
	Database.Connect();
end;{Start}
//------------------------------------------------------------------------------
end.
