//------------------------------------------------------------------------------
//WinLinux                                                                 UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This keeps our code clean. Certain operating systems need certain
//    things.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit WinLinux;

interface
uses
	Classes;

	type
		TIPSet = record
			Full : string;
			//Partial is just missing a value for the 4th octect (192.168.1.)
			Partial : string;
		end;

	function GetCardinalFromIPString(IPString : string) : Cardinal;
	function GetIPStringFromHostname(Hostname : String) : TIPSet;
	procedure SetupTerminationCapturing;
	procedure KillTerminationCapturing;
	procedure KillProcess;
	function GetTick : Cardinal;
	procedure LowerPriority(AThread : TThread);


implementation
uses
	{$IFDEF MSWINDOWS}
	Windows,
	MMSystem,
	Winsock,
	{$ENDIF}
	{$IFDEF LINUX}
	Libc,
	{$ENDIF}
	SysUtils,
	Version,
	Globals;

//------------------------------------------------------------------------------
//GetCardinalFromIPString                                              FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a cardinal IP value from an IP in string form.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	function GetCardinalFromIPString(IPString : string) : Cardinal;
	begin
		Result := Cardinal(inet_addr(PChar(IPString)));
	end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetIPStringFromHostname                                              FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Returns an IP from a hostname.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	function GetIPStringFromHostname(Hostname : string) : TIPSet;
	var
		h: PHostEnt;
	begin
		h := gethostbyname(PChar(Hostname));
		if h <> nil then
		begin
			with h^ do
			begin
				Result.Partial := Format('%d.%d.%d.', [ord(h_addr^[0]), ord(h_addr^[1]),
						ord(h_addr^[2])]);
				Result.Full := Result.Partial + IntToStr(ord(h_addr^[3]));
			end;
		end else
		begin
			Result.Full := '0.0.0.0';
			Result.Partial := '0.0.0.';
		end;
	end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ConProc                                                              STDCALL
//------------------------------------------------------------------------------
//	What it does-
//			Checks to see if windows is trying to terminate Helios, if it is it
//    shuts itself down.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	{$IFDEF MSWINDOWS}
	function ConProc(CtrlType : DWord) : Bool; stdcall; far;
	begin
		if CtrlType in
			[CTRL_C_EVENT,CTRL_BREAK_EVENT,
			 CTRL_CLOSE_EVENT,CTRL_LOGOFF_EVENT,
			 CTRL_SHUTDOWN_EVENT] then
			begin
				TerminateApplication;
			end;
		Result := true;
	end;{ConProc}
	{$ENDIF}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetupTerminationCapturing                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Sets us up to shutdown helios normally when windows or linux tries to
//    kill it.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure SetupTerminationCapturing;
	begin
		{$IFDEF MSWINDOWS}
		SetConsoleCtrlHandler(@ConProc,true);
		SetConsoleTitle(PChar(HeliosVersion));
		{$ENDIF}
		{$IFDEF LINUX}
    Signal(SIGINT,@TerminateApplication);
		Signal(SIGTERM,@TerminateApplication);
		Signal(SIGKILL,@TerminateApplication);
		{$ENDIF}
	end;{SetupTerminationCapturing}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//KillTerminationCapturing                                            PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Removes termination capturing from our executeable.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure KillTerminationCapturing;
	begin
		{$IFDEF MSWINDOWS}
		SetConsoleCtrlHandler(@ConProc,False);
		{$ENDIF}
	end;{KillTerminationCapturing}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//KillProcess                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Kills the Helios Process.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	procedure KillProcess;
	begin
		{$IFDEF MSWINDOWS}
		TerminateProcess(GetCurrentProcess, 0);
		{$ENDIF}
		{$IFDEF LINUX}
		kill(getpid,3);
		{$ENDIF}
	end;{KillProcess}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetTick                                                              FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets current time in milliseconds since system start.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	function GetTick : cardinal;
	{$IFDEF LINUX}
	var
		LinuxInfo : TSysInfo;
	{$ENDIF}
	begin
		{$IFDEF MSWINDOWS}
		Result := timegettime();
		{$ENDIF}
    {$IFDEF LINUX}
		sysinfo(LinuxInfo);
		Result := LinuxInfo.uptime;
		{$ENDIF}
	end;{GetTick}
//----------------------------------------------------------------------------

	procedure LowerPriority(AThread : TThread);
	begin
		{$IFDEF MSWINDOWS}
		AThread.Priority := tpLowest;
		{$ENDIF}
		{$IFDEF LINUX}
		Nice(getpid,0);
		{$ENDIF}
	end;

end.
