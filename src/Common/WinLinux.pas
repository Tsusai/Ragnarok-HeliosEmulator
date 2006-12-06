//special ifdef unit....keeps rest of code clean
unit WinLinux;

interface
uses
	{$IFDEF LINUX}
	Libc,
	{$ENDIF}
	Classes;

	function GetCardinalFromIPString(IPString : string) : Cardinal;
	function GetIPStringFromHostname(Hostname : String) : String;
	procedure SetupTerminationCapturing;
	procedure KillTerminationCapturing;
	procedure KillProcess;
	function GetTick : Cardinal;

	const
		{$IFDEF MSWINDOWS}
		PriorityLow = tpLowest;
		{$ENDIF}
		{$IFDEF LINUX}
		PriorityLow = PRIO_MIN;
		{$ENDIF}


implementation
uses
	{$IFDEF MSWINDOWS}
	Windows,
	MMSystem,
	Winsock,
	{$ENDIF}
	SysUtils,
	Version,
	Globals;


	function GetCardinalFromIPString(IPString : string) : Cardinal;
	begin
		Result := Cardinal(inet_addr(PChar(IPString)));
	end;

	function GetIPStringFromHostname(Hostname : string) : string;
	var
		h: PHostEnt;
	begin
		h := gethostbyname(PChar(Hostname));
		if h <> nil then
		begin
			with h^ do
			begin
				Result := format('%d.%d.%d.%d', [ord(h_addr^[0]), ord(h_addr^[1]),
						ord(h_addr^[2]), ord(h_addr^[3])]);
			end;
		end else
		begin
			Result := '0.0.0.0'
		end;
	end;

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
	end;
	{$ENDIF}

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
	end;

	procedure KillTerminationCapturing;
	begin
		{$IFDEF MSWINDOWS}
		SetConsoleCtrlHandler(@ConProc,False);
		{$ENDIF}
	end;

	procedure KillProcess;
	begin
		{$IFDEF MSWINDOWS}
		TerminateProcess(GetCurrentProcess, 0);
		{$ENDIF}
		{$IFDEF LINUX}
		kill(getpid,3);
		{$ENDIF}
	end;

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
	end;

end.
