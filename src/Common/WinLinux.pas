//special ifdef unit....keeps rest of code clean
unit WinLinux;

interface

	function GetCardinalFromIPString(IPString : string) : Cardinal;
	procedure SetupTerminationCapturing;
	procedure KillTerminationCapturing;
	procedure KillProcess;


implementation
uses
	Globals,
	{$IFDEF MSWINDOWS}
	Windows,
	Winsock;
	{$ENDIF}
	{$IFDEF LINUX}
	Libc;
	{$ENDIF}

	function GetCardinalFromIPString(IPString : string) : Cardinal;
	begin
		Result := Cardinal(inet_addr(PChar(IPString)));
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

end.
