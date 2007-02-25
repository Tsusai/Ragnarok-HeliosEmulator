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

	function GetLongWordFromIPString(IPString : string) : LongWord;
	function GetIPStringFromHostname(Hostname : String) : TIPSet;
	procedure SetupTerminationCapturing;
	procedure KillTerminationCapturing;
	procedure KillProcess;
	function GetTick : LongWord;
	procedure LowerPriority(AThread : TThread);
	function ExtractFileNameMod(Path : String) : string;
	function IsValidFolderName(FolderName : String) :Boolean;

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
//GetLongWordFromIPString                                              FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a LongWord IP value from an IP in string form.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
	function GetLongWordFromIPString(IPString : string) : LongWord;
	begin
		Result := LongWord(inet_addr(PChar(IPString)));
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
//		January 21, 2007 - Tsusai - Added a message for linux shutdown, and changed
//			the exit code.
//
//------------------------------------------------------------------------------
	procedure KillProcess;
	begin
		{$IFDEF MSWINDOWS}
		TerminateProcess(GetCurrentProcess, 0);
		{$ENDIF}
		{$IFDEF LINUX}
		Console.Message('Helios will generate a QUIT message.  Please ignore.', 'System', MS_INFO);
		kill(getpid,SIGQUIT);
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
	function GetTick : LongWord;
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
	procedure LowerPriority(AThread : TThread);
	begin
		{$IFDEF MSWINDOWS}
		AThread.Priority := tpLowest;
		{$ENDIF}
		{$IFDEF LINUX}
		//Tsusai 012107: CANNOT modify priority directly.  Libc.Nice isn't
		//helping, takes only 1 unknown var,  SetPriority may have
		//root only restrictions.  This will be blank for now.
		{$ENDIF}
	end;
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
	function ExtractFileNameMod(Path : String) : string;
	begin
		//Tsusai 012507: ExtractFileName does not like / in windows paths,
		//since that method uses \ as a delimiter, so we correct it
		//before calling :)
		{$IFDEF MSWINDOWS}
		Path := StringReplace(Path, '/', '\', [rfReplaceAll]);
		{$ENDIF}
		Result := ExtractFileName(Path);
	end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//IsValidFolderName                                                    FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Checks to see if a folder name is void of banned characters, if it isn't
//		it returns FALSE.
//
//	Changes -
//		February 25th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
function IsValidFolderName(FolderName : String) : Boolean;
const
//Tsusai - if the forbidden characters are different in linux, change this.
	ForbiddenChars  : set of Char = ['<', '>', '|', '"', ':', '*', '?'];
var
	Index : Integer;
begin
	Result := TRUE;

	if FolderName = '' then
	begin
		Result := FALSE;
		Exit;
	end;

	for Index := 1 to Length(FolderName) do
	begin
		if (FolderName[Index] in ForbiddenChars) then
		begin
			Result := FALSE;
			Exit;
		end;
	end;
end;
//------------------------------------------------------------------------------
end.
