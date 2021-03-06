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

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

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
	function GetTick : Cardinal;
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
	Libc,//For Socket stuff.  Hopefully temporarily.
	{$ENDIF}
	SysUtils,
	DateUtils,
	Version,
	Globals;

	//This is used for our tick counting.  Linux doesn't have a "uptime" counter
	//So we store the linux time we started the application, and then take a
	//difference later on.
	{$IFDEF LINUX}
	var StartTime : TDateTime;
	{$ENDIF}

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
//		April 24th, 2007 - Tsusai - Linux goes ahead and sets the current
//			time.
//
//------------------------------------------------------------------------------
	procedure SetupTerminationCapturing;
	begin
		{$IFDEF MSWINDOWS}
		SetConsoleCtrlHandler(@ConProc,true);
		SetConsoleTitle(PChar(HeliosVersion));
		{$ENDIF}
		{$IFDEF LINUX}
		//Set our Starttime for our uptime calculations.
		StartTime := Now;
		//These control ctrl c and stuff for linux
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
//GetTick                                                              FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets current time in milliseconds since system start.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//		April 24th, 2007 - Tsusai - Changed linux portion to compare
//			current time with the time that was stored on startup.
//
//------------------------------------------------------------------------------
	function GetTick : Cardinal;
        begin
        {$IFDEF LINUX}
                Result := MilliSecondsBetween( Now() , StartTime );
	{$ENDIF}
	//timegettime() gets Window's "How long has the PC been on for?" counter
	{$IFDEF MSWINDOWS}
		Result := timegettime();
        {$ENDIF}
	end;
	{GetTick}
//----------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LowerPriority                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Set Helios process to lowest Priority, it also affects
//		performance.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//		March 13th, 2007 - Aeomin - Modify Header
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
//ExtractFileNameMod                                                    FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Extract filename from a string
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//		March 13th, 2007 - Aeomin - Modify Header
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
