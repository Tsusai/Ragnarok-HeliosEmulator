//------------------------------------------------------------------------------
//Globals                                                                  UNIT
//------------------------------------------------------------------------------
//
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//		[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
//
//------------------------------------------------------------------------------
unit Globals;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface


uses
	{RTL/VCL}
	//none
	{Project}
	Packets,
	Terminal
	{3rd Party}
	//none
	;


	function InitGlobals : boolean;
	procedure DestroyGlobals;
	procedure TerminateApplication;

	function  GetMD5(const Input : string) : string;
	function  MakeRNDString(Count: Integer): string;
	function  ConvertMySQLTime(DateString: string) : TDateTime;
	function  IncSecond(const AValue: TDateTime; const ANumberOfSeconds: Int64): TDateTime;
	function  IncMinute(const AValue: TDateTime; const ANumberOfMinutes: Int64): TDateTime;

//------------------------------------------------------------------------------
//                              Global Variables
//------------------------------------------------------------------------------
var
	Console					: TConsole;

	AppPath         : String;
	ExeName					: String;

	LastAccountID   : Integer;

	//Our Global Packet Database
	PacketDB        : TPacketDB;
//------------------------------------------------------------------------------

const
	//message types
	MS_INFO			= 0;
	MS_NOTICE		= 1;
	MS_WARNING	= 2;
	MS_ERROR		= 3;
	MS_DEBUG		= 4;
	MS_ALERT		= 5;


implementation


uses
	{RTL/VCL}
	DateUtils,
	SysUtils,
	WinLinux,
	{Project}
	Main,
	{3rd Party}
	IdHashMessageDigest
	;

Const
	HoursPerDay   = 24;
	MinsPerHour   = 60;
	MinsPerDay    = HoursPerDay * MinsPerHour;

//------------------------------------------------------------------------------
//GetMD5                                                               FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Creates a MD5 Hash from a string.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function GetMD5(const Input : string) : String;
var
	Hash : TIdHashMessageDigest5;
begin
	Hash := TIdHashMessageDigest5.Create;
	{$IFDEF FPC}
	Result := Hash.HashStringAsHex(Input);
	{$ELSE}
	Result := Hash.AsHex(Hash.HashValue(Input));
	{$ENDIF}

	Hash.Free;
end;{GetMD5}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//InitGlobals                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our global variables.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//		January 20th, 2007 - Tsusai - Now a function that returns success of
//			Database connecting and packet_db
//		June 28th, 2008 - Tsusai - Inits the Packet Database object, and loads
//
//------------------------------------------------------------------------------
function InitGlobals : boolean;
begin
	PacketDB := TPacketDB.Create;
	Result := PacketDB.Load;
end; {InitGlobals}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//DestroyGlobals                                                      PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Free's up our Globals variables.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//		June 28th, 2008 - Tsusai - Inits the Packet Database object, and loads
//
//------------------------------------------------------------------------------
procedure DestroyGlobals;
begin
	PacketDB.Unload;
	PacketDB.Free;
end;{DestroyGlobals}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TerminateApplication                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Kills our main process and free's it.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//		[2007/06/21] Tsusai - Replaced KillProcess with Halt.  Halt does the job
//			better.
//
//------------------------------------------------------------------------------
procedure TerminateApplication;
begin
	if MainProc.Loaded then
	begin
		//remove the hooks for Ctrl-C, etc
		KillTerminationCapturing;
		//Start shutting down the server
		MainProc.Shutdown;
		FreeAndNil(MainProc);
		//Free up console handler.
		Console.Free;
		//Exit the program PROPERYLY
		Halt;
	end else
	begin
		Console.Message('Please wait to shutdown helios until after it has finished starting/stopping', 'System', MS_ALERT);
	end;
end;{TerminateApplication}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//MakeRNDString                                                       FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Makes a random string of length Count.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function MakeRNDString(Count: Integer): string;
const
	chars = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/';
var
	i, x: integer;
begin
	Result := '';
	for i := 1 to Count do
	begin
		x := Length(chars) - Random(Length(chars));
		Result := Result + chars[x];
	end;
end;{MakeRNDString}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ConvertMySQLTime                                                     FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Converts a MySQL Formatted Time into a TDateTime.
//
//	Changes -
//		December 22nd, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function ConvertMySQLTime(DateString: string) : TDateTime;
var
	Year, Month, Day, Hour, Min, Sec : word;
	Code : integer;
begin
	Result := 2;   //01 Jan 1900
	if Length(DateString) = 19 then
	begin
		if not (DateString = '0000-00-00 00:00:00') then
		begin
			Val(copy(DateString, 1, 4), Year, Code);
			Val(copy(DateString, 6, 2), Month, Code);
			Val(copy(DateString, 9, 2), Day, Code);
			Val(copy(DateString, 12, 2), Hour, Code);
			Val(copy(DateString, 15, 2), Min, Code);
			Val(copy(DateString, 18, 2), Sec, Code);
			Result := EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0);
		end;
	end;
end;{ConvertMySQLTime}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//IncSecond                                                            FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Adds a second to a TDateTime type.
//
//	Changes -
//		January 4th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
function IncSecond(const AValue: TDateTime;
	const ANumberOfSeconds: Int64): TDateTime;
begin
	Result := ((AValue * SecsPerDay) + ANumberOfSeconds) / SecsPerDay;
end;{IncSecond}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//IncMinute                                                            FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Adds a minute to a TDateTime type.
//
//	Changes -
//		January 4th, 2007 - RaX - Created Header.
//		March 13th, 2007 - Aeomin - Fix type.
//
//------------------------------------------------------------------------------
function IncMinute(const AValue: TDateTime;
	const ANumberOfMinutes: Int64): TDateTime;
begin
	Result := ((AValue * MinsPerDay) + ANumberOfMinutes) / MinsPerDay;
end;{IncMinute}
//------------------------------------------------------------------------------
end.
