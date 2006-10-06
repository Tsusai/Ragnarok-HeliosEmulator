//------------------------------------------------------------------------------
//Helios			                                                         Program
//------------------------------------------------------------------------------
//	What it does-
//			Helios is a cross-compatible Ragnarok Online Server Emulator.
//  License -
//        Real open source licensing. Meaning, take whatever you want, use it
//     wherever and however you want, just give credit where it's due.
//
//	Changes -
//		September 20th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
program Helios;

{$APPTYPE CONSOLE}
uses

{*These definitions make it possible to step through programs in windows and
compile for linux. at the same time*}
//------------------------------------------------------------------------------
//                            Windows Definitions
//------------------------------------------------------------------------------
	{$IFDEF MSWINDOWS}
	Account in 'Classes\Account.pas',
	Character in 'Classes\Character.pas',
  CharaList in 'Classes\CharaList.pas',
	CharaServerProcess in 'Character\CharaServerProcess.pas',
	CharaServerTypes in 'Character\CharaServerTypes.pas',
	Commands in 'Classes\Commands.pas',
  Database in 'Classes\Database.pas',
  DatabaseTemplate in 'Classes\Database\DatabaseTemplate.pas',
  DatabaseConstants in 'Constants\DatabaseConstants.pas',
  MySQLDatabase in 'Classes\Database\MySQLDatabase.pas',
	DatabaseTXT in 'Database\DatabaseTXT.pas',
	GameGlobals in 'Common\GameGlobals.pas',
	GameObjects in 'Database\GameObjects.pas',
	Globals in 'Common\Globals.pas',
	List32 in 'Common\3rdParty\List32.pas',
	LoginProcesses in 'Login\LoginProcesses.pas',
  madExcept,
	madLinkDisAsm,
	PacketTypes in 'Common\PacketTypes.pas',
	ServerOptions in 'Config\ServerOptions.pas',
	Socket in 'Common\Socket.pas',
	TCPServerRoutines in 'Common\TCPServerRoutines.pas',
	uMysqlClient in 'Common\3rdParty\SQL\uMysqlClient.pas',
	uMysqlCT in 'Common\3rdParty\SQL\uMysqlCT.pas',
	uMysqlErrors in 'Common\3rdParty\SQL\uMysqlErrors.pas',
	uMysqlNet in 'Common\3rdParty\SQL\uMysqlNet.pas',
	uMysqlVio in 'Common\3rdParty\SQL\uMysqlVio.pas',
	uMysqlNewPassword in 'Common\3rdParty\SQL\uMysqlNewPassword.pas',
	umysqlsha1 in 'Common\3rdParty\SQL\umysqlsha1.pas',
	WinLinux in 'Common\WinLinux.pas',
	ZoneCore in 'Zone\ZoneCore.pas',
	ZoneRecv in 'Zone\ZoneRecv.pas',
	{$ENDIF}

//------------------------------------------------------------------------------
//                            Linux Definitions
//------------------------------------------------------------------------------
	{$IFDEF LINUX}
	Account in 'Classes/Account.pas',
	Character in 'Classes/Character.pas',
  CharaList in 'Classes/CharaList.pas',
	CharaServerProcess in 'Character/CharaServerProcess.pas',
	CharaServerTypes in 'Character/CharaServerTypes.pas',
	Commands in 'Classes/Commands.pas',
  Database in 'Classes/Database.pas',
  DatabaseTemplate in 'Classes/Database/DatabaseTemplate.pas',
  DatabaseConstants in 'Constants/DatabaseConstants.pas',
  MySQLDatabase in 'Classes/Database/MySQLDatabase.pas',
	DatabaseTXT in 'Database/DatabaseTXT.pas',
	GameGlobals in 'Common/GameGlobals.pas',
	GameObjects in 'Database/GameObjects.pas',
	Globals in 'Common/Globals.pas',
	List32 in 'Common/3rdParty/List32.pas',
	LoginProcesses in 'Login/LoginProcesses.pas',
	PacketTypes in 'Common/PacketTypes.pas',
	ServerOptions in 'Config/ServerOptions.pas',
	Socket in 'Common/Socket.pas',
	TCPServerRoutines in 'Common/TCPServerRoutines.pas',
	uMysqlClient in 'Common/3rdParty/SQL/uMysqlClient.pas',
	uMysqlCT in 'Common/3rdParty/SQL/uMysqlCT.pas',
	uMysqlErrors in 'Common/3rdParty/SQL/uMysqlErrors.pas',
	uMysqlNet in 'Common/3rdParty/SQL/uMysqlNet.pas',
	uMysqlVio in 'Common/3rdParty/SQL/uMysqlVio.pas',
	uMysqlNewPassword in 'Common/3rdParty/SQL/uMysqlNewPassword.pas',
	umysqlsha1 in 'Common/3rdParty/SQL/umysqlsha1.pas',
	WinLinux in 'Common/WinLinux.pas',
	ZoneCore in 'Zone/ZoneCore.pas',
	ZoneRecv in 'Zone/ZoneRecv.pas',
	{$ENDIF}
//------------------------------------------------------------------------------
//                              Definitions for both.
//------------------------------------------------------------------------------
	SysUtils,
	Console in 'Console.pas';
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//                            Helios Main Routine
//  What it does -
//        This main routine keeps the console window open until a server
//    administrator sends it a command to shutdown.
//
//  Notes -
//    -To keep Helios in a Form-like organization, we've created TMainProc,
//      which replaces the Main Form. This also gives us the option to make
//      Helios log to a file instead of the screen by keeping all of our visual
//      related routines in one place =).
//    -HeliosVersion is located in Globals (only one place to change)
//    -This routine contains the loop that keeps the application open. On
//      termination of that loop, the program will shut down. Hence the return
//      value of Command.Parse(AInput).

//  Changes -
//    September 21st - RaX - Created Header.
//------------------------------------------------------------------------------
var
	AnInput   : string;
	Run       : Boolean;
begin

	//Tsusai 7/8/06 : Randomize added.  Learned from Prometheus.
	Randomize;
	Command := TCommands.Create;
	MainProc := TMainProc.Create(nil); //Form replacement
	Run := TRUE;

	MainProc.Console('  _    _          _   _                ');
	MainProc.Console(' | |  | |        | | (_)               ');
	MainProc.Console(' | |__| |   ___  | |  _    ___    ___  ');
	MainProc.Console(' |  __  |  / _ \ | | | |  / _ \  / __| ');
	MainProc.Console(' | |  | | |  __/ | | | | | (_) | \__ \ ');
	MainProc.Console(' |_|  |_|  \___| |_| |_|  \___/  |___/ ');


	MainProc.Console('');
	MainProc.Console(Format('- %s is starting...',[HeliosVersion]));

	MainProc.Startup; //Form Create replacement

	{Begin Main Loop}
	{Must keep application alive!}
	while Run do begin
		ReadLn(AnInput);
		Run := Command.Parse(AnInput);
	end;
	{End Main Loop}
	TerminateApplication;
end{Helios}.
