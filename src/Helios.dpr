program Helios;

{$APPTYPE CONSOLE}
uses
	{$IFDEF MSWINDOWS}
	madExcept,
	madLinkDisAsm,
	AccountDB in 'Database\AccountDB.pas',
	Account in 'Classes\Account.pas',
	Character in 'Classes\Character.pas',
	CharaServerProcess in 'Character\CharaServerProcess.pas',
	CharaServerTypes in 'Character\CharaServerTypes.pas',
	Commands in 'Classes\Commands.pas',
	GameGlobals in 'Common\GameGlobals.pas',
	GameObjects in 'Database\GameObjects.pas',
	Globals in 'Common\Globals.pas',
	List32 in 'Common\3rdParty\List32.pas',
	LoginProcesses in 'Login\LoginProcesses.pas',
	PacketTypes in 'Common\PacketTypes.pas',
	ServerOptions in 'Config\ServerOptions.pas',
	Socket in 'Common\Socket.pas',
	SQL in 'Common\SQL.pas',
	TCPServerRoutines in 'Common\TCPServerRoutines.pas',
	uMysqlClient in 'Common\3rdParty\SQL\uMysqlClient.pas',
	uMysqlCT in 'Common\3rdParty\SQL\uMysqlCT.pas',
	uMysqlErrors in 'Common\3rdParty\SQL\uMysqlErrors.pas',
	uMysqlNet in 'Common\3rdParty\SQL\uMysqlNet.pas',
	uMysqlVio in 'Common\3rdParty\SQL\uMysqlVio.pas',
	uMysqlNewPassword in 'Common\3rdParty\SQL\uMysqlNewPassword.pas',
	umysqlsha1 in 'Common\3rdParty\SQL\umysqlsha1.pas',
	WinLinux in 'Common\WinLinux.pas',
	{$ENDIF}

	{$IFDEF LINUX}
	AccountDB in 'Database/AccountDB.pas',
	Account in 'Classes/Account.pas',
	Character in 'Classes/Character.pas',
	CharaServerProcess in 'Character/CharaServerProcess.pas',
	CharaServerTypes in 'Character/CharaServerTypes.pas',
	Commands in 'Classes/Commands.pas',
	GameGlobals in 'Common/GameGlobals.pas',
	GameObjects in 'Database/GameObjects.pas',
	Globals in 'Common/Globals.pas',
	List32 in 'Common/3rdParty/List32.pas',
	LoginProcesses in 'Login/LoginProcesses.pas',
	PacketTypes in 'Common/PacketTypes.pas',
	ServerOptions in 'Config/ServerOptions.pas',
	Socket in 'Common/Socket.pas',
	SQL in 'Common/SQL.pas',
	TCPServerRoutines in 'Common/TCPServerRoutines.pas',
	uMysqlClient in 'Common/3rdParty/SQL/uMysqlClient.pas',
	uMysqlCT in 'Common/3rdParty/SQL/uMysqlCT.pas',
	uMysqlErrors in 'Common/3rdParty/SQL/uMysqlErrors.pas',
	uMysqlNet in 'Common/3rdParty/SQL/uMysqlNet.pas',
	uMysqlVio in 'Common/3rdParty/SQL/uMysqlVio.pas',
	uMysqlNewPassword in 'Common/3rdParty/SQL/uMysqlNewPassword.pas',
	umysqlsha1 in 'Common/3rdParty/SQL/umysqlsha1.pas',
	WinLinux in 'Common/WinLinux.pas',
	{$ENDIF}


	SysUtils,
	Console in 'Console.pas';


var
	AnInput : string;
	Run : Boolean;
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
end.
