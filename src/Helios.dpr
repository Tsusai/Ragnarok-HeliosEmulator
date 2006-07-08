program Helios;

{$APPTYPE CONSOLE}

uses
	{$IFDEF MSWINDOWS}
	AccountDB in 'Database\AccountDB.pas',
	AccountTypes in 'Common\AccountTypes.pas',
	CharacterTypes in 'Common\CharacterTypes.pas',
	CharaServerPacket in 'Character\CharaServerPacket.pas',
	CharaServerTypes in 'Character\CharaServerTypes.pas',
	Commands in 'Classes\Commands.pas',
	GameObjects in 'Database\GameObjects.pas',
	Globals in 'Common\Globals.pas',
	List32 in 'Common\3rdParty\List32.pas',
	LoginProcesses in 'Login\LoginProcesses.pas',
	PacketTypes in 'Common\PacketTypes.pas',
	SaveDB in 'Database\SaveDB.pas',
	ServerOptions in 'Config\ServerOptions.pas',
	Socket in 'Common\Socket.pas',
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
	AccountTypes in 'Common/AccountTypes.pas',
	CharacterTypes in 'Common/CharacterTypes.pas',
	CharaServerPacket in 'Character/CharaServerPacket.pas',
	CharaServerTypes in 'Character/CharaServerTypes.pas',
	Commands in 'Classes/Commands.pas',
	GameObjects in 'Database/GameObjects.pas',
	Globals in 'Common/Globals.pas',
	List32 in 'Common/3rdParty/List32.pas',
	LoginProcesses in 'Login/LoginProcesses.pas',
	PacketTypes in 'Common/PacketTypes.pas',
	SaveDB in 'Database/SaveDB.pas',
	ServerOptions in 'Config/ServerOptions.pas',
	Socket in 'Common/Socket.pas',
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
	Command : TCommands;
	Run : Boolean;
begin
	Command := TCommands.Create;
	MainProc := TMainProc.Create(nil); //Form replacement
	Run := TRUE;

	MainProc.Startup; //Form Create replacement

	{Begin Main Loop}
	while Run do begin
		ReadLn(AnInput);
		Run := Command.Parse(AnInput);
	end;
	{End Main Loop}

	Command.Free;
	MainProc.Shutdown;
	FreeAndNil(MainProc);
end.
