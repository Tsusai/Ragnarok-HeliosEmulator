program Helios;

{$APPTYPE CONSOLE}

uses
	{$IFDEF MSWINDOWS}
	CharaServerTypes in 'Character\CharaServerTypes.pas',
	CharaServerPacket in 'Character\CharaServerPacket.pas',
	Socket in 'Common\Socket.pas',
	AccountTypes in 'Common\AccountTypes.pas',
	Globals in 'Common\Globals.pas',
	PacketTypes in 'Common\PacketTypes.pas',
	List32 in 'Common\3rdParty\List32.pas',
	AccountDB in 'Database\AccountDB.pas',
	LoginProcesses in 'Login\LoginProcesses.pas',
	ServerOptions in 'Config\ServerOptions.pas',
	GameObjects in 'Database\GameObjects.pas',
	SaveDB in 'Database\SaveDB.pas',
	{$ENDIF}

	{$IFDEF LINUX}
	CharaServerTypes in 'Character/CharaServerTypes.pas',
	CharaServerPacket in 'Character/CharaServerPacket.pas',
	Socket in 'Common/Socket.pas',
	AccountTypes in 'Common/AccountTypes.pas',
	Globals in 'Common/Globals.pas',
	PacketTypes in 'Common/PacketTypes.pas',
	List32 in 'Common/3rdParty/List32.pas',
	AccountDB in 'Database/AccountDB.pas',
	LoginProcesses in 'Login/LoginProcesses.pas',
	ServerOptions in 'Config/ServerOptions.pas',
	GameObjects in 'Database/GameObjects.pas',
	SaveDB in 'Database/SaveDB.pas',
	{$ENDIF}


	SysUtils,
	Console in 'Console.pas',
	Commands in 'Commands.pas';


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
		Write('Command: ');
		ReadLn(AnInput);
		Run := Command.Parse(AnInput);
	end;
	{End Main Loop}

	Command.Free;
	MainProc.Shutdown;
	FreeAndNil(MainProc);
end.
