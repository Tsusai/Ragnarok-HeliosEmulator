program Helios;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  IdTCPServer,

  {$IFDEF MSWINDOWS}
  CharaServerTypes in 'Character\CharaServerTypes.pas',
  CharaServerPacket in 'Character\CharaServerPacket.pas',
  Socket in 'Common\Socket.pas',
  AccountTypes in 'Common\AccountTypes.pas',
  Globals in 'Common\Globals.pas',
  PacketTypes in 'Common\PacketTypes.pas',
  List32 in 'Common\3rdParty\List32.pas',
  Database in 'Database\Database.pas',
  LoginProcesses in 'Login\LoginProcesses.pas',
  {$ENDIF}

	//Tsusai - Linux directories are / based.  If we used / just mainly, we lose
	//ability to breakpoint in code.

	{$IFDEF LINUX}
	CharaServerTypes in 'Character/CharaServerTypes.pas',
	CharaServerPacket in 'Character/CharaServerPacket.pas',
	Socket in 'Common/Socket.pas',
	AccountTypes in 'Common/AccountTypes.pas',
	Globals in 'Common/Globals.pas',
	PacketTypes in 'Common/PacketTypes.pas',
	List32 in 'Common/3rdParty/List32.pas',
	Database in 'Database/Database.pas',
	LoginProcesses in 'Login/LoginProcesses.pas',
	{$ENDIF}

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
    ReadLn(AnInput);
    Run := Command.Parse(AnInput);
  end;
  {End Main Loop}
  
  Command.Free;
	MainProc.Shutdown;
	FreeAndNil(MainProc);
end.
