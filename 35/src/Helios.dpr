program Helios;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  CharaServerTypes in 'Character\CharaServerTypes.pas',
  CharaServerPacket in 'Character\CharaServerPacket.pas',
  Socket in 'Common\Socket.pas',
  AccountTypes in 'Common\AccountTypes.pas',
  Globals in 'Common\Globals.pas',
  PacketTypes in 'Common\PacketTypes.pas',
  List32 in 'Common\3rdParty\List32.pas',
  Database in 'Database\Database.pas',
  LoginProcesses in 'Login\LoginProcesses.pas',
  IdTCPServer,
  Console in 'Console.pas';

var
	AnInput : string;
	Terminate : Boolean;

begin
	Terminate := false;
	MainProc := TMainProc.Create(nil); //Form replacement

	MainProc.Startup; //Form Create replacement

	while (true) do begin
		ReadLn(AnInput);
		Terminate := MainProc.ParseInput(AnInput);
		if Terminate then break;
	end;

	MainProc.Shutdown;
	FreeAndNil(MainProc);

end.
