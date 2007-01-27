program Helios;

{$APPTYPE CONSOLE}

uses
	SysUtils,
	IdTCPServer,
	Console in 'Console.pas',
	{$IFDEF MSWINDOWS}
	CharaServerTypes in 'Character\CharaServerTypes.pas',
	CharaServerPacket in 'Character\CharaServerPacket.pas',
	Socket in 'Common\Socket.pas',
	AccountTypes in 'Common\AccountTypes.pas',
	Globals in 'Common\Globals.pas',
	PacketTypes in 'Common\PacketTypes.pas',
	List32 in 'Common\3rdParty\List32.pas',
	Database in 'Database\Database.pas',
	LoginProcesses in 'Login\LoginProcesses.pas';
	{$ENDIF}
	//Tsusai - Linux directories are / based.  If we used / just mainly, we loose
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
	LoginProcesses in 'Login/LoginProcesses.pas';
	{$ENDIF}

var
	AnInput : string;
	Terminate : Boolean;

begin
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
