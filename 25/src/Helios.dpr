program Helios;

uses
  QForms,
  Main in 'Main.pas' {MainForm},
  LoginPacket in 'Login\LoginPacket.pas',
  PacketFunc in 'Common\PacketFunc.pas',
  PacketTypes in 'Common\PacketTypes.pas',
  AccountTypes in 'Common\AccountTypes.pas',
  Globals in 'Common\Globals.pas',
  List32 in 'Common\3rdParty\List32.pas',
  Accounts in 'Database\Accounts.pas',
  CharaServerTypes in 'Character\CharaServerTypes.pas',
  CharaServerPacket in 'Character\CharaServerPacket.pas';

{$R *.res}

begin
	Application.Initialize;
	Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
