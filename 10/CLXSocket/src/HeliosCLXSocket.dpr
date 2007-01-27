program HeliosCLXSocket;

uses
  madExcept,
  madLinkDisAsm,
  QForms,
  Main in 'Main.pas' {MainForm},
  Database in 'Database.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Project Helios, CLX Socket Style';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
