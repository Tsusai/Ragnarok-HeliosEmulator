//------------------------------------------------------------------------------
//XTimer				                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is a timer thread I made because all the rest were either strictly
//    VCL, expensive, or didn't work. Executes our save procedures every
//    Interval.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit SaveLoop;

interface
uses
  Classes,
  SysUtils,
  SyncObjs;
type

//------------------------------------------------------------------------------
//TSaveLoop                                                                CLASS
//------------------------------------------------------------------------------
  TSaveLoop = class(TThread)
  private
		fInterval : Cardinal;

    Procedure   SetInterval(Value : Cardinal);
    Function    GetInterval() : Cardinal;

  public
    Constructor Create();
    Destructor  Destroy();override;

    Procedure   Execute;override;
    Procedure   Save();
    Property    Interval: Cardinal read GetInterval write SetInterval;
  end;
//------------------------------------------------------------------------------
implementation
uses
  Console;
//------------------------------------------------------------------------------
//TSaveLoop.Create                                                   CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes the TSaveLoop class.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Constructor TSaveLoop.Create();
begin
	inherited Create(FALSE);
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TSaveLoop.Destroy                                                   DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys the TSaveLoop class.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Destructor TSaveLoop.Destroy();
begin
	inherited Destroy;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TSaveLoop.Execute                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Houses the actual execution code of this thread. Executes OnTimer every
//    Interval.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TSaveLoop.Execute();
begin
  inherited;
	while NOT Terminated do
	begin
		Sleep(Interval);
    Save();
	end;

end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TSaveLoop.Save                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Saves all online characters.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TSaveLoop.Save();
begin
  MainProc.Console('Saving...');
  //Save code here
  MainProc.Console('Saved!!!');
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TSaveLoop Property related routines                           Property Related
//------------------------------------------------------------------------------
//	What they do-
//			They control the reading and writing to specific properties.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TSaveLoop.SetInterval(Value: Cardinal);
begin
	fInterval := Value;
end;

Function TSaveLoop.GetInterval;
var
  Value : Cardinal;
begin
	Value := fInterval;
	Result := Value;
end;

//------------------------------------------------------------------------------
end.
