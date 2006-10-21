//------------------------------------------------------------------------------
//SaveThread				                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is a singe execution thread which freeonterminates and forces a
//    save on our database.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit SaveThread;

interface
uses
  Classes;

type

//------------------------------------------------------------------------------
//TSaveThread                                                                CLASS
//------------------------------------------------------------------------------
  TSaveThread = class(TThread)
  public
    Constructor Create();

    Procedure   Execute;override;
    Procedure   Save();
  end;
//------------------------------------------------------------------------------
implementation
uses
  Console;
//------------------------------------------------------------------------------
//TSaveThread.Create                                                   CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes the TSaveThread class.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Constructor TSaveThread.Create();
begin
  FreeOnTerminate := TRUE;
	inherited Create(FALSE);

end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TSaveThread.Execute                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Houses the actual execution code of this thread. Executes OnTimer every
//    Interval.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TSaveThread.Execute();
begin
  inherited;
  Save();
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TSaveThread.Save                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Saves all online characters.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TSaveThread.Save();
begin
  MainProc.Console('Saving...');
  //Save code here
  MainProc.Console('Saved!!!');
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
end.
