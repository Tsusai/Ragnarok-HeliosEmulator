//------------------------------------------------------------------------------
//SaveLoop				                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			The Save Loop is a separate thread which handles routine saving every X
//    seconds. Not only does it force a save every so often, but to make it
//    more efficient, it only saves what needs saving from each character in the
//    online character list based on timestamps. This unit interfaces directly
//    with the Database object. Saves are forced in other areas of the code
//    when certain events take place.
//
//  Notes -
//    -  Saves should only occur when something changes. TDateTime related work.
//
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit SaveLoop;

interface
uses
  Classes;
type
//------------------------------------------------------------------------------
//TCommands                                                               CLASS
//------------------------------------------------------------------------------
  TSaveLoop = class(TThread)
    Constructor Create(CreateSuspended : Boolean);
    Procedure Execute();override;
  end;{TSaveLoop}
//------------------------------------------------------------------------------
implementation
uses
  Globals,
  SysUtils,
  Console;

//------------------------------------------------------------------------------
//TSaveLoop.Create()				                                        CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our thread object.
//
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Constructor TSaveLoop.Create(CreateSuspended : Boolean);
begin
  FreeOnTerminate := TRUE;
  inherited;
end;{TSaveLoop.Create(CreateSuspended)}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TSaveLoop.Execute()				                                        Procedure
//------------------------------------------------------------------------------
//	What it does-
//			This routine IS the thread execution path. Anything you wanna get done
//    do right here.
//
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TSaveLoop.Execute();
begin
  inherited;
  MainProc.Console('DEBUG:: SAVED!');
  //Execute Saving routines here.
  //
end;{TSaveLoop.Execute()}
//------------------------------------------------------------------------------

end{SaveLoop}.
