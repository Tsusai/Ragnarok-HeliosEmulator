//------------------------------------------------------------------------------
//XTimer				                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is a timer thread I made because all the rest were either strictly
//    VCL, expensive, or didn't work.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit XTimer;

interface
uses
  Classes,
  Types;
type

//------------------------------------------------------------------------------
//TXTimer                                                                CLASS
//------------------------------------------------------------------------------
  TXTimer = class(TThread)
  private
    function RdTSC : Int64;
  public
    Interval    : Cardinal;
    OnTimer     : TNotifyEvent;
    Enabled     : Boolean;
    Constructor Create();
    Destructor  Destroy();override;

    Procedure   Execute;override;
  end;
//------------------------------------------------------------------------------
implementation
uses
  SysUtils;
//------------------------------------------------------------------------------
//TSaveLoop.Create                                                   CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes the TXTimer class.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Constructor TXTimer.Create();
begin
  Interval    := 60;
  Enabled     := FALSE;
	inherited Create(FALSE);
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TXTimer.Destroy                                                   DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys the TXTimer class.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Destructor TXTimer.Destroy();
begin
	inherited Destroy;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TXTimer.RdTSC                                                       Function
//------------------------------------------------------------------------------
//	What it does-
//			Gets the amount of clock cycles executed since power up.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
function TXTimer.RdTSC : int64;
asm
  db   $0f, $31
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TSaveLoop.Execute                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Houses the actual execution code of this thread. Executes OnTimer every
//    Interval based on clock cycles.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TXTimer.Execute();
var
  Frequency       : Int64;
  StartCycles     : Int64;
  EndCycles       : Int64;
  CyclesSinceStart: Int64;
  EndLoop         : Boolean;
begin
  inherited;
  EndLoop := FALSE;
  CyclesSinceStart := 1;
  //get clock cycle count at start
  StartCycles := RdTSC;
  //sleep for 1 second
  sleep(1000);
  //get ending clock cycles
  EndCycles := RdTSC;
  //solve out how many have passed, which is our frequency.
  Frequency := EndCycles - StartCycles;
  //makes sure we can enable and disable this as much as we want without the
  //thread terminating on us.
  while NOT Terminated do
  begin
    //while we're enabled keep track of cycles
	  while Enabled do
	  begin
      //get new start cycles - fixes instant execution after disable/enable.
      StartCycles := RdTSC;
      repeat
        //if we're still enabled... - fixes hang until loop finishes.
        if NOT Enabled then
        begin
          EndLoop := TRUE;
        end else
        begin
          //get cycles since start for each iteration
          CyclesSinceStart := RdTSC-StartCycles;
        end;
      //until we pass the interval
      until ((CyclesSinceStart DIV Frequency) >= Interval) OR EndLoop;
      //if our ontimer event exists, we execute it.
      if Assigned(OnTimer) AND NOT EndLoop then
      begin
        self.OnTimer(NIL);
      end;
      //we reset the endloop variable for the next iteration.
      EndLoop := FALSE;
	  end;
  end;

end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
end.
