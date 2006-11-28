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
	Classes;

type

//------------------------------------------------------------------------------
//TXTimer                                                                CLASS
//------------------------------------------------------------------------------
	TXTimer = class(TThread)
	public
		Interval    : Int64;
		OnTimer     : TNotifyEvent;
		Enabled     : Boolean;
		Constructor Create();
		Destructor  Destroy();override;

		Procedure   Execute;override;
	end;
//------------------------------------------------------------------------------
implementation
uses
	SysUtils,
	WinLinux;
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
	EndLoop   : Boolean;
	Index     : Integer;
begin
	inherited;
	EndLoop := FALSE;

	Priority := PriorityLow;//TpLowest;
	//makes sure we can enable and disable this as much as we want without the
	//thread terminating on us.
	while NOT Terminated do
	begin
		//while we're enabled keep track of cycles
		while Enabled do
		begin
			for Index := 1 to (Interval*10) do
			begin
				if Enabled AND NOT Terminated then
				begin
					Sleep(100);
				end else
				begin
					EndLoop := TRUE;
					break;
				end;
			end;
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
