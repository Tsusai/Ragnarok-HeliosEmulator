//------------------------------------------------------------------------------
//XTimer				                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This is a timer thread I made because all the rest were either strictly
//    VCL, expensive, or didn't work. TXTimer...The RaX Timer =o
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit XTimer;

interface
uses
  Classes,
  SysUtils,
  SyncObjs;
type

//------------------------------------------------------------------------------
//TXTimer                                                                CLASS
//------------------------------------------------------------------------------
  TXTimer = class(TThread)
  private
    CriticalSection : TCriticalSection;
    fInterval : Cardinal;
    fEnabled  : Boolean;
    fOnTimer  : TNotifyEvent;

    Procedure   SetEnabled(Value : Boolean);
    Function    GetEnabled() : Boolean;

    Procedure   SetInterval(Value : Cardinal);
    Function    GetInterval() : Cardinal;

    Procedure   SetOnTimer(Value : TNotifyEvent);
    Function    GetOnTimer() : TNotifyEvent;

  public
    Constructor Create();
    Destructor  Destroy();override;

    Procedure   Execute;override;

    Property    Enabled : Boolean read GetEnabled write SetEnabled default FALSE;
    Property    Interval: Cardinal read GetInterval write SetInterval default 1000;
    Property    OnTimer : TNotifyEvent read GetOnTimer write SetOnTimer default NIL;
  end;
//------------------------------------------------------------------------------
implementation

//------------------------------------------------------------------------------
//TXTimer.Create                                                   CONSTRUCTOR
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
  inherited Create(FALSE);
  CriticalSection := TCriticalSection.Create;
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
  CriticalSection.Free();
  inherited Destroy;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TXTimer.Execute                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Houses the actual execution code of this thread. Executes OnTimer every
//    Interval.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TXTimer.Execute();
begin
  inherited;

  while (Enabled) do
  begin
    Sleep(Interval);//wait our specified time.
    if Assigned(OnTimer) then //if OnTimer is set
    begin
      Self.OnTimer(NIL);
    end;
  end;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//TXTimer Property related routines                           Property Related
//------------------------------------------------------------------------------
//	What they do-
//			They control the reading and writing to specific properties. They also
//    ensure we don't run into any nasty access violations by locking data
//    before it's read or written to.
//
//	Changes -
//		October 6th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TXTimer.SetEnabled(Value: Boolean);
var
  Changed : Boolean;
begin
  Changed := FALSE;

  CriticalSection.Enter;
  if Value <> fEnabled then
  begin
    fEnabled := Value;
    Changed := TRUE;
  end;
  CriticalSection.Leave;

  if Changed then
  begin
    if Value = TRUE then
    begin
      Execute;
    end;
  end;
end;

Function TXTimer.GetEnabled;
var
  Value : Boolean;
begin
  CriticalSection.Enter;
  Value := fEnabled;
  CriticalSection.Leave;
  Result := Value;
end;

Procedure TXTimer.SetInterval(Value: Cardinal);
begin
  CriticalSection.Enter;
  fInterval := Value;
  CriticalSection.Leave;
end;

Function TXTimer.GetInterval;
var
  Value : Cardinal;
begin
  CriticalSection.Enter;
  Value := fInterval;
  CriticalSection.Leave;
  Result := Value;
end;

Procedure TXTimer.SetOnTimer(Value: TNotifyEvent);
begin
  CriticalSection.Enter;
  fOnTimer := Value;
  CriticalSection.Leave;
end;

Function TXTimer.GetOnTimer;
var
  Value : TNotifyEvent;
begin
  CriticalSection.Enter;
  Value := fOnTimer;
  CriticalSection.Leave;
  Result := Value;
end;
//------------------------------------------------------------------------------
end.
