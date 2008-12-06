//------------------------------------------------------------------------------
//CGroundItemRemovalEventThread                                             UNIT
//------------------------------------------------------------------------------
//	What it does-
//		This thread currently only for one purpose:
//			To clean up ground items...
//
//	Changes -
//		[2008/12/05] Aeomin - Created.
//
//------------------------------------------------------------------------------
unit GroundItemRemovalEventThread;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
	{RTL/VCL}
	Classes,
	{Project}
	{Third Party}
	IdThread
	;

type
//------------------------------------------------------------------------------
//TCharacterEventThread
//------------------------------------------------------------------------------
	TGroundItemEventThread = class(TIdThread)
	public
		GroundItemList : TThreadList;
		constructor Create(AGroundItemList : TThreadList);reintroduce;
		procedure 	Run;override;
	end;
//------------------------------------------------------------------------------

implementation

uses
	{RTL/VCL}
	SysUtils,
	SyncObjs,
	{Project}
	Main,
	Event,
	ItemInstance,
	{3rd Party}
	WinLinux
	;

//------------------------------------------------------------------------------
//Create                                                             CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//		Initializes our EventThread
//
//	Changes -
//		[2008/12/05] Aeomin - Created
//
//------------------------------------------------------------------------------
Constructor TGroundItemEventThread.Create(AGroundItemList : TThreadList);
begin
	inherited Create(TRUE, TRUE, 'GroundItemRemovalThread');
	GroundItemList := AGroundItemList;
end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Run                                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Let's do it!
//
//	Changes -
//		[2008/12/05] Aeomin - Created
//
//------------------------------------------------------------------------------
Procedure TGroundItemEventThread.Run;
var
	EventIndex		: Integer;
	CurrentTime		: LongWord;
	AnItem			: TItemInstance;
	AGroundItemList		: TList;
	CriticalSection		: TCriticalSection;
begin
	//Get the current "Tick" or time.
	CurrentTime := GetTick;
	CriticalSection := TCriticalSection.Create;
	//Loop through the character list
	CriticalSection.Enter;
	AGroundItemList := GroundItemList.LockList;
	try
		//Loop through each character's eventlist.
		for EventIndex := (AGroundItemList.Count - 1) downto 0 do
		begin
			AnItem := AGroundItemList[EventIndex];
			//Check to see if the event needs to be fired.
			if CurrentTime >= AnItem.RemovalTime then
			begin
				AGroundItemList.Delete(EventIndex);
				AnItem.RemoveFromGround;
				AnItem.Free;
			end;
		end;
	finally
		GroundItemList.UnlockList;
	end;
	CriticalSection.Leave;
	CriticalSection.Free;
	//Free up the processor
	Sleep(MainProc.ZoneServer.Options.EventTick);
end;//Run
//------------------------------------------------------------------------------
end.