//------------------------------------------------------------------------------
//EventList                                                            UNIT
//------------------------------------------------------------------------------
//  What it does -
//      A list of TRootEvents
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
unit EventList;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
uses
	Classes,
	SysUtils,
	Event;

type
//------------------------------------------------------------------------------
//TEventList                                                          CLASS
//------------------------------------------------------------------------------
	TEventList = Class(TThreadList)
	Public
		Constructor Create(OwnsEvents : Boolean);
		Destructor Destroy; override;

		Procedure Add(const AnEvent : TRootEvent);

		Procedure DeleteMovementEvents;
		Procedure DeleteAttackEvents;
	end;
//------------------------------------------------------------------------------


implementation
uses
	MovementEvent,
	AttackEvent;

//------------------------------------------------------------------------------
//Create                                                            CONSTRUCTOR
//------------------------------------------------------------------------------
//  What it does -
//      Initializes our Eventlist. Creates a storage area.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
constructor TEventList.Create;
begin
	inherited Create;
end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy                                                            DESTRUCTOR
//------------------------------------------------------------------------------
//  What it does -
//      Destroys our list and frees any memory used.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
destructor TEventList.Destroy;
var
	Index : Integer;
	AList : TList;
begin

	AList := LockList;
	for Index := (AList.Count-1) downto 0 do
	begin
		TObject(AList.Items[Index]).Free;
		AList.Delete(Index);
	end;
	UnlockList;

	inherited;
end;{Destroy}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Add                                                                 PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Adds a TRootEvent to the list.
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TEventList.Add(const AnEvent : TRootEvent);
begin
	inherited Add(AnEvent);
end;{Add}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//DeleteMovementEvents                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Deletes all events that are TMovementEvents
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TEventList.DeleteMovementEvents;
var
	Index : Integer;
	AList : TList;
begin
	AList := LockList;
	try
		for Index := (AList.Count - 1) downto 0 do
		begin
			if TObject(AList.Items[Index]) IS TMovementEvent then
			begin
				TMovementEvent(AList.Items[Index]).Free;
				AList.Delete(Index);
			end;
		end;
	finally
		UnlockList;
	end;
end;{DeleteMovementEvents}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//DeleteAttacktEvents                                                PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      Deletes all events that are TMovementEvents
//
//  Changes -
//    December 22nd, 2006 - RaX - Created.
//------------------------------------------------------------------------------
procedure TEventList.DeleteAttackEvents;
var
	Index : Integer;
	AList : TList;
begin
	AList := LockList;
	try
		for Index := (AList.Count - 1) downto 0 do
		begin
			if TObject(AList.Items[Index]) IS TAttackEvent then
			begin
				TObject(AList.Items[Index]).Free;
				AList.Delete(Index);
			end;
		end;
	finally
		UnlockList;
	end;
end;{DeleteAttackEvents}
//------------------------------------------------------------------------------
end.
