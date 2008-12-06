//------------------------------------------------------------------------------
//RemoveGroundItemEvent                                                     UNIT
//------------------------------------------------------------------------------
//	What it does-
//		Sorry for this long name, but this is just an event to remove item
//	on ground
//
//	Changes -
//		[2008/12/05] Aeomin - Created
//
//------------------------------------------------------------------------------
unit RemoveGroundItemEvent;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
	Event,
	ItemInstance
	;

type

	TRemoveGroundItemEvent = class(TRootEvent)
	private
		AnItem : TItemInstance;
	public
		procedure Execute; override;
		constructor Create(SetExpiryTime : LongWord; Item : TItemInstance);
	end;
implementation

uses
	WinLinux;

procedure TRemoveGroundItemEvent.Execute;
begin
	AnItem.RemoveFromGround;
end;

constructor TRemoveGroundItemEvent.Create(SetExpiryTime : LongWord; Item : TItemInstance);
begin
	inherited Create(SetExpiryTime + GetTick);
	AnItem := Item;
end;
end.