//------------------------------------------------------------------------------
//AddFriendEvent                                                            UNIT
//------------------------------------------------------------------------------
//	What it does-
//      	An event used when add friend.
//
//	Changes -
//		[2007/12/08] - Aeomin - Created
//
//------------------------------------------------------------------------------
unit AddFriendEvent;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
	{RTL/VCL}
	//none
	{Project}
	Being,
	Event
	{3rd Party}
	//none
	;

type
//------------------------------------------------------------------------------
//TFriendRequestEvent
//------------------------------------------------------------------------------
	TFriendRequestEvent = class(TRootEvent)
	private
		ABeing : TBeing;
	public
		PendingFriend : LongWord;
		constructor Create(SetExpiryTime : LongWord; Being : TBeing);
	end;
//------------------------------------------------------------------------------
implementation

constructor TFriendRequestEvent.Create(SetExpiryTime : LongWord; Being : TBeing);
begin
	inherited Create(SetExpiryTime);
	Self.ABeing := Being;
end;
end.