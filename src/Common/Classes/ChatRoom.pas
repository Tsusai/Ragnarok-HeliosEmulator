//------------------------------------------------------------------------------
//ChatRoom                                                                  UNIT
//------------------------------------------------------------------------------
//	What it does-
//		Handles almost everything about chatroom
//
//	Changes -
//		[2009/01/17] Aeomin - Created.
//
//------------------------------------------------------------------------------
unit ChatRoom;

interface

uses
	Being,
	BeingList
	;

type
	TChatRoom = class
	private
		fOwner : TBeing;
		procedure SetOwner(NewOwner : TBeing);
	public
		ID : LongWord;
		isPublic : Boolean;
		PassWord : String;
		Title : String;
		Limit : Word;
		Characters : TBeingList;
		property Owner : TBeing read fOwner write SetOwner;
		constructor Create(Owner:TBeing);
		destructor Destroy; override;
	end;
implementation


//------------------------------------------------------------------------------
//SetOwner                                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Update owner.
//
//	Changes -
//		[2009/01/17] Aeomin - Created.
//
//------------------------------------------------------------------------------
procedure TChatRoom.SetOwner(NewOwner : TBeing);
begin
	{TODO:Update packet?}
	fOwner := NewOwner;
end;{SetOwner}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Create                                                             CONSTRUCOTR
//------------------------------------------------------------------------------
//	What it does-
//		Initialize class.
//
//	Changes -
//		[2009/01/17] Aeomin - Created.
//
//------------------------------------------------------------------------------
constructor TChatRoom.Create(Owner:TBeing);
begin
	fOwner := Owner;
	Characters := TBeingList.Create(FALSE);
	Characters.Add(fOwner);
end;{Creat}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//CDestroy                                                            DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//		Finalization class
//
//	Changes -
//		[2009/01/17] Aeomin - Created.
//
//------------------------------------------------------------------------------
destructor TChatRoom.Destroy;
begin
	Characters.Free;
	inherited;
end;{Destroy}
//------------------------------------------------------------------------------
end.