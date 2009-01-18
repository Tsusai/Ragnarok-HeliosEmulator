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
		function Quit(const ID:LongWord;const Kick:Boolean=False;const Safe : Boolean=False):Boolean;
		constructor Create(const Owner:TBeing);
		destructor Destroy; override;
	end;
implementation

uses
	Character,
	ZoneSend,
	AreaLoopEvents,
	ParameterList
	;

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
//Quit                                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Attempt to quit the channel
//
//	Changes -
//		[2009/01/18] Aeomin - Created.
//
//------------------------------------------------------------------------------
function TChatRoom.Quit(const ID:LongWord;const Kick:Boolean=False;const Safe : Boolean=False):Boolean;
var
	Index : Integer;
	AChara : TCharacter;
	Loop : Integer;
	ParameterList : TParameterList;
begin
	Result := False;
	Index := Characters.IndexOf(ID);
	if Index > -1 then
	begin
		AChara := TCharacter(Characters[Index]);
		for Loop := 0 to Characters.Count - 1 do
		begin
			if Safe AND (Characters[Loop] = AChara) then
				Continue;
			SendQuitChat(
				TCharacter(Characters[Loop]),
				Characters.Count-1,
				Kick
				);
		end;
		Characters.Delete(Index);
		AChara.ChatRoom := nil;
		if Owner.ID = ID then
		begin
			//Swap owner
			if Characters.Count > 0 then
			begin
				Owner := Characters[0];
			end;
		end;
		Result := True;
		if Characters.Count = 0 then
		begin
			ParameterList := TParameterList.Create;
			ParameterList.AddAsLongWord(1,Self.ID);
			AChara.AreaLoop(RemoveChatroomInArea,True,ParameterList);
			ParameterList.Free;
//			Free;
		end;
	end;
end;{Quit}
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
constructor TChatRoom.Create(const Owner:TBeing);
begin
	fOwner := Owner;
	Characters := TBeingList.Create(FALSE);
	Characters.Add(fOwner);
	ID := TCharacter(fOwner).MapInfo.NewObjectID;
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
	TCharacter(fOwner).MapInfo.DisposeObjectID(ID);
	inherited;
end;{Destroy}
//------------------------------------------------------------------------------
end.