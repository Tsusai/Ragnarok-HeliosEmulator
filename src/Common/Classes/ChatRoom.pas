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
		function Join(const ABeing:TBeing;const Password:String):Boolean;
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
var
	Index : Integer;
begin
	fOwner := NewOwner;
	for Index := 0 to Characters.Count - 1 do
	begin
		SendChangeChatOwner(
			TCharacter(Characters[Index]),
			fOwner.Name
		);
	end;
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
//Join                                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Attempt to join the channel
//
//	Changes -
//		[2009/01/18] Aeomin - Created.
//
//------------------------------------------------------------------------------
function TChatRoom.Join(const ABeing:TBeing;const Password:String):Boolean;
var
	AChara : TCharacter;
	Index : Word;
begin
	Result := False;
	AChara := TCharacter(Abeing);
	if (Characters.Count >= Limit) then
	begin
		SendJoinChatFailed(
			AChara,
			0
		);
	end else
	if (Characters.IndexOf(AChara.ID) > -1) then
	begin
		SendJoinChatFailed(
			AChara,
			1
		);
	end;
	if (isPublic or (Password = self.PassWord)) then
	begin
		AChara.EventList.DeleteMovementEvents;
		AChara.EventList.DeleteAttackEvents;
		AChara.ChatRoom := Self;
		for Index := 0 to Characters.Count - 1 do
		begin
			SendNotifyJoinChat(
				TCharacter(Characters.Items[Index]),
				Characters.Count +1,
				AChara.Name
			);
		end;
		Characters.Add(ABeing);
		SendJoinChatOK(
			AChara,
			Self
		);
		Result := True;
	end else
	begin
		SendJoinChatFailed(
			AChara,
			1
		);
	end;
end;{Join}
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
	TCharacter(fOwner).MapInfo.ChatroomList.AddObject(ID,Self)
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
var
	Index : Integer;
begin
	Characters.Free;
	Index := TCharacter(fOwner).MapInfo.ChatroomList.IndexOf(ID);
	if Index > -1 then
	begin
		TCharacter(fOwner).MapInfo.ChatroomList.Delete(Index);
	end;
	TCharacter(fOwner).MapInfo.DisposeObjectID(ID);
	inherited;
end;{Destroy}
//------------------------------------------------------------------------------
end.