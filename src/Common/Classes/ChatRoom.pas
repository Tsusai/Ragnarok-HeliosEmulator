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
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
interface

uses
	Being,
	BeingList,
	GameObject
	;

type
	TChatRoom = class(TGameObject)
	private
		fOwner : TBeing;
		fSafe : Boolean;
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
		procedure UpdateStatus;
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
	ParameterList : TParameterList;
	OldOwner : TCharacter;
	AChara : TCharacter;
begin
	OldOwner := TCharacter(fOwner);

	fOwner := NewOwner;

	ParameterList := TParameterList.Create;
	ParameterList.AddAsObject(1,Self);
	OldOwner.AreaLoop(RemoveChatroomInArea,True,ParameterList);
	ParameterList.Free;

	for Index := 0 to Characters.Count - 1 do
	begin
		AChara := TCharacter(Characters[Index]);
		if fSafe AND (AChara = OldOwner) then
			Continue;
		SendChangeChatOwner(
			AChara,
			OldOwner.Name,
			Owner.Name
		);
	end;

	ParameterList := TParameterList.Create;
	ParameterList.AddAsObject(1,Self);
	NewOwner.AreaLoop(ShowChatroom,False,ParameterList);
	ParameterList.Free;
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
	fSafe := Safe;
	Result := False;
	Index := Characters.IndexOf(ID);
	if Index > -1 then
	begin
		AChara := TCharacter(Characters[Index]);

		if Owner.ID = ID then
		begin
			//Swap owner
			if Characters.Count > 1 then
			begin
				Owner := Characters[1];
			end;
		end;

		for Loop := 0 to Characters.Count - 1 do
		begin
			if Safe AND (Characters[Loop] = AChara) then
				Continue;
			SendQuitChat(
				TCharacter(Characters[Loop]),
				AChara.Name,
				Characters.Count-1,
				Kick
				);
		end;

		Characters.Delete(Index);

		AChara.ChatRoom := nil;

		if Characters.Count = 0 then
		begin
			ParameterList := TParameterList.Create;
			ParameterList.AddAsObject(1,Self);
			AChara.AreaLoop(RemoveChatroomInArea,True,ParameterList);
			ParameterList.Free;
		end else
		begin
			ParameterList := TParameterList.Create;
			ParameterList.AddAsObject(1,Self);
			Owner.AreaLoop(ShowChatroom,False,ParameterList);
			ParameterList.Free;
		end;

		Result := True;
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
	ParameterList : TParameterList;
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

		ParameterList := TParameterList.Create;
		ParameterList.AddAsObject(1,Self);
		AChara.AreaLoop(ShowChatroom,False,ParameterList);
		ParameterList.Free;

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
//UpdateStatus                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Refresh chatroom
//
//	Changes -
//		[2009/06/27] Aeomin - Created.
//
//------------------------------------------------------------------------------
procedure TChatRoom.UpdateStatus;
var
	ParameterList : TParameterList;
	Index : Integer;
begin
	for Index := 0 to Characters.Count - 1 do
	begin
		UpdateChatroom(
			TCharacter(Characters[Index]),
			Self
		);
	end;
	ParameterList := TParameterList.Create;
	ParameterList.AddAsObject(1,Self);
	Owner.AreaLoop(ShowChatroom,False,ParameterList);
	ParameterList.Free;
end;{UpdateStatus}
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
	TCharacter(fOwner).MapInfo.ChatroomList.AddObject(ID,Self);
	fSafe := False;
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
