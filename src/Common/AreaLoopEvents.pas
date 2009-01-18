//------------------------------------------------------------------------------
//AreaLoopEvents                                                            UNIT
//------------------------------------------------------------------------------
//	What it does-
//		Sub Procedures for area loops
//			WARNING
//	THIS UNIT SHOULD ONLY USED BY BEING.PAS
//
//	Changes -
//		March 20th, 2007 - Aeomin - Created Header
//[2007/03/28] CR - Cleaned up uses clauses - unneeded units removed, and some
//		moved to local to the routines, not the declaration.
//[2007/03/28] CR - Made changes to parameter lists for the routines here,
//		following the new TLoopCall procedure declaration in Being.  All 
//		parameteters are constant, and eliminated the entirely uncalled X,Ys so
//		that we only have 2 parameters left (faster calls this way).
//
//------------------------------------------------------------------------------
unit AreaLoopEvents;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
	{RTL/VCL}
	Classes,
	{Project}
	GameObject,
	Being,
	ParameterList
	{Third Party}
	//none
	;

	procedure ShowBeingWalk(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
		);
	procedure ShowAreaObjects(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
		);
	procedure TeleOut(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
		);
	procedure UpdateDir(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
		);
	procedure Effect(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
		);

	procedure Emotion(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
		);

	procedure ShowSitStand(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
	);

	procedure ShowInitialAction(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
	);

	procedure ShowDeath(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
	);

	procedure JobChange(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
	);

	procedure ShowDropitem(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
	);

	procedure ShowPickupItem(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
	);

	procedure RemoveGroundItem(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
	);

	procedure ShowAttack(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
	);

	procedure ShowChangeLook(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
	);

	procedure SpawnMob(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
	);

	procedure ShowSkillUnit(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
	);

	procedure ShowChatroom(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
	);

implementation

uses
	{RTL/VCL}
	Math,
	{Project}
	Character,
	ZoneSend,
	GameConstants,
	GameTypes,
	ItemInstance,
	Globals,
	Mob,
	ChatRoom
	{Third Party}
	//none
	;


(*- Procedure -----------------------------------------------------------------*
ShowBeingWalk
--------------------------------------------------------------------------------
Overview:
--
	Show ACurrentBeing walking to other characters in visible range.

--
Revisions:
--
[2007/03/22] Aeomin - Created Header
[2007/03/28] CR - Modified Comment Header, eliminated with clause.  Reduced
	parameter list, and made all constant, to follow new TLoopCall declaration.
[2007/05/25] Tsusai - Added IS TCHARACTER conditionals	
[yyyy/mm/dd] <Author> - <Comment>
*-----------------------------------------------------------------------------*)
Procedure ShowBeingWalk(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
var
	ABeing : TBeing;
Begin
	if AObject is TCharacter then
	begin
		ABeing := TBeing(ACurrentObject);
		ZoneWalkingBeing(
			ABeing,
			ABeing.Position,
			ABeing.Path[ABeing.Path.Count -1],
			TCharacter(AObject).ClientInfo
		);
	end;
End; (* Proc ShowBeingWalk
*-----------------------------------------------------------------------------*)


(*- Procedure -----------------------------------------------------------------*
ShowAreaBeings
--------------------------------------------------------------------------------
Overview:
--
Show teleport in effect for ACurrentBeing to other characters in view


--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/03/22] Aeomin - Created Header
[2007/03/28] CR - Modified Comment Header.  Reduced parameter list, and made
	all constant, to follow new TLoopCall declaration.
[2007/05/25] Tsusai - Added IS TCHARACTER conditionals
*-----------------------------------------------------------------------------*)
Procedure ShowAreaObjects(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
Begin
	if AObject is TCharacter then
	begin
		ZoneSendBeing(TBeing(ACurrentObject), TCharacter(AObject), True);
	end;

	if (ACurrentObject is TCharacter)AND(AObject is TBeing) then
	begin
		ZoneSendBeing(TBeing(AObject), TCharacter(ACurrentObject));
	end else
	if AObject is TItemInstance then
	begin
		SendGroundItem(TCharacter(ACurrentObject), TItemInstance(AObject));
	end;

	//Show chatroom
	if (ACurrentObject is TCharacter)AND(AObject is TCharacter) then
	begin
		if Assigned(TCharacter(AObject).ChatRoom) then
		begin
			if TCharacter(AObject).ChatRoom.Owner = AObject then
			begin
				DisplayChatroomBar(TCharacter(ACurrentObject),TCharacter(AObject).ChatRoom);
			end;
		end;
	end;
End; (* Proc ShowAreaBeings
*-----------------------------------------------------------------------------*)


(*- Procedure -----------------------------------------------------------------*
TeleOut
--------------------------------------------------------------------------------
Overview:
--
	Shows teleport out effect for ACurrentBeing to other characters in view.

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/03/22] Aeomin - Created Header
[2007/03/28] CR - Modified Comment Header.  Reduced parameter list, and made
	all constant, to follow new TLoopCall declaration.
[2007/05/25] Tsusai - Added IS TCHARACTER conditionals
*-----------------------------------------------------------------------------*)
Procedure TeleOut(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
Begin
	if AObject is TCharacter then
	begin
		ZoneDisappearBeing(TBeing(ACurrentObject), TCharacter(AObject).ClientInfo, 2);
	end;
End; (* Proc TeleOut
*-----------------------------------------------------------------------------*)


(*- Procedure -----------------------------------------------------------------*
UpdateDir
--------------------------------------------------------------------------------
Overview:
--
Show ACurrentBeing's new direction to other characters in view


--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/03/22] Aeomin - Created Header
[2007/03/28] CR - Modified Comment Header.  Reduced parameter list, and made
	all constant, to follow new TLoopCall declaration.
[2007/05/25] Tsusai - Added IS TCHARACTER conditionals
*-----------------------------------------------------------------------------*)
Procedure UpdateDir(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
Begin
	if AObject is TCharacter then
	begin
		ZoneUpdateDirection(TBeing(ACurrentObject), TCharacter(AObject).ClientInfo);
	end;
End; (* Proc UpdateDir
*-----------------------------------------------------------------------------*)


//------------------------------------------------------------------------------
//Effect                                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Send effect packet
//
//	Changes-
//		[2007/11/24] Aeomin - Created.
//------------------------------------------------------------------------------
procedure Effect(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
begin
	if AObject is TCharacter then
	begin
		SendSpecialEffect(
			Tbeing(AObject),
			TCharacter(ACurrentObject).ClientInfo,
			AObjectID,
			AParameters.GetAsLongWord(1)
		);
	end;
end;{Effect}
//------------------------------------------------------------------------------

procedure Emotion(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
begin
	if AObject is TCharacter then
	begin
		SendEmotion(
			AObjectID,
			TCharacter(AObject).ClientInfo,
			EnsureRange(
				AParameters.GetAsLongWord(1),
				Low(Byte),
				High(Byte)
			)
		);
	end;
end;

//------------------------------------------------------------------------------
//ShowAction                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Show sitting/standing to surrounding characters.
//
//	Changes-
//		[2007/12/24] RaX - Created.
//------------------------------------------------------------------------------
procedure ShowSitStand(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
var
	ACurrentCharacter : TCharacter;
	ACharacter : TCharacter;
begin
	if (ACurrentObject is TCharacter) AND (AObject is TCharacter) then
	begin
		ACharacter := TCharacter(AObject);
		ACurrentCharacter := TCharacter(ACurrentObject);
		case ACurrentCharacter.BeingState of
			BeingSitting :begin
					DoAction(
						ACharacter.ClientInfo,
						AObjectID,
						0, 0, 0, ACTION_SIT, 0, 0, 0
					);
			end;

			BeingStanding :begin
					DoAction(
						ACharacter.ClientInfo,
						AObjectID,
						0, 0, 0, ACTION_STAND, 0, 0, 0
					);
			end;
		end;
	end;
end;{ShowAction}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ShowAction                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Show Actions of surrounding characters on connecting to map.
//
//	Changes-
//		[2007/12/24] RaX - Created.
//------------------------------------------------------------------------------
procedure ShowInitialAction(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
var
	ACurrentCharacter : TCharacter;
	ACharacter : TCharacter;
begin
	if (ACurrentObject is TCharacter) AND (AObject is TCharacter) then
	begin
		ACharacter := TCharacter(AObject);
		ACurrentCharacter := TCharacter(ACurrentObject);
		case ACharacter.BeingState of
			BeingSitting :
				begin
					DoAction(ACurrentCharacter.ClientInfo, ACharacter.ID, 0, 0, 0, ACTION_SIT, 0, 0, 0);
				end;

			BeingStanding :
				begin
					DoAction(ACurrentCharacter.ClientInfo, ACharacter.ID, 0, 0, 0, ACTION_STAND, 0, 0, 0);
				end;

			BeingAttacking :
				begin
					DoAction(ACharacter.ClientInfo, ACurrentCharacter.ID, ACurrentCharacter.TargetID, 0, 0, ACTION_ATTACK, 0, 0, 0);
				end;

			BeingDead :
				begin
					ZoneDisappearBeing(TBeing(AObject), TCharacter(ACurrentObject).ClientInfo, 1);
				end;
		end;
	end;
end;{ShowAction}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ShowDeath                                                            PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Show death to all players arround
//
//	Changes-
//		[2007/12/28] RaX - Created.
//------------------------------------------------------------------------------
procedure ShowDeath(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
begin
	if (ACurrentObject is TCharacter) AND (AObject is TCharacter) then
	begin
		ZoneDisappearBeing(
			TBeing(ACurrentObject),
			TCharacter(AObject).ClientInfo,
			1,
			AObjectID
		);
	end;
end;{ShowDeath}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//JobChange                                                            PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Show job change to everyone in the area
//
//	Changes-
//		[2008/9/27] RaX - Created.
//------------------------------------------------------------------------------
procedure JobChange(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
begin
	if (ACurrentObject is TCharacter) AND (AObject is TCharacter) then
	begin
		SendUpdatedLook(
			TCharacter(ACurrentObject),
			AObjectID,
			LOOK_JOB,
			TCharacter(AObject).JID,
			0
		);
	end;
end;{ShowDeath}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ShowDropitem                                                         PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Drops an item to ground
//
//	Changes-
//		[2008/10/02] Aeomin - Created
//------------------------------------------------------------------------------
procedure ShowDropitem(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
begin
	if (AObject is TCharacter) then
	begin
		SendDropItem(
			TCharacter(AObject),
			TItemInstance(AParameters.GetAsObject(1)),
			AParameters.GetAsLongWord(2)
		);
	end;
end;{ShowDropitem}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ShowPickupItem                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Show pickup animation
//
//	Changes-
//		[2008/10/03] Aeomin - Created
//------------------------------------------------------------------------------
procedure ShowPickupItem(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
begin
	if (AObject is TCharacter) then
	begin
		SendPickUpItemAnimation(
			TCharacter(AObject),
			AObjectID,
			AParameters.GetAsLongWord(1)
		);
	end;
end;{ShowPickupItem}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//RemoveGroundItem                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Delete item
//
//	Changes-
//		[2008/10/03] Aeomin - Created
//------------------------------------------------------------------------------
procedure RemoveGroundItem(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
begin
	if (AObject is TCharacter) then
	begin
		SendRemoveGroundItem(
			TCharacter(AObject),
			AParameters.GetAsLongWord(1)
		);
	end;
end;{RemoveGroundItem}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ShowAttack                                                           PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Shows a being attacking another being
//
//	Changes-
//		[2008/10/03] Aeomin - Created
//------------------------------------------------------------------------------
procedure ShowAttack(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
begin
	if (AObject is TCharacter) then
	begin
		DoAction(
			TCharacter(AObject).ClientInfo,
			AObjectID,
			AParameters.GetAsLongWord(2),
			AParameters.GetAsLongWord(1) DIV 2,
			0,
			ACTION_ATTACK,
			EnsureRange(AParameters.GetAsLongWord(3),
			0,
			High(Word)),
			AParameters.GetAsLongWord(5),
			AParameters.GetAsLongWord(4)
		);
	end;
end;{RemoveGroundItem}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ShowChangeLook                                                      PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Sprite change
//
//	Changes-
//		[2008/10/13] Aeomin - Created
//------------------------------------------------------------------------------
procedure ShowChangeLook(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
begin
	if (AObject is TCharacter) then
	begin
		SendUpdatedLook(
			TCharacter(AObject),
			AObjectID,
			TLookTypes(AParameters.GetAsLongWord(1)),
			AParameters.GetAsLongWord(2),
			0
		);
	end;
end;{ShowChangeLook}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SpawnMob                                                             PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Send mob to players; similar to ShowAreaObjects?
//
//	Changes-
//		[2008/12/12] Aeomin - Created
//------------------------------------------------------------------------------
procedure SpawnMob(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
begin
	if AObject is TCharacter then
	begin
		ZoneSendBeing(TBeing(ACurrentObject), TCharacter(AObject), True);
	end;
end;{SpawnMob}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ShowSkillUnit                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Send a single unit of skill effect to client
//
//	Changes-
//		[2009/01/16] Aeomin - Created
//------------------------------------------------------------------------------
procedure ShowSkillUnit(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
begin
	if AObject is TCharacter then
	begin
		SendSkillGroundUnit(
			TCharacter(AObject),
			AParameters.GetAsLongWord(1),
			AObjectID,
			AParameters.GetAsLongWord(2),
			AParameters.GetAsLongWord(3) ,
			AParameters.GetAsLongWord(4)
		);
	end;
end;{ShowSkillUnit}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// ShowChatroom                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Tell everyone about your chatroom!
//
//	Changes-
//		[2009/01/17] Aeomin - Created
//------------------------------------------------------------------------------
procedure ShowChatroom(
	const ACurrentObject	: TGameObject;
	const AObject		: TGameObject;
	const AObjectID		: LongWord;
	const AParameters	: TParameterList = nil
);
begin
	if AObject is TCharacter then
	begin
		DisplayChatroomBar(
			TCharacter(AObject),
			TChatRoom(AParameters.GetAsObject(1))
		);
	end;
end;{ShowChatroom}
//------------------------------------------------------------------------------
end.
