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
		const ACurrentObject : TGameObject;
		const AObject        : TGameObject;
		const AParameters    : TParameterList = nil
		);
	procedure ShowAreaObjects(
		const ACurrentObject : TGameObject;
		const AObject        : TGameObject;
		const AParameters    : TParameterList = nil
		);
	procedure TeleOut(
		const ACurrentObject : TGameObject;
		const AObject        : TGameObject;
		const AParameters    : TParameterList = nil
		);
	procedure UpdateDir(
		const ACurrentObject : TGameObject;
		const AObject        : TGameObject;
		const AParameters    : TParameterList = nil
		);
	procedure Effect(
		const ACurrentObject : TGameObject;
		const AObject        : TGameObject;
		const AParameters    : TParameterList = nil
		);

	procedure Emotion(
		const ACurrentObject : TGameObject;
		const AObject        : TGameObject;
		const AParameters    : TParameterList = nil
		);

	procedure ShowSitStand(
		const ACurrentObject : TGameObject;
		const AObject        : TGameObject;
		const AParameters    : TParameterList = nil
	);

	procedure ShowInitialAction(
		const ACurrentObject : TGameObject;
		const AObject        : TGameObject;
		const AParameters    : TParameterList = nil
	);

	procedure ShowDeath(
		const ACurrentObject : TGameObject;
		const AObject        : TGameObject;
		const AParameters    : TParameterList = nil
	);

	procedure JobChange(
		const ACurrentObject : TGameObject;
		const AObject        : TGameObject;
		const AParameters    : TParameterList = nil
	);

	procedure ShowDropitem(
		const ACurrentObject : TGameObject;
		const AObject        : TGameObject;
		const AParameters    : TParameterList = nil
	);

	procedure ShowPickupItem(
		const ACurrentObject : TGameObject;
		const AObject        : TGameObject;
		const AParameters    : TParameterList = nil
	);

	procedure RemoveGroundItem(
		const ACurrentObject : TGameObject;
		const AObject        : TGameObject;
		const AParameters    : TParameterList = nil
	);
	procedure ShowAttack(
		const ACurrentObject : TGameObject;
		const AObject        : TGameObject;
		const AParameters    : TParameterList = nil
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
	ItemInstance
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
	const ACurrentObject : TGameObject;
	const AObject        : TGameObject;
	const AParameters    : TParameterList = nil
);
var
	ABeing : TBeing;
Begin
	if AObject is TCharacter then
	begin
		ABeing := TBeing(ACurrentObject);
		ZoneWalkingBeing(
			ABeing,
			ABeing.Path[ABeing.Path.Count -1],
			ABeing.Position,
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
	const ACurrentObject : TGameObject;
	const AObject        : TGameObject;
	const AParameters    : TParameterList = nil
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
	const ACurrentObject : TGameObject;
	const AObject        : TGameObject;
	const AParameters    : TParameterList = nil
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
	const ACurrentObject : TGameObject;
	const AObject        : TGameObject;
	const AParameters    : TParameterList = nil
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
	const ACurrentObject : TGameObject;
	const AObject        : TGameObject;
	const AParameters    : TParameterList = nil
);
begin
	if AObject is TCharacter then
	begin
		if AObject = ACurrentObject then
			SendSpecialEffect(
				Tbeing(AObject),
				TCharacter(ACurrentObject).ClientInfo,
				TCharacter(ACurrentObject).AccountID,
				AParameters.GetAsLongWord(1)
			)
		else
			SendSpecialEffect(
				Tbeing(AObject),
				TCharacter(ACurrentObject).ClientInfo,
				TCharacter(ACurrentObject).ID,
				AParameters.GetAsLongWord(1)
			);
	end;
end;{Effect}
//------------------------------------------------------------------------------

procedure Emotion(
	const ACurrentObject : TGameObject;
	const AObject        : TGameObject;
	const AParameters    : TParameterList = nil
);
begin
	if AObject is TCharacter then
	begin
		SendEmotion(
			TBeing(ACurrentObject),
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
	const ACurrentObject : TGameObject;
	const AObject        : TGameObject;
	const AParameters    : TParameterList = nil
);
var
	ACurrentCharacter : TCharacter;
	ACharacter : TCharacter;
begin
	if (ACurrentObject is TCharacter) AND (AObject is TCharacter) then
	begin
		ACharacter := TCharacter(AObject);
		ACurrentCharacter := TCharacter(ACurrentObject);
		case ACurrentCharacter.CharaState of
			charaSitting :
				begin
					if AObject = ACurrentObject then
						DoAction(ACharacter.ClientInfo, ACurrentCharacter.AccountID, 0, 0, 0, ACTION_SIT, 0, 0, 0)
					else
						DoAction(ACharacter.ClientInfo, ACurrentCharacter.ID, 0, 0, 0, ACTION_SIT, 0, 0, 0);
				end;

			charaStanding :
				begin
					if AObject = ACurrentObject then
						DoAction(ACharacter.ClientInfo, ACurrentCharacter.AccountID, 0, 0, 0, ACTION_STAND, 0, 0, 0)
					else
						DoAction(ACharacter.ClientInfo, ACurrentCharacter.ID, 0, 0, 0, ACTION_STAND, 0, 0, 0);
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
	const ACurrentObject : TGameObject;
	const AObject        : TGameObject;
	const AParameters    : TParameterList = nil
);
var
	ACurrentCharacter : TCharacter;
	ACharacter : TCharacter;
begin
	if (ACurrentObject is TCharacter) AND (AObject is TCharacter) then
	begin
		ACharacter := TCharacter(AObject);
		ACurrentCharacter := TCharacter(ACurrentObject);
		case ACharacter.CharaState of
			charaSitting :
				begin
					DoAction(ACurrentCharacter.ClientInfo, ACharacter.ID, 0, 0, 0, ACTION_SIT, 0, 0, 0);
				end;

			charaStanding :
				begin
					DoAction(ACurrentCharacter.ClientInfo, ACharacter.ID, 0, 0, 0, ACTION_STAND, 0, 0, 0);
				end;

			charaAttacking :
				begin
					DoAction(ACharacter.ClientInfo, ACurrentCharacter.ID, ACurrentCharacter.TargetID, 0, 0, ACTION_ATTACK, 0, 0, 0);
				end;

			charaDead :
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
	const ACurrentObject : TGameObject;
	const AObject        : TGameObject;
	const AParameters    : TParameterList = nil
);
begin
	if (ACurrentObject is TCharacter) AND (AObject is TCharacter) then
	begin
		if AObject = ACurrentObject then
			ZoneDisappearBeing(
				TBeing(ACurrentObject),
				TCharacter(AObject).ClientInfo,
				1,
				TCharacter(AObject).AccountID
				)
		else
			ZoneDisappearBeing(
				TBeing(ACurrentObject),
				TCharacter(AObject).ClientInfo,
				1,
				TCharacter(AObject).ID
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
	const ACurrentObject : TGameObject;
	const AObject        : TGameObject;
	const AParameters    : TParameterList = nil
);
begin
	if (ACurrentObject is TCharacter) AND (AObject is TCharacter) then
	begin
		if ACurrentObject = AObject then
			SendUpdatedLook(TCharacter(ACurrentObject), TCharacter(AObject).AccountId, LOOK_JOB, TCharacter(AObject).JID, 0)
		else
			SendUpdatedLook(TCharacter(ACurrentObject), TCharacter(AObject).ID, LOOK_JOB, TCharacter(AObject).JID, 0)
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
	const ACurrentObject : TGameObject;
	const AObject        : TGameObject;
	const AParameters    : TParameterList = nil
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
	const ACurrentObject : TGameObject;
	const AObject        : TGameObject;
	const AParameters    : TParameterList = nil
);
begin
	if (AObject is TCharacter) then
	begin
		if AObject = ACurrentObject then
			SendPickUpItemAnimation(
				TCharacter(AObject),
				TCharacter(ACurrentObject).AccountID,
				AParameters.GetAsLongWord(1)
			)
		else
			SendPickUpItemAnimation(
				TCharacter(AObject),
				TCharacter(ACurrentObject).ID,
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
	const ACurrentObject : TGameObject;
	const AObject        : TGameObject;
	const AParameters    : TParameterList = nil
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
//ShowAttack                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Shows a being attacking another being
//
//	Changes-
//		[2008/10/03] Aeomin - Created
//------------------------------------------------------------------------------
procedure ShowAttack(
	const ACurrentObject : TGameObject;
	const AObject        : TGameObject;
	const AParameters    : TParameterList = nil
);
begin
	if (AObject is TCharacter) then
	begin
		if(AParameters.GetAsLongWord(6) = TBeing(AObject).ID) then
    begin
			DoAction(TCharacter(AObject).ClientInfo, TCharacter(ACurrentObject).AccountID, AParameters.GetAsLongWord(2), AParameters.GetAsLongWord(1) DIV 2, 0, ACTION_ATTACK, EnsureRange(AParameters.GetAsLongWord(3), 0, High(Word)), AParameters.GetAsLongWord(5), AParameters.GetAsLongWord(4));
		end else
		begin
			DoAction(TCharacter(AObject).ClientInfo, TBeing(ACurrentObject).ID, AParameters.GetAsLongWord(2), AParameters.GetAsLongWord(1) DIV 2, 0, ACTION_ATTACK, EnsureRange(AParameters.GetAsLongWord(3), 0, High(Word)), AParameters.GetAsLongWord(5), AParameters.GetAsLongWord(4));
		end;
	end;
end;{RemoveGroundItem}
//------------------------------------------------------------------------------

end.
