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

interface

uses
	{RTL/VCL}
	//none
	{Project}
	Being
	{Third Party}
	//none
	;

	procedure ShowBeingWalk(
		const
			ACurrentBeing : TBeing;
		const
			ABeing        : TBeing
		);
	procedure ShowTeleIn(
		const
			ACurrentBeing : TBeing;
		const
			ABeing        : TBeing
		);
	procedure TeleOut(
		const
			ACurrentBeing : TBeing;
		const
			ABeing        : TBeing
		);
	procedure UpdateDir(
		const
			ACurrentBeing : TBeing;
		const
			ABeing        : TBeing
		);

implementation

uses
	{RTL/VCL}
	//none
	{Project}
	Character,
	ZoneSend
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
[yyyy/mm/dd] <Author> - <Comment>
*-----------------------------------------------------------------------------*)
Procedure ShowBeingWalk(
	const
		ACurrentBeing : TBeing;
	const
		ABeing        : TBeing
	);
Begin
	ZoneWalkingBeing(
		ACurrentBeing,
		ACurrentBeing.Path[ACurrentBeing.Path.Count -1],
		ACurrentBeing.Position,
		TCharacter(ABeing).ClientInfo
	);
End; (* Proc ShowBeingWalk
*-----------------------------------------------------------------------------*)


(*- Procedure -----------------------------------------------------------------*
ShowTeleIn
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
*-----------------------------------------------------------------------------*)
Procedure ShowTeleIn(
	const
		ACurrentBeing : TBeing;
	const
		ABeing        : TBeing
	);
Begin
	ZoneSendBeing(ACurrentBeing, TCharacter(ABeing).ClientInfo, True);
	ZoneSendbeing(ABeing, TCharacter(ACurrentBeing).ClientInfo);
End; (* Proc ShowTeleIn
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
*-----------------------------------------------------------------------------*)
Procedure TeleOut(
	const
		ACurrentBeing : TBeing;
	const
		ABeing        : TBeing
	);
Begin
	ZoneDisappearBeing(ACurrentBeing, TCharacter(Abeing).ClientInfo, 2);
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
*-----------------------------------------------------------------------------*)
Procedure UpdateDir(
	const
		ACurrentBeing : TBeing;
	const
		ABeing        : TBeing
	);
Begin
	ZoneUpdateDirection(TCharacter(ACurrentBeing), TCharacter(Abeing).ClientInfo);
End; (* Proc UpdateDir
*-----------------------------------------------------------------------------*)

end.