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
	Classes,
	{Project}
	Being
	{Third Party}
	//none
	;

	procedure ShowBeingWalk(
		const ACurrentBeing : TBeing;
		const ABeing        : TBeing;
		const AParameters   : Cardinal
		);
	procedure ShowTeleIn(
		const ACurrentBeing : TBeing;
		const ABeing        : TBeing;
		const AParameters   : Cardinal
		);
	procedure TeleOut(
		const ACurrentBeing : TBeing;
		const ABeing        : TBeing;
		const AParameters   : Cardinal
		);
	procedure UpdateDir(
		const ACurrentBeing : TBeing;
		const ABeing        : TBeing;
		const AParameters   : Cardinal
		);
	procedure Effect(
		const ACurrentBeing : TBeing;
		const ABeing        : TBeing;
		const AParameters   : Cardinal
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
[2007/05/25] Tsusai - Added IS TCHARACTER conditionals	
[yyyy/mm/dd] <Author> - <Comment>
*-----------------------------------------------------------------------------*)
Procedure ShowBeingWalk(
	const ACurrentBeing : TBeing;
	const ABeing        : TBeing;
	const AParameters   : Cardinal
);
Begin
	if ABeing is TCharacter then
	begin
		ZoneWalkingBeing(
			ACurrentBeing,
			ACurrentBeing.Path[ACurrentBeing.Path.Count -1],
			ACurrentBeing.Position,
			TCharacter(ABeing).ClientInfo
		);
	end;
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
[2007/05/25] Tsusai - Added IS TCHARACTER conditionals
*-----------------------------------------------------------------------------*)
Procedure ShowTeleIn(
	const ACurrentBeing : TBeing;
	const ABeing        : TBeing;
	const AParameters   : Cardinal
);
Begin
	if ABeing is TCharacter then
	begin
		ZoneSendBeing(ACurrentBeing, TCharacter(ABeing).ClientInfo, True);
	end;
	if ACurrentBeing is TCharacter then
	begin
		ZoneSendBeing(ABeing, TCharacter(ACurrentBeing).ClientInfo);
	end;
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
[2007/05/25] Tsusai - Added IS TCHARACTER conditionals
*-----------------------------------------------------------------------------*)
Procedure TeleOut(
	const ACurrentBeing : TBeing;
	const ABeing        : TBeing;
	const AParameters   : Cardinal
);
Begin
	if ABeing is TCharacter then
	begin
		ZoneDisappearBeing(ACurrentBeing, TCharacter(Abeing).ClientInfo, 2);
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
	const ACurrentBeing : TBeing;
	const ABeing        : TBeing;
	const AParameters   : Cardinal
);
Begin
	if ABeing is TCharacter then
	begin
		ZoneUpdateDirection(ACurrentBeing, TCharacter(Abeing).ClientInfo);
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
	const ACurrentBeing : TBeing;
	const ABeing        : TBeing;
	const AParameters   : Cardinal
);
begin
	if ABeing is TCharacter then
	begin
		SendSpecialEffect(ACurrentBeing, TCharacter(Abeing).ClientInfo, AParameters);
	end;
end;{Effect}
//------------------------------------------------------------------------------
end.
