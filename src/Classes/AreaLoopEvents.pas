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
//
//------------------------------------------------------------------------------
unit AreaLoopEvents;
interface
uses
	Classes, Math, Main, Globals, Character, Being, ZoneSend;

	procedure ShowBeingWalk(X, Y: Integer; ACurrentBeing, ABeing: TBeing);
	procedure ShowTeleIn(X, Y: Integer; ACurrentBeing, ABeing: TBeing);
	procedure TeleOut(X, Y: Integer; ACurrentBeing, ABeing: TBeing);
	procedure UpdateDir(X, Y: Integer; ACurrentBeing, ABeing: TBeing);
implementation

//------------------------------------------------------------------------------
//ShowCharWalk                                                         PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
// 	Show walking to other characters
//
//  Changes -
//	March 22th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure ShowBeingWalk(X, Y: Integer; ACurrentBeing, ABeing: TBeing);
begin
with ACurrentBeing do
begin
	ZoneWalkingBeing(ACurrentBeing,Path[Path.count-1],Position,TCharacter(ABeing).ClientInfo);
end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//ShowTeleIn                                                           PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
// 	Show teleport in effect to other characters
//
//  Changes -
//	March 22th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure ShowTeleIn(X, Y: Integer; ACurrentBeing, ABeing: TBeing);
begin
	ZoneSendBeing(ACurrentBeing, TCharacter(ABeing).ClientInfo, True);
	ZoneSendbeing(ABeing, TCharacter(ACurrentBeing).ClientInfo);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TeleOut                                                              PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
// 	Show teleport out effect to other characters
//
//  Changes -
//	March 22th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure TeleOut(X, Y: Integer; ACurrentBeing, ABeing: TBeing);
begin
	ZoneDisappearBeing(ACurrentBeing, TCharacter(Abeing).ClientInfo, 2);
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//UpdateDir                                                            PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
// 	Show new direction to other characters
//
//  Changes -
//	March 22th, 2007 - Aeomin - Created Header
//------------------------------------------------------------------------------
procedure UpdateDir(X, Y: Integer; ACurrentBeing, ABeing: TBeing);
begin
	ZoneUpdateDirection(TCharacter(ACurrentBeing), TCharacter(Abeing).ClientInfo);
end;
//------------------------------------------------------------------------------

end.