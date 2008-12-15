unit GameObject;

interface

uses
	Types,
	ParameterList,
	Map;
type
	TGameObject = class;
	TBeingZoneStatus = (
		isOffline,
		isOnline
	);
	{[2007/03/28] CR - No X,Y parameters needed -- reduced and eliminated. }
	TLoopCall = procedure(
		const ACurrentObject	: TGameObject;
		const AObject		: TGameObject;
		const AObjectID		: LongWord;
		const AParameters	: TParameterList = nil
		);

	TGameObject = class
	protected
		fName             : String;
		fPosition         : TPoint;
		fMap              : String;
		procedure SetPosition(Value : TPoint); virtual;
		procedure SetMap(Value : string); virtual;
		procedure SetName(const Value : String); virtual;
	public
		ID	: LongWord;
		ZoneStatus		: TBeingZoneStatus; //Is the being online in the Zone or not?
		MapInfo			: TMap;
		property Name      : string     read fName    write SetName;
		property Position  : TPoint     read fPosition write SetPosition;
		property Map       : string     read fMap write SetMap;
		procedure AreaLoop(ALoopCall:TLoopCall; AIgnoreCurrentBeing:Boolean=True;AParameter : TParameterList = nil);
		function Distance(const APoint:TPoint):Word;
	end;

implementation

uses
	Math,
	Main,
	Being,
	Character
	;
procedure TGameObject.SetPosition(Value : TPoint);
var
	OldPt : TPoint;
	Index : Integer;
begin
	OldPt := Position;
	fPosition := Value;
	//check if we're online before we add/remove ourselves from the map...
	if ZoneStatus = isOnline then
	begin
		//Position is initialized to -1, -1 on create, so if we're not on the map
		//yet, don't remove us!
		if MapInfo.PointInRange(OldPt) then
		begin
			Index := MapInfo.Cell[OldPt.X][OldPt.Y].Beings.IndexOf(ID);
			if Index > -1 then
			begin
				MapInfo.Cell[OldPt.X][OldPt.Y].Beings.Delete(
					Index
				);
			end;
		end;

		//same as above, if we've set position to -1, -1, then don't add us to a
		//non-existant cell!
		if MapInfo.PointInRange(Position) then
		begin
			MapInfo.Cell[Position.X][Position.Y].Beings.AddObject(ID, self);
		end;
	end;
end;{SetPosition}
//------------------------------------------------------------------------------

procedure TGameObject.SetMap(Value : string); begin fMap := Value; end;

//------------------------------------------------------------------------------
//ViewAreaLoop                                                         PROCEDURE
//------------------------------------------------------------------------------
//  What it does -
//      master procedure for Area loop, and call Dynamic Sub Procedure.
//
//  Changes -
//    March 20th, 2007 - Aeomin - Created
//------------------------------------------------------------------------------
procedure TGameObject.AreaLoop(ALoopCall:TLoopCall; AIgnoreCurrentBeing:Boolean=True;AParameter : TParameterList = nil);
var
		idxY : integer;
		idxX : integer;
		ObjectIdx : integer;
		AObject : TGameObject;
begin
	for idxY := Max(0,Position.Y-MainProc.ZoneServer.Options.CharShowArea) to Min(Position.Y+MainProc.ZoneServer.Options.CharShowArea, MapInfo.Size.Y-1) do
	begin
		for idxX := Max(0,Position.X-MainProc.ZoneServer.Options.CharShowArea) to Min(Position.X+MainProc.ZoneServer.Options.CharShowArea, MapInfo.Size.X-1) do
		begin
			for ObjectIdx := MapInfo.Cell[idxX][idxY].Beings.Count -1 downto 0 do
			begin
				if MapInfo.Cell[idxX][idxY].Beings.Objects[ObjectIdx] is TBeing then
				begin
					AObject := MapInfo.Cell[idxX][idxY].Beings.Objects[ObjectIdx] as TGameObject;
					if (Self = AObject) and AIgnoreCurrentBeing then Continue;
					{if not (ABeing is TCharacter) then Continue;} //Target MUST be a TCharacter
					//Even though it's good idea to prevent packet send to non players
					//But the problem is that.. NPC is invisible now
					if (AObject is TCharacter) AND (Self = AObject) then
						ALoopCall(Self, AObject, TCharacter(Self).AccountID, AParameter)
					else
						ALoopCall(Self, AObject, TBeing(Self).ID, AParameter);
				end;
			end;
			if MapInfo.Cell[idxX][idxY].Items.Count > 0 then
			begin
				for ObjectIdx := MapInfo.Cell[idxX][idxY].Items.Count -1 downto 0 do
				begin
					AObject := MapInfo.Cell[idxX][idxY].Items.Objects[ObjectIdx] as TGameObject;
					ALoopCall(Self, AObject, TGameObject(AObject).ID, AParameter);
				end;
			end;
		end;
	end;
end;
//------------------------------------------------------------------------------


(*- Procedure -----------------------------------------------------------------*
TBeing.SetName
--------------------------------------------------------------------------------
Overview:
--

	Property method for Name.

--
Revisions:
--
(Format: [yyyy/mm/dd] <Author> - <Comment>)
[2007/04/24] CR - Added Comment Header, made Value parameter const.
*-----------------------------------------------------------------------------*)
Procedure TGameObject.SetName(
	const
		Value : String
	);
Begin
	fName := Value;
End; (* Proc TBeing.SetName
*-----------------------------------------------------------------------------*)


//------------------------------------------------------------------------------
//Distance                                                              FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Calculate distance between 2 TPoint.
//	Note that this routine only get either distance of X or Y.
//	Because visible range was in square...
//
//	Changes-
//		[2008/12/14] Aeomin - Create.
//------------------------------------------------------------------------------
function TGameObject.Distance(const APoint:TPoint):Word;
var
	DistanceX, DistanceY : Word;
begin
	DistanceX := Abs(APoint.X - fPosition.X);
	DistanceY := Abs(APoint.Y - fPosition.Y);
	if DistanceX > DistanceY then
		Result := DistanceX
	else
		Result := DistanceY;
end;{Distance}
//------------------------------------------------------------------------------
end.
