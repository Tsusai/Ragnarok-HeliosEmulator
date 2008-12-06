unit ItemInstance;

interface

uses
	GameObject,
	Item
	;
type

//------------------------------------------------------------------------------
//TItemInstance                                                            CLASS
//------------------------------------------------------------------------------
TItemInstance = Class(TGameObject)
	private
	public
		Index		: Word;  //Should only modify by TInventoryList

		Item		: TItem;
		Quantity	: Word;

		Identified : Boolean;
		Refined : Byte;
		Broken : Boolean;
		Equipped :Boolean;

		SubX,SubY : Byte;
		MapID : LongWord;
		procedure GenerateSubCoordinate;
		procedure Dropped;
		procedure RemoveFromGround;
		constructor Create;
end;
//------------------------------------------------------------------------------


implementation

uses
	Main,
	RemoveGroundItemEvent,
	AreaLoopEvents,
	ParameterList
	;

procedure TItemInstance.GenerateSubCoordinate;
var
	RandomValue: LongWord;
begin
	RandomValue := Random( $FFFFFF );
	SubX := (RandomValue AND 3) * 3 + 3;
	SubY := ((RandomValue SHR 2) AND 3) * 3 + 3;
end;{GenerateSubCoordinate}
//------------------------------------------------------------------------------


procedure TItemInstance.Dropped;
var
	RemoveGroundItemEvent : TRemoveGroundItemEvent;
begin
	RemoveGroundItemEvent := TRemoveGroundItemEvent.Create(
							MainProc.ZoneServer.Options.GroundItemTimeout * 1000,
							Self
							);
	MainProc.ZoneServer.GroundItemEventList.Add(RemoveGroundItemEvent);
end;

procedure TItemInstance.RemoveFromGround;
var
	AParameters : TParameterList;
	Index : Integer;
begin
	if MapID > 0 then
	begin
		Index := MapInfo.Cell[Position.X,Position.Y].Items.IndexOf(ID);
		if Index > -1 then
		begin
			MapInfo.Cell[
				Position.X,
				Position.Y
				].Items.Delete(Index);
		end;
		AParameters := TParameterList.Create;
		AParameters.AddAsLongWord(1,ID);
		AreaLoop(RemoveGroundItem, FALSE,AParameters);
		AParameters.Free;
		MainProc.ZoneServer.Database.Items.Delete(ID);
	end;
end;

constructor TItemInstance.Create;
begin
	GenerateSubCoordinate;
end;{Create}
//------------------------------------------------------------------------------
end.