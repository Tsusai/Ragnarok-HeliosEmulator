unit Equipment;

interface

uses
	ItemInstance,
	ItemTypes,
	{Third Party}
	List32,
	IdContext
	;

type
	TEquipLocationID = array[0..Byte(High(TEquipLocations))] of LongWord;
	TEquipment = class
	private
		ClientInfo : TIdContext;
		fList : TIntList32;
		fSprites : TEquipLocationID;
		fEquipments : TEquipLocationID;
		function GetInstance(const ID : LongWord):TItemInstance;
		function GetInstanceByIndex(const AnIndex:Word):TItemInstance;
		function GetInstanceByLocation(const ALocation : TEquipLocations):TItemInstance;
		function GetSpriteID(const ALocation : TEquipLocations):LongWord;
		procedure SetSpriteID(const ALocation : TEquipLocations;const Value:LongWord);
		function GetEquipmentID(const ALocation : TEquipLocations):LongWord;
		procedure SetEquipmentID(const ALocation : TEquipLocations;const Value:LongWord);
	public
		constructor Create(Parent : TObject);
		destructor Destroy; override;
		property Items[const ID:LongWord]:TItemInstance read GetInstance;default;
		property IndexItem[const AnIndex:Word]:TItemInstance read GetInstanceByIndex;
		property LocationItem[const ALocation : TEquipLocations]:TItemInstance read GetInstanceByLocation;
		property SpriteID[const ALocation : TEquipLocations]:LongWord read GetSpriteID write SetSpriteID;
		property EquipmentID[const ALocation : TEquipLocations]:LongWord read GetEquipmentID write SetEquipmentID;
		procedure Add(const AnInstance:TItemInstance;const DontSend:Boolean=False);
		procedure Remove(const AnIndex:Word);
		function IsEquipped(const ID:LongWord):Boolean;
	end;
implementation

uses
	Character,
	EquipmentItem,
	ZoneSend,
	PacketTypes
	;

//------------------------------------------------------------------------------
//Create                                                             CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does -
//
//
//	Changes -
//		[2008/10/08] Aeomin - Created
//------------------------------------------------------------------------------
constructor TEquipment.Create(Parent : TObject);
begin
	self.ClientInfo := TCharacter(Parent).ClientInfo;
	fList := TIntList32.Create;
end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy                                                             DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does -
//
//
//	Changes -
//		[2008/10/08] Aeomin - Created
//------------------------------------------------------------------------------
destructor TEquipment.Destroy;
begin
	fList.Free;
end;{Destroy}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetInstance                                                           FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Get item instance by instance id
//
//	Changes -
//		[2008/10/08] Aeomin - Created
//------------------------------------------------------------------------------
function TEquipment.GetInstance(const ID : LongWord):TItemInstance;
var
	Index : Integer;
begin
	Result := nil;
	Index := fList.IndexOf(ID);
	if Index > -1 then
	begin
		Result := fList.Objects[Index] as TItemInstance;
	end;
end;{GetInstance}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetInstanceByIndex                                                    FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Get item instance by instance index
//
//	Changes -
//		[2008/10/08] Aeomin - Created
//------------------------------------------------------------------------------
function TEquipment.GetInstanceByIndex(const AnIndex:Word):TItemInstance;
var
	Index : Integer;
begin
	Result := nil;
	if fList.Count > 0 then
	begin
		for Index := fList.Count -1 downto 0 do
		begin
			if TItemInstance(fList.Objects[Index]).Index = AnIndex then
			begin
				Result := fList.Objects[Index] as TItemInstance;
				Break;
			end;
		end;
	end;
end;{GetInstanceByIndex}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetInstanceByLocation                                                 FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Get Item instance by equipment location
//
//	Changes -
//		[2008/10/08] Aeomin - Created
//------------------------------------------------------------------------------
function TEquipment.GetInstanceByLocation(const ALocation : TEquipLocations):TItemInstance;
var
	Index : Integer;
begin
	Result := nil;
	if fList.Count > 0 then
	begin
		for Index := fList.Count -1 downto 0 do
		begin
			if TEquipmentItem(TItemInstance(fList.Objects[Index]).Item).EquipmentLocation = ALocation then
			begin
				Result := fList.Objects[Index] as TItemInstance;
				Break;
			end;
		end;
	end;
end;{GetInstanceByLocation}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetSpriteID                                                           FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Get item sprite ID
//
//	Changes -
//		[2008/10/10] Aeomin - Created
//------------------------------------------------------------------------------
function TEquipment.GetSpriteID(const ALocation : TEquipLocations):LongWord;
begin
	Result :=fSprites[Byte(ALocation)];
end;{GetSpriteID}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//SetSpriteID                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Set sprite id
//
//	Changes -
//		[2008/10/11] Aeomin - Created
//------------------------------------------------------------------------------
procedure TEquipment.SetSpriteID(const ALocation : TEquipLocations;const Value:LongWord);
begin
	fSprites[Byte(ALocation)] := Value;
end;{SetSpriteID}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetEquipmentID                                                        FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Get item ID
//
//	Changes -
//		[2008/10/10] Aeomin - Created
//------------------------------------------------------------------------------
function TEquipment.GetEquipmentID(const ALocation : TEquipLocations):LongWord;
begin
	Result :=fEquipments[Byte(ALocation)];
end;{GetEquipmentID}
//------------------------------------------------------------------------------


procedure TEquipment.SetEquipmentID(const ALocation : TEquipLocations;const Value:LongWord);
begin
	fEquipments[Byte(ALocation)] := Value;
end;

//------------------------------------------------------------------------------
//Add                                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Add item instance to equipment;
//	Note that the object should NOT freed in Inventory.
//
//	Changes -
//		[2008/10/08] Aeomin - Created
//------------------------------------------------------------------------------
procedure TEquipment.Add(const AnInstance:TItemInstance;const DontSend:Boolean=False);
var
	AChara : TCharacter;
	BeforeItem : TItemInstance;
	Equip : TEquipmentItem;
begin
	if AnInstance.Item is TEquipmentItem then
	begin
		AChara := TClientLink(ClientInfo.Data).CharacterLink;
		if AnInstance.Identified then
		begin
			Equip := TEquipmentItem(AnInstance.Item);
			if (Equip.MinimumLevel <= AChara.BaseLV) then
			begin
				BeforeItem := LocationItem[Equip.EquipmentLocation];
				if BeforeItem <> nil then
				begin
					Remove(
						BeforeItem.Index
					);
				end;
				fSprites[Byte(Equip.EquipmentLocation)]:=AnInstance.Item.SpriteID;
				EquipmentID[Equip.EquipmentLocation]:=AnInstance.ID;
				AnInstance.Equipped := True;
				fList.AddObject(
					AnInstance.ID,
					AnInstance
				);
				if not DontSend then
					SendEquipItemResult(
						AChara,
						AnInstance.Index,
						EquipLocationsToByte(
							Equip.EquipmentLocation
						),
						True
					);
			end else
			begin
				if not DontSend then
					SendEquipItemResult(
						AChara,
						0,
						0,
						False
					);
			end;
		end else
		begin
			if not DontSend then
				SendEquipItemResult(
					AChara,
					0,
					0,
					False
				);
		end;
	end;
end;{Add}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Remove                                                               PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Unequip an item.
//
//	Changes -
//		[2008/10/08] Aeomin - Created
//------------------------------------------------------------------------------
procedure TEquipment.Remove(const AnIndex:Word);
var
	AnInstance : TItemInstance;
	Index : Integer;
	AChara : TCharacter;
	Equip : TEquipmentItem;
begin
	AChara := TClientLink(ClientInfo.Data).CharacterLink;
	AnInstance := IndexItem[AnIndex];
	if AnInstance <> nil then
	begin
		Index := fList.IndexOf(AnInstance.ID);
		if Index > -1 then
		begin
			Equip := TEquipmentItem(AnInstance.Item);
			AnInstance.Equipped := False;
			fSprites[Byte(Equip.EquipmentLocation)]:= 0;
			EquipmentID[Equip.EquipmentLocation]:= 0;
			SendUnequipItemResult(
				AChara,
				AnIndex,
				EquipLocationsToByte(
					Equip.EquipmentLocation
				),
				True
			);
			fList.Delete(Index);
		end else
		begin
			SendUnequipItemResult(
				AChara,
				AnIndex,
				0,
				False
			);
		end;
	end else
	begin
		SendUnequipItemResult(
			AChara,
			AnIndex,
			0,
			False
		);
	end;
end;{Remove}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//IsEquipped                                                            FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Check if item is equipped against Instance ID
//
//	Changes -
//		[2008/10/11] Aeomin - Created
//------------------------------------------------------------------------------
function TEquipment.IsEquipped(const ID:LongWord):Boolean;
var
	Index : Byte;
begin
	Result := False;
	for Index := Byte(High(TEquipLocations)) downto 0 do
	begin
		if ID = fEquipments[Index] then
		begin
			Result := True;
			Break;
		end;
	end;
end;{IsEquipped}
//------------------------------------------------------------------------------
end.
