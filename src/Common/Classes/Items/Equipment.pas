unit Equipment;

interface

uses
	ItemInstance,
	ItemTypes,
	List32
	;

type
	TEquipment = class
	private
		fList : TIntList32;
		function GetInstance(const ID : LongWord):TItemInstance;
		function GetInstanceByIndex(const AnIndex:Word):TItemInstance;
		function GetInstanceByLocation(const ALocation : TEquipLocations):TItemInstance;
	public
		constructor Create;
		destructor Destroy; override;
		property Items[const ID:LongWord]:TItemInstance read GetInstance;default;
		property IndexItem[const AnIndex:Word]:TItemInstance read GetInstanceByIndex;
		property LocationItem[const ALocation : TEquipLocations]:TItemInstance read GetInstanceByLocation;
		procedure Add(const AnInstance:TItemInstance);
		procedure Remove(const AnIndex:Word);
	end;
implementation

uses
	EquipmentItem
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
constructor TEquipment.Create;
begin
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
//Add                                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Add item instance to equipment;
//	Note that the object should NOT freed in Inventory.
//
//	Changes -
//		[2008/10/08] Aeomin - Created
//------------------------------------------------------------------------------
procedure TEquipment.Add(const AnInstance:TItemInstance);
begin
	fList.AddObject(
		AnInstance.ID,
		AnInstance
		);
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
begin
	AnInstance := IndexItem[AnIndex];
	if AnInstance <> nil then
	begin
		Index := fList.IndexOf(AnInstance.ID);
		if Index > -1 then
		begin
			fList.Delete(Index);
		end;
	end;
end;{Remove}
//------------------------------------------------------------------------------
end.
