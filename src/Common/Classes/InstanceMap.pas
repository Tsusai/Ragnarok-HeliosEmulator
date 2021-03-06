//------------------------------------------------------------------------------
//InstanceMap                                                               UNIT
//------------------------------------------------------------------------------
//	What it does -
//		Object for instance map
//
//  Changes -
//		[2008/12/06] Aeomin - Created
//
//------------------------------------------------------------------------------
unit InstanceMap;
{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
interface

uses
	Classes,
	Map,
	List32
	;

type
	TInstanceMap = class(TMap)
	private
		//Instance Map ID
		Identifier : String;

		function LoadCache(const Name:String;var Memory : TMemoryStream):Boolean;
		procedure LoadNpc;
	public
		BaseName : String;
		procedure Load(const InstanceIdentifier, OriginalMapName:String);reintroduce;
		constructor Create;
		destructor Destroy;override;
	end;
implementation

uses
	SysUtils,
	Main,
	Globals,
	MapTypes,
	NPC
	;

//------------------------------------------------------------------------------
//LoadCache                                                            PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Well, we need to cache 'em XD
//
//	Changes -
//		[2008/12/07] Aeomin - Created.
//------------------------------------------------------------------------------
function TInstanceMap.LoadCache(const Name:String;var Memory : TMemoryStream):Boolean;
const
	FSTR_MAPFILE = '%s/%s.pms';
var
	Index : Integer;
	FileName : String;
begin
	Result := False;
	Index := MainProc.ZoneServer.InstanceCache.IndexOf(Name);
	if Index > -1 then
	begin
		//COOL, we have cache
		Memory := MainProc.ZoneServer.InstanceCache.Objects[Index] as TMemoryStream;
		Memory.Position := 0;
		Result := LoadFromStream(Memory);
	end else
	begin
		//No? CREATE IT..
		FileName :=
			Format(
				FSTR_MAPFILE,
				[MainProc.Options.MapDirectory, Name]
			);
		if FileExists(FileName) then
		begin
			Memory := TMemoryStream.Create;
			Memory.LoadFromFile(FileName);
			MainProc.ZoneServer.InstanceCache.AddObject(Name,Memory);
			Result := LoadFromStream(Memory);
		end else
		begin
			Console.Message('Map ' + FileName + ' does not exist!', 'Zone Server');
		end;
	end;
	
end;{LoadCache}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LOAD                                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Override of Load in TMap.
//
//	Changes -
//		[2008/12/06] Aeomin - Created.
//------------------------------------------------------------------------------
procedure TInstanceMap.Load(const InstanceIdentifier, OriginalMapName:String);
Var
	MapData : TMemoryStream;
	XIndex  : Integer;
	YIndex  : Integer;
Begin
	State  := LOADING;

	BaseName := OriginalMapName;
	Name :=  OriginalMapName;
	if LoadCache(OriginalMapName, MapData) then
	begin
		MainProc.ZoneServer.Database.Map.LoadFlags(Flags, Name);
		MapData.Seek(22,0);//skip other non-cell information

		//Load Cell Information
		SetLength(Cell, Size.X, Size.Y);
		for YIndex := 0 to Size.Y - 1 do begin
			for XIndex := 0 to Size.X - 1 do begin
				MapData.Read(Cell[XIndex][YIndex].Attribute,1);
				Cell[XIndex][YIndex].Position := Point(XIndex, YIndex);
				Cell[XIndex][YIndex].ObstructionCount := 0;
				Cell[XIndex][YIndex].Beings := TIntList32.Create;
				Cell[XIndex][YIndex].Items  := TIntList32.Create;
			end;
		end;

		LoadNpc;
		//Temporary hack..
//		Path := 'Maps/'+Name+'.pms';

		Identifier := InstanceIdentifier;
		Name :=  InstanceIdentifier + '#' + OriginalMapName;

		State := LOADED;
	end else
	begin
		State := UNLOADED;
	end;
end;{Load}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadNpc                                                              PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Load & Clone npc object
//
//	Changes -
//		[2008/12/08] Aeomin - Created.
//------------------------------------------------------------------------------
procedure TInstanceMap.LoadNpc;
Var
	ObjIndex    : Integer;
	AnNPC       : TScriptNPC;
	NewNPC : TScriptNPC;
begin
	//Enable all npcs on this map.
	for ObjIndex := 0 to MainProc.ZoneServer.NPCList.Count -1 do
	begin
		AnNPC := TScriptNPC(MainProc.ZoneServer.NPCList.Objects[ObjIndex]);
		if AnNPC.Map = BaseName then
		begin
			if PointInRange(AnNPC.Position) then
			begin
				if AnNPC is TWarpNPC then
					NewNPC := TWarpNPC.Create()
				else
					NewNPC := TScriptNPC.Create();

				AnNPC.CloneTo(NewNPC);

				//Let's clone it!
				NewNPC.Map := Self.BaseName;
				NewNPC.MapInfo := Self;
				Cell[NewNPC.Position.X][NewNPC.Position.Y].Beings.AddObject(NewNPC.ID, NewNPC);
				NewNPC.Enabled := True;
				NPCList.AddObject(NewNPC.ID,NewNPC);
			end;
		end;
	end;
end;{LoadNPC}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Create                                                             CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does -
//		Create..
//
//	Changes -
//		[2008/12/08] Aeomin - Created.
//------------------------------------------------------------------------------
constructor TInstanceMap.Create;
begin
	inherited;
end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy                                                             DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does -
//		Destroy..
//
//	Changes -
//		[2008/12/08] Aeomin - Created.
//------------------------------------------------------------------------------
destructor TInstanceMap.Destroy;
var
	Index : Integer;
begin
	for Index := 0 to NPCList.Count - 1 do
	begin
		NPCList.Objects[Index].Free;
	end;
	inherited;
end;{Destroy}
//------------------------------------------------------------------------------
end{InstanceMap}.
