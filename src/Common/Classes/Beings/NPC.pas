(*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

Unit
NPC

*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*

[2006/??/??] Helios - No author stated

================================================================================
License:  (FreeBSD, plus commercial with written permission clause.)
================================================================================

Project Helios - Copyright (c) 2005-2007

All rights reserved.

Please refer to Helios.dpr for full license terms.

================================================================================
Overview:
================================================================================

Class, interface, and common defaults for a "NPC" are defined here.

================================================================================
Revisions:
================================================================================
(Format: [yyyy/mm/dd] <Author> - <Desc of Changes>)
[2007/07/24] RaX - Created Header.
*@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@*)
unit NPC;

interface
uses
	Being,
	Character;

	
	(*
	Basic NPC Class
	Branches out like so
	
	TBeing
		TNPC
			TScriptNPC
				TWarpNPC
			TItemNPC
			TShopNPC (May not exist, may JUST be a npc 
				command to open the shop under)

	*)

	//Non Player Character class
	type TNPC = class(TBeing)
		protected
			fEnabled        : Boolean;
			Procedure SetEnabled(AValue : Boolean);virtual;

		public
			//Dunno why they are needed, but they are here to
			//kill compiler warnings.
			procedure CalcMaxHP; override;
			procedure CalcMaxSP; override;

			Constructor Create;
			Destructor	Destroy; override;

			property Enabled : Boolean read fEnabled write SetEnabled;


	end;

	//Basic scripted NPC.  Has ontouch & onclick function
	//string names.  They are filled in on creation and can
	//only be read for executing the lua script.
	type TScriptNPC = class(TNPC)
		protected

			fClickFunction	: string;
			fTouchFunction	: string;

			Procedure SetEnabled(AValue : Boolean);override;

		public
			//OnTouch
			OnTouchXRadius : word;
			OnTouchYRadius : word;
			OnTouchEnabled : boolean;

			procedure OnTouch(ACharacter : TCharacter);
			procedure OnClick(ACharacter : TCharacter);

			Procedure Enable;
			Procedure Disable;

			//Properties for lua function names
			property ClickFunction : string read fClickFunction;
			property TouchFunction : string read fTouchFunction;

			//Creation fills in the function names.
			constructor Create(
				ClickFunc : string = '';
				TouchFunc : string = ''
			);
			destructor Destroy();override;
	end;

	//Subclass of TScriptNPC, contains destination map and point, and
	//Only takes a ontouch script function name which is optional in the
	//creation
	type TWarpNPC = class(TScriptNPC)
		public
			constructor Create(
				TouchFunc : string = ''
			);

	end;

	//Holder for now, contains the NPC for an item
	//Contains an ExpireTime for cleanup (kill dropped items)
	type TItemNPC = class(TNPC)
		public
			ExpireTime : LongWord;
	end;

implementation
uses
	//RTL
	Types,
	//Project
	LuaNPCCore,
	Main,
	Map,
	OnTouchCellEvent;

//Start NPC IDs at 100K
const NPCIDBase = 100000;

var
	NowNPCID : LongWord = NPCIDBase;

constructor TNPC.Create;
begin
	inherited;
	fEnabled  := FALSE;
	ID := NowNPCID;
	Inc(NowNPCID);
end;

destructor TNPC.Destroy;
begin
	//TODO
	inherited;
end;

procedure TNPC.CalcMaxHP;
begin
	//No idea, placeholder for now
	MaxHP := HP;
end;

procedure TNPC.CalcMaxSP;
begin
	//No idea, placeholder for now
	MaxHP := HP;
end;

Procedure TNPC.SetEnabled(AValue: Boolean);
begin
	fEnabled := AValue;
end;



constructor TScriptNPC.Create(
	ClickFunc : string = '';
	TouchFunc : string = ''
);
begin
	inherited Create;
	fClickFunction := ClickFunc;
	fTouchFunction := TouchFunc;
end;

destructor TScriptNPC.Destroy;
begin
	//TODO
	inherited;
end;

procedure TScriptNPC.OnTouch(ACharacter : TCharacter);
begin
	if TouchFunction <> '' then
	begin
		RunLuaNPCScript(ACharacter, TouchFunction);
	end;
end;

procedure TScriptNPC.OnClick(ACharacter : TCharacter);
begin
	if ClickFunction <> '' then
	begin
		RunLuaNPCScript(ACharacter, ClickFunction);
	end;
end;

Procedure TScriptNPC.Enable;
var
	AMap						: TMap;
	XIndex					: Integer;
	YIndex					: Integer;
	AnOnTouchEvent	: TOnTouchCellEvent;
	MapIndex        : Integer;
begin
	fEnabled := TRUE;
	//if we're not already enabled and ontouch is enabled...
	if OnTouchEnabled then
	begin
		//get our map.
		MapIndex := MainProc.ZoneServer.MapList.IndexOf(self.Map);
		if MapIndex <> -1 then
		begin
			AMap := MainProc.ZoneServer.MapList[MapIndex];
			//make sure our ontouch coordinates are in the map's bounds.
			if ((Position.X + OnTouchXRadius) < AMap.Size.X) AND
			 ((Position.Y + OnTouchYRadius) < AMap.Size.Y) AND
			 ((Position.X - OnTouchXRadius) > 0) AND
			 ((Position.Y - OnTouchYRadius) > 0) then
			begin
				//loop through our ontouch area
				for XIndex := (Position.X-OnTouchXRadius) to (Position.X + OnTouchXRadius) do
				begin
					for YIndex := (Position.Y-OnTouchYRadius) to (Position.Y + OnTouchYRadius) do
					begin
						//add our ontouch events
						AnOnTouchEvent := TOnTouchCellEvent.Create(self);
						AMap.Cell[XIndex][YIndex].Beings.AddObject(0, AnOnTouchEvent);
					end;
				end;
			end;
		end;
	end;
end;

//[2007/05/27] Tsusai - Removed MapPointer (useless...)
Procedure TScriptNPC.Disable;
var
	AMap						: TMap;
	XIndex					: Integer;
	YIndex					: Integer;
	Index						: Integer;
	AnObject				: TObject;
begin
	fEnabled := FALSE;
	//If this npc has been enabled and ontouch is enabled...
	if OnTouchEnabled then
	begin
		//Get our map.
		AMap := MapInfo;
		//Loop through our ontouch area.
		for XIndex := (Position.X-OnTouchXRadius) to (Position.X + OnTouchXRadius) do
		begin
			for YIndex := (Position.Y-OnTouchYRadius) to (Position.Y + OnTouchYRadius) do
			begin
				//loop through our beings list
				for Index := AMap.Cell[XIndex][YIndex].Beings.Count - 1 downto 0 do
				begin
					//get our object
					AnObject := AMap.Cell[XIndex][YIndex].Beings.Objects[Index];
					//check to see if our object is an ontouch event
					if AnObject is TOnTouchCellEvent then
					begin
						//since it is, is it this object's cell event?
						if TOnTouchCellEvent(AnObject).ScriptNPC = self then
						begin
							//since it is, free and delete.
							AnObject.Free;
							AMap.Cell[XIndex][YIndex].Beings.Delete(Index);
            end;
					end;
        end;
			end;
		end;
	end;
end;

Procedure TScriptNPC.SetEnabled(AValue: Boolean);
begin
	if AValue <> fEnabled then
	begin
		Inherited;
		if fEnabled then
		begin
			Enable;
		end else
		begin
			Disable;
    end;
  end;
end;


constructor TWarpNPC.Create(
	TouchFunc : string = ''
);
begin
	inherited Create('',TouchFunc);
	OnTouchEnabled := true;
end;

end.
