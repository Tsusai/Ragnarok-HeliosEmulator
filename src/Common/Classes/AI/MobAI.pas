unit MobAI;

interface

uses
	Mob,
	AI,
	GameObject
	;

type
	TMobAI = class(TAI)
	private
		Mob  : TMob;
	protected
	public
		procedure Probe;override;
		procedure FoundObject(const AnObj:TGameObject); override;
		procedure ObjectNear(const AnObj:TGameObject); override;
		constructor Create(const AMob : TMob);
	end;

implementation

uses
	Math,
	ContNrs,
	Main,
	Character,
	ItemInstance
	;

procedure TMobAI.Probe;
var
	Beings : TObjectList;
	idxX,idxY:Integer;
	ObjectIdx : Integer;
	AObject : TCharacter;
begin
	writeln('LETS FIND CHKN');
	Beings := TObjectList.Create(FALSE);
	try
		for idxY := Max(0,Mob.Position.Y-MainProc.ZoneServer.Options.CharShowArea) to Min(Mob.Position.Y+MainProc.ZoneServer.Options.CharShowArea, Mob.MapInfo.Size.Y-1) do
		begin
			for idxX := Max(0,Mob.Position.X-MainProc.ZoneServer.Options.CharShowArea) to Min(Mob.Position.X+MainProc.ZoneServer.Options.CharShowArea, Mob.MapInfo.Size.X-1) do
			begin
				for ObjectIdx := Mob.MapInfo.Cell[idxX][idxY].Beings.Count -1 downto 0 do
				begin
					if Mob.MapInfo.Cell[idxX][idxY].Beings.Objects[ObjectIdx] is TCharacter then
					begin
						AObject := Mob.MapInfo.Cell[idxX][idxY].Beings.Objects[ObjectIdx] as TCharacter;
						Beings.Add(AObject);
					end;
				end;
			end;
		end;
		if Beings.Count > 0 then
		begin
			AObject := Beings.Items[Random(Beings.Count)] as TCharacter;
			FoundObject(AObject);
			writeln('FOUND ', AObject.Name);
		end;
	finally
		Beings.Free;
	end;

end;

procedure TMobAI.FoundObject(const AnObj:TGameObject);
begin
	writeln('I SAW CHIKEN!');
	if AnObj is TCharacter then
	begin
		{is agressive?}
	end
	else
	if AnObj is TItemInstance then
	begin
		{pick 'em?}
	end;
end;

procedure TMobAI.ObjectNear(const AnObj:TGameObject);
begin
	writeln('CHIKEN SAW ME!');
	FoundObject(AnObj);
end;

constructor TMobAI.Create(const AMob : TMob);
begin
	Mob := AMob;
end;
end.