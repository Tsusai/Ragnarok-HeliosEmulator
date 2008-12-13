unit MobAI;

interface

uses
	AI,
	GameObject
	;

type
	TMobAI = class(TAI)
	protected
	public
		procedure FoundObject(const AnObj:TGameObject); override;
		procedure ObjectNear(const AnObj:TGameObject); override;
	end;

implementation

procedure TMobAI.FoundObject(const AnObj:TGameObject);
begin
	writeln('I SAW A CHIKEN!');
end;

procedure TMobAI.ObjectNear(const AnObj:TGameObject);
begin
	writeln('CHIKEN SAW ME!');
end;
end.