//     "Project Super AI" (c) 3015.
//            (I Lied)
//       Okay, this is the base object for Game AI.
unit AI;

interface

uses
	GameObject
	;

type
	TAI = class
	protected
	public
		procedure FoundObject(const AnObj:TGameObject);virtual;abstract;
		procedure ObjectNear(const AnObj:TGameObject);virtual;abstract;
	end;

implementation

end.