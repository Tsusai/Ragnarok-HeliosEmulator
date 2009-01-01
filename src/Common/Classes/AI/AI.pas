//     "Project Super AI" (c) 3015.
//            (I lied)
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
		procedure Initiate; virtual;abstract;
		procedure Probe; virtual;abstract;
		procedure FoundObject(const AnObj:TGameObject);virtual;abstract;
		procedure ObjectNear(const AnObj:TGameObject);virtual;abstract;
		procedure FinishWalk;virtual;abstract;
	end;

implementation

end.