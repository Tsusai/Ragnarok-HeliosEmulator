//------------------------------------------------------------------------------
//AIbeing                                                                   UNIT
//------------------------------------------------------------------------------
//	What it does-
//		Being that uses AI.
//
//	Changes -
//		[2009/01/17] Aeomin - Created.
//
//------------------------------------------------------------------------------
unit AIBeing;

interface

uses
	Being ,
	AI
	;

type
	TAIBeing = class(TBeing)
	public
		AI : TAI;
	end;
implementation

end.