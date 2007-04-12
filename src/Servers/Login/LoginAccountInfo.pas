//------------------------------------------------------------------------------
//LoginAccountInfo			                                    UNIT
//------------------------------------------------------------------------------
//	What it does-
//      This class used by Login server to handle each account data in list
//
//	Changes -
//		April 12th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
unit LoginAccountInfo;

interface

type

	TLoginAccountInfo = class
	public
		//Acc id
		AccountID    : LongWord;
		//Which char server currently on?
		CharServerID : LongWord;
		//Still on Select Char Server?
		OnCharSrvList : Boolean;
		//Already attempt Duplicate login?
		UnderKickQuery : Boolean;
		Constructor Create(AID : LongWord);
	end;

implementation

Constructor TLoginAccountInfo.Create(AID : LongWord);
begin
	inherited Create;
	AccountID := AID;
	OnCharSrvList := False;
	UnderKickQuery := False;
end;
end.