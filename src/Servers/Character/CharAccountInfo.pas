//------------------------------------------------------------------------------
//CharAccountInfo			                                    UNIT
//------------------------------------------------------------------------------
//	What it does-
//      This class used by Char server to handle each account data in list
//
//	Changes -
//		April 12th, 2007 - Aeomin - Created Header
//
//------------------------------------------------------------------------------
unit CharAccountInfo;

interface
uses
	IdContext;
type

	TCharAccountInfo = class
	public
		//Acc id
		AccountID    : LongWord;
		//Char id
		CharacterID  : LongWord;
		//Which zone currently on?
		ZoneServerID : Word;
		//Is the account in game?
		InGame : Boolean;
		//Transfering back to char server select?
		Transfering : Boolean;
								//The socket of client
		ClientInfo : TIdContext;
		Constructor Create(AID : LongWord);
	end;

implementation

Constructor TCharAccountInfo.Create(AID : LongWord);
begin
	inherited Create;
	AccountID := AID;
	Ingame := False;
	Transfering := False;
end;

end.