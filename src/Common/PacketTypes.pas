unit PacketTypes;

interface
uses
	AccountTypes,
	CharacterTypes;

type
	TBufSize = 0..(High(Word) div 2);
	TBuffer  = array[TBufSize] of Byte;
	TCBuffer = array[TBufSize] of Char;


	TThreadLink = class
		AccountLink : TAccount;
		CharacterLink : TCharacter;
	end;

implementation

end.


 