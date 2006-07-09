(*------------------------------------------------------------------------------
PacketTypes
Tsusai 2006

Types used with communications are listed here.
TBuffer : Basic Buffer
TCBuffer : Used for string buffer procedures
TThreadLink : Used for linking accounts or characters to the client socket 
------------------------------------------------------------------------------*)
unit PacketTypes;

interface
uses
	Account,
	Character;

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


 