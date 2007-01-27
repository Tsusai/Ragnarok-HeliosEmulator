unit PacketTypes;

interface
type
	TBufSize = 0..(High(Word) div 2);
	TBuffer  = array[TBufSize] of Byte;
	TCBuffer = array[TBufSize] of Char;

implementation

end.


 