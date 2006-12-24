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
	Character,
	CharacterServerInfo;

type
	TBufSize = 0..(High(Word) div 2);
	TBuffer  = array[TBufSize] of Byte;
	TCBuffer = array[TBufSize] of Char;
	TReadPts = array of TBufSize;

	TThreadLink = class
		AccountLink : TAccount;
		CharacterLink : TCharacter;
	end;

	TCharaServerLink = class
		Info : TCharaServerInfo;
	end;

	TMD5String = class
		Key : string;
	end;

	Function ReadPoints(
			Args : array of Word
		) : TReadPts;

implementation
uses
	Math;

(*-----------------------------------------------------------------------------*
Func ReadPoints

--
Overview:
--
TODO

--
Pre:
	TODO
Post:
	TODO

--
Revisions:
--
[2005/07/10] CR - Added Comment Header
[yyyy/mm/dd] <Author> - <Comment>
*-----------------------------------------------------------------------------*)
Function ReadPoints(
		Args : array of Word
	) : TReadPts;
Var
	Idx : Integer;
Begin
	SetLength(Result, Length(Args));
	for Idx := Low(Args) to High(Args) do
	begin
		Result[Idx] := EnsureRange(Args[Idx], 0, High(TBufSize));
	end;
End; (* Func ReadPoints
*-----------------------------------------------------------------------------*)

end.


 
