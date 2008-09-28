(*------------------------------------------------------------------------------
PacketTypes
Tsusai 2006

Types used with communications are listed here.
TBuffer : Basic Buffer
TCBuffer : Used for string buffer procedures
TThreadLink : Used for linking accounts or characters to the client socket

--
Revisions:
--------------------------------------------------------------------------------
[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
//		December 26th, 2007 - Tsusai - Removed TMD5String class.
------------------------------------------------------------------------------*)
unit PacketTypes;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	//none
	{Project}
	Account,
	Character,
	CharacterServerInfo,
	Database,
	ZoneServerInfo,
	CharAccountInfo,
	{3rd Party}
	IdContext
	;


type
	TBufSize = 0..(High(Word) div 2);
	TBuffer  = array[TBufSize] of Byte;
	TCBuffer = array[TBufSize] of Char;
	TReadPts = array of TBufSize;

	TThreadLink = class
		Parent          : TIdContext;
		DatabaseLink    : TDatabase;
		CharRename : Boolean;  //Character Rename mode
		Constructor Create(AClient : TIdContext);
		Destructor Destroy();override;
	end;

	TClientLink = class(TThreadLink)
		AccountLink		: TAccount;
		CharacterLink : TCharacter;
		AccountInfo	: TCharAccountInfo;
		Transfering   : Boolean;

		EncKey1		: LongWord;
		EncKey2		: LongWord;
		procedure InitializeMessageID(const Key1,Key2:LongWord);
		function DecryptMessageID(const ID:Word):Word;
		constructor Create(AClient : TIdContext);
		Destructor Destroy();override;
	end;

	TCharaServerLink = class(TThreadLink)
		Info : TCharaServerInfo;
		Destructor Destroy();override;
	end;

	TZoneServerLink = class(TThreadLink)
		Info : TZoneServerInfo;
		Destructor Destroy();override;
	end;

	Function ReadPoints(
			Args : array of Word
		) : TReadPts;


implementation


uses
	{RTL/VCL}
	Math
	{Project}
	//none
	{3rd Party}
	//none
	;


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


Constructor TThreadLink.Create(AClient : TIdContext);
begin
	inherited Create;
	Parent := AClient;
	CharRename := False;
end;

Destructor TThreadLink.Destroy;
begin
	inherited;
end;
constructor TClientLink.Create(AClient : TIdContext);
begin
	inherited Create(AClient);
	EncKey1 := 0;
	EncKey2 := 0;
end;
procedure TClientLink.InitializeMessageID(const Key1,Key2:LongWord);
var
	TemporaryCode : array[1..8] of Byte;
	ShiftTemporary : LongWord;
	Index	: Byte;
	AWord : LongWord;
begin
	ShiftTemporary := Key1;
	for Index := 8 downto 1 do
	begin
		TemporaryCode[Index] := ShiftTemporary AND $F;
		ShiftTemporary := ShiftTemporary shr 4;
	end;
	AWord := (TemporaryCode[6] shl 12) + (TemporaryCode[4] shl 8) + (TemporaryCode[7] shl 4) + (TemporaryCode[1]);
	EncKey1 := (TemporaryCode[2] shl 12) + (TemporaryCode[3] shl 8) + (TemporaryCode[5] shl 4) + (TemporaryCode[8]);
	EncKey2 := ((((EncKey1 mod $F3AC) + AWord) shl 16) or ((EncKey1 mod $49DF)+EncKey1)) mod Key2;
end;

function TClientLink.DecryptMessageID(const ID:Word):Word;
begin
	EncKey1 := (($343FD * EncKey1) + EncKey2) AND $FFFFFFFF;
	Result := (ID mod ((EncKey1 shr 16) and $7FFF)) and $FFF;
end;

Destructor TClientLink.Destroy;
begin
	if Assigned(AccountLink) then
	begin
		AccountLink.Free;
	end;
	//We do NOT own the characters, only the zone's character list does. We don't
	//free them here.
	inherited;
end;

Destructor TCharaServerLink.Destroy;
begin
	if Assigned(Info) then
	begin
		Info.Free;
	end;
	inherited;
end;

Destructor TZoneServerLink.Destroy;
begin
	if Assigned(Info) then
	begin
		Info.Free;
	end;
	inherited;
end;
end.


 
