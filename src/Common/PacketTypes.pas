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
------------------------------------------------------------------------------*)
unit PacketTypes;


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
	{3rd Party}
	IdContext
	;


type
	TBufSize = 0..(High(Word) div 2);
	TBuffer  = array[TBufSize] of Byte;
	TCBuffer = array[TBufSize] of Char;
	TReadPts = array of TBufSize;

	TThreadLink = class
		Parent				: TIdContext;
		DatabaseLink	: TDatabase;
		Constructor Create(AClient : TIdContext);
		Destructor Destroy();override;
  end;

	TClientLink = class(TThreadLink)
		AccountLink		: TAccount;
		CharacterLink : TCharacter;
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

	TMD5String = class
		Key : string;
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
	DatabaseLink := TDatabase.Create(Parent);
end;

Destructor TThreadLink.Destroy;
begin
	DatabaseLink.Free;
	inherited;
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


 
