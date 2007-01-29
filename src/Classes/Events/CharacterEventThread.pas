unit CharacterEventThread;

interface
uses
	SyncObjs,
	IdThread,
	CharaList;
type
	TCharacterEventThread = class(TIdThread)
	private
		CriticalSection : TCriticalSection;

	public
		CharacterList : TCharacterList;
		Constructor Create(ACharacterList : TCharacterList);reintroduce;
		Destructor  Destroy;override;
		Procedure 	Run;override;
	end;

implementation
uses
	SysUtils,
	WinLinux;

Constructor TCharacterEventThread.Create(ACharacterList : TCharacterList);
begin
	inherited Create(TRUE, TRUE, 'CharacterEventThread');
	CharacterList := ACharacterList;
	CriticalSection := TCriticalSection.Create;
end;

Destructor TCharacterEventThread.Destroy;
begin
	CriticalSection.Free;
	inherited;
end;

Procedure TCharacterEventThread.Run;
var
	CharacterIndex	: Integer;
	EventIndex			: Integer;
begin
	inherited;
	CriticalSection.Enter;
	for CharacterIndex := CharacterList.Count - 1 downto 0 do
	begin
		for EventIndex := CharacterList[CharacterIndex].EventList.Count - 1 downto 0 do
		begin
			if GetTick <= CharacterList[CharacterIndex].EventList[EventIndex].ExpiryTime then
			begin
				CharacterList[CharacterIndex].EventList[EventIndex].Execute;
				CharacterList[CharacterIndex].EventList.Delete(EventIndex);
			end;
    end;
	end;
	CriticalSection.Leave;
	Sleep(1);
end;

end.
