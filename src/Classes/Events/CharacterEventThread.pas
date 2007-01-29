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
	DateUtils,
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
	CurrentTime			: TDateTime;
	CompareTimestamp: Integer;
begin
	inherited;
	CriticalSection.Enter;
	CurrentTime := GetTick;
	for CharacterIndex := 0 to CharacterList.Count - 1 do
	begin
		for EventIndex := CharacterList[CharacterIndex].EventList.Count - 1 downto 0 do
		begin
			CompareTimeStamp := CompareDateTime(CharacterList[CharacterIndex].EventList[EventIndex].ExpiryTime, CurrentTime);
			if  (CompareTimeStamp = 0) OR
					(CompareTimeStamp = -1)  then
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
