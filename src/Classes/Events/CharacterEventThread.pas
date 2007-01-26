unit CharacterEventThread;

interface
uses
	IdThread,
	CharaList;
type
	TCharacterEventThread = class(TIdThread)
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
end;

Destructor TCharacterEventThread.Destroy;
begin

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
	Sleep(1);
end;
end.
