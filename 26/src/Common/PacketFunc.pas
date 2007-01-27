unit PacketFunc;

interface
uses
	PacketTypes;


	procedure WFIFOB(index:word; b:byte; var buf : TBuffer);
	procedure WFIFOW(index:word; w:word; var buf : TBuffer);
	procedure WFIFOL(index:word; l:cardinal; var buf : TBuffer);
	procedure WFIFOS(index:word; str:string; cnt:word; var buf : TBuffer);

	function RFIFOW(index:word; var buf : TBuffer) : word;
	function RFIFOL(index:word; var buf : TBuffer) : cardinal;
	function RFIFOS(index:word; cnt:word; buf : TBuffer):string;

implementation
uses
	SysUtils;
	
	procedure WFIFOB(index:word; b:byte; var buf : TBuffer);
	begin
		Assert(index <= 32767, 'WFIFOB: index overflow ' + IntToStr(index));
		Move(b, buf[index], 1);
	end;

	procedure WFIFOW(index:word; w:word; var buf : TBuffer);
	begin
		Assert(index <= 32766, 'WFIFOW: index overflow ' + IntToStr(index));
		Move(w, buf[index], 2);
	end;

	procedure WFIFOL(index:word; l:cardinal; var buf : TBuffer);
	begin
		Assert(index <= 32766, 'WFIFOW: index overflow ' + IntToStr(index));
		Move(l, buf[index], 4);
	end;

	procedure WFIFOS(index:word; str:string; cnt:word; var buf : TBuffer);
	var
		i :integer;
	begin
		Assert(index <= 32767, 'WFIFOS: index overflow ' + IntToStr(index));
		Assert(index + cnt <= 32767, 'WFIFOS: index+cnt overflow ' + IntToStr(index+cnt));

		FillChar(buf[index], cnt, 0);
		i := Length(str);
		if i = 0 then exit;
		if i > cnt then i := cnt;
		Move(str[1], buf[index], i);
	end;

	function RFIFOW(index:word; var buf : TBuffer) : word;
	begin
		Assert(index <= 32766, 'WFIFOW: index overflow ' + IntToStr(index));
		Move(buf[index], Result, 2);
	end;

	function RFIFOL(index:word; var buf : TBuffer) : cardinal;
	begin
		Assert(index <= 32766, 'WFIFOW: index overflow ' + IntToStr(index));
		Move(buf[index], Result, 4);
	end;

	function RFIFOS(index:word; cnt:word; buf : TBuffer):string;
	var
		stra : TCBuffer;
	begin
		Assert(index <= 32767, 'RFIFOS: index overflow ' + IntToStr(index));
		Assert(index + cnt <= 32767, 'RFIFOS: index+cnt overflow ' + IntToStr(index+cnt));
		stra[cnt] := #0;
		Move(buf[index], stra, cnt);
		Result := stra;
	end;

end.
 