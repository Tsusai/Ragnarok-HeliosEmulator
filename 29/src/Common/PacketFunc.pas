unit PacketFunc;

interface
uses
	PacketTypes;

type TSocket = class
	procedure WriteBuffer(index:word; b:byte; var buf : TBuffer); overload;
	procedure WriteBuffer(index:word; w:word; var buf : TBuffer); overload;
	procedure WriteBuffer(index:word; l:cardinal; var buf : TBuffer); overload;
	procedure WriteBuffer(index:word; str:string; cnt:word; var buf : TBuffer); overload;

	function BufferReadWord(index:word; var buf : TBuffer) : word;
	function BufferReadCardinal(index:word; var buf : TBuffer) : cardinal;
	function BufferReadString(index:word; cnt:word; buf : TBuffer) : string;
end;

var
  Socket : TSocket;
implementation
uses
	SysUtils;
	
	procedure TSocket.WriteBuffer(index:word; b:byte; var buf : TBuffer);
	begin
		Assert(index <= 32767, 'WriteBuffer - Byte: index overflow ' + IntToStr(index));
		Move(b, buf[index], 1);
	end;

	procedure TSocket.WriteBuffer(index:word; w:word; var buf : TBuffer);
	begin
		Assert(index <= 32766, 'WriteBuffer - Word: index overflow ' + IntToStr(index));
		Move(w, buf[index], 2);
	end;

	procedure TSocket.WriteBuffer(index:word; l:cardinal; var buf : TBuffer);
	begin
		Assert(index <= 32766, 'WriteBuffer - Cardinal: index overflow ' + IntToStr(index));
		Move(l, buf[index], 4);
	end;

	procedure TSocket.WriteBuffer(index:word; str:string; cnt:word; var buf : TBuffer);
	var
		i :integer;
	begin
		Assert(index <= 32767, 'WriteBuffer - String: index overflow ' + IntToStr(index));
		Assert(index + cnt <= 32767, 'WriteBuffer - String: index+cnt overflow ' + IntToStr(index+cnt));

		FillChar(buf[index], cnt, 0);
		i := Length(str);
		if i = 0 then exit;
		if i > cnt then i := cnt;
		Move(str[1], buf[index], i);
	end;

	function TSocket.BufferReadWord(index:word; var buf : TBuffer) : word;
	begin
		Assert(index <= 32766, 'BufferReadWord: index overflow ' + IntToStr(index));
		Move(buf[index], Result, 2);
	end;

	function TSocket.BufferReadCardinal(index:word; var buf : TBuffer) : cardinal;
	begin
		Assert(index <= 32766, 'BufferReadCardinal: index overflow ' + IntToStr(index));
		Move(buf[index], Result, 4);
	end;

	function TSocket.BufferReadString(index:word; cnt:word; buf : TBuffer):string;
	var
		stra : TCBuffer;
	begin
		Assert(index <= 32767, 'BufferReadString: index overflow ' + IntToStr(index));
		Assert(index + cnt <= 32767, 'BufferReadString: index+cnt overflow ' + IntToStr(index+cnt));
		stra[cnt] := #0;
		Move(buf[index], stra, cnt);
		Result := stra;
	end;

end.
 