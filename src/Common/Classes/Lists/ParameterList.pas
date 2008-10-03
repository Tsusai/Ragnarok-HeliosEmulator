unit ParameterList;

interface

uses
	List32;
type
	TParameterType = (fString,fInteger,fLongWord,fObject);
	TParameterObject = class
	private
	public
		fType : TParameterType;
		fString: String;
		fInteger : Integer;
		fLongWord:LongWord;
		fObject : TObject;
	end;
	TParameterList = class
	private
		fList : TIntList32;
		function AddObject(const ID:LongWord;var AObject:TParameterObject):Boolean;
		function GetObject(const ID:LongWord;var AObject:TParameterObject):Boolean;
	public
		constructor Create;
		destructor Destroy; override;
		procedure AddAsLongWord(const ID:LongWord;const AValue:LongWord);
		function GetAsLongWord(const ID:LongWord):LongWord;
		procedure AddAsString(const ID:LongWord;const AValue:String);
		function GetAsString(const ID:LongWord):String;
		procedure AddAsObject(const ID:LongWord;const AnObject:TObject);
		function GetAsObject(const ID:LongWord):TObject;
	end;
implementation

uses
	SysUtils,
	Math;

constructor TParameterList.Create;
begin
	fList := TIntList32.Create;
end;

destructor TParameterList.Destroy;
var
	Index : Integer;
begin
	if fList.Count > 0 then
	begin
		for Index := 0 to fList.Count - 1 do
		begin
			fList.Objects[Index].Free;
		end;
	end;
	fList.Free;
end;
function TParameterList.AddObject(const ID:LongWord;var AObject:TParameterObject):Boolean;
begin
	Result := False;
	if fList.IndexOf(ID) = -1 then
	begin
		AObject := TParameterObject.Create;
		fList.AddObject(ID,AObject);
		Result := True;
	end;
end;
function TParameterList.GetObject(const ID:LongWord;var AObject:TParameterObject):Boolean;
var
	Index : Integer;
begin
	Result := False;
	Index := fList.IndexOf(ID);
	if Index > -1 then
	begin
		AObject := fList.Objects[Index] as TParameterObject;
		Result := True;
	end;
end;
procedure TParameterList.AddAsLongWord(const ID:LongWord;const AValue:LongWord);
var
	ParameterObject : TParameterObject;
begin
	if AddObject(ID,ParameterObject) then
	begin
		ParameterObject.fType := fLongWord;
		ParameterObject.fLongWord := AValue;
	end;
end;
function TParameterList.GetAsLongWord(const ID:LongWord):LongWord;
var
	ParameterObject : TParameterObject;
begin
	Result := 0;
	if GetObject(ID,ParameterObject) then
	begin
		case ParameterObject.fType of
			fLongWord : Result := ParameterObject.fLongWord;
			fInteger : Result := LongWord(ParameterObject.fInteger);
			fString : Result := StrToIntDef(ParameterObject.fString,0);
		end;
	end;
end;
procedure TParameterList.AddAsString(const ID:LongWord;const AValue:String);
var
	ParameterObject : TParameterObject;
begin
	if AddObject(ID,ParameterObject) then
	begin
		ParameterObject.fType := fString;
		ParameterObject.fString := AValue;
	end;
end;
function TParameterList.GetAsString(const ID:LongWord):String;
var
	ParameterObject : TParameterObject;
begin
	Result := '';
	if GetObject(ID,ParameterObject) then
	begin
		case ParameterObject.fType of
			fLongWord : Result := IntToStr(ParameterObject.fLongWord);
			fInteger : Result := IntToStr(ParameterObject.fInteger);
			fString : Result := ParameterObject.fString;
		end;
	end;
end;
procedure TParameterList.AddAsObject(const ID:LongWord;const AnObject:TObject);
var
	ParameterObject : TParameterObject;
begin
	if AddObject(ID,ParameterObject) then
	begin
		ParameterObject.fType := fObject;
		ParameterObject.fObject := AnObject;
	end;
end;
function TParameterList.GetAsObject(const ID:LongWord):TObject;
var
	ParameterObject : TParameterObject;
begin
	Result := nil;
	if GetObject(ID,ParameterObject) then
	begin
		Result := ParameterObject.fObject;
	end;
end;
end.