//----------------------------------------------------------------------
//ParameterList                                                     UNIT
//----------------------------------------------------------------------
//	What it does -
//		A "super" class that handles multi parameters used to
//	send in arealoop.
//
//----------------------------------------------------------------------
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
	//----------------------------------------------------------------------
	//TParameterList                                                   CLASS
	//----------------------------------------------------------------------
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
	//----------------------------------------------------------------------
implementation

uses
	SysUtils,
	Math;

//------------------------------------------------------------------------------
//Create                                                             CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does -
//		Initialize class.
//
//	Changes -
//		[2008/10/02] Aeomin - Created
//------------------------------------------------------------------------------
constructor TParameterList.Create;
begin
	fList := TIntList32.Create;
end;{Create}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy                                                             DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does -
//		Free up all objects and list.
//
//	Changes -
//		[2008/10/02] Aeomin - Created
//------------------------------------------------------------------------------
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
end;{Destroy}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//AddObject                                                             FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Add an object into list; return FALSE if ID exists.
//
//	Changes -
//		[2008/10/02] Aeomin - Created
//------------------------------------------------------------------------------
function TParameterList.AddObject(const ID:LongWord;var AObject:TParameterObject):Boolean;
begin
	Result := False;
	if fList.IndexOf(ID) = -1 then
	begin
		AObject := TParameterObject.Create;
		fList.AddObject(ID,AObject);
		Result := True;
	end;
end;{AddObject}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetObject                                                             FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Get an object from list by ID; return FALSE if not found.
//
//	Changes -
//		[2008/10/02] Aeomin - Created
//------------------------------------------------------------------------------
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
end;{GetObject}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//AddAsLongWord                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Add LongWord to list
//
//	Changes -
//		[2008/10/02] Aeomin - Created
//------------------------------------------------------------------------------
procedure TParameterList.AddAsLongWord(const ID:LongWord;const AValue:LongWord);
var
	ParameterObject : TParameterObject;
begin
	if AddObject(ID,ParameterObject) then
	begin
		ParameterObject.fType := fLongWord;
		ParameterObject.fLongWord := AValue;
	end;
end;{AddAsLongWord}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetAsLongWord                                                         FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Get as longword and convert from string if needed
//
//	Changes -
//		[2008/10/02] Aeomin - Created
//------------------------------------------------------------------------------
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
end;{GetAsLongWord}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//AddAsString                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Add string to list
//
//	Changes -
//		[2008/10/02] Aeomin - Created
//------------------------------------------------------------------------------
procedure TParameterList.AddAsString(const ID:LongWord;const AValue:String);
var
	ParameterObject : TParameterObject;
begin
	if AddObject(ID,ParameterObject) then
	begin
		ParameterObject.fType := fString;
		ParameterObject.fString := AValue;
	end;
end;{AddAsString}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetAsString                                                           FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Get string, and attempt to convert if it was integer.
//
//	Changes -
//		[2008/10/02] Aeomin - Created
//------------------------------------------------------------------------------
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
end;{GetAsString}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//AddAsObject                                                          PROCEDURE
//------------------------------------------------------------------------------
//	What it does -
//		Add object to list
//
//	Changes -
//		[2008/10/02] Aeomin - Created
//------------------------------------------------------------------------------
procedure TParameterList.AddAsObject(const ID:LongWord;const AnObject:TObject);
var
	ParameterObject : TParameterObject;
begin
	if AddObject(ID,ParameterObject) then
	begin
		ParameterObject.fType := fObject;
		ParameterObject.fObject := AnObject;
	end;
end;{AddAsObject}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetAsObject                                                           FUNCTION
//------------------------------------------------------------------------------
//	What it does -
//		Get an object from list
//
//	Changes -
//		[2008/10/02] Aeomin - Created
//------------------------------------------------------------------------------
function TParameterList.GetAsObject(const ID:LongWord):TObject;
var
	ParameterObject : TParameterObject;
begin
	Result := nil;
	if GetObject(ID,ParameterObject) then
	begin
		Result := ParameterObject.fObject;
	end;
end;{GetAsObject}
//------------------------------------------------------------------------------

end{ParameterList}.