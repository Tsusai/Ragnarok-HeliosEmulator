unit Database;

interface

procedure LoadDatabase;

implementation
uses
	//Project
	Main,
	//Delphi
	SysUtils;

procedure LoadMaps;
begin
	Debug('-Loading Maps-');
	Debug(Format(' %d Maps loaded sucessfully', [0]));
end;

procedure LoadDatabase;
begin
	LoadMaps;
end;

end.
 