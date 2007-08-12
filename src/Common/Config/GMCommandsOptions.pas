//------------------------------------------------------------------------------
//GMCommandsOptions                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//      	gain access to configuration variables loaded from GMCommands.ini
//
//	Changes -
//		[2007/08/10] Aeomin - Created.
//
//------------------------------------------------------------------------------
unit GMCommandsOptions;

interface

uses
	Classes,
	IniFiles,
	List32;

type
	//----------------------------------------------------------------------
	//TGMCommandsOptions                                               CLASS
	//----------------------------------------------------------------------
	TGMCommandsOptions = class(TMemIniFile)
	public
		procedure Load(const Commands : TStringList;var Levels: TIntList32);
		procedure Save(const Commands : TStringList;const Levels: TIntList32);
	end;

implementation

uses
	SysUtils,
	Math,
	GMCommands;

//------------------------------------------------------------------------------
//Load                                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Loop through list of commands, and try get each level
//		setting.
//
//	Changes -
//		[2007/08/10] Aeomin - Created.
//
//------------------------------------------------------------------------------
procedure TGMCommandsOptions.Load(const Commands : TStringList;var Levels: TIntList32);
var
	Section    : TStringList;

	//----------------------------------------------------------------------
	//LoadGMCommandsOptions                                    SUB PROCEDURE
	//----------------------------------------------------------------------
	procedure LoadGMCommandsOptions;
	var
		Index : Integer;
	begin
		ReadSectionValues('GMCommands', Section);

		//Loop through all commands, and try to get each level config
		for Index := Commands.Count -1 downto 0 do
		begin
			// Set default GM level require 99, to prevent mistakes
			Levels[Index] := EnsureRange(StrToIntDef(Section.Values[Commands[Index]], 99), 0, High(Byte));
		end;
	end;{LoadGMCommandsOptions}
	//----------------------------------------------------------------------
begin
	Section    := TStringList.Create;
	Section.QuoteChar := '"';
	Section.Delimiter := ',';

	LoadGMCommandsOptions;

	Section.Free;
end;
//------------------------------------------------------------------------------

procedure TGMCommandsOptions.Save(const Commands : TStringList;const Levels: TIntList32);
var
	Index : Integer;
begin
	for Index := Commands.Count -1 downto 0 do
	begin
		WriteString('GMCommands',Commands[Index], IntToStr(Levels[Index]));
	end;

	UpdateFile;
end;

end{GMCommandsOptions}.
