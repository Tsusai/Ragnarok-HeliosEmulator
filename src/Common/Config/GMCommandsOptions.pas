//------------------------------------------------------------------------------
//GMCommandsOptions                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//      	gain access to configuration variables loaded from GMCommands.ini
//
//	Changes -
//		[2007/08/10] Aeomin - Created.
//		[2007/09/08] Aeomin - Update to support new command structure
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
		procedure Load(const Commands : TStringList);
		procedure Save(const Commands : TStringList);
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
procedure TGMCommandsOptions.Load(const Commands : TStringList);
var
	Section    : TStringList;

	//----------------------------------------------------------------------
	//LoadGMCommandsOptions                                    SUB PROCEDURE
	//----------------------------------------------------------------------
	procedure LoadGMCommandsOptions;
	var
		Index   : Integer;
		Command : TCommand;
	begin
		ReadSectionValues('GMCommands', Section);

		//Loop through all commands, and try to get each level config
		for Index := Commands.Count -1 downto 0 do
		begin
			Command := Commands.Objects[Index] as TCommand;
			// Set default GM level require 99, to prevent mistakes
			Command.Level := EnsureRange(StrToIntDef(Section.Values[Commands[Index]], 99), 0, High(Byte));
		end;
	end;{LoadGMCommandsOptions}
	//----------------------------------------------------------------------
begin
	Section    := TStringList.Create;
	Section.QuoteChar := '"';
	Section.Delimiter := ',';

	LoadGMCommandsOptions;

	Section.Free;
end;{Load}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Load                                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Loop through list of commands, and try get each level
//		setting.
//
//	Changes -
//		[2007/09/28] Aeomin - Created header...
//
//------------------------------------------------------------------------------
procedure TGMCommandsOptions.Save(const Commands : TStringList);
var
	Index   : Integer;
	Command : TCommand;
begin
	for Index := Commands.Count -1 downto 0 do
	begin
		Command := Commands.Objects[Index] as TCommand;
		WriteString('GMCommands',Command.Name, IntToStr(Command.Level));
	end;

	UpdateFile;
end;{Save}
//------------------------------------------------------------------------------
end{GMCommandsOptions}.
