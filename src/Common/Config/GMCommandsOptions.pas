//------------------------------------------------------------------------------
//GMCommandsOptions                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//      	gain access to configuration variables loaded from GMCommands.ini
//
//	Changes -
//		[2007/08/10] Aeomin - Created.
//		[2007/09/08] Aeomin - Update to support new command structure
//		[2007/10/29] Aeomin - Merge from CustomGMCommandNameOptions.pas
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
		function Load(const CommandList : TStringList;var Commands: TStringList):Char;
		procedure Save(const Commands : TStringList);
	end;

implementation

uses
	SysUtils,
	Math,
	Globals,
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
//		[2007/10/29] Aeomin - Merge from CustomGMCommandNameOptions.pas
//
//------------------------------------------------------------------------------
function TGMCommandsOptions.Load(const CommandList : TStringList;var Commands: TStringList):Char;
var
	Section    : TStringList;

	//----------------------------------------------------------------------
	//LoadCommandConf                                          SUB PROCEDURE
	//----------------------------------------------------------------------
	function LoadCommandConf:Char;
	begin
		ReadSectionValues('Command Configs', Section);

		if Section.Values['Command Prefix'] = '' then
		begin
			Section.Values['Command Prefix'] := '#';

			WriteString('Command Configs', 'Command Prefix', '#');

			UpdateFile;

			Result := '#';
		end else
		begin
			Result := Section.Values['Command Prefix'][1];
		end;
	end;
	//----------------------------------------------------------------------


	//----------------------------------------------------------------------
	//LoadGMCommandsOptions                                    SUB PROCEDURE
	//----------------------------------------------------------------------
	procedure LoadGMCommandsOptions;
	var
		Index   : Integer;
		Command : TCommand;
	begin
		ReadSectionValues('Command Level', Section);

		//Loop through all commands, and try to get each level config
		for Index := CommandList.Count -1 downto 0 do
		begin
			Command := CommandList.Objects[Index] as TCommand;
			// Set default GM level require 99, to prevent mistakes
			Command.Level := EnsureRange(StrToIntDef(Section.Values[CommandList[Index]], 99), 0, High(Byte));
		end;
	end;{LoadGMCommandsOptions}
	//----------------------------------------------------------------------


	//----------------------------------------------------------------------
	//LoadCustomGMCommandNames                                 SUB PROCEDURE
	//----------------------------------------------------------------------
	procedure LoadCustomGMCommandNames;
	var
		Index      : Integer;
		Splitter   : TStringList;
		SplitIdx   : Integer;
		Command    : TCommand;
	begin
		ReadSectionValues('Command Names', Section);
		Commands.Clear;
		Splitter := TStringList.Create;
		Splitter.Delimiter := ',';
		//Loop through all commands
		for Index := CommandList.Count -1 downto 0 do
		begin
			// So yeh, nothing is best
			// If there's empty or called entry not exit, then just set as default name!
			if Section.Values[CommandList[Index]] = '' then
			begin
				Section.Values[CommandList[Index]] := CommandList[Index];
				//And don't forget to write it!
				WriteString('Command Names', CommandList[Index], CommandList[Index]);
			end;
			Splitter.DelimitedText := Section.Values[CommandList[Index]];
												// Get TCommand object
			Command := CommandList.Objects[Index] as TCommand;
			// TODO: use result isnt smart =(
			Command.Syntax := Result + Command.Name + ' ' + Command.Syntax;

			for SplitIdx := Splitter.Count -1 downto 0 do
			begin
				if ( Commands.IndexOf(Splitter[SplitIdx]) = -1 ) then
				begin
					Commands.AddObject(Splitter[SplitIdx], Command);
				end else
				begin
					// Duplicated!
					Console.Message('Duplicated GM Command name ''' + Splitter[SplitIdx] + '''', 'Config', MS_WARNING);
				end;
			end;
		end;
		Splitter.Clear;
		Splitter.Free;
		//Well, update it, even though "may" not needed it..
		UpdateFile;
	end;{LoadCustomGMCommandNames}
	//----------------------------------------------------------------------
begin
	Section    := TStringList.Create;
	Section.QuoteChar := '"';
	Section.Delimiter := ',';

	Result := LoadCommandConf;

	LoadGMCommandsOptions;
	LoadCustomGMCommandNames;

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
		WriteString('Command Level',Command.Name, IntToStr(Command.Level));
	end;

	UpdateFile;
end;{Save}
//------------------------------------------------------------------------------
end{GMCommandsOptions}.
