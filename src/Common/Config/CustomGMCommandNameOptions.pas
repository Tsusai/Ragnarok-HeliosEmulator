//------------------------------------------------------------------------------
//GMCommandsOptions                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//      	Let's customize Gm command name! There's no Save procedure as
//	only save when entry not exists (yes, in Load procedure)
//
//	Changes -
//		[2007/09/28] Aeomin - Created.
//
//------------------------------------------------------------------------------
unit CustomGMCommandNameOptions;

interface

uses
	Classes,
	Globals,
	IniFiles,
	List32;

type
	//----------------------------------------------------------------------
	//TCustomGMCommandNameOptions                                      CLASS
	//----------------------------------------------------------------------
	TCustomGMCommandNameOptions = class(TMemIniFile)
	public
		procedure Load(const CommandList : TStringList;var Commands: TStringList);
	end;
implementation

//------------------------------------------------------------------------------
//Load                                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Load em' ... ...
//
//	Changes -
//		[2007/09/28] Aeomin - Created.
//------------------------------------------------------------------------------
procedure TCustomGMCommandNameOptions.Load(const CommandList : TStringList;var Commands: TStringList);
var
	Section    : TStringList;

	//----------------------------------------------------------------------
	//LoadCustomGMCommandNames                                 SUB PROCEDURE
	//----------------------------------------------------------------------
	procedure LoadCustomGMCommandNames;
	var
		Index      : Integer;
		Splitter   : TStringList;
		SplitIdx   : Integer;
		CommandObj : TObject;
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
			CommandObj := CommandList.Objects[Index];

			for SplitIdx := Splitter.Count -1 downto 0 do
			begin
				if ( Commands.IndexOf(Splitter[SplitIdx]) = -1 ) then
				begin
					Commands.AddObject(Splitter[SplitIdx], CommandObj);
				end else
				begin
					// Duplicated!
					Console.Message('Duplicated GM Command name '''+Splitter[SplitIdx]+'''', 'Config', MS_WARNING);
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

	LoadCustomGMCommandNames;

	Section.Free;
end;{Load}
//------------------------------------------------------------------------------

end{CustomGMCommandNameOptions}.
