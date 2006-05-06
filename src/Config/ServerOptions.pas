unit ServerOptions;

interface
	uses
		IniFiles;

	type
		TServerOptions = class(TMemIniFile)
				procedure Load;
				procedure Save;
				procedure WriteNewINI;
			public
				LoginPort : Word;
				CharaPort : Word;
				ZonePort : Word;

				LANIp : String;
				WANIp : String;

				EnableMF : Boolean;
			private
		end;

implementation
	uses
		Classes,
		SysUtils,
		Console;

	procedure TServerOptions.Load;
	begin
		if NOT FileExists(FileName) then begin
			WriteNewINI;
		end;
			LoginPort		:= StrToInt(ReadString('Communication','LoginPort','6900'));
			CharaPort		:= StrToInt(ReadString('Communication','CharaPort','6121'));
			ZonePort		:= StrToInt(ReadString('Communication','ZonePort','5121'));
			WANIp				:= ReadString('Communication','WANIp','');
			LANIp				:= ReadString('Communication','LANIp','127.0.0.1');

			EnableMF := StrToBool(ReadString('LoginOptions','EnableMF','FALSE'));

	end;

	procedure TServerOptions.Save;
	begin
		WriteString('Communication','LoginPort',IntToStr(LoginPort));
		WriteString('Communication','CharaPort',IntToStr(CharaPort));
		WriteString('Communication','ZonePort',IntToStr(ZonePort));
		WriteString('Communication','WANIp',WANIp);
		WriteString('Communication','LANIp',LANIp);

		WriteString('LoginOptions','EnableMF',BoolToStr(EnableMF));
		UpdateFile;
	end;

	procedure TServerOptions.WriteNewINI;
	begin
		WriteString('Communication','LoginPort','6900');
		WriteString('Communication','CharaPort','6121');
		WriteString('Communication','ZonePort','5121');
		WriteString('Communication','WANIp','');
		WriteString('Communication','LANIp','127.0.0.1');

		WriteString('LoginOptions','EnableMF','FALSE');
		UpdateFile;
	end;
end.
