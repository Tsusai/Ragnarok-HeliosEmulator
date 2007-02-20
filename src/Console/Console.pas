//------------------------------------------------------------------------------
//Console()				                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This unit contains the Console unit, an object for managing console
//		output.
//
//	Changes -
//		September 19th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit Console;

interface

uses
	CRT;

type

//------------------------------------------------------------------------------
//TConsole                                                               CLASS
//------------------------------------------------------------------------------
	TConsole = class
	private
		LineColor : Boolean;

	public
		procedure WriteLn(
										AString : String;
										TextColor : Byte = CRTWhite
		);

		procedure Message(
			AString			: String;
			From				: String = 'General';
			MessageType : Byte = 255
		);

		Constructor Create;
		Destructor Destroy;override;
	end;{TMainProc}
//------------------------------------------------------------------------------

implementation
uses
	SyncObjs,
	Globals,
	Main;

var
	CriticalSection : TCriticalSection;


//------------------------------------------------------------------------------
//TConsole.Create()                                                 CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Creates our critical section for managing console output.
//
//	Changes -
//		February 19th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Constructor TConsole.Create;
begin
	inherited;
	LineColor := FALSE;
	CRT.TextBackground(CRTBlack);
	CriticalSection := TCriticalSection.Create;
end;//Create
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TConsole.Destroy()                                                 DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			frees up out object
//
//	Changes -
//		February 19th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Destructor TConsole.Destroy;
begin
	CriticalSection.Free;
	inherited;
end;//Destroy
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TConsole.WriteLn()                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Alias of WriteLn.
//
//	Changes -
//		February 19th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TConsole.WriteLn(
	AString 		: String;
	TextColor		: Byte = CRTWhite
	);
begin
	CriticalSection.Enter;

	CRT.TextColor(TextColor);
	System.Writeln(AString);

	CriticalSection.Leave;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TConsole.Message()                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			All Server messages will be displayed through here.
//
//	Changes -
//		February 19th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TConsole.Message(
	AString 		: String;
	From				: String = 'General';
	MessageType : Byte = 255
	);
var
	Index : Integer;
	Color : Byte;

begin
	CriticalSection.Enter;
	if LineColor then
	begin
		Color := CRTGray;
	end else
	begin
		Color := CRTWhite;
	end;

	//Output based on message type.
	case MessageType of

	MS_INFO ://Information
		begin
			if MainProc.Options.ShowInfo then
			begin
				CRT.TextColor(CRTWhite);
				System.Write('[');

				CRT.TextColor(CRTGray);
				System.Write(From);

				CRT.TextColor(CRTWhite);
				System.Write(']');

				for Index := Length(From) to 15 do
				begin
					System.Write(' ');
				end;

				System.Write(' -INFO  : ');

				CRT.TextColor(Color);
				System.Writeln(AString);
			end;
		end;

	MS_NOTICE ://Notice
		begin
			if MainProc.Options.ShowNotice then
			begin
				CRT.TextColor(CRTWhite);
				System.Write('[');

				CRT.TextColor(CRTCyan);
				System.Write(From);

				CRT.TextColor(CRTWhite);
				System.Write(']');

				for Index := Length(From) to 15 do
				begin
					System.Write(' ');
				end;

				System.Write(' -NOTICE: ');

				CRT.TextColor(Color);
				System.Writeln(AString);
			end;
		end;

	MS_WARNING ://Warning
		begin
			if MainProc.Options.ShowWarning then
			begin
				CRT.TextColor(CRTWhite);
				System.Write('[');

				CRT.TextColor(CRTYellow);
				System.Write(From);

				CRT.TextColor(CRTWhite);
				System.Write(']');

				for Index := Length(From) to 15 do
				begin
					System.Write(' ');
				end;

				System.Write(' -WARNING: ');

				CRT.TextColor(Color);
				System.Writeln(AString);
			end;
		end;

	MS_ERROR ://Error
		begin
			if MainProc.Options.ShowError then
			begin
				CRT.TextColor(CRTWhite);
				System.Write('[');

				CRT.TextColor(CRTLightRed);
				System.Write(From);

				CRT.TextColor(CRTWhite);
				System.Write(']');

				for Index := Length(From) to 15 do
				begin
					System.Write(' ');
				end;

				System.Write(' -ERROR : ');

				CRT.TextColor(Color);
				System.Writeln(AString);
			end;
		end;

	else //everything else
		begin
			CRT.TextColor(CRTWhite);
			System.Write('[');

			CRT.TextColor(CRTWhite);
			System.Write(From);

			CRT.TextColor(CRTWhite);
			System.Write(']');

			for Index := Length(From) to 15 do
			begin
				System.Write(' ');
			end;

			System.Write(' - ');

			CRT.TextColor(Color);
			System.Writeln(AString);
		end;
	end;

	if LineColor = TRUE then
	begin
		LineColor := FALSE;
	end else
	begin
		LineColor := TRUE;
	end;

	CriticalSection.Leave;
end;
//------------------------------------------------------------------------------

end{Console}.
