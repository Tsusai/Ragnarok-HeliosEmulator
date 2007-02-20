//------------------------------------------------------------------------------
//Console()				                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This unit contains the Console unit, an object for managing console
//		output.
//
//	Changes -
//		September 19th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit Console;

interface

uses
	CRT;

type

//------------------------------------------------------------------------------
//TMainProc                                                               CLASS
//------------------------------------------------------------------------------
	TConsole = class
	public
		procedure Write(
										AString : String;
										Text : Byte = CRTBlack;
										Background : Byte = CRTWhite
		);
		procedure WriteLn(
										AString : String;
										Text : Byte = CRTBlack;
										Background : Byte = CRTWhite
		);

		Constructor Create;
		Destructor Destroy;override;
	end;{TMainProc}
//------------------------------------------------------------------------------


implementation
uses
	SyncObjs;

var
	CriticalSection : TCriticalSection;


//------------------------------------------------------------------------------
//TConsole.Create()                                                 CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Creates our critical section for managing console output.
//
//	Changes -
//		February 19th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Constructor TConsole.Create;
begin
	inherited;
	CRT.TextBackground(CRTWhite);
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
//		February 19th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
Destructor TConsole.Destroy;
begin
	CriticalSection.Free;
	inherited;
end;//Destroy
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TConsole.Write()                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Alias of Write.
//
//	Changes -
//		February 19th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TConsole.Write(
	AString 	: String;
	Text 			: Byte = CRTBlack;
	Background: Byte = CRTWhite
	);
begin
	CriticalSection.Enter;

	CRT.TextColor(Text);
	System.Write(AString);

	CriticalSection.Leave;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//TConsole.WriteLn()                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Alias of WriteLn.
//
//	Changes -
//		February 19th, 2007 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TConsole.WriteLn(
	AString 	: String;
	Text 			: Byte = CRTBlack;
	Background: Byte = CRTWhite
	);
begin
	CriticalSection.Enter;

	CRT.TextColor(Text);
	System.WriteLn(AString);

	CriticalSection.Leave;
end;
//------------------------------------------------------------------------------

end{Console}.
