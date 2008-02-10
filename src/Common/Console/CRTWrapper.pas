unit CRTWrapper;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}

//This unit is a wrapper of sorts for the linux and windows api overrides for
//console coloring and such.
interface

const
	//The color codes are different per OS, "abstracted"
	CRTBlack        = 0;
	CRTBlue         = 01;
	CRTGreen        = 02;
	CRTCyan         = 03;
	CRTRed          = 04;
	CRTMagenta      = 05;
	CRTBrown        = 06;
	CRTGray         = 07;
	CRTDarkGray     = 08;
	CRTLightBlue    = 09;
	CRTLightGreen   = 10;
	CRTLightCyan    = 11;
	CRTLightRed     = 12;
	CRTLightMagenta = 13;
	CRTYellow       = 14;
	CRTWhite        = 15;
	CRTBlink        = 128;

	// Sets text foreground color.
	procedure TextColor(Color: Byte); overload;

	// Gets text forground color.
	function TextColor: Byte; overload;

	// Sets text background color.
	procedure TextBackground(Color: Byte); overload;

	// Gets text background color.
	function TextBackground: Byte; overload;

implementation

{$IFDEF MSWINDOWS}
uses
	WinConsole;
{$ENDIF}

	// Sets text foreground color.
	procedure TextColor(Color: Byte);
	begin
		{$IFDEF MSWINDOWS}
		WinConsole.TextColor(Color);
		{$ENDIF}
	end;

	// Gets text forground color.
	function TextColor : Byte;
	begin
		{$IFDEF MSWINDOWS}
		Result := WinConsole.TextColor;
		{$ELSE}
		Result := 0;
		{$ENDIF}
	end;

	// Sets text background color.
	procedure TextBackground(Color: Byte);
	begin
		{$IFDEF MSWINDOWS}
		WinConsole.TextBackground(Color);
		ClrScr;
		{$ENDIF}
	end;

	// Gets text background color.
	function TextBackground: Byte;
	begin
		{$IFDEF MSWINDOWS}
		Result := WinConsole.TextBackground;
		{$ELSE}
		Result := 0;
		{$ENDIF}
	end;


end.
