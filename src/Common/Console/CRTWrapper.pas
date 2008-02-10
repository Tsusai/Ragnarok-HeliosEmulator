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
	CRTBlue         = {$IFDEF MSWINDOWS}01{$ENDIF}{$IFDEF LINUX}04{$ENDIF};
	CRTGreen        = {$IFDEF MSWINDOWS}02{$ENDIF}{$IFDEF LINUX}03{$ENDIF};
	CRTCyan         = {$IFDEF MSWINDOWS}03{$ENDIF}{$IFDEF LINUX}06{$ENDIF};
	CRTRed          = {$IFDEF MSWINDOWS}04{$ENDIF}{$IFDEF LINUX}01{$ENDIF};
	CRTMagenta      = 5;
	CRTBrown        = {$IFDEF MSWINDOWS}06{$ENDIF}{$IFDEF LINUX}03{$ENDIF};
	CRTGray         = 7;

	CRTDarkGray     = 8;
	CRTLightBlue    = {$IFDEF MSWINDOWS}09{$ENDIF}{$IFDEF LINUX}12{$ENDIF};
	CRTLightGreen   = 10;
	CRTLightCyan    = {$IFDEF MSWINDOWS}11{$ENDIF}{$IFDEF LINUX}14{$ENDIF};
	CRTLightRed     = {$IFDEF MSWINDOWS}12{$ENDIF}{$IFDEF LINUX}09{$ENDIF};
	CRTLightMagenta = 13;
	CRTYellow       = {$IFDEF MSWINDOWS}14{$ENDIF}{$IFDEF LINUX}11{$ENDIF};
	CRTWhite        = 15;
	CRTBlink        = 128;

	//Intitialization and Termination of API overrides
	procedure SetupCRT;
	procedure EndCRT;

	// Sets text foreground color.
	procedure TextColor(Color: Byte); overload;

	// Gets text forground color.
	function TextColor: Byte; overload;

	// Sets text background color.
	procedure TextBackground(Color: Byte); overload;

	// Gets text background color.
	function TextBackground: Byte; overload;

implementation
uses
        {$IFDEF MSWINDOWS}
        WinConsole;
        {$ELSE}
        //
	{$ENDIF}

	procedure SetupCRT;
	begin
		//Windows does not have init routines, ignored.
		{$IFDEF LINUX}
		//LinCRT.InitLinCRT;
		//LinCRT.SetScrollWnd(true);
		{$ENDIF}
	end;

	procedure EndCRT;
	begin
		//Windows does not have API closing routines, ignored.
		{$IFDEF LINUX}
		//LinCRT.DoneLinCRT;
		{$ENDIF}
	end;

	// Sets text foreground color.
	procedure TextColor(Color: Byte);
	begin
                {$IFDEF LINUX}
                //
		{$ELSE}
                WinConsole.TextColor(Color);
		{$ENDIF}
	end;

	// Gets text forground color.
	function TextColor: Byte;
	begin
                {$IFDEF LINUX}
                //
                {$ELSE}
                Result := WinConsole.TextColor;
		{$ENDIF}
	end;

	// Sets text background color.
	procedure TextBackground(Color: Byte);
	begin
		{$IFDEF LINUX}
                //
		{$ELSE}
                WinConsole.TextBackground(Color);
		{$ENDIF}
		ClrScr;
	end;

	// Gets text background color.
	function TextBackground: Byte;
	begin
                {$IFDEF LINUX}
                //
                {$ELSE}
                Result := WinConsole.TextColor;
		{$ENDIF}
	end;


end.
