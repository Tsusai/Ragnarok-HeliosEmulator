//------------------------------------------------------------------------------
//Terminal				                                                         UNIT
//------------------------------------------------------------------------------
//	What it does-
//			This unit contains the Console unit, an object for managing console
//		output. This class uses a TCriticalSection to manage both console output
//		and log output. This ensures that we won't get any access violations from
//		file access and that our console messages, while being protected to begin
//		with, will output correctly and not "mix" together in the console window.
//
//	Changes -
//		September 19th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit Terminal;

interface

uses
	CRT,
	ConsoleOptions;

type

//------------------------------------------------------------------------------
//TConsole                                                               CLASS
//------------------------------------------------------------------------------
	TConsole = class
	private
		LineColor : Boolean;

		Procedure WriteLogEntry(AString : String; From : String; MessageType : Byte);

	public
		Options : TConsoleOptions;

		procedure WriteLn(
										AString : String;
										TextColor : Byte = CRTWhite
		);

		procedure Message(
			AString			: String;
			From				: String	= 'General';
			MessageType : Byte		= 255
		);

		Constructor Create;
		Destructor Destroy;override;
	end;{TMainProc}
//------------------------------------------------------------------------------

implementation
uses
	SyncObjs,
	Globals,
	Main,
	SysUtils,
	Classes;

var
	CriticalSection : TCriticalSection;


//------------------------------------------------------------------------------
//Create()                                                 				CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Creates our critical section for managing console + log output.
//
//	Changes -
//		February 19th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Constructor TConsole.Create;
begin
	inherited;
	Options := TConsoleOptions.Create(MainProc.Options.ConfigDirectory+'/'+'Console.ini');
	Options.Load;
	
	LineColor := FALSE;
	CRT.TextBackground(CRTBlack);
	
	CriticalSection := TCriticalSection.Create;
end;//Create
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy()					                                                 DESTRUCTOR
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

	Options.Save;
	Options.Free;
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
//Message()                                                 				PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			All Server messages will be displayed through here.
//
//	Changes -
//		February 19th, 2007 - RaX - Created.
//		February 24th, 2007 - RaX - Reorganized for simplicity and speed.
//
//------------------------------------------------------------------------------
procedure TConsole.Message(
	AString 		: String;
	From				: String = 'General';
	MessageType : Byte = 255
	);
var
	Index				: Integer;
	FromColor		: Byte;
	TypeString	: String;
	Color				: Byte;
	ShowMessage	: Boolean;

begin
	//Initialize our variables...
	FromColor		:= CRTWhite;
	TypeString	:= ' - ';
	ShowMessage := FALSE;
	//Output based on message type.
	case MessageType of

	MS_INFO ://Information
		begin
			if Options.ShowInfo then
			begin
				ShowMessage := TRUE;
				FromColor		:= CRTGray;
				TypeString	:= ' :INFO   - ';
			end;
		end;

	MS_NOTICE ://Notice
		begin
			if Options.ShowNotices then
			begin
				ShowMessage := TRUE;
				FromColor		:= CRTCyan;
				TypeString	:= ' :NOTICE - ';
			end;
		end;

	MS_WARNING ://Warning
		begin
			if Options.ShowWarnings then
			begin
				ShowMessage := TRUE;
				FromColor		:= CRTYellow;
				TypeString	:= ' :WARNING- ';
			end;
		end;

	MS_ERROR ://Error
		begin
			if Options.ShowErrors then
			begin
				ShowMessage := TRUE;
				FromColor		:= CRTLightRed;
				TypeString	:= ' :ERROR  - ';
			end;
		end;

	MS_DEBUG ://DEBUG lines
		begin
			if Options.ShowDebug then
			begin
				ShowMessage := TRUE;
				FromColor		:= CRTGray;
				TypeString	:= ' :DEBUG  - ';
			end;
		end;
	end;

	if ShowMessage then
	begin
		//Alternating line colors
		if LineColor then
		begin
			Color := CRTGray;
		end else
		begin
			Color := CRTWhite;
		end;

		//If the From string is over 15 characters, we truncate the last few.
		if Length(From) > 17 then
		begin
			SetLength(From, 17);
		end;

		//Enter a critical section to avoid mixed messages.
		CriticalSection.Enter;

		//Set our random characters color
		CRT.TextColor(CRTWhite);
		System.Write('[');

		//Set our From color, the server/routine this message is being sent from.
		CRT.TextColor(FromColor);
		System.Write(From);

		//Set our Random character color, again.
		CRT.TextColor(CRTWhite);
		System.Write(']');

		//Match up the messages no matter how long the from string is by adding spaces
		for Index := Length(From) to 15 do
		begin
			System.Write(' ');
		end;

		//Write the type of message
		System.Write(TypeString);

		//write the body of the message
		CRT.TextColor(Color);
		System.Writeln(AString);

		//Write our log entry if applicable
		if Options.LogsEnabled then
		begin
			WriteLogentry(AString, From, MessageType);
		end;

		//Leave our critical section
		CriticalSection.Leave;

		//Alternate our line colors
		if LineColor = TRUE then
		begin
			LineColor := FALSE;
		end else
		begin
			LineColor := TRUE;
		end;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//WriteLogEntry()                                               			  PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Writes a log message to file.
//
//	Changes -
//		February 24th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TConsole.WriteLogEntry(AString: string; From: string; MessageType : Byte);
var
	DebugFile		: TextFile;
	LogMessage	: Boolean;
begin
	LogMessage := FALSE;

	case MessageType of

	MS_INFO ://Information
		begin
			if Options.LogInfo then
			begin
				LogMessage := TRUE;
			end;
		end;

	MS_NOTICE ://Notice
		begin
			if Options.LogNotices then
			begin
				LogMessage := TRUE;
			end;
		end;

	MS_WARNING ://Warning
		begin
			if Options.LogWarnings then
			begin
				LogMessage := TRUE;
			end;
		end;

	MS_ERROR ://Error
		begin
			if Options.LogErrors then
			begin
				LogMessage := TRUE;
			end;
		end;

	MS_DEBUG ://DEBUG lines
		begin
			if Options.LogDebug then
			begin
				LogMessage := TRUE;
			end;
		end;
	end;

	if LogMessage then
	begin
		AssignFile(DebugFile, Options.LogsFileName);

		if NOT FileExists(Options.LogsFileName) then
		begin
			ReWrite(DebugFile);
			CloseFile(DebugFile);
		end;
		// Open to append a enw log entry
		Append(DebugFile);

		// Write this final line
		System.WriteLn(DebugFile, Format('[%s %s] - %s - %s', [DateToStr(Date),TimeToStr(Time),From, AString]));

		// Close the file
		CloseFile(DebugFile);
	end;
end;
//------------------------------------------------------------------------------
end{Console}.
