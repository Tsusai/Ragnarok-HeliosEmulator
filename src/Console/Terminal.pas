//------------------------------------------------------------------------------
//Terminal				                                                         UNIT
//------------------------------------------------------------------------------
//	What it does -
//			This unit contains the Console unit, an object for managing console
//		output. This class uses a TCriticalSection to manage both console output
//		and log output. This ensures that we won't get any access violations from
//		file access and that our console messages, while being protected to begin
//		with, will output correctly and not "mix" together in the console window.
//
//	Documentation -
//			Here is a short list of possible message types and their uses.
//		MS_INFO - this is information that an administrator would usually want to
//			see, such as when and if the zone server connects to the character, or
//			a console command's error message.
//		MS_NOTICE - this is a notice letting an administrator know what is going
//			on internally. This includes things like when a character server starts
//			reading a zone server's connection.
//		MS_WARNING - This includes all messages that could be potentially bad,
//			such as when an unidentified packet is received.
//		MS_ERROR - This is for use only when a message is worthy of stopping a
//			server, such as when a database cannot be queried, etc.
//		MS_DEBUG - Debug messages. Developers use at your own descretion.
//		MS_ALERT - Alerts are any other sort of information that needs to be shown
//			to an administrator. These sorts of messages cannot be logged and so
//			should not be used to convey any sort of vital information.

//	Changes -
//		September 19th, 2006 - RaX - Created.
//
//------------------------------------------------------------------------------
unit Terminal;

interface

uses
	CRT,
	ConsoleOptions,
	Commands;

type

//------------------------------------------------------------------------------
//TConsole                                                               CLASS
//------------------------------------------------------------------------------
	TConsole = class
	private
		LineColor : Boolean;

		Procedure WriteLogEntry(AString : String; From : String; MessageType : Byte);

	public
		Options					: TConsoleOptions;
		Commands				: TCommands;
		EnableCommands	: Boolean;
		procedure WriteLn(
			AString 		: String;
			TextColor 	: Byte = CRTWhite
		);

		procedure ReadLn(
			var AString 		: String
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
	SysUtils;

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
	EnableCommands  := TRUE;
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
//ReadLn()                                                 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Alias of WriteLn.
//
//	Changes -
//		February 19th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TConsole.ReadLn(
	var AString 		: String
	);
begin
	System.ReadLn(AString);
	if EnableCommands then
	begin
		self.Commands.Parse(AString);
	end else
	begin
		self.Message('Console Commands are disabled. Check above for errors.', 'Command Parser', MS_ALERT)
  end;
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

	MS_ALERT ://Alerts to internal events. NOT AN ERROR.
		begin
			ShowMessage := TRUE;
			FromColor		:= CRTLightCyan;
			TypeString	:= ' :ALERT  - ';
		end;
	end;

	if ShowMessage then
	begin

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

		//Alternating line colors
		if LineColor then
		begin
			Color := CRTGray;
		end else
		begin
			Color := CRTWhite;
		end;

		//write the body of the message
		CRT.TextColor(Color);
		System.Writeln(AString);
		CRT.TextColor(CRTWhite);//reset the color to default
		
		//Write our log entry if applicable
		if Options.LogsEnabled then
		begin
			WriteLogEntry(AString, From, MessageType);
		end;

		//Alternate our line colors
		if LineColor = TRUE then
		begin
			LineColor := FALSE;
		end else
		begin
			LineColor := TRUE;
		end;

		//Leave our critical section
		CriticalSection.Leave;
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
