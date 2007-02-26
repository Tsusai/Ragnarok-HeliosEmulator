//------------------------------------------------------------------------------
//Helios			                                                         Program
//------------------------------------------------------------------------------
//	What it does-
//			Helios is a cross-compatible(Windows & Linux), multi-threaded,
//    multi-database, Ragnarok Online Server Emulator.
//
//  License -
//------------------------------------------------------------------------------
//                 Project Helios - Copyright (c) 2005-2006
//
//    Contributors(A-Z) -
//        Matthew Mazanec (Tsusai - tsusai at gmail dot com)
//        Robert Ditthardt (RaX - onerax at gmail dot com)
//
//    All rights reserved.
//
//    Redistribution and use in source and binary forms, with or without
//    modification, are permitted provided that the following conditions are
//    met:
//
//    		* Redistributions of source code must retain the above copyright
//    			notice, this list of conditions and the following disclaimer.
//    		* Redistributions in binary form must reproduce the above copyright
//    			notice, this list of conditions and the following disclaimer in the
//    			documentation and/or other materials provided with the distribution.
//    		* Neither the name of Project Helios nor the names of its contributors
//    			may be used to endorse or promote products derived from this
//    			software without specific prior written permission.
//    		* Redistributions in any COMMERCIAL form, not limited to programs
//    			in other programming or human languages, derivatives, and anything
//    			your head can think of on how to make money off our code, is
//    			PROHIBITED WITHOUT EXPLICIT PERMISSION FROM THE AUTHORS.
//
//    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
//    TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//    PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
//    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
//    EXEMPLARY, OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO,
//    PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
//    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
//    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING
//    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
//    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//------------------------------------------------------------------------------
//
//	Changes -
//		September 20th, 2006 - RaX - Created Header.
//		November  11th, 2006 - Tsusai - Updated License.
//
//------------------------------------------------------------------------------
program Helios;

{$APPTYPE CONSOLE}
uses
{*These definitions make it possible to step through programs in windows and
compile for linux, at the same time*}
//------------------------------------------------------------------------------
//                            Windows Definitions
//------------------------------------------------------------------------------
	{$IFDEF MSWINDOWS}
	{$R *.res}

	//Console Related Units
	Terminal in 'Console\Terminal.pas',
	CRT in 'Console\CRT.pas',
	WinConsole in 'Console\WinCRT\WinConsole.pas',

	//Login Server
	LoginServer in 'Login\LoginServer.pas',

	//Character Server
	CharacterServer in 'Character\CharacterServer.pas',
	CharacterServerInfo in 'Character\CharacterServerInfo.pas',
	CharaLoginPackets in 'Character\CharaLoginPackets.pas',

	//InterServer
	InterServer in 'Inter\InterServer.pas',

	//Zone Server
	ZoneCharaPackets in 'Zone\ZoneCharaPackets.pas',
	ZoneServer in 'Zone\ZoneServer.pas',
	ZoneServerInfo in 'Zone\ZoneServerInfo.pas',
	ZoneRecv in 'Zone\ZoneRecv.pas',
	ZoneSend in 'Zone\ZoneSend.pas',

	//Constants
	GameConstants in 'Constants\GameConstants.pas',

	//Game Objects
	Account in 'Classes\Account.pas',
	Character in 'Classes\Character.pas',
	Being in 'Classes\Being.pas',
	Map in 'Classes\Map.pas',

	//Lists
	CharaList in 'Classes\CharaList.pas',
  MapList in 'Classes\MapList.pas',
	List32 in 'Common\3rdParty\List32.pas',
	PointList in 'Classes\PointList.pas',
	EventList in 'Classes\Events\EventList.pas',

  	//Events
	Event in 'Classes\Events\Event.pas',
	MovementEvent in 'Classes\Events\MovementEvent.pas',
	CharacterEventThread in 'Classes\Events\CharacterEventThread.pas',

	//Database
	Database in 'Classes\Database.pas',
	CommonDatabaseTemplate in 'Classes\Database\Common\CommonDatabaseTemplate.pas',
	GameDatabaseTemplate in 'Classes\Database\Game\GameDatabaseTemplate.pas',
	StaticDatabaseTemplate in 'Classes\Database\Static\StaticDatabaseTemplate.pas',
	DatabaseConstants in 'Constants\DatabaseConstants.pas',
	PacketDB in 'Database\PacketDB.pas',
	JanSQLCommonDatabase in 'Classes\Database\Common\JanSQLCommonDatabase.pas',
	MySQLCommonDatabase in 'Classes\Database\Common\MySQLCommonDatabase.pas',
	JanSQLGameDatabase in 'Classes\Database\Game\JanSQLGameDatabase.pas',
	MySQLGameDatabase in 'Classes\Database\Game\MySQLGameDatabase.pas',
	JanSQLStaticDatabase in 'Classes\Database\Static\JanSQLStaticDatabase.pas',
	MySQLStaticDatabase in 'Classes\Database\Static\MySQLStaticDatabase.pas',

	//Database Clients
		//jansql
	janSQL in 'Common\3rdParty\JanSQL\janSQL.pas',
	janSQLExpression2 in 'Common\3rdParty\JanSQL\janSQLExpression2.pas',
	janSQLStrings in 'Common\3rdParty\JanSQL\janSQLStrings.pas',
	janSQLTokenizer in 'Common\3rdParty\JanSQL\janSQLTokenizer.pas',
	mwStringHashList in 'Common\3rdParty\JanSQL\mwStringHashList.pas',
		//mysql
	uMysqlClient in 'Common\3rdParty\MySQL\uMysqlClient.pas',
	uMysqlCT in 'Common\3rdParty\MySQL\uMysqlCT.pas',
	uMysqlErrors in 'Common\3rdParty\MySQL\uMysqlErrors.pas',
	uMysqlNet in 'Common\3rdParty\MySQL\uMysqlNet.pas',
	uMysqlNewPassword in 'Common\3rdParty\MySQL\uMysqlNewPassword.pas',
	umysqlsha1 in 'Common\3rdParty\MySQL\umysqlsha1.pas',
	uMysqlVio in 'Common\3rdParty\MySQL\uMysqlVio.pas',

	//Configuration
	HeliosOptions in 'Config\HeliosOptions.pas',
	LoginOptions in 'Config\LoginOptions.pas',
	CharaOptions in 'Config\CharaOptions.pas',
	InterOptions in 'Config\InterOptions.pas',
	ZoneOptions in 'Config\ZoneOptions.pas',
	DatabaseOptions in 'Config\DatabaseOptions.pas',
	ConsoleOptions in 'Config\ConsoleOptions.pas',

  //Types
  MapTypes in 'Common\MapTypes.pas',

	//Other
	BufferIO in 'Common\BufferIO.pas',
	Commands in 'Classes\Commands.pas',
	CommClient in 'Classes\CommClient.pas',
	Globals in 'Common\Globals.pas',
	madExcept,
	madLinkDisAsm,
	PacketTypes in 'Common\PacketTypes.pas',
	ServerInfo in 'Classes\ServerInfo.pas',
	TCPServerRoutines in 'Common\TCPServerRoutines.pas',
	Version in 'Common\Version.pas',
	WinLinux in 'Common\WinLinux.pas',
	{$ENDIF}

//------------------------------------------------------------------------------
//                            Linux Definitions
//------------------------------------------------------------------------------
	{$IFDEF LINUX}

	//Console Related Units
	Terminal in 'Console/Terminal.pas',
	CRT in 'Console/CRT.pas',
	LinCRT in 'Console/LinCRT/LinCRT.pas',
	NCurses in 'Console/LinCRT/NCurses.pas',
	
	//Login Server
	LoginServer in 'Login/LoginServer.pas',

	//Character Server
	CharacterServer in 'Character/CharacterServer.pas',
	CharacterServerInfo in 'Character/CharacterServerInfo.pas',
	CharaLoginPackets in 'Character/CharaLoginPackets.pas',

	//InterServer
	InterServer in 'Inter/InterServer.pas',

	//Zone Server
  ZoneCharaPackets in 'Zone/ZoneCharaPackets.pas',
	ZoneServer in 'Zone/ZoneServer.pas',
	ZoneServerInfo in 'Zone/ZoneServerInfo.pas',
	ZoneRecv in 'Zone/ZoneRecv.pas',
	ZoneSend in 'Zone/ZoneSend.pas',

	//Constants
	GameConstants in 'Constants/GameConstants.pas',

	//Game Objects
	Account in 'Classes/Account.pas',
	Character in 'Classes/Character.pas',
	Being in 'Classes/Being.pas',
	Map in 'Classes/Map.pas',

	//Lists
	CharaList in 'Classes/CharaList.pas',
  MapList in 'Classes/MapList.pas',
	List32 in 'Common/3rdParty/List32.pas',
	PointList in 'Classes/PointList.pas',
	EventList in 'Classes/Events/EventList.pas',

	//Events
	Event in 'Classes/Events/Event.pas',
	MovementEvent in 'Classes/Events/MovementEvent.pas',
	CharacterEventThread in 'Classes/Events/CharacterEventThread.pas',

	//Database
	Database in 'Classes/Database.pas',
	CommonDatabaseTemplate in 'Classes/Database/Common/CommonDatabaseTemplate.pas',
	GameDatabaseTemplate in 'Classes/Database/Game/GameDatabaseTemplate.pas',
	StaticDatabaseTemplate in 'Classes/Database/Static/StaticDatabaseTemplate.pas',
	DatabaseConstants in 'Constants/DatabaseConstants.pas',
	PacketDB in 'Database/PacketDB.pas',
	JanSQLCommonDatabase in 'Classes/Database/Common/JanSQLCommonDatabase.pas',
	MySQLCommonDatabase in 'Classes/Database/Common/MySQLCommonDatabase.pas',
	JanSQLGameDatabase in 'Classes/Database/Game/JanSQLGameDatabase.pas',
	MySQLGameDatabase in 'Classes/Database/Game/MySQLGameDatabase.pas',
	JanSQLStaticDatabase in 'Classes/Database/Static/JanSQLStaticDatabase.pas',
	MySQLStaticDatabase in 'Classes/Database/Static/MySQLStaticDatabase.pas',

	//Database Clients
		//jansql
	janSQL in 'Common/3rdParty/JanSQL/janSQL.pas',
	janSQLExpression2 in 'Common/3rdParty/JanSQL/janSQLExpression2.pas',
	janSQLStrings in 'Common/3rdParty/JanSQL/janSQLStrings.pas',
	janSQLTokenizer in 'Common/3rdParty/JanSQL/janSQLTokenizer.pas',
	mwStringHashList in 'Common/3rdParty/JanSQL/mwStringHashList.pas',
		//mysql
	uMysqlClient in 'Common/3rdParty/MySQL/uMysqlClient.pas',
	uMysqlCT in 'Common/3rdParty/MySQL/uMysqlCT.pas',
	uMysqlErrors in 'Common/3rdParty/MySQL/uMysqlErrors.pas',
	uMysqlNet in 'Common/3rdParty/MySQL/uMysqlNet.pas',
	uMysqlNewPassword in 'Common/3rdParty/MySQL/uMysqlNewPassword.pas',
	umysqlsha1 in 'Common/3rdParty/MySQL/umysqlsha1.pas',
	uMysqlVio in 'Common/3rdParty/MySQL/uMysqlVio.pas',

	//Configuration
	HeliosOptions in 'Config/HeliosOptions.pas',
	LoginOptions in 'Config/LoginOptions.pas',
	CharaOptions in 'Config/CharaOptions.pas',
	InterOptions in 'Config/InterOptions.pas',
	ZoneOptions in 'Config/ZoneOptions.pas',
	DatabaseOptions in 'Config/DatabaseOptions.pas',
	ConsoleOptions in 'Config/ConsoleOptions.pas',

  //Types
  MapTypes in 'Common/MapTypes.pas',

	//Other
	BufferIO in 'Common/BufferIO.pas',
	Commands in 'Classes/Commands.pas',
	CommClient in 'Classes/CommClient.pas',
	Globals in 'Common/Globals.pas',
	PacketTypes in 'Common/PacketTypes.pas',
	ServerInfo in 'Classes/ServerInfo.pas',
	TCPServerRoutines in 'Common/TCPServerRoutines.pas',
	Version in 'Common/Version.pas',
	WinLinux in 'Common/WinLinux.pas',
	{$ENDIF}
//------------------------------------------------------------------------------
//                              Definitions for both.
//------------------------------------------------------------------------------
	//Main Unit
	Main in 'Main.pas',

	SysUtils;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//                            Helios Main Routine
//  What it does -
//        This main routine keeps the console window open until a server
//    administrator sends it a command to shutdown.
//
//  Notes -
//    -To keep Helios in a Form-like organization, we've created TMainProc,
//      which replaces the Main Form. This also gives us the option to make
//      Helios log to a file instead of the screen by keeping all of our visual
//      related routines in one place =).
//    -HeliosVersion is located in Version (only one place to change)
//    -This routine contains the loop that keeps the application open. On
//      termination of that loop, the program will shut down.

//  Changes -
//    September 21st - RaX - Created Header.
//------------------------------------------------------------------------------
var
	AnInput   : string;
begin
	//Tsusai 7/8/06 : Randomize added.  Learned from Prometheus.
	Randomize;
	SetupCRT;
	SetupTerminationCapturing;

	//setup our paths before anything else is done.
	AppPath		:= ExtractFilePath(ParamStr(0));
	ExeName		:= ExtractFileNameMod(ParamStr(0));

	//Create our main process.
	MainProc := TMainProc.Create(nil); //Form replacement

	//Setup console interface and command parser.
	Console := TConsole.Create;

	//Show our header ONCE
	MainProc.DisplayHeader;

	//Initialize our Main Process
	MainProc.Startup; //Form Create replacement

	{Begin Main Loop}
	{Must keep application alive!}
	while MainProc.Run do
	begin
		Console.ReadLn(AnInput);
	end;
	{End Main Loop}
	
	Console.Free;
	TerminateApplication;
end{Helios}.
