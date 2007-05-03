//------------------------------------------------------------------------------
//Helios			                                                         Program
//------------------------------------------------------------------------------
//	What it does-
//			Helios is a cross-compatible(Windows & Linux), multi-threaded,
//    multi-database, Ragnarok Online Server Emulator.
//
//  License -
//------------------------------------------------------------------------------
//                 Project Helios - Copyright (c) 2005-2007
//
//    Contributors(A-Z) -
//        Matthew Mazanec (Tsusai - tsusai at gmail dot com)
//        Robert Ditthardt (RaX - onerax at gmail dot com)
//        Lin Ling (Aeomin - aeomin [AT] gmail [DOT] com)
//        Christopher Wilding (ChrstphrR (CR) - chrstphrr at tubbybears dot net)
//
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
//    		* All Third Party licensing and rules still apply and are not and
//    			cannot be superceded by this license.
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
//		[2007/03/24] CR - Partially updated Contributors, minor indent changes.
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
	Terminal in 'Common\Console\Terminal.pas',
	CRT in 'Common\Console\CRT.pas',
	WinConsole in 'Common\Console\WinCRT\WinConsole.pas',

	//Login Server
	LoginServer in 'Servers\Login\LoginServer.pas',
	LoginAccountInfo in 'Servers\Login\LoginAccountInfo.pas',

	//Character Server
	CharacterServer in 'Servers\Character\CharacterServer.pas',
	CharacterServerInfo in 'Servers\Character\CharacterServerInfo.pas',
	CharaLoginCommunication in 'Servers\Character\CharaLoginCommunication.pas',
	CharAccountInfo in 'Servers\Character\CharAccountInfo.pas',

	//InterServer
	InterServer in 'Servers\Inter\InterServer.pas',

	//Zone Server
	ZoneCharaCommunication in 'Servers\Zone\ZoneCharaCommunication.pas',
	ZoneInterCommunication in 'Servers\Zone\ZoneInterCommunication.pas',
	ZoneServer in 'Servers\Zone\ZoneServer.pas',
	ZoneServerInfo in 'Servers\Zone\ZoneServerInfo.pas',
	ZoneRecv in 'Servers\Zone\ZoneRecv.pas',
	ZoneSend in 'Servers\Zone\ZoneSend.pas',
	GMCommands in 'Servers\Zone\GMCommands.pas',
	PacketDB in 'Servers\Zone\PacketDB.pas',

	//Constants
	GameConstants in 'Constants\GameConstants.pas',
	DatabaseConstants in 'Constants\DatabaseConstants.pas',
	NetworkConstants in 'Constants\NetworkConstants.pas',
	
	//Game Objects
	Account in 'Common\Classes\Account.pas',
	Character in 'Common\Classes\Beings\Character.pas',
	Being in 'Common\Classes\Beings\Being.pas',
	Map in 'Common\Classes\Map.pas',
	NPC in 'Common\Classes\Beings\NPC.pas',

	//Lua
	LuaPas in 'Common\ThirdParty\LuaPas.pas',
	LuaCoreRoutines in 'Servers\Zone\Lua\LuaCoreRoutines.pas',
	LuaNPCCore in 'Servers\Zone\Lua\NPC\LuaNPCCore.pas',
	LuaNPCCommands in 'Servers\Zone\Lua\NPC\LuaNPCCommands.pas',

	//Lists
	CharaList in 'Common\Classes\Lists\CharaList.pas',
	MapList in 'Common\Classes\Lists\MapList.pas',
	List32 in 'Common\ThirdParty\List32.pas',
	PointList in 'Common\Classes\Lists\PointList.pas',
	EventList in 'Common\Classes\Events\EventList.pas',

	//Events
	Event in 'Common\Classes\Events\Event.pas',
	DelayDisconnectEvent in 'Common\Classes\Events\DelayDisconnectEvent.pas',
	MovementEvent in 'Common\Classes\Events\MovementEvent.pas',
	CharacterEventThread in 'Common\Classes\Events\CharacterEventThread.pas',

	//Database
	Database in 'Common\Classes\Database\Database.pas',
	CommonDatabaseTemplate in 'Common\Classes\Database\Common\CommonDatabaseTemplate.pas',
	GameDatabaseTemplate in 'Common\Classes\Database\Game\GameDatabaseTemplate.pas',
	StaticDatabaseTemplate in 'Common\Classes\Database\Static\StaticDatabaseTemplate.pas',
	JanSQLCommonDatabase in 'Common\Classes\Database\Common\JanSQLCommonDatabase.pas',
	MySQLCommonDatabase in 'Common\Classes\Database\Common\MySQLCommonDatabase.pas',
	JanSQLGameDatabase in 'Common\Classes\Database\Game\JanSQLGameDatabase.pas',
	MySQLGameDatabase in 'Common\Classes\Database\Game\MySQLGameDatabase.pas',
	JanSQLStaticDatabase in 'Common\Classes\Database\Static\JanSQLStaticDatabase.pas',
	MySQLStaticDatabase in 'Common\Classes\Database\Static\MySQLStaticDatabase.pas',

	//Database Clients
	//jansql
	janSQL in 'Common\ThirdParty\JanSQL\janSQL.pas',
	janSQLExpression2 in 'Common\ThirdParty\JanSQL\janSQLExpression2.pas',
	janSQLStrings in 'Common\ThirdParty\JanSQL\janSQLStrings.pas',
	janSQLTokenizer in 'Common\ThirdParty\JanSQL\janSQLTokenizer.pas',
	mwStringHashList in 'Common\ThirdParty\JanSQL\mwStringHashList.pas',
	//mysql
	uMysqlClient in 'Common\ThirdParty\MySQL\uMysqlClient.pas',
	uMysqlCT in 'Common\ThirdParty\MySQL\uMysqlCT.pas',
	uMysqlErrors in 'Common\ThirdParty\MySQL\uMysqlErrors.pas',
	uMysqlNet in 'Common\ThirdParty\MySQL\uMysqlNet.pas',
	uMysqlNewPassword in 'Common\ThirdParty\MySQL\uMysqlNewPassword.pas',
	umysqlsha1 in 'Common\ThirdParty\MySQL\umysqlsha1.pas',
	uMysqlVio in 'Common\ThirdParty\MySQL\uMysqlVio.pas',
	//mssql
	MSSQLClient in 'Common\ThirdParty\MSSQL\MSSQLClient.pas',

	//Configuration
	HeliosOptions in 'Common\Config\HeliosOptions.pas',
	LoginOptions in 'Common\Config\LoginOptions.pas',
	CharaOptions in 'Common\Config\CharaOptions.pas',
	InterOptions in 'Common\Config\InterOptions.pas',
	ZoneOptions in 'Common\Config\ZoneOptions.pas',
	DatabaseOptions in 'Common\Config\DatabaseOptions.pas',
	ConsoleOptions in 'Common\Config\ConsoleOptions.pas',

	//Types
	MapTypes in 'Common\MapTypes.pas',

	//Other
	AreaLoopEvents in 'Common\AreaLoopEvents.pas',
	BufferIO in 'Common\BufferIO.pas',
	Commands in 'Common\Classes\Commands.pas',
	CommClient in 'Common\Classes\CommClient.pas',
	Globals in 'Common\Globals.pas',
	madExcept,
	madLinkDisAsm,
	PacketTypes in 'Common\PacketTypes.pas',
	ServerInfo in 'Common\Classes\ServerInfo.pas',
	TCPServerRoutines in 'Common\TCPServerRoutines.pas',
	Version in 'Common\Version.pas',
	WinLinux in 'Common\WinLinux.pas',
	{$ENDIF}

//------------------------------------------------------------------------------
//                            Linux Definitions
//------------------------------------------------------------------------------
	{$IFDEF LINUX}

	//Console Related Units
	Terminal in 'Common/Console/Terminal.pas',
	CRT in 'Common/Console/CRT.pas',
	LinCRT in 'Common/Console/LinCRT/LinCRT.pas',
	NCurses in 'Common/Console/LinCRT/NCurses.pas',

	//Login Server
	LoginServer in 'Servers/Login/LoginServer.pas',
	LoginAccountInfo in 'Servers/Login/LoginAccountInfo.pas',

	//Character Server
	CharacterServer in 'Servers/Character/CharacterServer.pas',
	CharacterServerInfo in 'Servers/Character/CharacterServerInfo.pas',
	CharaLoginCommunication in 'Servers/Character/CharaLoginCommunication.pas',
	CharAccountInfo in 'Servers/Character/CharAccountInfo.pas',

	//InterServer
	InterServer in 'Servers/Inter/InterServer.pas',

	//Zone Server
	ZoneCharaCommunication in 'Servers/Zone/ZoneCharaCommunication.pas',
	ZoneInterCommunication in 'Servers/Zone/ZoneInterCommunication.pas',
	ZoneServer in 'Servers/Zone/ZoneServer.pas',
	ZoneServerInfo in 'Servers/Zone/ZoneServerInfo.pas',
	ZoneRecv in 'Servers/Zone/ZoneRecv.pas',
	ZoneSend in 'Servers/Zone/ZoneSend.pas',
	GMCommands in 'Servers/Zone/GMCommands.pas',
	PacketDB in 'Servers/Zone/PacketDB.pas',

	//Constants
	GameConstants in 'Constants/GameConstants.pas',
	DatabaseConstants in 'Constants/DatabaseConstants.pas',
	NetworkConstants in 'Constants/NetworkConstants.pas',
	
	//Game Objects
	Account in 'Common/Classes/Account.pas',
	Character in 'Common/Classes/Beings/Character.pas',
	Being in 'Common/Classes/Beings/Being.pas',
	Map in 'Common/Classes/Map.pas',
	NPC in 'Common/Classes/Beings/NPC.pas',

	//Lua
	LuaPas in 'Common/ThirdParty/LuaPas.pas',
	LuaCoreRoutines in 'Servers/Zone/Lua/LuaCoreRoutines.pas',
	LuaNPCCore in 'Servers/Zone/Lua/NPC/LuaNPCCore.pas',
	LuaNPCCommands in 'Servers/Zone/Lua/NPC/LuaNPCCommands.pas',

	//Lists
	CharaList in 'Common/Classes/Lists/CharaList.pas',
	MapList in 'Common/Classes/Lists/MapList.pas',
	List32 in 'Common/ThirdParty/List32.pas',
	PointList in 'Common/Classes/Lists/PointList.pas',
	EventList in 'Common/Classes/Events/EventList.pas',

	//Events
	Event in 'Common/Classes/Events/Event.pas',
	DelayDisconnectEvent in 'Common/Classes/Events/DelayDisconnectEvent.pas',
	MovementEvent in 'Common/Classes/Events/MovementEvent.pas',
	CharacterEventThread in 'Common/Classes/Events/CharacterEventThread.pas',

	//Database
	Database in 'Common/Classes/Database/Database.pas',
	CommonDatabaseTemplate in 'Common/Classes/Database/Common/CommonDatabaseTemplate.pas',
	GameDatabaseTemplate in 'Common/Classes/Database/Game/GameDatabaseTemplate.pas',
	StaticDatabaseTemplate in 'Common/Classes/Database/Static/StaticDatabaseTemplate.pas',
	JanSQLCommonDatabase in 'Common/Classes/Database/Common/JanSQLCommonDatabase.pas',
	MySQLCommonDatabase in 'Common/Classes/Database/Common/MySQLCommonDatabase.pas',
	JanSQLGameDatabase in 'Common/Classes/Database/Game/JanSQLGameDatabase.pas',
	MySQLGameDatabase in 'Common/Classes/Database/Game/MySQLGameDatabase.pas',
	JanSQLStaticDatabase in 'Common/Classes/Database/Static/JanSQLStaticDatabase.pas',
	MySQLStaticDatabase in 'Common/Classes/Database/Static/MySQLStaticDatabase.pas',

	//Database Clients
	//jansql
	janSQL in 'Common/ThirdParty/JanSQL/janSQL.pas',
	janSQLExpression2 in 'Common/ThirdParty/JanSQL/janSQLExpression2.pas',
	janSQLStrings in 'Common/ThirdParty/JanSQL/janSQLStrings.pas',
	janSQLTokenizer in 'Common/ThirdParty/JanSQL/janSQLTokenizer.pas',
	mwStringHashList in 'Common/ThirdParty/JanSQL/mwStringHashList.pas',
	//mysql
	uMysqlClient in 'Common/ThirdParty/MySQL/uMysqlClient.pas',
	uMysqlCT in 'Common/ThirdParty/MySQL/uMysqlCT.pas',
	uMysqlErrors in 'Common/ThirdParty/MySQL/uMysqlErrors.pas',
	uMysqlNet in 'Common/ThirdParty/MySQL/uMysqlNet.pas',
	uMysqlNewPassword in 'Common/ThirdParty/MySQL/uMysqlNewPassword.pas',
	umysqlsha1 in 'Common/ThirdParty/MySQL/umysqlsha1.pas',
	uMysqlVio in 'Common/ThirdParty/MySQL/uMysqlVio.pas',
	//mssql
	MSSQLClient in 'Common/ThirdParty/MSSQL/MSSQLClient.pas',

	//Configuration
	HeliosOptions in 'Common/Config/HeliosOptions.pas',
	LoginOptions in 'Common/Config/LoginOptions.pas',
	CharaOptions in 'Common/Config/CharaOptions.pas',
	InterOptions in 'Common/Config/InterOptions.pas',
	ZoneOptions in 'Common/Config/ZoneOptions.pas',
	DatabaseOptions in 'Common/Config/DatabaseOptions.pas',
	ConsoleOptions in 'Common/Config/ConsoleOptions.pas',

	//Types
	MapTypes in 'Common/MapTypes.pas',

	//Other
	AreaLoopEvents in 'Common/AreaLoopEvents.pas',
	BufferIO in 'Common/BufferIO.pas',
	Commands in 'Common/Classes/Commands.pas',
	CommClient in 'Common/Classes/CommClient.pas',
	Globals in 'Common/Globals.pas',
	PacketTypes in 'Common/PacketTypes.pas',
	ServerInfo in 'Common/Classes/ServerInfo.pas',
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
//    September 21st, 2006 - RaX - Created Header.
//------------------------------------------------------------------------------
var
	AnInput   : string;
begin
	//Tsusai 7/8/06 : Randomize added.  Learned from Prometheus.
	Randomize;
	//Setup our CRT controller
	SetupCRT;
	//Allow Helios to capture termination messages
	SetupTerminationCapturing;

	//setup our paths before anything else is done.
	AppPath		:= ExtractFilePath(ParamStr(0));
	ExeName		:= ExtractFileNameMod(ParamStr(0));

	//Create our main process.
	MainProc := TMainProc.Create(nil); //Form replacement

	//Setup console interface and command parser.
	//This has to be set up here because It relies on the options in MainProc.
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

	//Terminate the process cleanly.
	TerminateApplication;
end{Helios}.
