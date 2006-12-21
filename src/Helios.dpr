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
//        Robert Ditthardt (RaX)
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
	Account in 'Classes\Account.pas',
	Character in 'Classes\Character.pas',
	CharaList in 'Classes\CharaList.pas',
  CharacterServer in 'Character\CharacterServer.pas',
	Commands in 'Classes\Commands.pas',
	Database in 'Classes\Database.pas',
	DatabaseTemplate in 'Classes\Database\DatabaseTemplate.pas',
	DatabaseConstants in 'Constants\DatabaseConstants.pas',
	DatabaseTXT in 'Database\DatabaseTXT.pas',
	GameConstants in 'Constants\GameConstants.pas',
	GameObjects in 'Database\GameObjects.pas',
	Globals in 'Common\Globals.pas',
  InterServer in 'Inter\InterServer.pas',
	janSQL in 'Common\3rdParty\JanSQL\janSQL.pas',
	janSQLExpression2 in 'Common\3rdParty\JanSQL\janSQLExpression2.pas',
	JanSQLDatabase in 'Classes\Database\JanSQLDatabase.pas',
	janSQLStrings in 'Common\3rdParty\JanSQL\janSQLStrings.pas',
	janSQLTokenizer in 'Common\3rdParty\JanSQL\janSQLTokenizer.pas',
	mwStringHashList in 'Common\3rdParty\JanSQL\mwStringHashList.pas',
	List32 in 'Common\3rdParty\List32.pas',
  LoginServer in 'Login\LoginServer.pas',
	Map in 'Classes\Map.pas',
	madExcept,
	madLinkDisAsm,
	MySQLDatabase in 'Classes\Database\MySQLDatabase.pas',
	PacketTypes in 'Common\PacketTypes.pas',
	PointList in 'Classes\PointList.pas',
	SaveThread in 'Classes\SaveThread.pas',
	ServerOptions in 'Config\ServerOptions.pas',
	Socket in 'Common\Socket.pas',
	TCPServerRoutines in 'Common\TCPServerRoutines.pas',
	uMysqlClient in 'Common\3rdParty\MySQL\uMysqlClient.pas',
	uMysqlCT in 'Common\3rdParty\MySQL\uMysqlCT.pas',
	uMysqlErrors in 'Common\3rdParty\MySQL\uMysqlErrors.pas',
	uMysqlNet in 'Common\3rdParty\MySQL\uMysqlNet.pas',
	uMysqlNewPassword in 'Common\3rdParty\MySQL\uMysqlNewPassword.pas',
	umysqlsha1 in 'Common\3rdParty\MySQL\umysqlsha1.pas',
	uMysqlVio in 'Common\3rdParty\MySQL\uMysqlVio.pas',
	Version in 'Common\Version.pas',
	WinLinux in 'Common\WinLinux.pas',
	XTimer in 'Classes\XTimer.pas',
  ZoneServer in 'Zone\ZoneServer.pas',
	ZoneRecv in 'Zone\ZoneRecv.pas',
	ZoneSend in 'Zone\ZoneSend.pas',
	{$ENDIF}

//------------------------------------------------------------------------------
//                            Linux Definitions
//------------------------------------------------------------------------------
	{$IFDEF LINUX}
	Account in 'Classes/Account.pas', 
	Character in 'Classes/Character.pas',
	CharaList in 'Classes/CharaList.pas',
  CharacterServer in 'Character/CharacterServer.pas',
	Commands in 'Classes/Commands.pas',
	Database in 'Classes/Database.pas',
	DatabaseTemplate in 'Classes/Database/DatabaseTemplate.pas',
	DatabaseConstants in 'Constants/DatabaseConstants.pas',
	DatabaseTXT in 'Database/DatabaseTXT.pas',
	GameConstants in 'Constants/GameConstants.pas',
	GameObjects in 'Database/GameObjects.pas',
	Globals in 'Common/Globals.pas',
  InterServer in 'Inter/InterServer.pas',
	janSQL in 'Common/3rdParty/JanSQL/janSQL.pas',
	janSQLExpression2 in 'Common/3rdParty/JanSQL/janSQLExpression2.pas',
  JanSQLDatabase in 'Classes/Database/JanSQLDatabase.pas',
	janSQLStrings in 'Common/3rdParty/JanSQL/janSQLStrings.pas',
	janSQLTokenizer in 'Common/3rdParty/JanSQL/janSQLTokenizer.pas',
	mwStringHashList in 'Common/3rdParty/JanSQL/mwStringHashList.pas',
	List32 in 'Common/3rdParty/List32.pas',
  LoginServer in 'Login/LoginServer.pas',
	Map in 'Classes/Map.pas',
	MySQLDatabase in 'Classes/Database/MySQLDatabase.pas',
	PacketTypes in 'Common/PacketTypes.pas',
	PointList in 'Classes/PointList.pas',
	SaveThread in 'Classes/SaveThread.pas',
	ServerOptions in 'Config/ServerOptions.pas',
	Socket in 'Common/Socket.pas',
	TCPServerRoutines in 'Common/TCPServerRoutines.pas',
	uMysqlClient in 'Common/3rdParty/MySQL/uMysqlClient.pas',
	uMysqlCT in 'Common/3rdParty/MySQL/uMysqlCT.pas',
	uMysqlErrors in 'Common/3rdParty/MySQL/uMysqlErrors.pas',
	uMysqlNet in 'Common/3rdParty/MySQL/uMysqlNet.pas',
	uMysqlNewPassword in 'Common/3rdParty/MySQL/uMysqlNewPassword.pas',
	umysqlsha1 in 'Common/3rdParty/MySQL/umysqlsha1.pas',
	uMysqlVio in 'Common/3rdParty/MySQL/uMysqlVio.pas',
	Version in 'Common/Version.pas',
	WinLinux in 'Common/WinLinux.pas',
	XTimer in 'Classes/XTimer.pas',
  ZoneServer in 'Zone/ZoneServer.pas',
	ZoneRecv in 'Zone/ZoneRecv.pas',
	ZoneSend in 'Zone/ZoneSend.pas',
	{$ENDIF}
//------------------------------------------------------------------------------
//                              Definitions for both.
//------------------------------------------------------------------------------
	SysUtils,
	Console in 'Console.pas';
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
//    -HeliosVersion is located in Globals (only one place to change)
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
	Command := TCommands.Create;
	MainProc := TMainProc.Create(nil); //Form replacement

	MainProc.Console('  _    _          _   _                ');
	MainProc.Console(' | |  | |        | | (_)               ');
	MainProc.Console(' | |__| |   ___  | |  _    ___    ___  ');
	MainProc.Console(' |  __  |  / _ \ | | | |  / _ \  / __| ');
	MainProc.Console(' | |  | | |  __/ | | | | | (_) | \__ \ ');
	MainProc.Console(' |_|  |_|  \___| |_| |_|  \___/  |___/ ');


	MainProc.Console('');
	MainProc.Console(Format('- %s is starting...',[HeliosVersion]));

	MainProc.Startup; //Form Create replacement

	{Begin Main Loop}
	{Must keep application alive!}
	while MainProc.Run do begin
		ReadLn(AnInput);
		Command.Parse(AnInput);
	end;
	{End Main Loop}

	TerminateApplication;
end{Helios}.
