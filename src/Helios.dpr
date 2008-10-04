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
{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}

{$APPTYPE CONSOLE}
uses
  madLinkDisAsm,
{*These definitions make it possible to step through programs in windows and
compile for linux, at the same time*}
//------------------------------------------------------------------------------
//                            Windows Definitions
//------------------------------------------------------------------------------
	{$IFDEF MSWINDOWS}
	{$R *.res}

	//Link special resource file for x64
	{$IFDEF WIN64}
		{$R Helios64.res}
	{$ENDIF}

	//Console Related Units
	CRTWrapper in 'Common\Console\CRTWrapper.pas',
	WinConsole in 'Common\Console\WinCRT\WinConsole.pas',
	Terminal in 'Common\Console\Terminal.pas',

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
	InterSend in  'Servers\Inter\InterSend.pas',
	InterRecv in  'Servers\Inter\InterRecv.pas',

	//Zone Server
	ZoneCharaCommunication in 'Servers\Zone\ZoneCharaCommunication.pas',
	ZoneInterCommunication in 'Servers\Zone\ZoneInterCommunication.pas',
	ZoneServer in 'Servers\Zone\ZoneServer.pas',
	ZoneServerInfo in 'Servers\Zone\ZoneServerInfo.pas',
	ZoneRecv in 'Servers\Zone\ZoneRecv.pas',
	ZoneSend in 'Servers\Zone\ZoneSend.pas',
	GMCommands in 'Servers\Zone\GMCommands.pas',
	GMCommandExe in 'Servers\Zone\GMCommandExe.pas',

	//Constants
	GameConstants in 'Constants\GameConstants.pas',
	DatabaseConstants in 'Constants\DatabaseConstants.pas',
	NetworkConstants in 'Constants\NetworkConstants.pas',
	LuaVarConstants in 'Constants\LuaVarConstants.pas',
	ErrorConstants in 'Constants\ErrorConstants.pas',

	//Game Objects
	Account in 'Common\Classes\Account.pas',
	Character in 'Common\Classes\Beings\Character.pas',
	Being in 'Common\Classes\Beings\Being.pas',
	Map in 'Common\Classes\Map.pas',
	NPC in 'Common\Classes\Beings\NPC.pas',
	Item in 'Common\Classes\Items\Item.pas',
	EquipmentItem in 'Common\Classes\Items\EquipmentItem.pas',
	UseableItem in 'Common\Classes\Items\UseableItem.pas',
	MiscItem in 'Common\Classes\Items\MiscItem.pas',
	Inventory in 'Common\Classes\Inventory.pas',
	ItemInstance in 'Common\Classes\Items\ItemInstance.pas',
	Mob in 'Common\Classes\Beings\Mob.pas',
	Mailbox in 'Common\Classes\Mailbox.pas',
	Packets in 'Common\Classes\Packets.pas',
	GameObject in 'Common\Classes\GameObject.pas',

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
	InventoryList in 'Common\Classes\Lists\InventoryList.pas',
	ParameterList in 'Common\Classes\Lists\ParameterList.pas',

	//Events
	Event in 'Common\Classes\Events\Event.pas',
	DelayDisconnectEvent in 'Common\Classes\Events\DelayDisconnectEvent.pas',
	MovementEvent in 'Common\Classes\Events\MovementEvent.pas',
	CharacterEventThread in 'Common\Classes\Events\CharacterEventThread.pas',
	OnTouchCellEvent in 'Common\Classes\Events\OnTouchCellEvent.pas',
	AddFriendEvent in 'Common\Classes\Events\AddFriendEvent.pas',
	AttackEvent in 'Common\Classes\Events\AttackEvent.pas',

	//Database
	Database in 'Common\Classes\Database\Database.pas',
	AccountQueries in 'Common\Classes\Database\AccountQueries.pas',
	CharacterQueries in 'Common\Classes\Database\CharacterQueries.pas',
	CharacterConstantQueries in 'Common\Classes\Database\CharacterConstantQueries.pas',
	FriendQueries in 'Common\Classes\Database\FriendQueries.pas',
	MapQueries in 'Common\Classes\Database\MapQueries.pas',
	ItemQueries in 'Common\Classes\Database\ItemQueries.pas',
	MobQueries in 'Common\Classes\Database\MobQueries.pas',
	QueryBase in 'Common\Classes\Database\QueryBase.pas',
	MailQueries in 'Common\Classes\Database\MailQueries.pas',

	//Configuration
	HeliosOptions in 'Common\Config\HeliosOptions.pas',
	LoginOptions in 'Common\Config\LoginOptions.pas',
	CharaOptions in 'Common\Config\CharaOptions.pas',
	InterOptions in 'Common\Config\InterOptions.pas',
	ZoneOptions in 'Common\Config\ZoneOptions.pas',
	DatabaseOptions in 'Common\Config\DatabaseOptions.pas',
	ConsoleOptions in 'Common\Config\ConsoleOptions.pas',
	GMCommandsOptions in 'Common\Config\GMCommandsOptions.pas',

	//Types
	MapTypes in 'Common\MapTypes.pas',
	ItemTypes in 'Common\ItemTypes.pas',
	GameTypes in 'Common\GameTypes.pas',

	//Other
	AreaLoopEvents in 'Common\AreaLoopEvents.pas',
	BufferIO in 'Common\BufferIO.pas',
	Commands in 'Common\Classes\Commands.pas',
	CommClient in 'Common\Classes\CommClient.pas',
	Globals in 'Common\Globals.pas',
	{$IFNDEF FPC} //If we aren't using Lazarus to compile, use madshi components
		madExcept,
	{$ENDIF}
	PacketTypes in 'Common\PacketTypes.pas',
	Server in 'Servers\Server.pas',
	ServerInfo in 'Common\Classes\ServerInfo.pas',
	TCPServerRoutines in 'Common\TCPServerRoutines.pas',
	Version in 'Common\Version.pas',
	WinLinux in 'Common\WinLinux.pas',
	{$ENDIF}

//------------------------------------------------------------------------------
//                            Linux Definitions
//------------------------------------------------------------------------------
	{$IFDEF LINUX}
	{$IFDEF FPC}
	//Special Threads Unit.
	cthreads,
	{$ENDIF}
	//Console Related Units
	Terminal in 'Common/Console/Terminal.pas',
	CRTWrapper in 'Common/Console/CRTWrapper.pas',
	//LinCRT in 'Common/Console/LinCRT/LinCRT.pas',
	//NCurses in 'Common/Console/LinCRT/NCurses.pas',

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
	InterSend in  'Servers/Inter/InterSend.pas',
	InterRecv in  'Servers/Inter/InterRecv.pas',

	//Zone Server
	ZoneCharaCommunication in 'Servers/Zone/ZoneCharaCommunication.pas',
	ZoneInterCommunication in 'Servers/Zone/ZoneInterCommunication.pas',
	ZoneServer in 'Servers/Zone/ZoneServer.pas',
	ZoneServerInfo in 'Servers/Zone/ZoneServerInfo.pas',
	ZoneRecv in 'Servers/Zone/ZoneRecv.pas',
	ZoneSend in 'Servers/Zone/ZoneSend.pas',
	GMCommands in 'Servers/Zone/GMCommands.pas',
	GMCommandExe in 'Servers/Zone/GMCommandExe.pas',

	//Constants
	GameConstants in 'Constants/GameConstants.pas',
	DatabaseConstants in 'Constants/DatabaseConstants.pas',
	NetworkConstants in 'Constants/NetworkConstants.pas',
	LuaVarConstants in 'Constants/LuaVarConstants.pas',
	ErrorConstants in 'Constants/ErrorConstants.pas',

	//Game Objects
	Account in 'Common/Classes/Account.pas',
	Character in 'Common/Classes/Beings/Character.pas',
	Being in 'Common/Classes/Beings/Being.pas',
	Map in 'Common/Classes/Map.pas',
	NPC in 'Common/Classes/Beings/NPC.pas',
	Item in 'Common/Classes/Items/Item.pas',
	EquipmentItem in 'Common/Classes/Items/EquipmentItem.pas',
	UseableItem in 'Common/Classes/Items/UseableItem.pas',
	MiscItem in 'Common/Classes/Items/MiscItem.pas',
	Inventory in 'Common/Classes/Inventory.pas',
	ItemInstance in 'Common/Classes/Items/ItemInstance.pas',
	Mob in 'Common/Classes/Beings/Mob.pas',
	Mailbox in 'Common/Classes/Mailbox.pas',
	Packets in 'Common/Classes/Packets.pas',
	GameObject in 'Common/Classes/GameObject.pas',

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
	InventoryList in 'Common/Classes/Lists/InventoryList.pas',
	ParameterList in 'Common/Classes/Lists/ParameterList.pas',

	//Events
	Event in 'Common/Classes/Events/Event.pas',
	DelayDisconnectEvent in 'Common/Classes/Events/DelayDisconnectEvent.pas',
	MovementEvent in 'Common/Classes/Events/MovementEvent.pas',
	CharacterEventThread in 'Common/Classes/Events/CharacterEventThread.pas',
	OnTouchCellEvent in 'Common/Classes/Events/OnTouchCellEvent.pas',
	AddFriendEvent in 'Common/Classes/Events/AddFriendEvent.pas',
	AttackEvent in 'Common/Classes/Events/AttackEvent.pas',

	//Database
	Database in 'Common/Classes/Database/Database.pas',
	AccountQueries in 'Common/Classes/Database/AccountQueries.pas',
	CharacterQueries in 'Common/Classes/Database/CharacterQueries.pas',
	CharacterConstantQueries in 'Common/Classes/Database/CharacterConstantQueries.pas',
	FriendQueries in 'Common/Classes/Database/FriendQueries.pas',
	MapQueries in 'Common/Classes/Database/MapQueries.pas',
	ItemQueries in 'Common/Classes/Database/ItemQueries.pas',
	MobQueries in 'Common/Classes/Database/MobQueries.pas',
	QueryBase in 'Common/Classes/Database/QueryBase.pas',
	MailQueries in 'Common/Classes/Database/MailQueries.pas',

	//Configuration
	HeliosOptions in 'Common/Config/HeliosOptions.pas',
	LoginOptions in 'Common/Config/LoginOptions.pas',
	CharaOptions in 'Common/Config/CharaOptions.pas',
	InterOptions in 'Common/Config/InterOptions.pas',
	ZoneOptions in 'Common/Config/ZoneOptions.pas',
	DatabaseOptions in 'Common/Config/DatabaseOptions.pas',
	ConsoleOptions in 'Common/Config/ConsoleOptions.pas',
	GMCommandsOptions in 'Common/Config/GMCommandsOptions.pas',

	//Types
	MapTypes in 'Common/MapTypes.pas',
	ItemTypes in 'Common/ItemTypes.pas',
	GameTypes in 'Common/GameTypes.pas',

	//Other
	AreaLoopEvents in 'Common/AreaLoopEvents.pas',
	BufferIO in 'Common/BufferIO.pas',
	Commands in 'Common/Classes/Commands.pas',
	CommClient in 'Common/Classes/CommClient.pas',
	Globals in 'Common/Globals.pas',
	PacketTypes in 'Common/PacketTypes.pas',
	Server in 'Servers/Server.pas',
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
	try
		//Tsusai 7/8/06 : Randomize added.  Learned from Prometheus.
		Randomize;

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
	finally
		//Terminate the process cleanly.
		TerminateApplication;
	end;
end{Helios}.
