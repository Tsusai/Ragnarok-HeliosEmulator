//This unit controls loading the packet_db.txt into memory, and setting up
//procedures for executing packets.
unit Packets;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface
uses
	Character,
	Classes,
	Contnrs,
	PacketTypes;



(*------------------------------------------------------------------------------
Its a bit complex, but here is a rundown. (June 28th, 2008 - Tsusai)
You have a TPacketDB class.  This contains multiple methods of accessing
packet data.  The packet data breaks down into the following:

TPacketDB.fCodebase[0->? Array].EAPACKETVER -
TPacketDB.fCodebase[0->? Array].PacketList - List of TPackets.  Stores individual packets data
	as objects.  Owns its list of objects.
TPacketDB.GetLength - Returns length of a packet based on ID and fCodeBase index
TPacketDB.GetPacketInfo - Same as above, but returns a TPacket object.
TPacketDB.GetMapConnectPacket - Finds the proper mapconnect packet
TPacketDB.GetEAPacketVer - Takes the fCodeBase index, and pulls out the EAPacketVer
------------------------------------------------------------------------------*)
type

	//The more usual of the zone procedures
	TPacketProc = procedure (var AChara : TCharacter; const RecvBuffer : TBuffer; const ReadPts : TReadPts);
	//The Tchara, Avoidself packets.  About 3 exist
	TAvoidSelfPacketProc = procedure (var AChara : TCharacter; const AvoidSelf : boolean = False);


	TPackets = class
		ID : word; //PacketID

		//Length, trying not to conflict with Length() command.
		//[2006/03/12] Tsusai - MUST BE INTEGER TYPE! I messed it up using word.
		PLength : integer;

		Command								: string;
		ReadPoints						: TReadPts;
		ExecCommand						: TPacketProc; //Regular procedure link
		ExecAvoidSelfCommand	: TAvoidSelfPacketProc; //avoidself special procedure link
	end;

	PPackets = ^TPackets;

	//Packet List to replace PacketArray.
	//Needs some destruction stuffs.
	TPacketList = class(TObjectList)
	public
		procedure Assign(Source : TPacketList);
		procedure Add(PacketObject : TPackets);
		constructor Create;
	end;

	TCodebaseData = record
		EAPACKETVER : word;
		Packets : TPacketList;
	end;

	TPacketDB = class
		private
			fCodeBase : array of TCodeBaseData;
		public
			function GetLength(
				ID : Word;
				Version : Word = 0
			) : LongWord;

			procedure GetPacketInfo(
				const Packet  : word;
				const Version : Word;
				var ReturnPackets : TPackets
			);

			procedure GetMapConnectPacket(
				const Packet : word;
				const PLength : word;
				var ReturnVersion : word;
				var ReturnPacketInfo : TPackets
			);

			function GetEAPacketVer(InternalPacketDBVer : word) : word;
			function Load : boolean;
			procedure Unload;

	end;


implementation
uses
	SysUtils,
	Main,
	Globals,
	ZoneRecv;


(**)
(**)
(*TPACKETLIST METHODS!!!!!*)
(**)
(**)


//------------------------------------------------------------------------------
//TPacketList.Assign                                                   PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Copies contents of one PacketList to another.
//
//	Changes -
//		June 28th, 2008 - Tsusai - Created.
//
//------------------------------------------------------------------------------
procedure TPacketList.Assign(Source : TPacketList);
var
	idx : integer;
	PacketObject : TPackets;
begin
	if not Assigned(Self) then Self := TPacketList.Create;
	Self.Clear;
	for idx := 0 to Source.Count - 1 do
	begin
		PacketObject := TPackets.Create;
		PacketObject.ID := TPackets(Source.Items[idx]).ID;
		PacketObject.PLength := TPackets(Source.Items[idx]).PLength;
		PacketObject.Command := TPackets(Source.Items[idx]).Command;
		SetLength(PacketObject.ReadPoints,Length(TPackets(Source.Items[idx]).ReadPoints));
		PacketObject.ReadPoints := Copy(TPackets(Source.Items[idx]).ReadPoints,0,Length(PacketObject.ReadPoints));
		PacketObject.ExecCommand := TPackets(Source.Items[idx]).ExecCommand;
		PacketObject.ExecAvoidSelfCommand := TPackets(Source.Items[idx]).ExecAvoidSelfCommand;
		Add(PacketObject);
	end;
end;

//------------------------------------------------------------------------------
//TPacketList.Add                                                      PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Packet adding/updating procedure
//		For all new codelists after the first one, any different packets are
//		updated, since each packet ver after the base is an updated diff.
//		so packet ver 3 = packet ver 0 + 1 diff, + 2 diff + 3 diff
//
//	Changes -
//		June 28th, 2008 - Tsusai - Created.
//
//------------------------------------------------------------------------------
procedure TPacketList.Add(PacketObject : TPackets);
var
	idx : integer;
begin
	//Check for existing to update
	for idx := 0 to Count - 1 do
	begin
		If TPackets(Items[idx]).ID = PacketObject.ID then
		begin
			TPackets(Items[idx]).ID := PacketObject.ID;
			TPackets(Items[idx]).PLength := PacketObject.PLength;
			TPackets(Items[idx]).Command := PacketObject.Command;
			SetLength(TPackets(Items[idx]).ReadPoints,Length(PacketObject.ReadPoints));
			TPackets(Items[idx]).ReadPoints := Copy(PacketObject.ReadPoints,0,Length(PacketObject.ReadPoints));
			TPackets(Items[idx]).ExecCommand := PacketObject.ExecCommand;
			TPackets(Items[idx]).ExecAvoidSelfCommand := PacketObject.ExecAvoidSelfCommand;
			Exit; //Don't need to readd another, since we updated
		end;
	end;
	//Didn't find one to update, add it.
	inherited Add(PacketObject);
end;

//------------------------------------------------------------------------------
//TPacketList.Create                                                 CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//		Sets the list to own its objects
//
//	Changes -
//		June 28th, 2008 - Tsusai - Created.
//
//------------------------------------------------------------------------------
constructor TPacketList.Create;
begin
	inherited;
	Self.OwnsObjects := True;
end;


(**)
(**)
(*TPACKETDB METHODS!!!!!*)
(**)
(**)


//------------------------------------------------------------------------------
//TPacketDB.GetLength                                                  FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets the length of a packet specified by ID for version Version
//
//	Changes -
//		December 22nd, 2006 - RaX - Created.
//		June 8th, 2008 - Tsusai - Updated to use the Packet Object List
//		June 28th, 2008 - Tsusai - Moved into TPacketDB as GetLength
//
//------------------------------------------------------------------------------
function TPacketDB.GetLength(ID : Word; Version : Word = 0) : LongWord;
var
	Index           : Integer;
	CodebaseLength  : Integer;
begin
	Result := 0;
	CodebaseLength := fCodebase[Version].Packets.Count;
	for Index := 0 to CodebaseLength - 1 do
	begin
		if TPackets(fCodebase[Version].Packets[Index]).ID = ID then
		begin
			Result := TPackets(fCodebase[Version].Packets[Index]).PLength;
			break;
		end;
	end;
end;


//------------------------------------------------------------------------------
//TPacketDB.GetPacketInfo                                              PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      This is only executed to find information about a packet for ingame/zone
//    packets.  If there is data, it returns a a valid object.
//		External procedures must check this via if Assigned()
//
//  Notes -
//    packet - The integer packet id.

//    Ver - The version of packets we are going to look at.  This is called
//      twice, first if we need to look through it with our new client packet,
//      and if we can't find it, we need to search through the 0 version aka
//      oldschool aka 0628sakexe based.

//
//	Changes -
//		June 28th, 2008 - Tsusai - Moved to TPacketDB, renamed, and upgraded
//
//------------------------------------------------------------------------------
Procedure TPacketDB.GetPacketInfo(
	const Packet  : word;
	const Version : Word;
	var ReturnPackets : TPackets
);
var
	Index : Integer;
begin
	ReturnPackets := nil; //Shouldn't have anything, and shuts up compiler warning
												//about it being unassigned.
	//Check packet list based on Version
	for Index := 0 to fCodeBase[Version].Packets.Count - 1 do
	begin
		//With Statement..
		with TPackets(fCodeBase[Version].Packets.Items[Index]) do
		begin
			if (ID = Packet) then
			begin
				//Ok so we found a matching packet ID, fill the info
				ReturnPackets := TPackets.Create;
				ReturnPackets.PLength := PLength;
				SetLength(ReturnPackets.ReadPoints,Length(ReadPoints));
				ReturnPackets.ReadPoints := Copy(ReadPoints,0,Length(ReadPoints));
				ReturnPackets.Command := Command;
				ReturnPackets.ExecCommand := ExecCommand;
				ReturnPackets.ExecAvoidSelfCommand := ExecAvoidSelfCommand;
				Break;
			end;
		end;
	end;
end;


//------------------------------------------------------------------------------
//TPacketDB.GetMapConnectPacket                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      Digs through all packet codebases, from newest to oldest lookgin for a
//    'wanttoconnect' that matches the ID, and PacketLength given.  This routine
//		will comb through using TPacketDB.GetPacketInfo until the end.  If data is
//		found, the TPacket object has data and is sent up to the caller in
//		ZoneServer, where if there is no data, then ZoneServer gets no data.
//		Obviously this must be checked with an if Assigned().
//
//	Changes -
//		June 28th, 2008 - Tsusai - Created
//
//------------------------------------------------------------------------------
procedure TPacketDB.GetMapConnectPacket(
	const Packet : word;
	const PLength : word;
	var ReturnVersion : word;
	var ReturnPacketInfo : TPackets
);
var
	ClientBaseIndex : integer;
begin
	for ClientBaseIndex := (Length(fCodeBase) -1) downto 0 do
	begin
		//Use the other TPacketDB method to find it based on ID and Version.
		GetPacketInfo(Packet,ClientBaseIndex,ReturnPacketInfo);
		if Assigned(ReturnPacketInfo) then
		begin
			//Check length and command
			if (ReturnPacketInfo.Command = 'wanttoconnection') and (ReturnPacketInfo.PLength = PLength) then
			begin
				//Found it, set the version and exit
				ReturnVersion := ClientBaseIndex;
				Exit; //Exit since we found it.
			end;
		end;
	end;
	//Didn't find it, so free it.  This will cause a unknown client error higher up
  ReturnPacketInfo.Free;
end;


//------------------------------------------------------------------------------
//TPacketDB.GetEAPacketVer                                             FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//      By using our fCodeBase index number, which is usually stored in
//			ACharacter.ClientVer, we can pull out the eA packet ver which is specified
//			by the packet_db file
//
//	Changes -
//		June 28th, 2008 - Tsusai - Created
//
//------------------------------------------------------------------------------
function TPacketDB.GetEAPacketVer(InternalPacketDBVer : word) : word;
begin
	Result := fCodeBase[InternalPacketDBVer].EAPACKETVER;
end;


(*------------------------------------------------------------------------------
TPacketDB.Load

Basic logic: load packets in a set in CodeBase[] until you see a packet that is
already listed.  If so, increment CodeBase[], clone the data, and add/update

TODO:
--
Unknown.

--
Revisions:
--
[2006/01/01] CR - Reindented.  Moved creation of "sl" stringlists closer to
	first use.  Left a note where a reported bug was, but could not figure out a
	solution at this time.
[2006/03/10] Tsusai - Moved the Command elseif check to assign procedures to the
	database to be called.
[2006/03/10] Tsusai - Renamed ExecSpecialCommand to ExecAvoidSelfCommand.
[2006/03/11] Tsusai - Renamed variables to something less stupid like sl.
	changed variable types from integer to word/byte for certain parts of the loops.
	Also removed packetID being stored as string, now being stored as a word type.
[2006/04/09] Tsusai - Adjusted expanding of packet array to prevent blank end
	packet info.
[2006/09/26] Tsusai - Imported to Helios
January 20th 2007 - Tsusai - Now a function
June 8th 2008 - Tsusai - Overhauled.  Introduced a packet object list, to hold
	packet objects, instead of another array.  Also implemented use of eAthena's
	packet_db to help with easy updating.
June 28th, 2008 - Tsusai - Put in TPacketDB
------------------------------------------------------------------------------*)
function TPacketDB.Load : boolean;
Var
	sidx : integer;
	CBIdx : word;
	ReadLoc : byte;
	PacketVersion : integer;
	packet_db : TStringList;
	PacketInfo : TStringList;
	PacketData : TPackets;
	ReadPtInfo : TStringList;
Begin
	Result := FileExists(MainProc.Options.DatabaseDirectory+'/'+'packet_db.txt');
	if Result then
	begin
		PacketVersion := 0;
		CBIdx := 0;
		//Init the first list
		SetLength(fCodebase,CBIdx+1);
		fCodebase[CBIdx].EAPACKETVER := PacketVersion;
		fCodebase[CBIdx].Packets := TPacketList.Create;

		//load the packet_db file
		packet_db := TStringList.Create;
		packet_db.LoadFromFile(MainProc.Options.DatabaseDirectory+'/'+'packet_db.txt');

		PacketInfo := TStringList.Create;

		for sidx := 0 to (packet_db.Count -1) do
		begin
			//Look for packet_ver: sections
			if Copy(packet_db.Strings[sidx],1,12) = ('packet_ver: ') then
			begin

				//Get the packet version # out of eA
				PacketVersion := StrToInt(
					StringReplace(packet_db.Strings[sidx],'packet_ver: ','',[rfReplaceAll])
				);
				//skip making a new packet version group if we're already using it (ie. packetver 0)
				if PacketVersion = fCodebase[CBIdx].EAPACKETVER then continue;

				//if we're hitting our first REAL packet version (ie the Client <> server packets)
				//Then we're just going to give it a packetversion number, and add onto it.
				//There should be no packetver 0, just whatever the first packetver is, plus
				//any random packets like inter before it.
				if fCodebase[CBIdx].EAPACKETVER = 0 then
				begin
					fCodebase[CBIdx].EAPACKETVER := PacketVersion;
					Continue;
				end;

				Inc(CBIdx);
				//Init New list
				SetLength(fCodebase,CBIdx+1);
				fCodebase[CBIdx].EAPACKETVER := PacketVersion;
				fCodebase[CBIdx].Packets := TPacketList.Create;
				//Copy contents from previous to new list
				fCodebase[CBIdx].Packets.Assign(fCodeBase[CBIdx-1].Packets);
				//Go to next line, we're done with setup
				Continue;
			end else if (Copy(packet_db[sidx],1,2) <> '0x') then
			begin
				Continue; //we are ignoring comments and other crap
			end;
			//READ THE PACKET DATA (>^.^>)
			PacketData := TPackets.Create;
			PacketInfo.CommaText := packet_db[sidx];// break up the information by commas


			if (PacketInfo.Count >= 2) then
			begin
				PacketData.ID := StrToIntDef( StringReplace(PacketInfo[0], 'Ox', '$', [rfReplaceAll]),0);
				PacketData.PLength := StrToIntDef(PacketInfo[1],0);//Save the packet length
			end;

			if (PacketInfo.Count = 4) then
			begin
				PacketData.Command := PacketInfo[2]; //Procedure name to run
				//Loading all the procedural read packet locations
				ReadPtInfo := TStringList.Create;
				ReadPtInfo.Delimiter := ':';
				ReadPtInfo.DelimitedText := PacketInfo[3];
				SetLength(PacketData.ReadPoints, ReadPtInfo.Count);
				for ReadLoc := 0 to (ReadPtInfo.Count -1) do
				begin
					PacketData.ReadPoints[ReadLoc] := StrToInt(ReadPtInfo[ReadLoc]);
					{[2006/01/01] CR - ERangeError reported here at +36 in Rev 582...
					don't know if it was a corrupt line in the file, or what the issue is,
					at this point. }
				end;
				ReadPtInfo.Free;
			end else begin
				PacketData.Command := 'nocommand';
				SetLength(PacketData.ReadPoints, 1);
				PacketData.ReadPoints[0] := 0;
			end;
			with PacketData do begin
				//Blank out the procedures with dummy ones.
				ExecCommand := NoCommand;
				ExecAvoidSelfCommand := NoCommand;
				//Find the right procedure to assign to the database
				if Command = 'loadendack' then begin
					ExecCommand := ShowMap;
				end else if Command = 'ticksend' then begin
					ExecCommand := RecvTick;
				end else if Command = 'walktoxy' then begin
					ExecCommand := CharacterWalkRequest;
				end else if Command = 'actionrequest' then begin
					ExecCommand := ActionRequest;
				end else if Command = 'globalmessage' then begin
					ExecCommand := AreaChat;
				end else if Command = 'npcclicked' then begin
					ExecCommand := NPCClick;
				end else if Command = 'getcharnamerequest' then begin
					ExecCommand := GetNameAndID;
				end else if Command = 'wis' then begin
					ExecCommand := Whisper;
				end else if Command = 'gmmessage' then begin
					ExecCommand := GMBroadcast;
				end	else if Command = 'changedir' then begin
					ExecCommand := CharaRotation;
				{end	else if Command = 'takeitem' then	begin
					ExecCommand := TakeItem;}
				end else if Command = 'dropitem' then begin
					ExecCommand := DropItem;
				{end else if Command = 'useitem' then begin
					ExecCommand := ItemUse;
				end else if Command = 'equipitem' then begin
					ExecCommand := ItemEquip;
				end else if Command = 'unequipitem' then begin
					ExecCommand := ItemUnequip;}
				end else if Command = 'restart' then begin
					ExecCommand := ReturnToCharacterSelect;
				end else if Command = 'npcselectmenu' then begin
					ExecCommand := NPCMenu;
				end else if Command = 'npcnextclicked' then begin
					ExecCommand := NPCNext;
				end else if Command = 'statusup' then begin
					ExecCommand := StatUP;
				{end else if Command = 'emotion' then begin
					ExecCommand := EmotionCheck;}
				end else if Command = 'howmanyconnections' then begin
					ExecCommand := SlashWho;
				{end else if Command = 'npcbuysellselected' then begin
					ExecCommand := ClickNPCshop;
				end else if Command = 'npcbuylistsend' then begin
					ExecCommand := BuyFromNPC;
				end else if Command = 'npcselllistsend' then begin
					ExecCommand := SellToNPC;
				end else if Command = 'gmkick' then begin
					ExecCommand := GMRightClickKill;
				//Unknown eA block here
				end else if Command = 'killall' then begin
					ExecCommand := KillAll;
				end else if Command = 'wisexin' then begin
					ExecCommand := wisexin;
				end else if Command = 'wisall' then begin
					ExecCommand := wisall;
				end else if Command = 'wisexlist' then begin
					ExecCommand := wisexlist;
				//end of first unknown eA block
				end else if Command = 'createchatroom' then begin
					ExecCommand := CreateChatroom;
				end else if Command = 'chataddmember' then begin
					ExecCommand := JoinChatroom;
				end else if Command = 'chatroomstatuschange' then begin
					ExecCommand := UpdateChatroom;
				end else if Command = 'changechatowner' then begin
					ExecCommand := ChatroomOwnerChange;
				end else if Command = 'kickfromchat' then begin
					ExecCommand := KickFromChat;
				end else if Command = 'chatleave' then begin
					ExecAvoidSelfCommand := ChatRoomExit;
				end else if Command = 'traderequest' then begin
					ExecCommand := RequestTrade;
				end else if Command = 'tradeack' then begin
					ExecCommand := TradeRequestAcceptDeny;
				end else if Command = 'tradeadditem' then begin
					ExecCommand := AddItemToTrade;
				end else if Command = 'tradeok' then begin
					ExecCommand := TradeAccept;
				end else if Command = 'tradecancel' then begin
					ExecAvoidSelfCommand := CancelDealings;
				end else if Command = 'tradecommit' then begin
					ExecCommand := ConfirmTrade;
				end else if Command = 'movetokafra' then begin
					ExecCommand := AddToStorage;
				end else if Command = 'movefromkafra' then begin
					ExecCommand := RemoveFromStorage;
				end else if Command = 'closekafra' then begin
					ExecCommand := CloseStorage;
				end else if Command = 'createparty' then begin
					ExecCommand := CreateParty;
				end else if Command = 'partyinvite' then begin
					ExecCommand := InviteToParty;
				end else if Command = 'replypartyinvite' then begin
					ExecCommand := PartyInviteReply;
				end else if Command = 'leaveparty' then begin
					ExecCommand := RequestLeaveParty;
				end else if Command = 'partychangeoption' then begin
					ExecCommand := ChangePartySettings;
				end else if Command = 'removepartymember' then begin
					ExecCommand := KickPartyMember;
				end else if Command = 'partymessage' then begin
					ExecCommand := MessageParty;
				end else if Command = 'skillup' then begin
					ExecCommand := SkillUP;
				end else if Command = 'useskilltoid' then begin
					ExecCommand := UseSkillOnID;
				end else if Command = 'useskilltopos' then begin
					if Length(ReadPoints) = 4 then
					begin
						ExecCommand := UseSkillOnXY;
					end;
					if Length(ReadPoints) = 5 then
					begin
						ExecCommand := UseSkillOnXYExtra;
					end;}
				end else if Command = 'stopattack' then begin
					ExecCommand := CancelAttack;{
				end else if Command = 'useskillmap' then begin
					ExecCommand := UseSkillOnMap;
				end else if Command = 'requestmemo' then begin
					ExecCommand := SaveMemoRequest;
				end else if Command = 'putitemtocart' then begin
					ExecCommand := ItemInventoryToCart;
				end else if Command = 'getitemfromcart' then begin
					ExecCommand := ItemCartToInventory;
				end else if Command = 'movefromkafratocart' then begin
					ExecCommand := ItemKafraToCart;
				end else if Command = 'movetokafrafromcart' then begin
					ExecCommand := ItemCartToKafra;
				end else if Command = 'removeoption' then begin
					ExecCommand := CharaOptionOff;
				end else if Command = 'closevending' then begin
					ExecAvoidSelfCommand := VenderExit;
				end else if Command = 'vendinglistreq' then begin
					ExecCommand := VenderItemList;
				end else if Command = 'purchasereq' then begin
					ExecCommand := BuyFromVender;
				end else if Command = 'itemmonster' then begin
					ExecCommand := GMSummonItemMob;}
				end else if Command = 'mapmove' then begin
					ExecCommand := GMMapMove;
				end else if Command = 'recall' then begin
					ExecCommand := GMRecall;
				{end else if Command = 'npcamountinput' then begin
					ExecCommand := NPCIntegerInput;
				end else if Command = 'npccloseclicked' then begin
					ExecCommand := NPCClickClose;
				end else if Command = 'gmreqnochat' then begin
					ExecCommand := GMRequestNoChat;
				end else if Command = 'guildcheckmaster' then begin
					ExecCommand := GuildCheckMaster;
				end else if Command = 'guildrequestinfo' then begin
					ExecCommand := GuildRequestInfoPage;
				end else if Command = 'guildrequestemblem' then begin
					ExecCommand := GuildRequestEmblem;
				end else if Command = 'guildchangeemblem' then begin
					ExecCommand := GuildChangeEmblem;
				end else if Command = 'guildchangememberposition' then begin
					ExecCommand := GuildRequestMemberChange;
				end else if Command = 'guildleave' then begin
					ExecCommand := GuildCharaLeave;
				end else if Command = 'guildexplusion' then begin
					ExecCommand := GuildCharaBan;
				end else if Command = 'guildbreak' then begin
					ExecCommand := GuildDissolve;
				end else if Command = 'guildchangepositioninfo' then begin
					ExecCommand := GuildChangePositionInfo;
				end else if Command = 'createguild' then begin
					ExecCommand := CreateGuild;
				end else if Command = 'guildinvite' then begin
					ExecCommand := GuildInviteChara;
				end else if Command = 'guildreplyinvite' then begin
					ExecCommand := GuildCharaInviteReply;
				end else if Command = 'guildchangenotice' then begin
					ExecCommand := GuildSetAnnouncement;
				end else if Command = 'guildrequestalliance' then begin
					ExecCommand := GuildRequestAlly;
				end else if Command = 'guildreplyalliance' then begin
					ExecCommand := GuildRequestAllyResponse;
				end else if Command = 'itemidentify' then begin
					ExecCommand := AppraiseItem;
				end else if Command = 'usecard' then begin
					ExecCommand := UseCard;
				end else if Command = 'insertcard' then begin
					ExecCommand := CardMountRequest;
				end else if Command = 'guildmessage' then begin
					ExecCommand := MessageGuild;
				end else if Command = 'guildopposition' then begin
					ExecCommand := OpposeGuild;
				end else if Command = 'guilddelalliance' then begin
					ExecCommand := CancelGuildRelation;}
				end else if Command = 'quitgame' then begin
					ExecCommand := QuitGame;{
				end else if Command = 'producemix' then begin
					ExecCommand := CreateItem;
				end else if Command = 'useskilltoposinfo' then begin
					ExecCommand := UseSkillOnXYExtra;
				end else if Command = 'solvecharname' then begin
					ExecCommand := SolveCharaName;
				end else if Command = 'resetchar' then begin
					ExecCommand := GMReset;
				end else if Command = 'lgmmessage' then begin
					ExecCommand := lgmmessage;
				end else if Command = 'gmhide' then begin
					ExecCommand := GMHide;
				end else if Command = 'catchpet' then begin
					ExecCommand := CaptureMonster;
				end else if Command = 'petmenu' then begin
					ExecCommand := PetMenuAction;
				end else if Command = 'changepetname' then begin
					ExecCommand := ChangePetName;
				end else if Command = 'selectegg' then begin
					ExecCommand := SelectPetEgg;
				end else if Command = 'sendemotion' then begin
					ExecCommand := SendPetEmotion;
				end else if Command = 'selectarrow' then begin
					ExecCommand := SelectArrow;
				end else if Command = 'changecart' then begin
					ExecCommand := ChangeCart;
				end else if Command = 'openvending' then begin
					ExecCommand := OpenVendingShop;}
				end else if Command = 'shift' then begin
					ExecCommand := GMShiftChar;
				{end else if Command = 'summon' then begin
					ExecCommand := GMSummonChar;
				end else if Command = 'autospell' then begin
					ExecCommand := AutoSpellSelect;
				end else if Command = 'npcstringinput' then begin
					ExecCommand := NPCStringInput;
				end else if Command = 'gmreqnochatcount' then begin
					ExecCommand := GMReqNoChatCount;
				end else if Command = 'sndoridori' then begin
					ExecCommand := SNDoridori;
				end else if Command = 'createparty2' then begin
					ExecCommand := CreatParty2;
				end else if Command = 'snexplosionspirits' then begin
					ExecCommand := SNExplosionSpirits; }
				end else if Command = 'friendslistadd' then begin
					ExecCommand := RequestToAddFriend;
				end else if Command = 'friendslistremove' then begin
					ExecCommand := RemoveFriendFromList;
				end else if Command = 'friendlistreply' then begin
					ExecCommand := RequestToAddFriendResponse;
				end else if Command = 'mailwinopen' then begin
					ExecCommand := MailWindowSwitch;
				end else if Command = 'mailrefresh' then begin
					ExecCommand := RequestMailRefresh;
				end else if Command = 'mailread' then begin
					ExecCommand := ReadMail;
				end else if Command = 'maildelete' then begin
					ExecCommand := DeleteMail;
				end else if Command = 'mailsend' then begin
					ExecCommand := RequestSendMail;
				end;
			end;
			//Add data (or update, .Add knows how) to current work list
			fCodeBase[CBIdx].Packets.Add(PacketData);
		end;

		packet_db.Free; //free the stringlist with the text
		//PacketInfo.Free; (Can't free, else it'll free the last object in the last list!!!!!! (x.x)'

	end else begin
		Console.WriteLn('*** '+MainProc.Options.DatabaseDirectory+'/'+'packet_db.txt was not found.');
	end;
End; (* Proc TPacketDB.Load
*-----------------------------------------------------------------------------*)

///------------------------------------------------------------------------------
//TPacketDB.Unload                                                     PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//      Unloads the database
//
//	Changes -
//		June 28th, 2008 - Tsusai - Created
//
//------------------------------------------------------------------------------
procedure TPacketDB.Unload;
var
	idx : integer;
begin
	for idx := 0 to Length(fCodeBase) - 1 do
	begin
		//Free lists
		fCodeBase[idx].Packets.Free;
	end;
	//Shut down the array of lists.
	SetLength(fCodebase,0);
end;

end.
