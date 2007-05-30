unit PacketDB;

interface
uses
	Character,
	PacketTypes;

	function Load_PacketDB : boolean;

(*------------------------------------------------------------------------------
Here we are setting up a table of packets for the Load_PacketDB to fill up.
CodeBase[] - Holds the different client sets of packets
CodeBase[].PacketArray[] - An array of the packets (1 line from packet_db)
CodeBase[].PacketArray[].ID - the packet ID in integer form
CodeBase[].PacketArray[].Lth - length of this packet
CodeBase[].PacketArray[].Command - what procedure keyword does this packet represent
CodeBase[].PacketArray[].ReadPoints[] - The reading locations of the packet to get information
CodeBase[].PacketArray[].ExecCommand - Special type: links to tchara, readpt packet procedures
CodeBase[].PacketArray[].ExecAvoidSelfCommand - Special type: links to tchara avoidself packet procedures
------------------------------------------------------------------------------*)
type
	//The more usual of the zone procedures
	TPacketProc = procedure (var AChara : TCharacter; const RecvBuffer : TBuffer; const ReadPts : TReadPts);
	//The Tchara, Avoidself packets.  About 3 exist
	TAvoidSelfPacketProc = procedure (var AChara : TCharacter; const AvoidSelf : boolean = False);

type
	TPackets = record
		ID : word; //PacketID

		//Length, trying not to conflict with Length() command.
		//[2006/03/12] Tsusai - MUST BE INTEGER TYPE! I messed it up using word.
		PLength : integer;

		Command								: string;
		ReadPoints						: TReadPts;
		ExecCommand						: TPacketProc; //Regular procedure link
		ExecAvoidSelfCommand	: TAvoidSelfPacketProc; //avoidself special procedure link
	end;

	PacketArray = record
		Packet : array of TPackets;
	end;
	CodeArray  = array of PacketArray;

var
	CodeBase : CodeArray;


	procedure LoadStaticDatabase;

implementation
uses
	SysUtils,
	Classes,

	Main,
	Globals,
	ZoneRecv;


(*------------------------------------------------------------------------------
Load_PacketDB

Basic logic: load packets in a set in CodeBase[] until you see a packet that is
already listed.  If so, increment CodeBase[] and start again.

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
------------------------------------------------------------------------------*)
function Load_PacketDB : boolean;
Var
	sidx : word;
	CB : word;
	PK : integer;
	idx : integer;
	ReadLoc : byte;

	packet_db : TStringList;
	PacketInfo : TStringList;
	ReadPtInfo : TStringList;
Begin
	Result := FileExists(MainProc.Options.DatabaseDirectory+'/'+'packet_db.txt');
	if Result then
	begin
		//load the packet_db file
		packet_db := TStringList.Create;
		packet_db.LoadFromFile(MainProc.Options.DatabaseDirectory+'/'+'packet_db.txt');

		CB := 0;
		PK := 0;
		SetLength(CodeBase, CB + 1); //setup the CodeBase for informations

		PacketInfo := TStringList.Create;
		for sidx := 0 to (packet_db.Count -1) do
		begin
			if (Copy(packet_db[sidx],1,2) <> '0x') then
			begin
				Continue; //we are ignoring comments and other crap
			end;

			PacketInfo.CommaText := packet_db[sidx];// break up the information by commas


			//Check for existing packet, if found, start new codebase
			for idx := 0 to (Length(CodeBase[CB].Packet) -1) do
			begin
				if (CodeBase[CB].Packet[idx].ID = StrToIntDef( StringReplace(PacketInfo[0], 'Ox', '$', [rfReplaceAll]),0)) then
				begin
					PK := 0; //Preparing for the next CodeBase
					Inc(CB); //Go to the next codebase
					SetLength(CodeBase, CB +1); //setup the next codebase
					Break;
				end;
			end;

			SetLength(CodeBase[CB].Packet, PK +1);//Setup the packet array

			if (PacketInfo.Count >= 2) then
			begin
				CodeBase[CB].Packet[PK].ID := StrToIntDef( StringReplace(PacketInfo[0], 'Ox', '$', [rfReplaceAll]),0);
				CodeBase[CB].Packet[PK].PLength := StrToIntDef(PacketInfo[1],0);//Save the packet length
			end;

			if (PacketInfo.Count = 4) then
			begin
				CodeBase[CB].Packet[PK].Command := PacketInfo[2]; //Procedure name to run
				//Loading all the procedural read packet locations
				ReadPtInfo := TStringList.Create;
				ReadPtInfo.Delimiter := ':';
				ReadPtInfo.DelimitedText := PacketInfo[3];
				SetLength(CodeBase[CB].Packet[PK].ReadPoints, ReadPtInfo.Count);
				for ReadLoc := 0 to (ReadPtInfo.Count -1) do
				begin
					CodeBase[CB].Packet[PK].ReadPoints[ReadLoc] := StrToInt(ReadPtInfo[ReadLoc]);
					{[2006/01/01] CR - ERangeError reported here at +36 in Rev 582...
					don't know if it was a corrupt line in the file, or what the issue is,
					at this point. }
				end;
				ReadPtInfo.Free;
			end else begin
				CodeBase[CB].Packet[PK].Command := 'nocommand';
				SetLength(CodeBase[CB].Packet[PK].ReadPoints, 1);
				CodeBase[CB].Packet[PK].ReadPoints[0] := 0;
			end;
			with CodeBase[CB].Packet[PK] do begin
				//Blank out the procedures with dummy ones.
				ExecCommand := NoCommand;
				ExecAvoidSelfCommand := NoCommand;
				//Find the right procedure to assign to the database
				if Command = 'loadendack' then begin
					ExecCommand := ShowMap;
				end else if Command = 'ticksend' then begin
					ExecCommand := RecvTick;
				end else if Command = 'walktoxy' then begin
					ExecCommand := CharacterWalkRequest;{
				end else if Command = 'actionrequest' then begin
					ExecCommand := ActionRequest;}
				end else if Command = 'globalmessage' then begin
					ExecCommand := AreaChat;
				end else if Command = 'npcclicked' then begin
					ExecCommand := NPCClick;
				end else if Command = 'getcharnamerequest' then begin
					ExecCommand := GetNameAndID;
				end else if Command = 'wis' then begin
					ExecCommand := Whisper;
				{end else if Command = 'gmmessage' then begin
					ExecCommand := GMBroadcast;}
				end	else if Command = 'changedir' then begin
					ExecCommand := CharaRotation;
				{end	else if Command = 'takeitem' then	begin
					ExecCommand := TakeItem;
				end else if Command = 'dropitem' then begin
					ExecCommand := DropItem;
				end else if Command = 'useitem' then begin
					ExecCommand := ItemUse;
				end else if Command = 'equipitem' then begin
					ExecCommand := ItemEquip;
				end else if Command = 'unequipitem' then begin
					ExecCommand := ItemUnequip;}
				end else if Command = 'restart' then begin
					ExecCommand := ReturnToCharacterSelect;
				{end else if Command = 'npcselectmenu' then begin
					ExecCommand := NPCMenu;}
				end else if Command = 'npcnextclicked' then begin
					ExecCommand := NPCNext;
				{end else if Command = 'statusup' then begin
					ExecCommand := StatUP;
				end else if Command = 'emotion' then begin
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
					end;
				end else if Command = 'stopattack' then begin
					ExecCommand := CancelAttack;
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
					ExecCommand := GMSummonItemMob;
				end else if Command = 'mapmove' then begin
					ExecCommand := GMMapMove;
				end else if Command = 'npcamountinput' then begin
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
					ExecCommand := OpenVendingShop;
				end else if Command = 'shift' then begin
					ExecCommand := GMShiftChar;
				end else if Command = 'summon' then begin
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
					ExecCommand := SNExplosionSpirits;
				end else if Command = 'friendaddrequest' then begin
					ExecCommand := RequestToAddFriend;
				end else if Command = 'frienddeleterequest' then begin
					ExecCommand := RemoveFriendFromList;
				end else if Command = 'friendaddreply' then begin
					ExecCommand := RequestToAddFriendResponse;
				end;} end;
			end;
			Inc(PK);//jump to the next packet record
		end;

		packet_db.Free;
		PacketInfo.Free;
	end else begin
		Console.WriteLn('*** '+MainProc.Options.DatabaseDirectory+'/'+'packet_db.txt was not found.');
	end;
End; (* Proc Load_PacketDB
*-----------------------------------------------------------------------------*)

procedure LoadStaticDatabase;
begin
	Load_PacketDB;
end;


end.
