unit NPC;

interface
uses
	Being,
	Types;

	
	(*
	Basic NPC Class
	Branches out like so
	
	TBeing
		TNPC
			TScriptNPC
				TWarpNPC
			TItemNPC
			TShopNPC (May not exist, may JUST be a npc 
				command to open the shop under)
	
	*)
	
	//Non Player Character class
	type TNPC = class(TBeing)
		public
			Enabled : boolean;
			//Dunno why they are needed, but they are here to
			//kill compiler warnings.
			procedure CalcMaxHP; override;
			procedure CalcMaxSP; override;
			Constructor Create;
	end;

	//Basic scripted NPC.  Has ontouch & onclick function
	//string names.  They are filled in on creation and can
	//only be read for executing the lua script.
	type TScriptNPC = class(TNPC)
		private
			fClickFunction : string;
			fTouchFunction : string;
		public
			//OnTouch
			XRadius : word;
			YRadius : word;
			OnTouch : boolean;

			//Properties for lua function names
			property ClickFunction : string read fClickFunction;
			property TouchFunction : string read fTouchFunction;

			//Creation fills in the function names.
			constructor Create(
				ClickFunc : string = '';
				TouchFunc : string = ''
			);
	end;

	//Subclass of TScriptNPC, contains destination map and point, and
	//Only takes a ontouch script function name which is optional in the
	//creation
	type TWarpNPC = class(TScriptNPC)
		public
			constructor Create(
				TouchFunc : string = ''
			);

	end;

	//Holder for now, contains the NPC for an item
	//Contains an ExpireTime for cleanup (kill dropped items)
	type TItemNPC = class(TNPC)
		public
			ExpireTime : LongWord;
	end;

implementation

//Start NPC IDs at 100K
const NPCIDBase = 100000;

var
	NowNPCID : LongWord = NPCIDBase;

constructor TNPC.Create;
begin
	ID := NowNPCID;
	Inc(NowNPCID);
end;

procedure TNPC.CalcMaxHP;
begin
	//No idea, placeholder for now
	MaxHP := HP;
end;

procedure TNPC.CalcMaxSP;
begin
	//No idea, placeholder for now
	MaxHP := HP;
end;

constructor TScriptNPC.Create(
	ClickFunc : string = '';
	TouchFunc : string = ''
);
begin
	inherited Create;
	fClickFunction := ClickFunc;
	fTouchFunction := TouchFunc;
end;

constructor TWarpNPC.Create(
	TouchFunc : string = ''
);
begin
	inherited Create('',TouchFunc);
end;

end.
