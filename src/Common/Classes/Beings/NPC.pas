unit NPC;

interface
uses
	Being,
	Types;

	type TNPC = class(TBeing)
		public
			Enabled : boolean;
			procedure CalcMaxHP; override;
			procedure CalcMaxSP; override;
			Constructor Create;
	end;

	type TScriptNPC = class(TNPC)
		private
			fClickFunction : string;
			fTouchFunction : string;
		public
			XRadius : word;
			YRadius : word;
			OnTouch : boolean;

			property ClickFunction : string read fClickFunction;
			property TouchFunction : string read fTouchFunction;

			constructor Create(
				ClickFunc : string = '';
				TouchFunc : string = ''
			);
	end;

	type TWarpNPC = class(TScriptNPC)
		public
			DestMap : String;
			DestPoint : TPoint;

			constructor Create(
				TouchFunc : string = ''
			);

	end;

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
	MaxHP := HP;
end;

procedure TNPC.CalcMaxSP;
begin
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
	JID := 45;
end;

end.