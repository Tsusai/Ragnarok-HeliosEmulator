(*------------------------------------------------------------------------------
Class Character
Tsusai July 2006

Description:
 Basic character object.

[2006/07/08] RaX - Moved into the 'Classes' Directory and renamed to
 'Character.pas'. One class per file is the name of the game.
------------------------------------------------------------------------------*)
unit Character;

interface
uses
	//IDE
	Types,
	//Helios
	Globals,
	GameObjects;

type
	TParamBase = record
		STR : byte;
		AGI : byte;
		VIT : byte;
		INT : byte;
		DEX : byte;
		LUK : byte;
	end;

type TCharacter = class(TBeing)
	private
		function  GetCharaNum : byte;
		procedure SetCharaNum(Value : byte);
		function  GetName : string;
		procedure SetName(Value : string);
		function  GetClass : byte;
		procedure SetClass(Value : byte);
		function  GetBaseLV : byte;
		procedure SetBaseLV(Value : byte);
		function  GetJobLV : byte;
		procedure SetJobLV(Value : byte);
		function  GetBaseEXP : byte;
		procedure SetBaseEXP(Value : byte);
		function  GetJobEXP : byte;
		procedure SetJobEXP(Value : byte);
		function  GetZeny : Cardinal;
		procedure SetZeny(Value : Cardinal);
		function  GetBaseStats : TParamBase;
		procedure SetBaseStats(Value : TParamBase);
		function  GetMaxHP : word;
		procedure SetMaxHP(Value : word);
		function  GetHP : word;
		procedure SetHP(Value : word);
		function  GetMaxSP : word;
		procedure SetMaxSP(Value : word);
		function  GetSP : word;
		procedure SetSP(Value : word);

		function  GetStatusPts : word;
		procedure SetStatusPts(Value : word);
		function  GetSkillPts : word;
		procedure SetSkillPts(Value : word);

		function  GetMap : string;
		procedure SetMap(Value : string);
		function  GetMapPt : TPoint;
		procedure SetMapPt(Value : TPoint);
		function  GetSMap : string;
		procedure SetSMap(Value : string);
		function  GetSMapPt : TPoint;
		procedure SetSMapPt(Value : TPoint);

	public
		CID : Cardinal;

		property CharaNum  : Byte       read GetCharaNum write SetCharaNum;
		property Name      : string     read GetName write SetName;
		property JobClass  : Byte       read GetClass write SetClass;
		property BaseLV    : Byte       read GetBaseLV write SetBaseLV;
		property JobLV     : Byte       read GetJobLV write SetJobLV;
		property BaseEXP   : Byte       read GetBaseEXP write SetBaseEXP;
		property JobEXP    : Byte       read GetJobEXP write SetJobEXP;
		property Zeny      : Cardinal   read GetZeny write SetZeny;
		property StatBase  : TParamBase read GetBaseStats write SetBaseStats;
		property MaxHP     : Word       read GetMaxHP write SetMaxHP;
		property HP        : Word       read GetHP write SetHP;
		property MaxSP     : Word       read GetMaxSP write SetMaxSP;
		property SP        : Word       read GetSP write SetSP;
		property StatusPts : Word       read GetStatusPts write SetStatusPts;
		property SkillPts  : Word       read GetSkillPts write SetSkillPts;


		property Map       : string read GetMap write SetMap;
		property Point     : TPoint read GetMapPt write SetMapPt;
		property SaveMap   : string read GetSMap write SetSMap;
		property SavePoint : TPoint read GetSMapPt write SetSMapPt;

		function CreateInSQL(AID : Cardinal; NName : string) : cardinal;
end;

implementation
uses
	//IDE
	SysUtils,
	//Helios
	SQL;

function TCharacter.GetCharaNum : byte;
begin
	Result := GetSQLDataInt('char',CID,'char_num');
end;

procedure TCharacter.SetCharaNum(Value : byte);
begin
	SetSQLDataInt('char',CID,'char_num',Value);
end;

function TCharacter.GetName : string;
begin
	Result := GetSQLDataStr('char',CID,'name');
end;

procedure TCharacter.SetName(Value : string);
begin
	SetSQLDataStr('char',CID,'name',Value);
end;

function TCharacter.GetClass : byte;
begin
	Result := GetSQLDataInt('char',CID,'class');
end;

procedure TCharacter.SetClass(Value : byte);
begin
	SetSQLDataInt('char',CID,'class',Value);
end;

function TCharacter.GetBaseLV : byte;
begin
	Result := GetSQLDataInt('char',CID,'base_level');
end;

procedure TCharacter.SetBaseLV(Value : byte);
begin
	SetSQLDataInt('char',CID,'base_level',Value);
end;

function TCharacter.GetJobLV : byte;
begin
	Result := GetSQLDataInt('char',CID,'job_level');
end;

procedure TCharacter.SetJobLV(Value : byte);
begin
	SetSQLDataInt('char',CID,'job_level',Value);
end;

function TCharacter.GetBaseEXP : byte;
begin
	Result := GetSQLDataInt('char',CID,'base_exp');
end;

procedure TCharacter.SetBaseEXP(Value : byte);
begin
	SetSQLDataInt('char',CID,'base_exp',Value);
end;

function TCharacter.GetJobEXP : byte;
begin
	Result := GetSQLDataInt('char',CID,'job_exp');
end;

procedure TCharacter.SetJobEXP(Value : byte);
begin
	SetSQLDataInt('char',CID,'job_exp',Value);
end;

function TCharacter.GetZeny : Cardinal;
begin
	Result := GetSQLDataInt('char',CID,'zeny');
end;

procedure TCharacter.SetZeny(Value : Cardinal);
begin
	SetSQLDataInt('char',CID,'zeny',Value);
end;

function TCharacter.GetBaseStats : TParamBase;
begin
	Result.STR := GetSQLDataInt('char',CID,'STR');
	Result.AGI := GetSQLDataInt('char',CID,'AGI');
	Result.VIT := GetSQLDataInt('char',CID,'VIT');
	Result.INT := GetSQLDataInt('char',CID,'INT');
	Result.DEX := GetSQLDataInt('char',CID,'DEX');
	Result.LUK := GetSQLDataInt('char',CID,'LUK');
end;

procedure TCharacter.SetBaseStats(Value : TParamBase);
begin
	SetSQLDataInt('char',CID,'STR',Value.STR);
	SetSQLDataInt('char',CID,'AGI',Value.AGI);
	SetSQLDataInt('char',CID,'VIT',Value.VIT);
	SetSQLDataInt('char',CID,'INT',Value.INT);
	SetSQLDataInt('char',CID,'DEX',Value.DEX);
	SetSQLDataInt('char',CID,'LUK',Value.LUK);
end;

function  TCharacter.GetMaxHP : word;
begin
	Result := GetSQLDataInt('char',CID,'max_hp');
end;

procedure TCharacter.SetMaxHP(Value : word);
begin
	SetSQLDataInt('char',CID,'max_hp',Value);
end;

function  TCharacter.GetHP : word;
begin
	Result := GetSQLDataInt('char',CID,'hp');
end;

procedure TCharacter.SetHP(Value : word);
begin
	SetSQLDataInt('char',CID,'hp',Value);
end;

function  TCharacter.GetMaxSP : word;
begin
	Result := GetSQLDataInt('char',CID,'max_sp');
end;

procedure TCharacter.SetMaxSP(Value : word);
begin
	SetSQLDataInt('char',CID,'max_sp',Value);
end;

function  TCharacter.GetSP : word;
begin
	Result := GetSQLDataInt('char',CID,'sp');
end;

procedure TCharacter.SetSP(Value : word);
begin
	SetSQLDataInt('char',CID,'sp',Value);
end;

function TCharacter.GetStatusPts : Word;
begin
	Result := GetSQLDataInt('char',CID,'status_point');
end;

procedure TCharacter.SetStatusPts(Value : Word);
begin
	SetSQLDataInt('char',CID,'status_point',Value);
end;

function TCharacter.GetSkillPts : Word;
begin
	Result := GetSQLDataInt('char',CID,'skill_point');
end;

procedure TCharacter.SetSkillPts(Value : Word);
begin
	SetSQLDataInt('char',CID,'skill_point',Value);
end;



function TCharacter.GetMap : string;
begin
	Result := GetSQLDataStr('char',CID,'last_map');
end;

procedure TCharacter.SetMap(Value : string);
begin
	SetSQLDataStr('char',CID,'last_map',Value);
end;

function TCharacter.GetMapPt : TPoint;
begin
	Result.X := GetSQLDataInt('char',CID,'last_x');
	Result.Y := GetSQLDataInt('char',CID,'last_y');
end;

procedure TCharacter.SetMapPt(Value : TPoint);
begin
	SetSQLDataInt('char',CID,'last_x',Value.X);
	SetSQLDataInt('char',CID,'last_y',Value.Y);
end;

function TCharacter.GetSMap : string;
begin
	Result := GetSQLDataStr('char',CID,'save_map');
end;

procedure TCharacter.SetSMap(Value : string);
begin
	SetSQLDataStr('char',CID,'save_map',Value);
end;

function TCharacter.GetSMapPt : TPoint;
begin
	Result.X := GetSQLDataInt('char',CID,'save_x');
	Result.Y := GetSQLDataInt('char',CID,'save_y');
end;

procedure TCharacter.SetSMapPt(Value : TPoint);
begin
	SetSQLDataInt('char',CID,'save_x',Value.X);
	SetSQLDataInt('char',CID,'save_y',Value.Y);
end;

function TCharacter.CreateInSQL(AID : Cardinal; NName : string) : cardinal;
var
	Success : boolean;
begin
	Result := 0;
	SQLConnection.query(
		Format('INSERT INTO `char` (account_id, name) VALUES(%d, "%s");',
		[AID,NName])
	,true,Success);
	if Success then
	begin
		SQLQueryResult :=
				SQLConnection.query(
				Format('SELECT char_id FROM `char` WHERE account_id = %d AND name = "%s";',
				[AID,NName])
			,true,Success);
		if (SQLQueryResult.RowsCount = 1) then
		begin
			Result := StrToInt(SQLQueryResult.FieldValue(0));
		end;
	end;
end;

end.
