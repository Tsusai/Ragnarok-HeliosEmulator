//------------------------------------------------------------------------------
//MapQueries						                                                   UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Map related database routines
//
//	Changes -
//		February 12th, 2008
//
//------------------------------------------------------------------------------
unit MapQueries;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	Classes,
	{Project}
	QueryBase,
	MapTypes,
	{3rd Party}
	ZSqlUpdate
	;


type

//------------------------------------------------------------------------------
//TMapQueries                                                       CLASS
//------------------------------------------------------------------------------
	TMapQueries = class(TQueryBase)

	protected


	public
		Function  CantSave(
			const MapName : String
		) : Boolean;

		Function  GetZoneID(
			const MapName : string
		): Integer;

		Procedure  LoadFlags(
			var Flags : TFlags;
			const MapName : String
		);

		Procedure  LoadList(
			const MapList: TStringList;
			const ZoneID : LongWord
		);
	end;
//------------------------------------------------------------------------------


implementation


uses
	{RTL/VCL}
	SysUtils,
	Types,
	{Project}
	{3rd Party}
	ZDataset,
	DB
	//none
	;


//------------------------------------------------------------------------------
//CantSave						                                                FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			checks to see if we can save on a map
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
function TMapQueries.CantSave(
	const MapName : String
) : Boolean;

const
	AQuery =
		'SELECT no_return_on_dc FROM maps WHERE map_name=:MapName;';

var
	ADataSet		: TZQuery;
	AParam			: TParam;
	NoReturnOnDC: Integer;
begin
	Result := TRUE;
	ADataSet			:= TZQuery.Create(nil);
	try
		//MapName
		AParam := ADataset.Params.CreateParam(ftString, 'MapName', ptInput);
		AParam.AsString := MapName;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		ADataset.First;
		if NOT ADataSet.Eof then
		begin
			noReturnOnDC := ADataset.Fields[0].AsInteger;
			if NoReturnOnDC = 0 then
			begin
				Result := FALSE;
			end;
		end;


	finally
		ADataSet.Free;
	end;
end;//CantSave
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetZoneID						                                                FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets a map's zone id
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
function TMapQueries.GetZoneID(
	const MapName : String
) : integer;

const
	AQuery =
		'SELECT zone_id FROM maps WHERE map_name=:MapName;';

var
	ADataSet		: TZQuery;
	AParam			: TParam;

begin
	Result := 0;
	ADataSet			:= TZQuery.Create(nil);
	try
		//MapName
		AParam := ADataset.Params.CreateParam(ftString, 'MapName', ptInput);
		AParam.AsString := MapName;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		ADataset.First;
		if NOT ADataSet.Eof then
		begin
			Result := ADataset.Fields[0].AsInteger;
		end;


	finally
		ADataSet.Free;
	end;
end;//GetZoneID
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadFlags						                                                FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Loads a map's flags
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TMapQueries.LoadFlags(
	var Flags				: TFlags;
	const MapName		: string
);

const
	AQuery =
		'SELECT memo, no_return_on_dc, teleport, item_drop, exp_loss, pvp, '+
		'pvp_nightmare, guild_pvp, items, skill, dead_branches, fly_wings, '+
		'butterfly_wings, turbo_track, no_party, no_guild, weather FROM maps '+
		'WHERE map_name=:MapName;';

var
	ADataSet		: TZQuery;
	AParam			: TParam;
	Weather			: Integer;
begin
  Weather := 0;
	ADataSet			:= TZQuery.Create(nil);
	try
		//MapName
		AParam := ADataset.Params.CreateParam(ftString, 'MapName', ptInput);
		AParam.AsString := MapName;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		ADataSet.First;
		if NOT ADataSet.Eof then
		begin
			Flags.Memo							:= Boolean(ADataset.Fields[0].AsInteger);
			Flags.NoReturnOnDC			:= Boolean(ADataset.Fields[1].AsInteger);
			Flags.Teleport					:= Boolean(ADataset.Fields[2].AsInteger);
			Flags.ItemDrop					:= Boolean(ADataset.Fields[3].AsInteger);
			Flags.ExpLoss						:= Boolean(ADataset.Fields[4].AsInteger);
			Flags.PvP								:= Boolean(ADataset.Fields[5].AsInteger);
			Flags.PvPNightmare			:= Boolean(ADataset.Fields[6].AsInteger);
			Flags.GuildPvP					:= Boolean(ADataset.Fields[7].AsInteger);
			Flags.Items							:= Boolean(ADataset.Fields[8].AsInteger);
			Flags.Skill							:= Boolean(ADataset.Fields[9].AsInteger);
			Flags.DeadBranches			:= Boolean(ADataset.Fields[10].AsInteger);
			Flags.FlyWings					:= Boolean(ADataset.Fields[11].AsInteger);
			Flags.ButterflyWings		:= Boolean(ADataset.Fields[12].AsInteger);
			Flags.TurboTrack				:= Boolean(ADataset.Fields[13].AsInteger);
			Flags.NoParty						:= Boolean(ADataset.Fields[14].AsInteger);
			Flags.NoGuild						:= Boolean(ADataset.Fields[15].AsInteger);
			Weather									:= ADataset.Fields[16].AsInteger;
		end;
		//initialize weather
		Flags.Rain   := FALSE;
		Flags.Snow   := FALSE;
		Flags.Sakura := FALSE;
		Flags.Fog    := FALSE;
		Flags.Leaves := FALSE;
		Flags.Smog   := FALSE;

		//Figure out weather.
		case Weather of
			1 : Flags.Rain     := TRUE;
			2 : Flags.Snow     := TRUE;
			3 : Flags.Sakura   := TRUE;
			4 : Flags.Fog      := TRUE;
			5 : Flags.Leaves   := TRUE;
			6 : Flags.Smog     := TRUE;
		end;


	finally
		ADataSet.Free;
	end;
end;//LoadFlags
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LoadList						                                                FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Loads a map list for a zone
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TMapQueries.LoadList(
	const MapList						: TStringList;
	const ZoneID						: LongWord
);

const
	AQuery =
		'SELECT map_name FROM maps '+
		'WHERE zone_id=:ID;';

var
	ADataSet		: TZQuery;
	AParam			: TParam;
begin

	ADataSet			:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftString, 'ID', ptInput);
		AParam.AsInteger:= ZoneID;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		ADataSet.First;
		while NOT ADataSet.Eof do
		begin
			MapList.Add(ADataset.Fields[0].AsString);
			ADataset.Next;
		end;


	finally
		ADataSet.Free;
	end;
end;//GetZoneID
//------------------------------------------------------------------------------
end.
