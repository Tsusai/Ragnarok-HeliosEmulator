//------------------------------------------------------------------------------
//CharacterConstantQueries						                                                   UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Character related database routines
//
//	Changes -
//		February 12th, 2008
//
//------------------------------------------------------------------------------
unit CharacterConstantQueries;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	Classes,
	{Project}
	Character,
	Being,
	QueryBase,
	{3rd Party}
	ZSQLUpdate
	;


type

//------------------------------------------------------------------------------
//TCharacterConstantQueries                                                       CLASS
//------------------------------------------------------------------------------
	TCharacterConstantQueries = class(TQueryBase)

	protected


	public
		Function  GetMaxHP(
			const ACharacter : TCharacter
		) : LongWord;

		Function  GetMaxSP(
			const ACharacter : TCharacter
		) : LongWord;

		Function  GetMaxWeight(
			const ACharacter : TCharacter
		) : LongWord;

		Function  GetBaseEXPToNextLevel(
			const ACharacter		: TCharacter;
			Level : LongWord
		) : LongWord;

		Function  GetJobEXPToNextLevel(
			const ACharacter		: TCharacter;
			Level : LongWord
		) : LongWord;

		Function GetStatPoints(
			const Level : LongWord
		) : LongWord;

		Function GetSkillPoints(
			const ACharacter		: TCharacter;
			Level : LongWord
		) : LongWord;

		Function GetJobBonus(
			const ACharacter		: TCharacter;
			Level : LongWord
		) : StatArray;

	end;
//------------------------------------------------------------------------------


implementation


uses
	{RTL/VCL}
	SysUtils,
	Types,
	{Project}
	Main,
	Globals,
	GameConstants,
	{3rd Party}
	ZDataset,
	DB
	//none
	;


//------------------------------------------------------------------------------
//GetMaxHP				          		                                      FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets the max hp for a job at a level
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
function TCharacterConstantQueries.GetMaxHP(
	const ACharacter: TCharacter
) : LongWord;

var
	ADataSet		: TZQuery;
	AParam			: TParam;
	AQuery			: String;
begin

	AQuery :=
		'SELECT '+ACharacter.JobName+' FROM hp WHERE level=:Level;';
	Result := 0;
	ADataSet			:= TZQuery.Create(nil);
	try
		//Level
		AParam := ADataset.Params.CreateParam(ftInteger, 'Level', ptInput);
		AParam.AsInteger := ACharacter.BaseLV;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		if ADataSet.RowsAffected > 0 then
		begin
			Result := ADataset.Fields[0].AsInteger;
		end;


	finally
		ADataSet.Free;
	end;
end;//GetMaxHP
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetMaxSP				          		                                      FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets the max sp for a job at a level
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
function TCharacterConstantQueries.GetMaxSP(
	const ACharacter: TCharacter
) : LongWord;

var
	ADataSet		: TZQuery;
	AParam			: TParam;
	AQuery			: String;
begin

	AQuery :=
		'SELECT '+ACharacter.JobName+' FROM sp WHERE level=:Level;';
	Result := 0;
	ADataSet			:= TZQuery.Create(nil);
	try
		//JobName
		AParam := ADataset.Params.CreateParam(ftString, 'JobName', ptInput);
		AParam.AsString := ACharacter.JobName;
		ADataSet.Params.AddParam(
			AParam
		);
		//Level
		AParam := ADataset.Params.CreateParam(ftInteger, 'Level', ptInput);
		AParam.AsInteger := ACharacter.BaseLV;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		if ADataSet.RowsAffected > 0 then
		begin
			Result := ADataset.Fields[0].AsInteger;
		end;


	finally
		ADataSet.Free;
	end;
end;//GetMaxSP
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetMaxWeight		          		                                      FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets the max weight for a job at a level
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
function TCharacterConstantQueries.GetMaxWeight(
	const ACharacter: TCharacter
) : LongWord;

var
	ADataSet		: TZQuery;
	AParam			: TParam;
	AQuery			: String;
begin

	AQuery :=
		'SELECT '+ACharacter.JobName+' FROM weight WHERE level=:Level;';
	Result := 0;
	ADataSet			:= TZQuery.Create(nil);
	try
		//Level
		AParam := ADataset.Params.CreateParam(ftInteger, 'Level', ptInput);
		AParam.AsInteger := ACharacter.BaseLV;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		if ADataSet.RowsAffected > 0 then
		begin
			Result := ADataset.Fields[0].AsInteger;
		end;


	finally
		ADataSet.Free;
	end;
end;//GetMaxWeight
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetBaseEXPToNextLevel      		                                      FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets the base experience for a job at a level
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
function TCharacterConstantQueries.GetBaseEXPToNextLevel(
	const ACharacter		: TCharacter;
	Level								: LongWord
) : LongWord;

var
	ADataSet		: TZQuery;
	AParam			: TParam;
	AQuery			: String;
begin

	AQuery :=
		'SELECT '+ACharacter.JobName+' FROM baseexperience WHERE level=:Level;';
	Result := 0;
	ADataSet			:= TZQuery.Create(nil);
	try
		//Level
		AParam := ADataset.Params.CreateParam(ftInteger, 'Level', ptInput);
		AParam.AsInteger := Level;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		if ADataSet.RowsAffected > 0 then
		begin
			Result := ADataset.Fields[0].AsInteger;
		end;


	finally
		ADataSet.Free;
	end;
end;//GetBaseExpToNextLevel
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetJobEXPToNextLevel       		                                      FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets the job experience for a job at a level
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
function TCharacterConstantQueries.GetJobEXPToNextLevel(
	const ACharacter		: TCharacter;
	Level								: LongWord
) : LongWord;

var
	ADataSet		: TZQuery;
	AParam			: TParam;
	AQuery			: String;
begin

	AQuery :=
		'SELECT '+ACharacter.JobName+' FROM jobexperience WHERE level=:Level;';
	Result := 0;
	ADataSet			:= TZQuery.Create(nil);
	try
		//Level
		AParam := ADataset.Params.CreateParam(ftInteger, 'Level', ptInput);
		AParam.AsInteger := Level;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		if ADataSet.RowsAffected > 0 then
		begin
			Result := ADataset.Fields[0].AsInteger;
		end;


	finally
		ADataSet.Free;
	end;
end;//GetJobExpToNextLevel
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetStatPoints				       		                                      FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets the stat poitns for a level
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
function TCharacterConstantQueries.GetStatPoints(
	const Level								: LongWord
) : LongWord;

const
	AQuery =
		'SELECT points FROM statpoints WHERE level=:Level;';

var
	ADataSet		: TZQuery;
	AParam			: TParam;
begin
	Result := 0;
	ADataSet			:= TZQuery.Create(nil);
	try
		//Level
		AParam := ADataset.Params.CreateParam(ftInteger, 'Level', ptInput);
		AParam.AsInteger := Level;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		if ADataSet.RowsAffected > 0 then
		begin
			Result := ADataset.Fields[0].AsInteger;
		end;


	finally
		ADataSet.Free;
	end;
end;//GetStatPoints
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetSkillPoints			      		                                      FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets the skill points for a job level
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
function TCharacterConstantQueries.GetSkillPoints(
	const ACharacter		: TCharacter;
	Level								: LongWord
) : LongWord;

var
	ADataSet		: TZQuery;
	AParam			: TParam;
	AQuery			: String;
begin

	AQuery :=
		'SELECT '+ACharacter.JobName+' FROM skillpoints WHERE level=:Level;';
	Result := 0;
	ADataSet			:= TZQuery.Create(nil);
	try
		//Level
		AParam := ADataset.Params.CreateParam(ftInteger, 'Level', ptInput);
		AParam.AsInteger := Level;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		if ADataSet.RowsAffected > 0 then
		begin
			Result := ADataset.Fields[0].AsInteger;
		end;


	finally
		ADataSet.Free;
	end;
end;//GetSkillPoints
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//GetJobBonus			      		                                      FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			Gets the job bonus for a level
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
function TCharacterConstantQueries.GetJobBonus(
	const ACharacter		: TCharacter;
	Level								: LongWord
) : StatArray;

var
	ADataSet		: TZQuery;
	AParam			: TParam;
	AStringList : TStringList;
	AQuery			: String;
begin
	AQuery :=
		'SELECT '+ACharacter.JobName+' FROM jobbonus WHERE level=:Level;';
	ADataSet			:= TZQuery.Create(nil);
	try
		//Level
		AParam := ADataset.Params.CreateParam(ftInteger, 'Level', ptInput);
		AParam.AsInteger := Level;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		if ADataSet.RowsAffected > 0 then
		begin
			AStringList := TStringList.Create;
			try
				AStringList.Delimiter := ',';
				AStringList.QuoteChar := '"';
				AStringList.DelimitedText := ADataset.Fields[0].AsString;
				if AStringList.Count = 6 then
				begin
					Result[0] := StrToIntDef(AStringList.Strings[0], 0);
					Result[1] := StrToIntDef(AStringList.Strings[1], 0);
					Result[2] := StrToIntDef(AStringList.Strings[2], 0);
					Result[3] := StrToIntDef(AStringList.Strings[3], 0);
					Result[4] := StrToIntDef(AStringList.Strings[4], 0);
					Result[5] := StrToIntDef(AStringList.Strings[5], 0);
				end else
				begin
					Console.Message('jobbonus Database Error. Count of parameters should be 6, actual value : '
						+IntToStr(AStringList.Count)+ ' for job '+ACharacter.JobName
						+' at level '+IntToStr(level)+'. Please fix this and try again.', 'Database', MS_ERROR);
					Result[0] := 0;
					Result[1] := 0;
					Result[2] := 0;
					Result[3] := 0;
					Result[4] := 0;
					Result[5] := 0;
				end;
			finally
				AStringList.Free;
			end;
		end else
		begin
			Result[0] := 0;
			Result[1] := 0;
			Result[2] := 0;
			Result[3] := 0;
			Result[4] := 0;
			Result[5] := 0;
		end;

	finally
		ADataSet.Free;
	end;
end;//GetJobBonus
//------------------------------------------------------------------------------
end.
