//------------------------------------------------------------------------------
//FriendQueries			                                                   UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Friend related database routines
//
//	Changes -
//		February 12th, 2008
//
//------------------------------------------------------------------------------
unit FriendQueries;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	{Project}
	Character,
	QueryBase,
	BeingList,
	{3rd Party}
	ZSqlUpdate
	;


type

//------------------------------------------------------------------------------
//TFriendQueries                                                       CLASS
//------------------------------------------------------------------------------
	TFriendQueries = class(TQueryBase)

	protected


	public
		procedure  LoadList(
			const ACharacterList	: TBeingList;
			const ACharacter			: TCharacter
		);

		procedure Delete(
			const ReqID     : LongWord;
			const CharID    : LongWord
		);

		procedure Add(
			const OrigID   : LongWord;
			const CharID   : LongWord
		);

		function IsFriend(
			const CharID   : LongWord;
			const TargetID : LongWord
		):Boolean;
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
//LoadList						                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Loads a character's friends
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TFriendQueries.LoadList(
	const ACharacterList : TBeingList;
	const ACharacter : TCharacter
);

const
	GetFriendListQuery =
		'SELECT `contacter_id`, `contactee_id` FROM characterfriendships '+
		'WHERE contacter_id=:ID OR contactee_id=:ID;';

	GetCharacterQuery =
		'SELECT `account_id`, `name`, `last_map` FROM characters WHERE id=:FriendID;';

var
	ADataSet		: TZQuery;
	ADataSet2		: TZQuery;
	AParam			: TParam;
	AFriend			: TCharacter;
begin
	ADataSet			:= TZQuery.Create(nil);
	ADataSet2			:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := ACharacter.ID;
		ADataSet.Params.AddParam(
			AParam
		);
		Query(ADataSet, GetFriendListQuery);

		ADataset.First;
		while NOT ADataSet.Eof do
		begin
			AFriend := TCharacter.Create(ACharacter.ClientInfo);
			if LongWord(ADataSet.Fields[0].AsInteger) = ACharacter.ID then
			begin
				AFriend.ID := ADataSet.Fields[1].AsInteger;
			end else
			begin
				AFriend.ID := ADataSet.Fields[0].AsInteger;
			end;

			//FriendID
			AParam := ADataset2.Params.CreateParam(ftInteger, 'FriendID', ptInput);
			AParam.AsInteger := AFriend.ID;
			ADataSet2.Params.AddParam(
				AParam
			);

			Query(ADataSet2, GetCharacterQuery);
			ADataset2.First;
			if NOT ADataSet2.Eof then
			begin
				AFriend.AccountID	:= ADataSet2.Fields[0].AsInteger;
				AFriend.Name			:= ADataSet2.Fields[1].AsString;
				AFriend.Map		:= ADataSet2.Fields[2].AsString;
				ACharacterList.Add(AFriend);
			end else
			begin
				AFriend.Free;
			end;

			ADataSet2.Params.Clear;
			ADataSet2.EmptyDataSet;
			ADataSet.Next;
		end;
	finally
		ADataSet.Free;
		ADataSet2.Free;
	end;
end;//LoadList
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Delete						                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Deletes a friend
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TFriendQueries.Delete(
	const ReqID			: LongWord;
	const CharID		: LongWord
);

const
	AQuery =
		'DELETE FROM characterfriendships WHERE '+
		'(contacter_id=:ID AND contactee_id=:OtherID) OR '+
		'(contacter_id=:OtherID AND contactee_id=:ID);';

var
	ADataSet		: TZQuery;
	AParam			: TParam;
begin

	ADataSet			:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := CharID;
		ADataSet.Params.AddParam(
			AParam
		);
		//OtherID
		AParam := ADataset.Params.CreateParam(ftInteger, 'OtherID', ptInput);
		AParam.AsInteger := ReqID;
		ADataSet.Params.AddParam(
			AParam
		);

		QueryNoResult(ADataSet, AQuery);


	finally
		ADataSet.Free;
	end;
end;//Delete
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Add								                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Add a friend
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TFriendQueries.Add(
	const OrigID			: LongWord;
	const CharID			: LongWord
);

const
	AQuery =
		'INSERT INTO characterfriendships '+
		'(contacter_id, contactee_id) '+
		'VALUES (:ID, :OtherID);';

var
	ADataSet		: TZQuery;
	AParam			: TParam;
begin

	ADataSet			:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := OrigID;
		ADataSet.Params.AddParam(
			AParam
		);
		//OtherID
		AParam := ADataset.Params.CreateParam(ftInteger, 'OtherID', ptInput);
		AParam.AsInteger := CharID;
		ADataSet.Params.AddParam(
			AParam
		);

		QueryNoResult(ADataSet, AQuery);


	finally
		ADataSet.Free;
	end;
end;//Add
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//IsFriend						                                                FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//			checks to see if 2 char ids are friends
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
function TFriendQueries.IsFriend(
	const CharID			: LongWord;
	const TargetID		: LongWord
) : Boolean;

const
	AQuery =
		'SELECT contacter_id, contactee_id FROM characterfriendships WHERE '+
		'(contacter_id=:ID AND contactee_id=:OtherID) OR '+
		'(contacter_id=:OtherID AND contactee_id=:ID);';

var
	ADataSet		: TZQuery;
	AParam			: TParam;
begin
	Result := FALSE;
	ADataSet			:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := CharID;
		ADataSet.Params.AddParam(
			AParam
		);
		//OtherID
		AParam := ADataset.Params.CreateParam(ftInteger, 'OtherID', ptInput);
		AParam.AsInteger := TargetID;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		ADataset.First;
		if NOT ADataSet.Eof then
		begin
			Result := TRUE;
    end;
		

	finally
		ADataSet.Free;
	end;
end;//Add
//------------------------------------------------------------------------------
end.
