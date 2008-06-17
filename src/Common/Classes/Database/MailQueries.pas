unit MailQueries;

interface

uses
	{RTL/VCL}
	Contnrs,
	{Project}
	MailBox,
	QueryBase,
	{3rd Party}
	ZSqlUpdate
	;

type
//------------------------------------------------------------------------------
//TMailQueries                                                             CLASS
//------------------------------------------------------------------------------
	TMailQueries = class(TQuerybase)
	protected
	public
		procedure LoadMails(
			const AMailList : TObjectList;
			const CharID : LongWord;
			var AMails,ANewmails :Byte
		);
		function Get(
			const CharID : LongWord;
			const MailID : LongWord
		):TMail;
	end;

implementation

uses
	ZDataset,
	DB
	;

//------------------------------------------------------------------------------
//LoadMails                                                            PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Load mail list
//
//	Changes -
//		[2008/06/11] Aeomin - Created
//
//------------------------------------------------------------------------------
procedure TMailQueries.LoadMails(
	const AMailList : TObjectList;
	const CharID : LongWord;
	var AMails,ANewmails :Byte
);
const
	AQuery =
		'SELECT * FROM mailbox WHERE `receiver_id`=:ID ORDER BY `time` DESC LIMIT 30;';
var
	ADataSet	: TZQuery;
	AParam		: TParam;
	Mail		: TMail;
begin
	{Reset First}
	AMails := 0;
	ANewmails := 0;
	if AMailList.Count >0 then
		AMailList.Clear;
	ADataSet	:= TZQuery.Create(nil);
	//ID
	AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
	AParam.AsInteger := CharID;
	ADataSet.Params.AddParam(
		AParam
	);
	try
		Query(ADataSet, AQuery);
		ADataSet.First;
		while NOT ADataSet.Eof do
		begin
			Mail := TMail.Create;
			Mail.ID := ADataSet.Fields[0].AsInteger;
			Mail.SenderID := ADataSet.Fields[1].AsInteger;
			Mail.SenderName := ADataSet.Fields[2].AsString;
			Mail.ReceiverID := ADataSet.Fields[3].AsInteger;
			Mail.ReceiverName := ADataSet.Fields[4].AsString;
			Mail.Title := ADataSet.Fields[5].AsString;
			Mail.SendTime := ADataSet.Fields[7].AsInteger;
			Mail.Read := Boolean(ADataSet.Fields[8].AsInteger);
			if not Mail.Read then
				Inc(ANewMails);
			AMailList.Add(Mail);
			ADataSet.Next;
		end;
		AMails := AMailList.Count;
	finally
		ADataSet.Free;
	end;
end;{LoadMails}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Get                                                                  PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Get a mail
//
//	Changes -
//		[2008/06/12] Aeomin - Created
//
//------------------------------------------------------------------------------
function TMailQueries.Get(
	const CharID : LongWord;
	const MailID : LongWord
):TMail;
const
	AQuery =
		'SELECT * FROM mailbox WHERE `id`=:ID AND `receiver_id`=:CID';
	AUpdateQuery =
		'UPDATE mailbox SET `read`=1 WHERE `id`=:ID AND `receiver_id`=:CID;';
var
	ADataSet	: TZQuery;
	ADataSet2		: TZQuery;
	AParam		: TParam;
begin
	Result := nil;
	ADataSet	:= TZQuery.Create(nil);
	ADataSet2	:= TZQuery.Create(nil);
	//ID
	AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
	AParam.AsInteger := MailID;
	ADataSet.Params.AddParam(
		AParam
	);
	AParam := ADataset2.Params.CreateParam(ftInteger, 'ID', ptInput);
	AParam.AsInteger := MailID;
	ADataSet2.Params.AddParam(
		AParam
	);
	//CID
	AParam := ADataset.Params.CreateParam(ftInteger, 'CID', ptInput);
	AParam.AsInteger := CharID;
	ADataSet.Params.AddParam(
		AParam
	);
	AParam := ADataset2.Params.CreateParam(ftInteger, 'CID', ptInput);
	AParam.AsInteger := CharID;
	ADataSet2.Params.AddParam(
		AParam
	);
	try
		Query(ADataSet, AQuery);
		ADataSet.First;
		if NOT ADataSet.Eof then
		begin
			Result := TMail.Create;
			Result.ID := ADataSet.Fields[0].AsInteger;
			Result.SenderID := ADataSet.Fields[1].AsInteger;
			Result.SenderName := ADataSet.Fields[2].AsString;
			Result.ReceiverID := ADataSet.Fields[3].AsInteger;
			Result.ReceiverName := ADataSet.Fields[4].AsString;
			Result.Title := ADataSet.Fields[5].AsString;
			Result.Content := ADataSet.Fields[6].AsString;
			Result.SendTime := ADataSet.Fields[7].AsInteger;
			Result.Read := Boolean(ADataSet.Fields[8].AsInteger);
			QueryNoResult(ADataSet2, AUpdateQuery);
		end;
	finally
		ADataSet.Free;
		ADataSet2.Free;
	end;
end;{Get}
//------------------------------------------------------------------------------
end.