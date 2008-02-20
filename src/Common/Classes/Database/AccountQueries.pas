//------------------------------------------------------------------------------
//AccountQueries			                                                           UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Account related database routines
//
//	Changes -
//		February 11th, 2008
//
//------------------------------------------------------------------------------
unit AccountQueries;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	{Project}
	Account,
	QueryBase,
	{3rd Party}
	ZSQLUpdate
	;


type

//------------------------------------------------------------------------------
//TAccountQueries                                                         CLASS
//------------------------------------------------------------------------------
	TAccountQueries = class(TQueryBase)

	protected


	public
		Function Exists(
			const AnAccount : TAccount
		) : Boolean;

		Procedure Load(
			const AnAccount : TAccount
		);

		Procedure Save(
			const AnAccount : TAccount
		);

		Procedure New(
			const AnAccount : TAccount
		);

		procedure Refresh(
		const
			AnAccount : TAccount
		);
	end;
//------------------------------------------------------------------------------


implementation


uses
	{RTL/VCL}
	SysUtils,
	{Project}
	Main,
	Globals,
	{3rd Party}
	ZDataset,
	DB
	//none
	;

//------------------------------------------------------------------------------
//Load							                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Loads an account
//
//	Changes -
//		February 11th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TAccountQueries.Load(
	const AnAccount : TAccount
);

const
	AQuery =
		'SELECT id, name, password, last_login, login_count, gender, email_address, '+
		'login_key_1, login_key_2, level, connect_until, banned_until, last_ip, state'+
		' FROM accounts';

var
	WhereClause : String;
	ADataSet		: TZQuery;
	AParam			: TParam;
begin
	if AnAccount.ID > 0 then
	begin
		WhereClause := ' WHERE id=:ID;';
	end else
	begin
		WhereClause := ' WHERE name=:Name;'
	end;

	ADataSet			:= TZQuery.Create(nil);
	//ID
	AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
	AParam.AsInteger := AnAccount.ID;
	ADataSet.Params.AddParam(
		AParam
	);
	//Name
	AParam := ADataset.Params.CreateParam(ftString, 'Name', ptInput);
	AParam.AsString := AnAccount.Name;
	ADataSet.Params.AddParam(
		AParam
	);
	//

	try
		Query(ADataSet, AQuery+WhereClause);
		ADataSet.First;
		if NOT ADataSet.Eof then
		begin
			//fill character data
			AnAccount.ID						:= ADataSet.Fields[0].AsInteger;
			AnAccount.Name					:= ADataSet.Fields[1].AsString;
			AnAccount.Password			:= ADataSet.Fields[2].AsString;
			//Tsusai - For Gender, we need to return the first char, thats
			//why there is a [1]
			AnAccount.LastLoginTime := ADataSet.Fields[3].AsDateTime;
			AnAccount.LoginCount		:= ADataSet.Fields[4].AsInteger;
			AnAccount.Gender				:= (ADataSet.Fields[5].AsString)[1];
			AnAccount.EMail					:= ADataSet.Fields[6].AsString;
			AnAccount.LoginKey[1]		:= ADataSet.Fields[7].AsInteger;
			AnAccount.LoginKey[2]		:= ADataSet.Fields[8].AsInteger;
			AnAccount.Level					:= ADataSet.Fields[9].AsInteger;
			AnAccount.ConnectUntil	:= ADataSet.Fields[10].AsDateTime;
			AnAccount.BannedUntil		:= ADataSet.Fields[11].AsDateTime;
			AnAccount.LastIP				:= ADataSet.Fields[12].AsString;
			AnAccount.State					:= ADataSet.Fields[13].AsInteger;
		end;
	finally
		ADataSet.Free;
	end;

	Inherited;
end;//Load
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Exists							                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Checks to see if an account exists
//
//	Changes -
//		February 11th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
Function TAccountQueries.Exists(
	const AnAccount : TAccount
) : Boolean;

const
	AQuery =
		'SELECT id FROM accounts';

var
	WhereClause : String;
	ADataSet		: TZQuery;
	AParam			: TParam;
begin
	Result := TRUE;

	if AnAccount.ID > 0 then
	begin
		WhereClause := ' WHERE id=:ID';
	end else
	begin
		WhereClause := ' WHERE name=:Name'
	end;

	ADataSet			:= TZQuery.Create(nil);
	//ID
	AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
	AParam.AsInteger := AnAccount.ID;
	ADataSet.Params.AddParam(
		AParam
	);
	//Name
	AParam := ADataset.Params.CreateParam(ftString, 'Name', ptInput);
	AParam.AsString := AnAccount.Name;
	ADataSet.Params.AddParam(
		AParam
	);

	try
		Query(ADataSet, AQuery+WhereClause);
		ADataset.First;
		if ADataSet.Eof then
		begin
			Result := FALSE;
		end;

	finally
		ADataSet.Free;
	end;
end;//Exists
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Save				 						      																			 PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//		Save an account.
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TAccountQueries.Save(
	const
		AnAccount : TAccount
	);
const
	AQuery =
		'UPDATE accounts SET '+
		'name=:Name ,' +
		'password=:Password , ' +
		'last_login=:LastLogin , ' +
		'gender=:Gender , ' +
		'login_count=:LoginCount , ' +
		'email_address=:EmailAddress , ' +
		'login_key_1=:LoginKey1 , ' +
		'login_key_2=:LoginKey2 , ' +
		'connect_until=:ConnectUntil , ' +
		'banned_until=:BannedUntil , ' +
		'last_ip=:LastIp, ' +
		'state=:State ' +
		'WHERE id=:ID;';
var
	ADataSet		: TZQuery;
	AParam			: TParam;
begin

	ADataSet			:= TZQuery.Create(nil);
	//ID
	AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
	AParam.AsInteger := AnAccount.ID;
	ADataSet.Params.AddParam(
		AParam
	);
	//Name
	AParam := ADataset.Params.CreateParam(ftString, 'Name', ptInput);
	AParam.AsString := AnAccount.Name;
	ADataSet.Params.AddParam(
		AParam
	);
	//Password
	AParam := ADataset.Params.CreateParam(ftString, 'Password', ptInput);
	AParam.AsString := AnAccount.Password;
	ADataSet.Params.AddParam(
		AParam
	);
	//LastLogin
	AParam := ADataset.Params.CreateParam(ftTimestamp, 'LastLogin', ptInput);
	AParam.AsString := FormatDateTime('yyyy-mm-dd hh:mm:ss',AnAccount.LastLoginTime);
	ADataSet.Params.AddParam(
		AParam
	);
	//Gender
	AParam := ADataset.Params.CreateParam(ftString, 'Gender', ptInput);
	AParam.AsString := AnAccount.Gender;
	ADataSet.Params.AddParam(
		AParam
	);
	//LoginCount
	AParam := ADataset.Params.CreateParam(ftInteger, 'LoginCount', ptInput);
	AParam.AsInteger := AnAccount.LoginCount;
	ADataSet.Params.AddParam(
		AParam
	);
	//EmailAddress
	AParam := ADataset.Params.CreateParam(ftString, 'EmailAddress', ptInput);
	AParam.AsString := AnAccount.EMail;
	ADataSet.Params.AddParam(
		AParam
	);
	//LoginKey1
	AParam := ADataset.Params.CreateParam(ftInteger, 'LoginKey1', ptInput);
	AParam.AsInteger := AnAccount.LoginKey[1];
	ADataSet.Params.AddParam(
		AParam
	);
	//LoginKey2
	AParam := ADataset.Params.CreateParam(ftInteger, 'LoginKey2', ptInput);
	AParam.AsInteger := AnAccount.LoginKey[2];
	ADataSet.Params.AddParam(
		AParam
	);
	//ConnectUntil
	AParam := ADataset.Params.CreateParam(ftTimestamp, 'ConnectUntil', ptInput);
	AParam.AsString := FormatDateTime('yyyy-mm-dd hh:mm:ss',AnAccount.ConnectUntil);
	ADataSet.Params.AddParam(
		AParam
	);
	//BannedUntil
	AParam := ADataset.Params.CreateParam(ftTimestamp, 'BannedUntil', ptInput);
	AParam.AsString := FormatDateTime('yyyy-mm-dd hh:mm:ss',AnAccount.BannedUntil);
	ADataSet.Params.AddParam(
		AParam
	);
	//LastIp
	AParam := ADataset.Params.CreateParam(ftString, 'LastIp', ptInput);
	AParam.AsString := AnAccount.LastIP;
	ADataSet.Params.AddParam(
		AParam
	);
	//State
	AParam := ADataset.Params.CreateParam(ftInteger, 'State', ptInput);
	AParam.AsInteger := AnAccount.State;
	ADataSet.Params.AddParam(
		AParam
	);

	try
		QueryNoResult(ADataSet, AQuery);

	finally
		ADataSet.Free;
	end;

end;//Save
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//New					                                                       PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Creates an account.
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TAccountQueries.New(
const
	AnAccount : TAccount
);
const
	AQuery =
		'INSERT INTO accounts '+
		'(name, password, gender) '+
		'VALUES'+
		'(:Name , :Password, :Gender);';
var
	ADataSet		: TZQuery;
	AParam			: TParam;
begin

	ADataSet			:= TZQuery.Create(nil);
	//Name
	AParam := ADataset.Params.CreateParam(ftString, 'Name', ptInput);
	AParam.AsString := AnAccount.Name;
	ADataSet.Params.AddParam(
		AParam
	);
	//Password
	AParam := ADataset.Params.CreateParam(ftString, 'Password', ptInput);
	AParam.AsString := AnAccount.Password;
	ADataSet.Params.AddParam(
		AParam
	);
	//Gender
	AParam := ADataset.Params.CreateParam(ftString, 'Gender', ptInput);
	AParam.AsString := AnAccount.Gender;
	ADataSet.Params.AddParam(
		AParam
	);
	try
		QueryNoResult(ADataSet, AQuery);

	finally
		ADataSet.Free;
	end;

end;//New
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Refresh																								               Procedure
//------------------------------------------------------------------------------
//	What it does-
//			Refreshes some account data
//
//	Changes -
//		February 12th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
procedure TAccountQueries.Refresh(
	const
		AnAccount : TAccount
	);
const
	AQuery =
	 'SELECT login_key_1, login_key_2, connect_until, banned_until '+
	 'FROM accounts WHERE id=:ID';
var
	ADataSet		: TZQuery;
	AParam			: TParam;
begin

	ADataSet			:= TZQuery.Create(nil);
	try
		//ID
		AParam := ADataset.Params.CreateParam(ftInteger, 'ID', ptInput);
		AParam.AsInteger := AnAccount.ID;
		ADataSet.Params.AddParam(
			AParam
		);

		Query(ADataSet, AQuery);
		ADataSet.First;
		if NOT ADataSet.Eof then
		begin
			AnAccount.LoginKey[1]		:= ADataSet.Fields[0].AsInteger;
			AnAccount.LoginKey[2]		:= ADataSet.Fields[1].AsInteger;
			AnAccount.ConnectUntil	:= ADataSet.Fields[2].AsDateTime;
			AnAccount.BannedUntil		:= ADataSet.Fields[3].AsDateTime;
		end;
	finally
		ADataSet.Free;
	end;
end;//Refresh
//------------------------------------------------------------------------------

end.
