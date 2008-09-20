//------------------------------------------------------------------------------
//QueryBase                                                                 UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Base class for query collections
//
//	Changes -
//		February 11th, 2008
//
//------------------------------------------------------------------------------
unit QueryBase;

{$IFDEF FPC}
	{$MODE Delphi}
{$ENDIF}


interface


uses
	{RTL/VCL}
	{Project}
	{3rd Party}
	ZConnection,
	ZSqlUpdate,
	ZDataset
	;


type

//------------------------------------------------------------------------------
//TQueryBase                                                               CLASS
//------------------------------------------------------------------------------
	TQueryBase = class(TObject)

	protected
		Connection								: TZConnection;

	public

		Constructor Create(
			AConnection							: TZConnection
		);

		Destructor  Destroy
		(

		);override;

		Procedure Query(
			ADataSet								: TZQuery;
			const AQuery						: String
		);

		Procedure QueryNoResult(
			ADataSet					: TZQuery;
			const AQuery			: String
		);

		function LastID(
			const Field : String;
			const Table : String;
			const Append : String = ''
		):LongWord;
	end;
//------------------------------------------------------------------------------


implementation


uses
	{RTL/VCL}
	SysUtils,
	Classes
	{Project}
	{3rd Party}
	//none
	;


//------------------------------------------------------------------------------
//Create                                                             CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Builds our object.
//
//	Changes -
//		February 11th, 2008 - RaX - Created
//
//------------------------------------------------------------------------------
Constructor TQueryBase.Create(
	AConnection : TZConnection
);
Begin
	Inherited Create;
	Connection := AConnection;
End;{Create}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//Destroy                                                             DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Frees up our custom properties.
//
//	Changes -
//		February 11th, 2008 - RaX - Created.
//
//------------------------------------------------------------------------------
Destructor TQueryBase.Destroy(

);
begin

	Inherited;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Query                                                                PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Queries the database
//
//	Changes -
//		February 11th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TQueryBase.Query(
	ADataSet					: TZQuery;
	const AQuery			: String
);
var
	Pinged : Boolean;
begin

	try
		Pinged := Connection.Ping;
	except
		Pinged := TRUE;
	end;

	if NOT Pinged then
	begin
		try
			Connection.Connect;
		except
			Connection.Disconnect;
		end;
	end;

	if Connection.Connected then
	begin
		ADataSet.Connection	:= Connection;
		ADataSet.SQL.Add(AQuery);
		ADataSet.ExecSQL;
		ADataSet.Open;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//QueryNoResult                                                        PROCEDURE
//------------------------------------------------------------------------------
//	What it does-
//			Queries the database with no result
//
//	Changes -
//		February 11th, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Procedure TQueryBase.QueryNoResult(
	ADataSet					: TZQuery;
	const AQuery			: String
);
var
	Pinged : Boolean;
begin

	try
		Pinged := Connection.Ping;
	except
		Pinged := TRUE;
	end;

	if NOT Pinged then
	begin
		try
			Connection.Connect;
		except
			Connection.Disconnect;
		end;
	end;

	if Connection.Connected then
	begin
		ADataSet.Connection	:= Connection;
		ADataSet.SQL.Add(AQuery);
		ADataSet.ExecSQL;
	end;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//LastID                                                                FUNCTION
//------------------------------------------------------------------------------
//	What it does-
//		Get "last insert ID" by finx max
//
//	Changes -
//		[2008/09/20] Aeomin - Created
//
//------------------------------------------------------------------------------
function TQueryBase.LastID(
	const Field : String;
	const Table : String;
	const Append : String = ''
):LongWord;
var
	AQuery		: String;
	ADataSet	: TZQuery;
begin
	Result := 0;

	AQuery := 'SELECT MAX('+Field+') FROM '+Table+' '+Append + ';';
	
	ADataSet	:= TZQuery.Create(nil);
	try
		Query(ADataSet, AQuery);
		ADataSet.First;
		if NOT ADataSet.Eof then
		begin
			Result := ADataSet.Fields[0].AsInteger;
		end;
	finally
		ADataSet.Free;
	end;
end;{LastID}
//------------------------------------------------------------------------------
end.
