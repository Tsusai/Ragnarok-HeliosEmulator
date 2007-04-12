//------------------------------------------------------------------------------
//MSSQL	                                                        					UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Contains a wrapper class for MSSQL database interface.
//
//	Header From the original author...
//-----------------------------------------------------
//Import routines and some constants for MS SQL server.
//32-bit only... Use Freely, but I warranty nothing.
//Ed Lyk 1997-08-27
//elyk@sprynet.com
//-----------------------------------------------------
//
//	Changes -
//		March 1st, 2007 - Tsusai - Loads DLL dynamically, Loads FreeTDS equiv for linux
//		March 2nd, 2007 - RaX - Created.
//		[2007/03/28] CR - Cleaned up uses clauses, using Icarus as a guide.
//
//------------------------------------------------------------------------------
unit MSSQLClient;


interface


type
//------------------------------------------------------------------------------
//TMSSQLClient                                                         CLASS
//------------------------------------------------------------------------------
//	What it does-
//			An interface to the mssql databasing system.
//	Changes -
//		March 2nd, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
	TMSSQLClient = class
	public
		Loaded			: Boolean;

		Constructor	Create();
		Destructor	Destroy();override;
		
	private
		MSSQLLib		:  THandle;

	end;
//------------------------------------------------------------------------------


implementation


uses
	{RTL/VCL}
	{$IFDEF MSWINDOWS}
	Windows,
	{$ENDIF}
	{Project}
	Globals
	{3rd Party}
	//none
	;


	type
		//mssql client dll types.
		DBPROCESS = Pointer;
		LOGINREC = Pointer;
		DBCURSOR = Pointer;

	const
		//mssql client dll constants
		DBSETHOST  = 1;
		DBSETUSER  = 2;
		DBSETPWD   = 3;
		DBSETAPP   = 4;
		DBSETID    = 5;
		DBSETLANG  = 6;
		DBSUCCEED  = 1;
		DBFAIL     = 0;
		INTBIND    = 3;
		CHARBIND   = 4;
		BINARYBIND = 5;
		DATETIMEBIND = 7;
		STRINGBIND = 10;
		NTBSTRINGBIND = 11;
		VARYCHARBIND = 12;
		VARYBINBIND = 13;
		MORE_ROWS   = -1;
		NO_MORE_ROWS = -2;
		BUF_FULL  = -3;
		NO_MORE_RESULTS = 2;
		REG_ROW = MORE_ROWS;
		DBRPCRETURN = 1;
		SQLTEXT = $23;
		SQLVARBINARY = $25;
		SQLVARCHAR = $27;
		SQLBINARY = $2d;
		SQLIMAGE =  $22;
		SQLCHAR =  $2f;
		SQLINT1 =  $30;
		SQLINT2 = $34;
		SQLINT4 = $38;
		SQLINTN = $26;
		SQLDATETIME = $3d;
		SQLFLT8 = $3e;
		SQLFLTN = $6d;
		SQLMONEYN = $6e;
		SQLDATETIMN = $6f;

		{$IFDEF MSWINDOWS}
		SQLDLL = 'ntwdblib.dll';
		{$ENDIF}
		{$IFDEF LINUX}
		//I believe this is right.  Need FreeTDS
		SQLDLL = 'libsybdb.so';
		{$ENDIF}

	var
		//mssql client dll routines.
		dblogin     : function : LOGINREC; cdecl;
		dbopen      : function (login: LOGINREC; ServerName: PChar): DBPROCESS; cdecl;
		dbinit      : function : PChar; cdecl;
		dbexit      : procedure; cdecl;
		dbsetlname  : function (login: LOGINREC; val: PChar; item: Integer): Integer; cdecl;
		dbfreelogin : procedure (login: LOGINREC); cdecl;
		dbclose     : procedure (proc: DBPROCESS); cdecl;
		dbcmd       : function (proc: DBPROCESS; cmd: PChar): Integer; cdecl;
		dbsqlexec   : function (proc: DBPROCESS): Integer; cdecl;
		dbresults   : function (proc: DBPROCESS): Integer; cdecl;
		dbbind      : function (proc: DBPROCESS; Column, VarType, VarLen: Integer;
											VarAddr: Pointer):Integer; cdecl;
		dbnextrow   : function (proc: DBPROCESS): Integer; cdecl;
		dbdata      : function (proc: DBPROCESS; column: Integer): Pointer; cdecl;
		dbdatlen    : function (proc: DBPROCESS; column: Integer): Integer; cdecl;
		dbrpcinit   : function (proc: DBPROCESS; ProcName: PChar; Options: Integer
									): Integer; cdecl;
		dbrpcparam  : function (proc: DBPROCESS; paramname: PChar; status: Byte;
											ptype: Integer; maxlen, datalen: Integer;
											value: Pointer): Integer; cdecl;
		dbrpcsend   : function (proc: DBPROCESS): Integer; cdecl;
		dbsqlok     : function (proc: DBPROCESS): Integer; cdecl;
		dbretdata   : function (proc: DBPROCESS; column: Integer): Pointer; cdecl;
		dbrpcexec   : function (proc: DBPROCESS): Integer; cdecl;
		dbuse       : function (proc: DBPROCESS; DBName: PChar): Integer; cdecl;

//------------------------------------------------------------------------------
//Create()								                                          CONSTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Initializes our interface.
//
//	Changes -
//		March 2nd, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Constructor TMSSQLClient.Create();
begin
	inherited;

	//Load our library, then setup our routines in that library.
	MSSQLLib := LoadLibrary(SQLDLL);
	//If our library loaded successfully...
	if MSSQLLib <> 0 then
	begin
		@dblogin 		:= GetProcAddress(MSSQLLib, 'dblogin');
		if NOT Assigned(@dblogin) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dblogin routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbopen			:= GetProcAddress(MSSQLLib, 'dbopen');
		if NOT Assigned(@dbopen) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbopen routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbinit			:= GetProcAddress(MSSQLLib, 'dbinit');
		if NOT Assigned(@dbinit) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbinit routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbexit			:= GetProcAddress(MSSQLLib, 'dbexit');
		if NOT Assigned(@dbexit) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbexit routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbsetlname	:= GetProcAddress(MSSQLLib, 'dbsetlname');
		if NOT Assigned(@dbsetlname) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbsetlname routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbfreelogin:= GetProcAddress(MSSQLLib, 'dbfreelogin');
		if NOT Assigned(@dbfreelogin) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbfreelogin routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbclose		:= GetProcAddress(MSSQLLib, 'dbclose');
		if NOT Assigned(@dbclose) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbclose routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbcmd			:= GetProcAddress(MSSQLLib, 'dbdbcmd');
		if NOT Assigned(@dbcmd) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbcmd routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbsqlexec	:= GetProcAddress(MSSQLLib, 'dbsqlexec');
		if NOT Assigned(@dbsqlexec) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbsqlexec routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbresults	:= GetProcAddress(MSSQLLib, 'dbresults');
		if NOT Assigned(@dbresults) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbresults routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbbind			:= GetProcAddress(MSSQLLib, 'dbbind');
		if NOT Assigned(@dbbind) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbbind routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbnextrow	:= GetProcAddress(MSSQLLib, 'dbnextrow');
		if NOT Assigned(@dbnextrow) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbnextrow routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbdata			:= GetProcAddress(MSSQLLib, 'dbdata');
		if NOT Assigned(@dbdata) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbdata routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbdatlen		:= GetProcAddress(MSSQLLib, 'dbdatlen');
		if NOT Assigned(@dbdatlen) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbdatlen routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbrpcinit	:= GetProcAddress(MSSQLLib, 'dbrpcinit');
		if NOT Assigned(@dbrpcinit) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbrpcinit routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbrpcparam	:= GetProcAddress(MSSQLLib, 'dbrpcparam');
		if NOT Assigned(@dbrpcparam) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbrpcparam routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbrpcsend	:= GetProcAddress(MSSQLLib, 'dbrpcsend');
		if NOT Assigned(@dbrpcsend) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbrpcsend routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbsqlok		:= GetProcAddress(MSSQLLib, 'dbsqlok');
		if NOT Assigned(@dbsqlok) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbsqlok routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbretdata	:= GetProcAddress(MSSQLLib, 'dbretdata');
		if NOT Assigned(@dbretdata) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbretdata routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbrpcexec	:= GetProcAddress(MSSQLLib, 'dbrpcexec');
		if NOT Assigned(@dbrpcexec) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbrpcexec routine!' +
											' Please update your libraries or Helios will not function.');
		end;
		@dbuse			:= GetProcAddress(MSSQLLib, 'dbuse');
		if NOT Assigned(@dbuse) then
		begin
			Console.Message('The Library, '+SQLDLL+', does not contain the dbuse routine!' +
											' Please update your libraries or Helios will not function.');
		end;

		Loaded 			:= TRUE;
	end else
	begin
  	//Since it did not, we throw an error message and set Loaded to false.
		Console.Message(
			'Cannot find '+SQLDLL+', please ensure the library exists before using' +
			' MSSQL as a database type.',
			'Database',
			MS_ERROR
		);
		
		Loaded			:= FALSE;
	end;

end;//Create
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//Destroy()								                                          DESTRUCTOR
//------------------------------------------------------------------------------
//	What it does-
//			Destroys our interface.
//
//	Changes -
//		March 2nd, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
Destructor TMSSQLClient.Destroy();
begin
	if MSSQLLib <> 0 then
	begin
		FreeLibrary(MSSQLLib);
  end;
	inherited;
end;//Destroy
//------------------------------------------------------------------------------

end.
