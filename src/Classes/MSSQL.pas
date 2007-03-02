//------------------------------------------------------------------------------
//MSSQL	                                                        					UNIT
//------------------------------------------------------------------------------
//	What it does-
//			Contains a wrapper class for MSSQL database interface.
//
//	Changes -
//		March 2nd, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
unit MSSQL;

interface
type
//------------------------------------------------------------------------------
//TMSSQLInterface                                                         CLASS
//------------------------------------------------------------------------------
//	What it does-
//			This is a child class for our database object system. It allows Helios
//    to communicate with a MySQL database and houses all routines for doing so.
//
//	Changes -
//		March 2nd, 2007 - RaX - Created.
//
//------------------------------------------------------------------------------
	TMSSQLInterface = class
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
	{$IFDEF MSWINDOWS}
		Windows,
	{$ENDIF}
		Globals,
		mssql32_mod,
		SysUtils;

		
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
Constructor TMSSQLInterface.Create();
begin
	inherited;

	//Load our library, then setup our routines in that library.
	MSSQLLib := LoadLibrary(SQLDLL);
	//If our library loaded successfully...
	if MSSQLLib <> 0 then
	begin
		@dblogin 		:= GetProcAddress(MSSQLLib, 'dblogin');
		@dbopen			:= GetProcAddress(MSSQLLib, 'dbopen');
		@dbinit			:= GetProcAddress(MSSQLLib, 'dbinit');
		@dbexit			:= GetProcAddress(MSSQLLib, 'dbexit');
		@dbsetlname	:= GetProcAddress(MSSQLLib, 'dbsetlname');
		@dbfreelogin:= GetProcAddress(MSSQLLib, 'dbfreelogin');
		@dbclose		:= GetProcAddress(MSSQLLib, 'dbclose');
		@dbcmd			:= GetProcAddress(MSSQLLib, 'dbdbcmd');
		@dbsqlexec	:= GetProcAddress(MSSQLLib, 'dbsqlexec');
		@dbresults	:= GetProcAddress(MSSQLLib, 'dbresults');
		@dbbind			:= GetProcAddress(MSSQLLib, 'dbbind');
		@dbnextrow	:= GetProcAddress(MSSQLLib, 'dbnextrow');
		@dbdata			:= GetProcAddress(MSSQLLib, 'dbdata');
		@dbdatlen		:= GetProcAddress(MSSQLLib, 'dbdatlen');
		@dbrpcinit	:= GetProcAddress(MSSQLLib, 'dbrpcinit');
		@dbrpcparam	:= GetProcAddress(MSSQLLib, 'dbrpcparam');
		@dbrpcsend	:= GetProcAddress(MSSQLLib, 'dbrpcsend');
		@dbsqlok		:= GetProcAddress(MSSQLLib, 'dbsqlok');
		@dbretdata	:= GetProcAddress(MSSQLLib, 'dbretdata');
		@dbrpcexec	:= GetProcAddress(MSSQLLib, 'dbrpcexec');
		@dbuse			:= GetProcAddress(MSSQLLib, 'dbuse');

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
Destructor TMSSQLInterface.Destroy();
begin
	if MSSQLLib <> 0 then
	begin
		FreeLibrary(MSSQLLib);
  end;
	inherited;
end;//Destroy
//------------------------------------------------------------------------------

end.
