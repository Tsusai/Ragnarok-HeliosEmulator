unit SQLite3;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{
	Helios Changes
	*All library calls are now dynamic.  Uses loadlibrary to assign
	 the dll commands w/o forcing the server to have the dll.
}
{
	Simplified interface for SQLite.
	Updated for Sqlite 3 by Tim Anderson (tim@itwriting.com)
	Note: NOT COMPLETE for version 3, just minimal functionality
	Adapted from file created by Pablo Pissanetzky (pablo@myhtpc.net)
	which was based on SQLite.pas by Ben Hochstrasser (bhoc@surfeu.ch)
}

interface

const
	{$IFDEF MSWINDOWS}
	SQLiteDLL = 'sqlite3.dll';
	{$ENDIF}
	{$IFDEF LINUX}
	SQLiteDLL = 'libsqlite3.so';
	{$ENDIF}

// Return values for sqlite3_exec() and sqlite3_step()

	SQLITE_OK = 0; // Successful result
	SQLITE_ERROR = 1; // SQL error or missing database
	SQLITE_INTERNAL = 2; // An internal logic error in SQLite
	SQLITE_PERM = 3; // Access permission denied
	SQLITE_ABORT = 4; // Callback routine requested an abort
	SQLITE_BUSY = 5; // The database file is locked
	SQLITE_LOCKED = 6; // A table in the database is locked
	SQLITE_NOMEM = 7; // A malloc() failed
	SQLITE_READONLY = 8; // Attempt to write a readonly database
	SQLITE_INTERRUPT = 9; // Operation terminated by sqlite3_interrupt()
	SQLITE_IOERR = 10; // Some kind of disk I/O error occurred
	SQLITE_CORRUPT = 11; // The database disk image is malformed
	SQLITE_NOTFOUND = 12; // (Internal Only) Table or record not found
	SQLITE_FULL = 13; // Insertion failed because database is full
	SQLITE_CANTOPEN = 14; // Unable to open the database file
	SQLITE_PROTOCOL = 15; // Database lock protocol error
	SQLITE_EMPTY = 16; // Database is empty
	SQLITE_SCHEMA = 17; // The database schema changed
	SQLITE_TOOBIG = 18; // Too much data for one row of a table
	SQLITE_CONSTRAINT = 19; // Abort due to contraint violation
	SQLITE_MISMATCH = 20; // Data type mismatch
	SQLITE_MISUSE = 21; // Library used incorrectly
	SQLITE_NOLFS = 22; // Uses OS features not supported on host
	SQLITE_AUTH = 23; // Authorization denied
	SQLITE_FORMAT = 24; // Auxiliary database format error
	SQLITE_RANGE = 25; // 2nd parameter to sqlite3_bind out of range
	SQLITE_NOTADB = 26; // File opened that is not a database file
	SQLITE_ROW = 100; // sqlite3_step() has another row ready
	SQLITE_DONE = 101; // sqlite3_step() has finished executing

	SQLITE_INTEGER = 1;
	SQLITE_FLOAT = 2;
	SQLITE_TEXT = 3;
	SQLITE_BLOB = 4;
	SQLITE_NULL = 5;

	function LoadLibs(DLLHandle : THandle) : boolean;
	procedure FreeLibs(DLLHandle : THandle);

type
	TSQLiteDB = Pointer;
	TSQLiteResult = ^PChar;
	TSQLiteStmt = Pointer;

var
	SQLite3_Open: function(dbname: PChar; var db: TSqliteDB): integer; cdecl;
	SQLite3_Close: function(db: TSQLiteDB): integer; cdecl;
	SQLite3_Exec: function(db: TSQLiteDB; SQLStatement: PChar; CallbackPtr: Pointer; Sender: TObject; var ErrMsg: PChar): integer; cdecl;
	SQLite3_Version: function(): PChar; cdecl;
	SQLite3_ErrMsg: function(db: TSQLiteDB): PChar; cdecl;
	SQLite3_ErrCode: function(db: TSQLiteDB): integer; cdecl;
	SQlite3_Free: procedure(P: PChar); cdecl;
	SQLite3_GetTable: function(db: TSQLiteDB; SQLStatement: PChar; var ResultPtr: TSQLiteResult; var RowCount: Cardinal; var ColCount: Cardinal; var ErrMsg: PChar): integer; cdecl;
	SQLite3_FreeTable: procedure(Table: TSQLiteResult); cdecl;
	SQLite3_Complete: function(P: PChar): boolean; cdecl;
	SQLite3_LastInsertRowID: function(db: TSQLiteDB): int64; cdecl;
	SQLite3_Interrupt: procedure(db: TSQLiteDB); cdecl;
	SQLite3_BusyHandler: procedure(db: TSQLiteDB; CallbackPtr: Pointer; Sender: TObject); cdecl;
	SQLite3_BusyTimeout: procedure(db: TSQLiteDB; TimeOut: integer); cdecl;
	SQLite3_Changes: function(db: TSQLiteDB): integer; cdecl;
	SQLite3_TotalChanges: function(db: TSQLiteDB): integer; cdecl;
	SQLite3_Prepare: function(db: TSQLiteDB; SQLStatement: PChar; nBytes: integer; var hStmt: TSqliteStmt; var pzTail: PChar): integer; cdecl;
	SQLite3_ColumnCount: function(hStmt: TSqliteStmt): integer; cdecl;
	Sqlite3_ColumnName: function(hStmt: TSqliteStmt; ColNum: integer): pchar; cdecl;
	Sqlite3_ColumnDeclType: function(hStmt: TSqliteStmt; ColNum: integer): pchar; cdecl;
	Sqlite3_Step: function(hStmt: TSqliteStmt): integer; cdecl;
	SQLite3_DataCount: function(hStmt: TSqliteStmt): integer; cdecl;

	Sqlite3_ColumnBlob: function(hStmt: TSqliteStmt; ColNum: integer): pointer; cdecl;
	Sqlite3_ColumnBytes: function(hStmt: TSqliteStmt; ColNum: integer): integer; cdecl;
	Sqlite3_ColumnDouble: function(hStmt: TSqliteStmt; ColNum: integer): double; cdecl;
	Sqlite3_ColumnInt: function(hStmt: TSqliteStmt; ColNum: integer): integer; cdecl;
	Sqlite3_ColumnText: function(hStmt: TSqliteStmt; ColNum: integer): pchar; cdecl;
	Sqlite3_ColumnType: function(hStmt: TSqliteStmt; ColNum: integer): integer; cdecl;
	Sqlite3_ColumnInt64: function(hStmt: TSqliteStmt; ColNum: integer): Int64; cdecl;
	SQLite3_Finalize: function(hStmt: TSqliteStmt): integer; cdecl;
	SQLite3_Reset: function(hStmt: TSqliteStmt): integer; cdecl;

//
// In the SQL strings input to sqlite3_prepare() and sqlite3_prepare16(),
// one or more literals can be replace by a wildcard "?" or ":N:" where
// N is an integer.  These value of these wildcard literals can be set
// using the routines listed below.
//
// In every case, the first parameter is a pointer to the sqlite3_stmt
// structure returned from sqlite3_prepare().  The second parameter is the
// index of the wildcard.  The first "?" has an index of 1.  ":N:" wildcards
// use the index N.
//
 // The fifth parameter to sqlite3_bind_blob(), sqlite3_bind_text(), and
 //sqlite3_bind_text16() is a destructor used to dispose of the BLOB or
//text after SQLite has finished with it.  If the fifth argument is the
// special value SQLITE_STATIC, then the library assumes that the information
// is in static, unmanaged space and does not need to be freed.  If the
// fifth argument has the value SQLITE_TRANSIENT, then SQLite makes its
// own private copy of the data.
//
// The sqlite3_bind_* routine must be called before sqlite3_step() after
// an sqlite3_prepare() or sqlite3_reset().  Unbound wildcards are interpreted
// as NULL.
//

	SQLite3_BindBlob :
		function(hStmt: TSqliteStmt; ParamNum: integer; ptrData: pointer; numBytes: integer;
		ptrDestructor: pointer
	): integer; cdecl;

	function SQLiteFieldType(SQLiteFieldTypeCode: Integer): AnsiString;
	function SQLiteErrorStr(SQLiteErrorCode: Integer): AnsiString;

implementation

uses
	{$IFDEF MSWINDOWS}
	Windows,
	{$ENDIF}
	SysUtils;

function SQLiteFieldType(SQLiteFieldTypeCode: Integer): AnsiString;
begin
	case SQLiteFieldTypeCode of
		SQLITE_INTEGER: Result := 'Integer';
		SQLITE_FLOAT: Result := 'Float';
		SQLITE_TEXT: Result := 'Text';
		SQLITE_BLOB: Result := 'Blob';
		SQLITE_NULL: Result := 'Null';
	else
		Result := 'Unknown SQLite Field Type Code "' + IntToStr(SQLiteFieldTypeCode) + '"';
	end;
end;

function SQLiteErrorStr(SQLiteErrorCode: Integer): AnsiString;
begin
	case SQLiteErrorCode of
		SQLITE_OK: Result := 'Successful result';
		SQLITE_ERROR: Result := 'SQL error or missing database';
		SQLITE_INTERNAL: Result := 'An internal logic error in SQLite';
		SQLITE_PERM: Result := 'Access permission denied';
		SQLITE_ABORT: Result := 'Callback routine requested an abort';
		SQLITE_BUSY: Result := 'The database file is locked';
		SQLITE_LOCKED: Result := 'A table in the database is locked';
		SQLITE_NOMEM: Result := 'A malloc() failed';
		SQLITE_READONLY: Result := 'Attempt to write a readonly database';
		SQLITE_INTERRUPT: Result := 'Operation terminated by sqlite3_interrupt()';
		SQLITE_IOERR: Result := 'Some kind of disk I/O error occurred';
		SQLITE_CORRUPT: Result := 'The database disk image is malformed';
		SQLITE_NOTFOUND: Result := '(Internal Only) Table or record not found';
		SQLITE_FULL: Result := 'Insertion failed because database is full';
		SQLITE_CANTOPEN: Result := 'Unable to open the database file';
		SQLITE_PROTOCOL: Result := 'Database lock protocol error';
		SQLITE_EMPTY: Result := 'Database is empty';
		SQLITE_SCHEMA: Result := 'The database schema changed';
		SQLITE_TOOBIG: Result := 'Too much data for one row of a table';
		SQLITE_CONSTRAINT: Result := 'Abort due to contraint violation';
		SQLITE_MISMATCH: Result := 'Data type mismatch';
		SQLITE_MISUSE: Result := 'Library used incorrectly';
		SQLITE_NOLFS: Result := 'Uses OS features not supported on host';
		SQLITE_AUTH: Result := 'Authorization denied';
		SQLITE_FORMAT: Result := 'Auxiliary database format error';
		SQLITE_RANGE: Result := '2nd parameter to sqlite3_bind out of range';
		SQLITE_NOTADB: Result := 'File opened that is not a database file';
		SQLITE_ROW: Result := 'sqlite3_step() has another row ready';
		SQLITE_DONE: Result := 'sqlite3_step() has finished executing';
	else
		Result := 'Unknown SQLite Error Code "' + IntToStr(SQLiteErrorCode) + '"';
	end;
end;

function ColValueToStr(Value: PChar): AnsiString;
begin
	if (Value = nil) then
		Result := 'NULL'
	else
		Result := Value;
end;

function LoadLibs(DLLHandle : THandle) : boolean;
begin
	Result := False;
	DLLHandle := LoadLibrary(SQLiteDLL);
	if DLLHandle <> 0 then
	begin
		@SQLite3_Open := GetProcAddress(DLLHandle, 'sqlite3_open');
		if not Assigned(@SQLite3_Open) then exit;
		@SQLite3_Close := GetProcAddress(DLLHandle, 'sqlite3_close');
		if not Assigned(@SQLite3_Close) then exit;
		@SQLite3_Exec := GetProcAddress(DLLHandle, 'sqlite3_exec');
		if not Assigned(@SQLite3_Exec) then exit;
		@SQLite3_Version := GetProcAddress(DLLHandle, 'sqlite3_libversion');
		if not Assigned(@SQLite3_Version) then exit;
		@SQLite3_ErrMsg := GetProcAddress(DLLHandle, 'sqlite3_errmsg');
		if not Assigned(@SQLite3_ErrMsg) then exit;
		@SQLite3_ErrCode := GetProcAddress(DLLHandle, 'sqlite3_errcode');
		if not Assigned(@SQLite3_ErrCode) then exit;
		@SQlite3_Free := GetProcAddress(DLLHandle, 'sqlite3_free');
		if not Assigned(@SQlite3_Free) then exit;
		@SQLite3_GetTable := GetProcAddress(DLLHandle, 'sqlite3_get_table');
		if not Assigned(@SQLite3_GetTable) then exit;
		@SQLite3_FreeTable := GetProcAddress(DLLHandle, 'sqlite3_free_table');
		if not Assigned(@SQLite3_FreeTable) then exit;
		@SQLite3_Complete := GetProcAddress(DLLHandle, 'sqlite3_complete');
		if not Assigned(@SQLite3_Complete) then exit;
		@SQLite3_LastInsertRowID := GetProcAddress(DLLHandle, 'sqlite3_last_insert_rowid');
		if not Assigned(@SQLite3_LastInsertRowID) then exit;
		@SQLite3_Interrupt := GetProcAddress(DLLHandle, 'sqlite3_interrupt');
		if not Assigned(@SQLite3_Interrupt) then exit;
		@SQLite3_BusyHandler := GetProcAddress(DLLHandle, 'sqlite3_busy_handler');
		if not Assigned(@SQLite3_BusyHandler) then exit;
		@SQLite3_BusyTimeout := GetProcAddress(DLLHandle, 'sqlite3_busy_timeout');
		if not Assigned(@SQLite3_BusyTimeout) then exit;
		@SQLite3_Changes := GetProcAddress(DLLHandle, 'sqlite3_changes');
		if not Assigned(@SQLite3_Changes) then exit;
		@SQLite3_TotalChanges := GetProcAddress(DLLHandle, 'sqlite3_total_changes');
		if not Assigned(@SQLite3_TotalChanges) then exit;
		@SQLite3_Prepare := GetProcAddress(DLLHandle, 'sqlite3_prepare');
		if not Assigned(@SQLite3_Prepare) then exit;
		@SQLite3_ColumnCount := GetProcAddress(DLLHandle, 'sqlite3_column_count');
		if not Assigned(@SQLite3_ColumnCount) then exit;
		@Sqlite3_ColumnName := GetProcAddress(DLLHandle, 'sqlite3_column_name');
		if not Assigned(@Sqlite3_ColumnName) then exit;
		@Sqlite3_ColumnDeclType := GetProcAddress(DLLHandle, 'sqlite3_column_decltype');
		if not Assigned(@Sqlite3_ColumnDeclType) then exit;
		@Sqlite3_Step := GetProcAddress(DLLHandle, 'sqlite3_step');
		if not Assigned(@Sqlite3_Step) then exit;
		@SQLite3_DataCount := GetProcAddress(DLLHandle, 'sqlite3_data_count');
		if not Assigned(@SQLite3_DataCount) then exit;
		@Sqlite3_ColumnBlob := GetProcAddress(DLLHandle, 'sqlite3_column_blob');
		if not Assigned(@Sqlite3_ColumnBlob) then exit;
		@Sqlite3_ColumnBytes := GetProcAddress(DLLHandle, 'sqlite3_column_bytes');
		if not Assigned(@Sqlite3_ColumnBytes) then exit;
		@Sqlite3_ColumnDouble := GetProcAddress(DLLHandle, 'sqlite3_column_double');
		if not Assigned(@Sqlite3_ColumnDouble) then exit;
		@Sqlite3_ColumnInt := GetProcAddress(DLLHandle, 'sqlite3_column_int');
		if not Assigned(@Sqlite3_ColumnInt) then exit;
		@Sqlite3_ColumnText := GetProcAddress(DLLHandle, 'sqlite3_column_text');
		if not Assigned(@Sqlite3_ColumnText) then exit;
		@Sqlite3_ColumnType := GetProcAddress(DLLHandle, 'sqlite3_column_type');
		if not Assigned(@Sqlite3_ColumnType) then exit;
		@Sqlite3_ColumnInt64 := GetProcAddress(DLLHandle, 'sqlite3_column_int64');
		if not Assigned(@Sqlite3_ColumnInt64) then exit;
		@SQLite3_Finalize := GetProcAddress(DLLHandle, 'sqlite3_finalize');
		if not Assigned(@SQLite3_Finalize) then exit;
		@SQLite3_Reset := GetProcAddress(DLLHandle, 'sqlite3_reset');
		if not Assigned(@SQLite3_Reset) then exit;
		@SQLite3_BindBlob := GetProcAddress(DLLHandle, 'sqlite3_bind_blob');
		if not Assigned(@SQLite3_BindBlob) then exit;
		Result := true;
	end;
end;

procedure FreeLibs(DLLHandle : THandle);
begin
	if DLLHandle <> 0 then FreeLibrary(DLLHandle);
end;


end.

