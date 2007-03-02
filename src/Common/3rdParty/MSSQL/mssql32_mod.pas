unit mssql32_mod;

{ Import routines and some constants for MS SQL server.
  32-bit only... Use Freely, but I warranty nothing.
  Ed Lyk  1997-08-27
  elyk@sprynet.com
}

interface

//uses
	//SysUtils, Windows, Messages, Classes, Forms, Dialogs;

type

  DBPROCESS = Pointer;
  LOGINREC = Pointer; 
  DBCURSOR = Pointer; 

const
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
dbbind      : function (proc: DBPROCESS; Column, VarType, VarLen: Integer; VarAddr: Pointer):Integer; cdecl;
dbnextrow   : function (proc: DBPROCESS): Integer; cdecl;
dbdata      : function (proc: DBPROCESS; column: Integer): Pointer; cdecl;
dbdatlen    : function (proc: DBPROCESS; column: Integer): Integer; cdecl;
dbrpcinit   : function (proc: DBPROCESS; ProcName: PChar; Options: Integer): Integer; cdecl;
dbrpcparam  : function (proc: DBPROCESS; paramname: PChar; status: Byte; ptype: Integer; maxlen, datalen: Integer;
										value: Pointer): Integer; cdecl;
dbrpcsend   : function (proc: DBPROCESS): Integer; cdecl;
dbsqlok     : function (proc: DBPROCESS): Integer; cdecl;
dbretdata   : function (proc: DBPROCESS; column: Integer): Pointer; cdecl;
dbrpcexec   : function (proc: DBPROCESS): Integer; cdecl;
dbuse       : function (proc: DBPROCESS; DBName: PChar): Integer; cdecl;

implementation



end.
