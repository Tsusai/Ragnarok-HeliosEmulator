{--------------------------------------------------------------------------------
Licencing issues:
13-December-2003      ©Cristian Nicola
Note:
 Mysql is copyright by MySQL AB. Refer to their site ( http://www.mysql.com )
for licencing issues.
 Zlib is copyright by Jean-loup Gailly and Mark Adler. Refer to their site for
licencing issues. ( http://www.info-zip.org/pub/infozip/zlib/ )

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

NOTES:
  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. If you are using it for a commercial software it must be open source and
     it must include full source code of this library in an unaltered fashion
     or you would need to ask for permission to use it. This library will be
     considered donationware which means if you want to contribute with any money
     or hardware you are more than welcome.
  4. This notice may not be removed or altered from any source distribution.

  Cristian Nicola
  n_cristian@hotmail.com

If you use the mysqldirect library in a product, i would appreciate *not*
receiving lengthy legal documents to sign. The sources are provided
for free but without warranty of any kind.  The library has been
entirely written by Cristian Nicola after libmysql of MYSQL AB.
--------------------------------------------------------------------------------}
unit uMysqlCT;
////////////////////////////////////////////////////////////////////////////////
// CT = constants and types
// various constants and types imported from mysql definition

interface

const
  MyObjVer  = '1.2.2'; //direct-mysql objects version
  MYSQL_ERRMSG_SIZE     = 200;
  MYSQL_NAMEDPIPE       = 'MySQL';
  LOCAL_HOST_NAMEDPIPE  = '.';
  MYSQL_PORT            = 3306;
  MYSQL_UNIX_ADDR       = '\\socket\\MySQL';                        // ;06-02 - for unix pipe
  PACKET_ERROR          = -1;
  IO_SIZE               = 4096;
  MAX_ALLOWED_PACKET    = 16*1024*1024;
  NET_HEADER_SIZE       = 4;
  RETRY_COUNT           = 2;
  COMP_HEADER_SIZE      = 3;
  MIN_COMPRESS_LENGTH   = 50;
  NET_BUFFER_LENGTH     = 8192;
  NET_READ_TIMEOUT      = 30;
  NULL_LENGTH               = NOT 0;

  SERVER_STATUS_IN_TRANS    = 1;
  SERVER_STATUS_AUTOCOMMIT  = 2;
  SERVER_STATUS_MORE_RESULTS      = 4;
  SERVER_MORE_RESULTS_EXISTS      = 8;
  SERVER_QUERY_NO_GOOD_INDEX_USED = 16;
  SERVER_QUERY_NO_INDEX_USED      = 32;

  MYSQL_SERVER_VERSION      = '4.1.1';//'3.23.38';
  NAME_LEN                  = 64;
  USERNAME_LENGTH           = 16;
  PROTOCOL_VERSION          = 10;
  //refresh flags
  REFRESH_GRANT     = 1;
  REFRESH_LOG       = 2;
  REFRESH_TABLES    = 4;
  REFRESH_HOSTS     = 8;
  REFRESH_STATUS    = 16;
  REFRESH_THREADS   = 32;
  REFRESH_SLAVE     = 64;
  REFRESH_MASTER    = 128;
  REFRESH_READ_LOCK = 16384;
  REFRESH_FAST      = 32768;
  //client flags
  CLIENT_LONG_PASSWORD    = 1;
  CLIENT_FOUND_ROWS       = 2;
  CLIENT_LONG_FLAG        = 4;
  CLIENT_CONNECT_WITH_DB  = 8;
  CLIENT_NO_SCHEMA        = 16;
  CLIENT_COMPRESS         = 32;
  CLIENT_ODBC             = 64;
  CLIENT_LOCAL_FILES      = 128;
  CLIENT_IGNORE_SPACE     = 256;
  CLIENT_CHANGE_USER      = 512;
  CLIENT_INTERACTIVE      = 1024;
  CLIENT_SSL              = 2048;
  CLIENT_IGNORE_SIGPIPE   = 4096;
  CLIENT_TRANSACTIONS     = 8192;
  CLIENT_RESERVED         = 16384;

  CLIENT_MULTI_STATEMENTS = 65536;
  CLIENT_MULTI_RESULTS    = 131072;
  CLIENT_REMEMBER_OPTIONS = (1 shl 31);

  CLIENT_SECURE_CONNECTION=32768;

  CLIENT_CAPABILITIES     = CLIENT_RESERVED or CLIENT_CHANGE_USER or
                            CLIENT_LONG_PASSWORD or
                            CLIENT_LONG_FLAG or
                            CLIENT_LOCAL_FILES or
                            CLIENT_TRANSACTIONS  or
                            CLIENT_SECURE_CONNECTION;
  // field types
  FIELD_TYPE_DECIMAL      = 0;
  FIELD_TYPE_TINY         = 1;
  FIELD_TYPE_SHORT        = 2;
  FIELD_TYPE_LONG         = 3;
  FIELD_TYPE_FLOAT        = 4;
  FIELD_TYPE_DOUBLE       = 5;
  FIELD_TYPE_NULL         = 6;
  FIELD_TYPE_TIMESTAMP    = 7;
  FIELD_TYPE_LONGLONG     = 8;
  FIELD_TYPE_INT24        = 9;
  FIELD_TYPE_DATE         = 10;
  FIELD_TYPE_TIME         = 11;
  FIELD_TYPE_DATETIME     = 12;
  FIELD_TYPE_YEAR         = 13;
  FIELD_TYPE_NEWDATE      = 14;
  FIELD_TYPE_NEWDECIMAL   = 246;
  FIELD_TYPE_ENUM         = 247;
  FIELD_TYPE_SET          = 248;
  FIELD_TYPE_TINY_BLOB    = 249;
  FIELD_TYPE_MEDIUM_BLOB  = 250;
  FIELD_TYPE_LONG_BLOB    = 251;
  FIELD_TYPE_BLOB         = 252;
  FIELD_TYPE_VAR_STRING   = 253;
  FIELD_TYPE_STRING       = 254;
  FIELD_TYPE_GEOMETRY     = 255;
  FIELD_TYPE_CHAR         = FIELD_TYPE_TINY;
  FIELD_TYPE_INTERVAL     = FIELD_TYPE_ENUM;
  //field flags
  NOT_NULL_FLAG       = 1;
  PRI_KEY_FLAG        = 2;
  UNIQUE_KEY_FLAG     = 4;
  MULTIPLE_KEY_FLAG   = 8;
  BLOB_FLAG           = 16;
  UNSIGNED_FLAG       = 32;
  ZEROFILL_FLAG       = 64;
  BINARY_FLAG         = 128;
  ENUM_FLAG           = 256;
  AUTO_INCREMENT_FLAG = 512;
  TIMESTAMP_FLAG      = 1024;
  SET_FLAG            = 2048;
  NUM_FLAG            = 32768;
  PART_KEY_FLAG       = 16384;
  GROUP_FLAG          = 32768;
  UNIQUE_FLAG         = 65536;
  BINCMP_FLAG         = 131072;

  MAX_PACKET_LENGTH  = 256*256*256-1;
    
type
  TEnumVioType = (  VIO_CLOSED, VIO_TYPE_TCPIP, VIO_TYPE_SOCKET,
                    VIO_TYPE_NAMEDPIPE, VIO_TYPE_SSL);
  TEnumServerCommand = (COM_SLEEP,        COM_QUIT,           COM_INIT_DB,
                        COM_QUERY,        COM_FIELD_LIST,     COM_CREATE_DB,
                        COM_DROP_DB,      COM_REFRESH,        COM_SHUTDOWN,
                        COM_STATISTICS,   COM_PROCESS_INFO,   COM_CONNECT,
                        COM_PROCESS_KILL, COM_DEBUG,          COM_PING,
                        COM_TIME,         COM_DELAYED_INSERT, COM_CHANGE_USER,
                        COM_BINLOG_DUMP,  COM_TABLE_DUMP,     COM_CONNECT_OUT);
  TMysql_Status = ( MYSQL_STATUS_READY, MYSQL_STATUS_GET_RESULT, MYSQL_STATUS_USE_RESULT );

implementation

end.
