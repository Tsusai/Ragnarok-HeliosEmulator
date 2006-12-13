{--------------------------------------------------------------------------------
Licencing issues:
17-September-2001      ©Cristian Nicola
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
unit uMysqlErrors;
////////////////////////////////////////////////////////////////////////////////
// Errors imported from mysql

interface

const
  ER_HASHCHK                            = 1000;
  ER_NISAMCHK                           = 1001;
  ER_NO                                 = 1002;
  ER_YES                                = 1003;
  ER_CANT_CREATE_FILE                   = 1004;
  ER_CANT_CREATE_TABLE                  = 1005;
  ER_CANT_CREATE_DB                     = 1006;
  ER_DB_CREATE_EXISTS                   = 1007;
  ER_DB_DROP_EXISTS                     = 1008;
  ER_DB_DROP_DELETE                     = 1009;
  ER_DB_DROP_RMDIR                      = 1010;
  ER_CANT_DELETE_FILE                   = 1011;
  ER_CANT_FIND_SYSTEM_REC               = 1012;
  ER_CANT_GET_STAT                      = 1013;
  ER_CANT_GET_WD                        = 1014;
  ER_CANT_LOCK                          = 1015;
  ER_CANT_OPEN_FILE                     = 1016;
  ER_FILE_NOT_FOUND                     = 1017;
  ER_CANT_READ_DIR                      = 1018;
  ER_CANT_SET_WD                        = 1019;
  ER_CHECKREAD                          = 1020;
  ER_DISK_FULL                          = 1021;
  ER_DUP_KEY                            = 1022;
  ER_ERROR_ON_CLOSE                     = 1023;
  ER_ERROR_ON_READ                      = 1024;
  ER_ERROR_ON_RENAME                    = 1025;
  ER_ERROR_ON_WRITE                     = 1026;
  ER_FILE_USED                          = 1027;
  ER_FILSORT_ABORT                      = 1028;
  ER_FORM_NOT_FOUND                     = 1029;
  ER_GET_ERRNO                          = 1030;
  ER_ILLEGAL_HA                         = 1031;
  ER_KEY_NOT_FOUND                      = 1032;
  ER_NOT_FORM_FILE                      = 1033;
  ER_NOT_KEYFILE                        = 1034;
  ER_OLD_KEYFILE                        = 1035;
  ER_OPEN_AS_READONLY                   = 1036;
  ER_OUTOFMEMORY                        = 1037;
  ER_OUT_OF_SORTMEMORY                  = 1038;
  ER_UNEXPECTED_EOF                     = 1039;
  ER_CON_COUNT_ERROR                    = 1040;
  ER_OUT_OF_RESOURCES                   = 1041;
  ER_BAD_HOST_ERROR                     = 1042;
  ER_HANDSHAKE_ERROR                    = 1043;
  ER_DBACCESS_DENIED_ERROR              = 1044;
  ER_ACCESS_DENIED_ERROR                = 1045;
  ER_NO_DB_ERROR                        = 1046;
  ER_UNKNOWN_COM_ERROR                  = 1047;
  ER_BAD_NULL_ERROR                     = 1048;
  ER_BAD_DB_ERROR                       = 1049;
  ER_TABLE_EXISTS_ERROR                 = 1050;
  ER_BAD_TABLE_ERROR                    = 1051;
  ER_NON_UNIQ_ERROR                     = 1052;
  ER_SERVER_SHUTDOWN                    = 1053;
  ER_BAD_FIELD_ERROR                    = 1054;
  ER_WRONG_FIELD_WITH_GROUP             = 1055;
  ER_WRONG_GROUP_FIELD                  = 1056;
  ER_WRONG_SUM_SELECT                   = 1057;
  ER_WRONG_VALUE_COUNT                  = 1058;
  ER_TOO_LONG_IDENT                     = 1059;
  ER_DUP_FIELDNAME                      = 1060;
  ER_DUP_KEYNAME                        = 1061;
  ER_DUP_ENTRY                          = 1062;
  ER_WRONG_FIELD_SPEC                   = 1063;
  ER_PARSE_ERROR                        = 1064;
  ER_EMPTY_QUERY                        = 1065;
  ER_NONUNIQ_TABLE                      = 1066;
  ER_INVALID_DEFAULT                    = 1067;
  ER_MULTIPLE_PRI_KEY                   = 1068;
  ER_TOO_MANY_KEYS                      = 1069;
  ER_TOO_MANY_KEY_PARTS                 = 1070;
  ER_TOO_LONG_KEY                       = 1071;
  ER_KEY_COLUMN_DOES_NOT_EXITS          = 1072;
  ER_BLOB_USED_AS_KEY                   = 1073;
  ER_TOO_BIG_FIELDLENGTH                = 1074;
  ER_WRONG_AUTO_KEY                     = 1075;
  ER_READY                              = 1076;
  ER_NORMAL_SHUTDOWN                    = 1077;
  ER_GOT_SIGNAL                         = 1078;
  ER_SHUTDOWN_COMPLETE                  = 1079;
  ER_FORCING_CLOSE                      = 1080;
  ER_IPSOCK_ERROR                       = 1081;
  ER_NO_SUCH_INDEX                      = 1082;
  ER_WRONG_FIELD_TERMINATORS            = 1083;
  ER_BLOBS_AND_NO_TERMINATED            = 1084;
  ER_TEXTFILE_NOT_READABLE              = 1085;
  ER_FILE_EXISTS_ERROR                  = 1086;
  ER_LOAD_INFO                          = 1087;
  ER_ALTER_INFO                         = 1088;
  ER_WRONG_SUB_KEY                      = 1089;
  ER_CANT_REMOVE_ALL_FIELDS             = 1090;
  ER_CANT_DROP_FIELD_OR_KEY             = 1091;
  ER_INSERT_INFO                        = 1092;
  ER_INSERT_TABLE_USED                  = 1093;
  ER_NO_SUCH_THREAD                     = 1094;
  ER_KILL_DENIED_ERROR                  = 1095;
  ER_NO_TABLES_USED                     = 1096;
  ER_TOO_BIG_SET                        = 1097;
  ER_NO_UNIQUE_LOGFILE                  = 1098;
  ER_TABLE_NOT_LOCKED_FOR_WRITE         = 1099;
  ER_TABLE_NOT_LOCKED                   = 1100;
  ER_BLOB_CANT_HAVE_DEFAULT             = 1101;
  ER_WRONG_DB_NAME                      = 1102;
  ER_WRONG_TABLE_NAME                   = 1103;
  ER_TOO_BIG_SELECT                     = 1104;
  ER_UNKNOWN_ERROR                      = 1105;
  ER_UNKNOWN_PROCEDURE                  = 1106;
  ER_WRONG_PARAMCOUNT_TO_PROCEDURE      = 1107;
  ER_WRONG_PARAMETERS_TO_PROCEDURE      = 1108;
  ER_UNKNOWN_TABLE                      = 1109;
  ER_FIELD_SPECIFIED_TWICE              = 1110;
  ER_INVALID_GROUP_FUNC_USE             = 1111;
  ER_UNSUPPORTED_EXTENSION              = 1112;
  ER_TABLE_MUST_HAVE_COLUMNS            = 1113;
  ER_RECORD_FILE_FULL                   = 1114;
  ER_UNKNOWN_CHARACTER_SET              = 1115;
  ER_TOO_MANY_TABLES                    = 1116;
  ER_TOO_MANY_FIELDS                    = 1117;
  ER_TOO_BIG_ROWSIZE                    = 1118;
  ER_STACK_OVERRUN                      = 1119;
  ER_WRONG_OUTER_JOIN                   = 1120;
  ER_NULL_COLUMN_IN_INDEX               = 1121;
  ER_CANT_FIND_UDF                      = 1122;
  ER_CANT_INITIALIZE_UDF                = 1123;
  ER_UDF_NO_PATHS                       = 1124;
  ER_UDF_EXISTS                         = 1125;
  ER_CANT_OPEN_LIBRARY                  = 1126;
  ER_CANT_FIND_DL_ENTRY                 = 1127;
  ER_FUNCTION_NOT_DEFINED               = 1128;
  ER_HOST_IS_BLOCKED                    = 1129;
  ER_HOST_NOT_PRIVILEGED                = 1130;
  ER_PASSWORD_ANONYMOUS_USER            = 1131;
  ER_PASSWORD_NOT_ALLOWED               = 1132;
  ER_PASSWORD_NO_MATCH                  = 1133;
  ER_UPDATE_INFO                        = 1134;
  ER_CANT_CREATE_THREAD                 = 1135;
  ER_WRONG_VALUE_COUNT_ON_ROW           = 1136;
  ER_CANT_REOPEN_TABLE                  = 1137;
  ER_INVALID_USE_OF_NULL                = 1138;
  ER_REGEXP_ERROR                       = 1139;
  ER_MIX_OF_GROUP_FUNC_AND_FIELDS       = 1140;
  ER_NONEXISTING_GRANT                  = 1141;
  ER_TABLEACCESS_DENIED_ERROR           = 1142;
  ER_COLUMNACCESS_DENIED_ERROR          = 1143;
  ER_ILLEGAL_GRANT_FOR_TABLE            = 1144;
  ER_GRANT_WRONG_HOST_OR_USER           = 1145;
  ER_NO_SUCH_TABLE                      = 1146;
  ER_NONEXISTING_TABLE_GRANT            = 1147;
  ER_NOT_ALLOWED_COMMAND                = 1148;
  ER_SYNTAX_ERROR                       = 1149;
  ER_DELAYED_CANT_CHANGE_LOCK           = 1150;
  ER_TOO_MANY_DELAYED_THREADS           = 1151;
  ER_ABORTING_CONNECTION                = 1152;
  ER_NET_PACKET_TOO_LARGE               = 1153;
  ER_NET_READ_ERROR_FROM_PIPE           = 1154;
  ER_NET_FCNTL_ERROR                    = 1155;
  ER_NET_PACKETS_OUT_OF_ORDER           = 1156;
  ER_NET_UNCOMPRESS_ERROR               = 1157;
  ER_NET_READ_ERROR                     = 1158;
  ER_NET_READ_INTERRUPTED               = 1159;
  ER_NET_ERROR_ON_WRITE                 = 1160;
  ER_NET_WRITE_INTERRUPTED              = 1161;
  ER_TOO_LONG_STRING                    = 1162;
  ER_TABLE_CANT_HANDLE_BLOB             = 1163;
  ER_TABLE_CANT_HANDLE_AUTO_INCREMENT   = 1164;
  ER_DELAYED_INSERT_TABLE_LOCKED        = 1165;
  ER_WRONG_COLUMN_NAME                  = 1166;
  ER_WRONG_KEY_COLUMN                   = 1167;
  ER_WRONG_MRG_TABLE                    = 1168;
  ER_DUP_UNIQUE                         = 1169;
  ER_BLOB_KEY_WITHOUT_LENGTH            = 1170;
  ER_PRIMARY_CANT_HAVE_NULL             = 1171;
  ER_TOO_MANY_ROWS                      = 1172;
  ER_REQUIRES_PRIMARY_KEY               = 1173;
  ER_NO_RAID_COMPILED                   = 1174;
  ER_UPDATE_WITHOUT_KEY_IN_SAFE_MODE    = 1175;
  ER_KEY_DOES_NOT_EXITS                 = 1176;
  ER_CHECK_NO_SUCH_TABLE                = 1177;
  ER_CHECK_NOT_IMPLEMENTED              = 1178;
  ER_CANT_DO_THIS_DURING_AN_TRANSACTION = 1179;
  ER_ERROR_DURING_COMMIT                = 1180;
  ER_ERROR_DURING_ROLLBACK              = 1181;
  ER_ERROR_DURING_FLUSH_LOGS            = 1182;
  ER_ERROR_DURING_CHECKPOINT            = 1183;
  ER_NEW_ABORTING_CONNECTION            = 1184;
  ER_DUMP_NOT_IMPLEMENTED               = 1185;
  ER_FLUSH_MASTER_BINLOG_CLOSED         = 1186;
  ER_INDEX_REBUILD                      = 1187;
  ER_MASTER                             = 1188;
  ER_MASTER_NET_READ                    = 1189;
  ER_MASTER_NET_WRITE                   = 1190;
  ER_FT_MATCHING_KEY_NOT_FOUND          = 1191;
  ER_LOCK_OR_ACTIVE_TRANSACTION         = 1192;
  ER_UNKNOWN_SYSTEM_VARIABLE            = 1193;
  ER_CRASHED_ON_USAGE                   = 1194;
  ER_CRASHED_ON_REPAIR                  = 1195;
  ER_WARNING_NOT_COMPLETE_ROLLBACK      = 1196;
  ER_TRANS_CACHE_FULL                   = 1197;
  ER_SLAVE_MUST_STOP                    = 1198;
  ER_SLAVE_NOT_RUNNING                  = 1199;
  ER_BAD_SLAVE                          = 1200;
  ER_MASTER_INFO                        = 1201;
  ER_SLAVE_THREAD                       = 1202;
  ER_TOO_MANY_USER_CONNECTIONS          = 1203;
  ER_SET_CONSTANTS_ONLY                 = 1204;
  ER_ERROR_MESSAGES                     = 205;

  CR_MIN_ERROR                          = 2000;
  CR_MAX_ERROR                          = 2999;
  CLIENT_ERRMAP                         = 2;

  CR_UNKNOWN_ERROR                      = 2000;
  CR_SOCKET_CREATE_ERROR                = 2001;
  CR_CONNECTION_ERROR                   = 2002;
  CR_CONN_HOST_ERROR                    = 2003;
  CR_IPSOCK_ERROR                       = 2004;
  CR_UNKNOWN_HOST                       = 2005;
  CR_SERVER_GONE_ERROR                  = 2006;
  CR_VERSION_ERROR                      = 2007;
  CR_OUT_OF_MEMORY                      = 2008;
  CR_WRONG_HOST_INFO                    = 2009;
  CR_LOCALHOST_CONNECTION               = 2010;
  CR_TCP_CONNECTION                     = 2011;
  CR_SERVER_HANDSHAKE_ERR               = 2012;
  CR_SERVER_LOST                        = 2013;
  CR_COMMANDS_OUT_OF_SYNC               = 2014;
  CR_NAMEDPIPE_CONNECTION               = 2015;
  CR_NAMEDPIPEWAIT_ERROR                = 2016;
  CR_NAMEDPIPEOPEN_ERROR                = 2017;
  CR_NAMEDPIPESETSTATE_ERROR            = 2018;
  CR_CANT_READ_CHARSET                  = 2019;
  CR_NET_PACKET_TOO_LARGE               = 2020;
  
var
  client_errors: array [0..20]of string=(
    'Unknown MySQL error',                                                     {0}
    'Can''t create UNIX socket (%d)',                                          {1}
    'Can''t connect to local MySQL server through socket ''%-.64s'' (%d)',     {2}
    'Can''t connect to MySQL server on ''%-.64s'' (%d)',                       {3}
    'Can''t create TCP/IP socket (%d)',                                        {4}
    'Unknown MySQL Server Host ''%-.64s'' (%d)',                               {5}
    'MySQL server has gone away',                                              {6}
    'Protocol mismatch. Server Version = %d Client Version = %d',              {7}
    'MySQL client run out of memory',                                          {8}
    'Wrong host info',                                                         {9}
    'Localhost via UNIX socket',                                              {10}
    '%-.64s via TCP/IP',                                                      {11}
    'Error in server handshake',                                              {12}
    'Lost connection to MySQL server during query',                           {13}
    'Commands out of sync;  You can''t run this command now',                 {14}
    '%-.64s via named pipe',                                                  {15}
    'Can''t wait for named pipe to host: %-.64s  pipe: %-.32s (%lu)',         {16}
    'Can''t open named pipe to host: %-.64s  pipe: %-.32s (%u)',              {17}
    'Can''t set state of named pipe to host: %-.64s  pipe: %-.32s (%lu)',     {18}
    'Can''t initialize character set %-.64s (path: %-.64s)',                  {19}
    'Got packet bigger than ''max_allowed_packet'''                           {20}
  );
  
implementation

end.
