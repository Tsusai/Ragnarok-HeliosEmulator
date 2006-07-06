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
unit uMysqlNet;
////////////////////////////////////////////////////////////////////////////////
// Net structure it is responsible of packing/unpacking packets
// it uses Vio to read/write and if using compressed protocol
// it decompress/compress packets
// controlled by Mysql Client no one should ever use it directly

interface

{$I mysqlinc.inc}

uses
  sysutils, uMysqlErrors, uMysqlCT, uMysqlVio;

type
  TMysqlNet = class (TObject)
  private
    fvio                : TMysqlVIO;
    fcompress           : boolean;
    freading_or_writing : byte;
    fbuff               : pointer;
    fpkt_nr             : byte;
    fmax_packet         : longint;
    fwrite_pos          : longint;
    fread_pos           : longint;
    fbuf_length         : longint;
    fwhere_b            : longint;
    fremain_in_buf      : longint;
    fsave_char          : char;
    //timeout            : cardinal;
    //error              : byte;
    fprotocol_version   : cardinal;
    procedure Setlast_error(const Value: string);
    function Getlast_error: string;
    procedure Setlast_errorno(const Value: cardinal);
    function Getlast_errorno: cardinal;
    procedure setcompress(const Value: boolean);
    function GetNetConnected: boolean;
    function GetVioType: TEnumVioType;
    procedure SetNoTimeOut(const Value: Boolean);
    function GetNoTimeOut: Boolean;
  public
    property last_error : string read Getlast_error write Setlast_error;
    property last_errno : cardinal read Getlast_errorno write Setlast_errorno;
    property read_pos : longint read fread_pos;
    property protocol_version:cardinal read fprotocol_version write fprotocol_version;
    property compress:boolean read fcompress write setcompress;
    property net_connected:boolean read GetNetConnected;
    property vio_type:TEnumVioType read GetVioType;
    property NoTimeOut : Boolean read GetNoTimeOut write SetNoTimeOut;
    constructor Create;
    destructor Destroy;override;
    function net_open( VioType:TEnumVioType; host:string='localhost'; unix_socket:string={$IFDEF MSWINDOWS}MYSQL_NAMEDPIPE{$ELSE}MYSQL_UNIX_ADDR{$ENDIF}; port:longint=0; connect_timeout:cardinal=0; trysock:boolean=true):longint;
    function net_close:longint;
    function net_pack:boolean;
    function net_realloc(len1:cardinal):boolean;
    procedure net_clear;
    function net_flush:longint;
    function my_net_write(const packet:PCHAR;len1:cardinal):longint;
    function net_write_command(command:char;const packet:pchar;len1:cardinal):longint;
    function net_write_buff({const }packet:pchar;len1:longint):longint;
    function net_real_write({const }packet1:pchar;len1:longint):longint;
    function my_real_read(var complen:longint):longint;
    function my_net_read:longint;
    function net_safe_read(fclient_flag: integer):longint;
    {$IFDEF HAVE_SSL}
    procedure SwitchToSSL(const key_file:pchar;const cert_file:pchar;const ca_file:pchar;const ca_path:pchar;var cipher:pchar; timeout:cardinal);
    {$ENDIF}
  end;

////////////////////////////////////////////////////////////////////////////////
implementation
////////////////////////////////////////////////////////////////////////////////

{$IFDEF HAVE_COMPRESS}
//zlib imports
{$IFDEF MSWINDOWS}
{$L zlib\compress.obj}
{$L zlib\uncompr.obj}
{$L zlib\deflate.obj}
{$L zlib\adler32.obj}
{$L zlib\trees.obj}

{$L zlib\inflate.obj}
{$L zlib\infblock.obj}
{$L zlib\infcodes.obj}
{$L zlib\inffast.obj}
{$L zlib\inftrees.obj}
{$L zlib\infutil.obj}
function compress(dest:pointer; destLen: pointer; const source:pointer; sourceLen:longint):longint;external;
function uncompress(dest:pointer; destlen:pointer; source:pointer;sourceLne:longint):longint;external;
{$ELSE}
function compress(dest:pointer; destLen: pointer; const source:pointer; sourceLen:longint):longint; cdecl; external 'libz.so' name 'compress';
function uncompress(dest:pointer; destlen:pointer; source:pointer;sourceLne:longint):longint; cdecl; external 'libz.so' name 'uncompress';
{$ENDIF}

const
  _z_errmsg: array[0..9] of PChar = (
    'need dictionary',      // Z_NEED_DICT      (2)
    'stream end',           // Z_STREAM_END     (1)
    '',                     // Z_OK             (0)
    'file error',           // Z_ERRNO          (-1)
    'stream error',         // Z_STREAM_ERROR   (-2)
    'data error',           // Z_DATA_ERROR     (-3)
    'insufficient memory',  // Z_MEM_ERROR      (-4)
    'buffer error',         // Z_BUF_ERROR      (-5)
    'incompatible version', // Z_VERSION_ERROR  (-6)
    '');
    
//needed by zlib
procedure _memcpy(dest, source: Pointer; count: Integer);cdecl;
begin
  Move(source^, dest^, count);
end;

{$IFDEF MSWINDOWS}
//needed by zlib
procedure _memset(P: Pointer; B: Byte; count: Integer);cdecl;
begin
  FillChar(P^, count, B);
end;

//needed by zlib
function zcalloc(AppData: Pointer; Items, Size: Integer): Pointer;
begin
  GetMem(Result, Items*Size);
end;

//needed by zlib
procedure zcfree(AppData, Block: Pointer);
begin
  FreeMem(Block);
end;
{$ENDIF}

type
  PByte=^byte;
  PLongInt=^longint;

////////////////////////////////////////////////////////////////////////////////
// allocates a buffer and compress it
function my_compress_alloc(const packet:PByte;_len:PLongint; complen:PLongint):PByte;
var
  compbuf:PByte;
  tmp:Longint;
begin
  //how much we need
  complen^ :=  trunc(_len^ * 120 / 100 + 12); // should be integer
  getmem(compbuf,complen^);
  if (compbuf=nil)then //out of memory?
    begin
      result:=nil;
      exit;
    end;
  if (compress(compbuf,complen, packet, _len^ ) <> 0) then //compress it
    begin
      freemem(compbuf); //we got an error
      result:=nil;
      exit;
    end;
  if (complen^ >= _len^) then //compressed packet is bigger than the uncompressed one ?
    begin
      complen^:=0;
      freemem(compbuf);
      result:=nil;
      exit;
    end;
  tmp:=_len^; //return the compressed packet
  _len^:=complen^;
  complen^:=tmp;
  result:=compbuf;
end;

////////////////////////////////////////////////////////////////////////////////
//compression function
// it calls my_compress_alloc to check whenever the new packet(compressed)
// is bigger than the original one
function my_compress(packet:PByte; _len:PLongint; complen:PLongint):boolean;
var
  compbuf:PByte;
begin
  if (_len^ < MIN_COMPRESS_LENGTH) then //do we need compression?
    complen^:=0
  else
    begin
      compbuf:=my_compress_alloc(packet,_len,complen);
      if (compbuf=nil) then //doesnt look like compression will help
        begin
          if complen^<>0 then
            result:=true
          else
            result:=false;
          exit;
        end;
      _memcpy(packet,compbuf,_len^); //switch the result
      freemem(compbuf);
    end;
  result:=true;
end;

////////////////////////////////////////////////////////////////////////////////
//uncompression function
function my_uncompress(packet:PByte; _len:PLongint; complen:PLongint):boolean;
var
  compbuf:PByte;
begin
  if (complen^<>0)then //do we have anything to uncompress
    begin
      getmem(compbuf,complen^);
      if (compbuf=nil)then //out of memory
        begin
          result:=false;
          exit;
        end;
      if (uncompress(compbuf, complen, packet, _len^) <>0)then //try to uncompress
        begin
          freemem(compbuf);
          result:=false;
          exit;
        end;
      _len^ := complen^; //give back the uncompressed packet
      _memcpy(packet,compbuf,_len^);
      freemem(compbuf);
    end;
  result:=true;
end;
{$ENDIF} //HAVE_COMPRESS

//------------------------------------------------------------------------------
{ TMysqlNet }
//------------------------------------------------------------------------------

////////////////////////////////////////////////////////////////////////////////
// class constructor
constructor TMysqlNet.create;
begin
  inherited;
  //create a vio
  fvio:= TMysqlVIO.Create;
end;

////////////////////////////////////////////////////////////////////////////////
// class destructor
destructor TMysqlNet.destroy;
begin
  if assigned(fvio) then
    begin
      fvio.Free;
      fvio:=nil;
    end;
  if fbuff<>nil then
    begin
      freemem(fbuff);
      fbuff:=nil;
    end;
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
// maps vio last_error to net last_error
function TMysqlNet.Getlast_error: string;
begin
  result:=fvio.last_error;
end;

////////////////////////////////////////////////////////////////////////////////
// maps vio last_errno to net last_errno
function TMysqlNet.Getlast_errorno: cardinal;
begin
  result:=fvio.last_errno;
end;

////////////////////////////////////////////////////////////////////////////////
// returns true if we have a connection to the server
function TMysqlNet.GetNetConnected: boolean;
begin
  result:=fvio.VIO_type<>VIO_CLOSED;
end;

////////////////////////////////////////////////////////////////////////////////
// reads a packet from VIO and decompress it if needed
function TMysqlNet.my_net_read: longint;
var
  len1, complen: longint;
  {$IFDEF HAVE_COMPRESS}
  len2:longint;
  p:pchar;
  {$ENDIF}
begin
  complen:=0;
  {$IFDEF HAVE_COMPRESS}
  len1:=0;
  if not(fcompress) then //if we use compression
  {$ENDIF}
    begin
      len1 := my_real_read (complen); //is there anything to read?
      fread_pos := longint(fbuff) + fwhere_b; //where are we?
      if (len1 <> packet_error) then
        pchar(fread_pos)[len1]:=#0;
      result:=len1;
      exit; //<- not needed
    end;
  {$IFDEF HAVE_COMPRESS}
  if (fremain_in_buf<>0) then
    pchar(fbuff)[fbuf_length - fremain_in_buf]:=fsave_char; //restore the saved char
  while true do
    begin
      if (fremain_in_buf<>0) then
        begin
          p := pchar(longint(fbuff) + fbuf_length - fremain_in_buf);
          if (fremain_in_buf >= 4) then //read next packet
            begin
              len2 :=byte(p[0])+(byte(p[1])shl 8)+(byte(p[2])shl 16);
              if (len2 <= fremain_in_buf - 4) then
                begin
                  len1:=len2;
                  fremain_in_buf :=fremain_in_buf - (len2 + 4);
                  fread_pos:=longint(p) + 4;
                  break;
                end;
            end;
          if (fbuf_length <> fremain_in_buf)then
            begin
              move(p[0],pointer(fbuff)^,fremain_in_buf);
              fbuf_length:=fremain_in_buf;
            end;
          fwhere_b:=fbuf_length;
        end
      else
        begin
          fwhere_b:=0;
          fbuf_length:=0;
        end;
      len1:=my_real_read(complen); //another read
      if (len1 = packet_error) then
        break;
      if not(my_uncompress(pbyte(longint(fbuff) + fwhere_b),@len1, @complen)) then //now we can uncompress
        begin
          len1:= packet_error;
          break;
        end;
      fbuf_length:=fbuf_length+len1;
      fremain_in_buf:=fremain_in_buf+len1;
    end;
  if (len1 <> packet_error) then
    begin
      fsave_char:= pchar(fread_pos)[len1]; //save the next char (first on new packet)
      pchar(fread_pos)[len1]:=#0; //and make it #0 for pchar operations
    end;
  result:=len1;
  {$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
// writes a packet to vio
function TMysqlNet.my_net_write(const packet: PCHAR;
  len1: cardinal): longint;
var
  buf:array[0..NET_HEADER_SIZE-1]of char;
begin
  if (len1 >= MAX_PACKET_LENGTH) then
    begin
      last_errno:=ER_NET_PACKET_TOO_LARGE;
      result:=1;
      exit;
    end;
  buf[0]:= chr(len1);
  buf[1]:= chr((len1) shr 8);
  buf[2]:= chr((len1) shr 16);
  if fcompress then //if compressed packet no. is 0
    buf[3]:= #0
  else
    begin
      buf[3]:=chr(fpkt_nr); //inc packet no.
      if fpkt_nr=255 then
        fpkt_nr:=0
      else
        inc(fpkt_nr);
    end;
  if net_write_buff(buf,NET_HEADER_SIZE)<>0 then //write the header
    result:=1
  else
    result:=net_write_buff(packet,len1); //write the packet
end;

////////////////////////////////////////////////////////////////////////////////
// low level read
//todo clean this code
function TMysqlNet.my_real_read(var complen: Integer): longint;
var
  p:longint;
  leng:longint;
  retr_count:longint;
  i,len1:longint;
  net_blocking:boolean;
  remain:longint;
  helping:longint;
begin
  if fVio.VIO_type<>VIO_CLOSED then //are we connected to read?
    begin
      retr_count:=0;
      len1:=packet_error;
      net_blocking:=not (fvio.fcntl_mode and 1=1); //save blocking mode
      if fcompress then //calculate next packet size
        remain:=NET_HEADER_SIZE+COMP_HEADER_SIZE
      else
         remain:=NET_HEADER_SIZE;
      freading_or_writing:=1; //mark reading
      p := longint(fbuff) + fwhere_b;
      complen := 0;
      for i:=0 to 1 do //try twice
        begin
          while (remain > 0) do  //do we have anything anymore to read?
            begin
              leng:=fvio.vio_read(pchar(p),remain); //let's read
              if (leng <= 0) then //we got an error?
                begin
                  if (fvio.vio_should_retry) then
                    begin
                      inc(retr_count);
                      if (retr_count<= RETRY_COUNT) then //shall we retry?
                        continue;
                    end;
                  if (fvio.vio_intrerupted) then //just intrerupted
                    continue;
                  len1:= packet_error; //oops ...error
                  fvio.vio_blocking(net_blocking); //restore blocking mode
                  freading_or_writing:=0;
                  result:=len1;
                  exit;
                end;
              remain := remain-leng; //we read some
              p:=p+leng;
            end;
          if (i = 0)then //first try?
            begin
              if (pchar(fbuff)[fwhere_b + 3] <> chr(fpkt_nr)) then //wrong packet?
                begin
                  len1:= packet_error;
                  fvio.vio_blocking(net_blocking); //restore blocking mode
                  freading_or_writing:=0;
                  result:=len1;
                  exit;
               end;
              if fpkt_nr=255 then
                fpkt_nr:=0
              else
                inc(fpkt_nr);
              {$IFDEF HAVE_COMPRESS}
              if (fcompress) then  //calculate compressed size
                complen:=byte(pchar(fbuff)[fwhere_b + NET_HEADER_SIZE])+(byte(pchar(fbuff)[fwhere_b + NET_HEADER_SIZE+1]) shl 8)+(byte(pchar(fbuff)[fwhere_b + NET_HEADER_SIZE+2])shl 16);
              {$ENDIF}
              len1:=byte(pchar(fbuff)[fwhere_b])+(byte(pchar(fbuff)[fwhere_b+1]) shl 8)+(byte(pchar(fbuff)[fwhere_b+2])shl 16);
              if len1>complen then
                helping := len1+ fwhere_b
              else
                helping := complen + fwhere_b;

              if (helping >= fmax_packet) then //do we have enough space to read the packet?
                if not(net_realloc(helping+1)) then //make some
                  begin //out of memory?
                    len1:= packet_error;
                    fvio.vio_blocking(net_blocking); //restore blocking mode
                    freading_or_writing:=0;
                    result:=len1;
                    exit;
                  end;
              p:=longint(fbuff) + fwhere_b;
              remain := len1;
            end;
        end;
      fvio.vio_blocking(net_blocking); //restore blocking mode
      freading_or_writing:=0;
      result:=len1;
    end
  else
    result:=-1;
end;

////////////////////////////////////////////////////////////////////////////////
// clears the net making it read for use into a new command
// if there is any previos packets waiting just ignore them
procedure TMysqlNet.net_clear;
{$IFDEF MSWINDOWS}
{$IFDEF NEVERENABLEME}
var
  count:longint;
  tmp:integer;
  is_blocking:boolean;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  {$IFDEF NEVERENABLEME}
  tmp:=fvio.ftimeout; //save timeoutvalue
  fvio.ftimeout:=0;//make it quick
  is_blocking:=not (fvio.fcntl_mode and 1=1);
  if (is_blocking) then
    fvio.vio_blocking(false);
  if not(not (fvio.fcntl_mode and 1=1))then
    begin
      count:=fvio.vio_read(fbuff,fmax_packet);
      while (count > 0) do
        count:=fvio.vio_read(fbuff,fmax_packet);
      if (is_blocking) then
        fvio.vio_blocking(true);
    end;
  fvio.ftimeout:=tmp; //restore timeoutvalue
  {$ENDIF}
  {$ENDIF}
  fpkt_nr:=0;
  fwrite_pos:=longint(fbuff);
end;

////////////////////////////////////////////////////////////////////////////////
//closes vio and release the buffer memory
function TMysqlNet.net_close: longint;
begin
  result:=0;
  if fVio.VIO_type<>VIO_CLOSED then
    begin
      result:=fvio.vio_close;
      freemem(fbuff);
      fbuff:=nil;
      fmax_packet:=0;
    end;
end;

////////////////////////////////////////////////////////////////////////////////
// send any packets that could be waiting in buffer
function TMysqlNet.net_flush: longint;
var
  error:longint;
begin
  error:=0;
  if (longint(fbuff) <> fwrite_pos) then //do we have anything in buffer?
    begin
      error:=net_real_write(fbuff,(fwrite_pos - longint(fbuff)));
      fwrite_pos:=longint(fbuff);
    end;
  result:=error;
end;

////////////////////////////////////////////////////////////////////////////////
// opens vio for read/write and allocates memory for buffer
function TMysqlNet.net_open(VioType: TEnumVioType; host, unix_socket: string;
  port: Integer; connect_timeout: cardinal; trysock: boolean): longint;
begin
  result:=0;
  if fVio.VIO_type=VIO_CLOSED then
    begin
      result:=fVio.vio_open(VioType,host,unix_socket,port,connect_timeout,trysock);
      if result=0 then //we are connected
        begin
          //get a buffer for reading/writing
          if (fbuff <> nil) then
            freemem(fbuff);
          getmem(fbuff,net_buffer_length);
          fmax_packet:=net_buffer_length;
          fwrite_pos:=longint(fbuff);
          fread_pos := longint(fbuff);
          fpkt_nr:=0;
          fcompress:=false;
          fbuf_length:=net_buffer_length;
          fwhere_b := 0;
          fremain_in_buf:=0;
          fprotocol_version:=10;
          freading_or_writing:=0;
          fsave_char:=#0;
        end;
    end;
end;

////////////////////////////////////////////////////////////////////////////////
// if after a big row operation we want to release some memory this does it
// it just reallocs the buffer to be "net_buffer_length" size
// returns true on no error
function TMysqlNet.net_pack: boolean;
begin
  if freading_or_writing=0 then //if we are not reading/writing
  if fbuf_length>net_buffer_length then
    begin
      reallocmem(fbuff,net_buffer_length);
      if (fbuff=nil)then
        begin
          result:=false;
          exit;
        end;
      fwrite_pos:=longint(fbuff);
      fread_pos:=longint(fbuff);
      fmax_packet:=net_buffer_length;
    end;
  result:=true;
end;

////////////////////////////////////////////////////////////////////////////////
// real write to vio (lowest level in net)
//todo clean this code
function TMysqlNet.net_real_write(packet1: pchar; len1: Integer): longint;
var
  leng:longint;
  retr_count:cardinal;
  net_blocking:boolean;
  {$IFDEF HAVE_COMPRESS}
  complen:cardinal;
  b:pchar;
  header_length:byte;
  {$ENDIF}
  p1:longint;
  en:longint;
  pac:pchar;
begin
  if fVio.VIO_type<>VIO_CLOSED then //are we connected?
    begin
      pac:=packet1; //get some memory ???
      retr_count:=0;
      net_blocking := not (fvio.fcntl_mode and 1=1);
      freading_or_writing:=2;
      {$IFDEF HAVE_COMPRESS}
      if (fcompress)then //are we using compression?
        begin
          header_length:=NET_HEADER_SIZE+COMP_HEADER_SIZE;
          b:=StrAlloc(len1 + NET_HEADER_SIZE + COMP_HEADER_SIZE);
          if b=nil then
            begin
              freading_or_writing:=0;
              result:=1;
              exit;
            end;
          p1:=longint(b)+header_length;
          move(pac[0],pointer(p1)^,len1);
          if not(my_compress(pbyte(longint(b)+header_length),@len1,@complen)) then //try compress it
            complen:=0;
          b[NET_HEADER_SIZE]:= chr(complen); //write compressed header
          b[NET_HEADER_SIZE+1]:= chr((complen) shr 8);
          b[NET_HEADER_SIZE+2]:= chr((complen) shr 16);
          b[0]:= chr(len1); //write packet header
          b[1]:= chr((len1) shr 8);
          b[2]:= chr((len1) shr 16);
          b[3]:=chr(fpkt_nr);
          if fpkt_nr=255 then
            fpkt_nr:=0
          else
            inc(fpkt_nr);
          len1:=len1+ header_length;
          pac:=b;
        end;
      {$ENDIF}
      p1:=longint(@pac);
      en:=longint(@pac)+len1;
      while (p1 <> en) do //walk the packet to send
        begin
          leng:=fvio.vio_write(pchar(p1),(en-p1)); //write
          if (leng<= 0) then //we got an error
            begin
              if (fvio.fcntl_mode and 1=1) then //are we in blocking mode?
                begin
                  fvio.vio_blocking(true);
                  inc(retr_count);
                  if (retr_count <= RETRY_COUNT) then
                    continue;
                end;
              if (fvio.vio_should_retry) then //should retry?
                begin
                  inc(retr_count);
                  if (retr_count <= RETRY_COUNT) then
                    continue;
                end;
              if (fvio.vio_intrerupted) then //try on more time
                continue;
              break;
            end;
          p1:=p1+leng;
        end;
      {$IFDEF HAVE_COMPRESS}
      if pac<>packet1 then
        strdispose(pac);
      {$ENDIF}
      fvio.vio_blocking(net_blocking);
      freading_or_writing:=0;
      if p1<>en then //did we send everything
        result:=2
      else
         result:=0;
    end
  else
    result:=-1; //we are not connected
end;

////////////////////////////////////////////////////////////////////////////////
// if we get a bigger packet than what the buffer can store we need to resize it
// returns true on no error
function TMysqlNet.net_realloc(len1: cardinal): boolean;
var
  pkt_length:cardinal;
begin
  if (len1 >= max_allowed_packet) then //arent we over the max packet size?
    begin
      fvio.last_errno:=ER_NET_PACKET_TOO_LARGE;
      result:=false;
      exit;
    end;
  pkt_length := (len1+IO_SIZE-1) AND (NOT(IO_SIZE-1)); //rather than doing it in bytes we do it in chunks of 4k
  reallocmem(fbuff,pkt_length);
  if (fbuff=nil)then //did realloc worked?
    begin
      result:=false;
      exit;
    end;
  //point read/write to the new buffer
  fwrite_pos:=longint(fbuff);
  fread_pos:=longint(fbuff);
  fmax_packet:=pkt_length; //set the new max packet
  result:=true;
end;

////////////////////////////////////////////////////////////////////////////////
// a special read which just checks for errors from server
function TMysqlNet.net_safe_read(fclient_flag: integer): longint;
var
  len1:longint;
  p:pchar;
begin
  len1:=my_net_read; //try to read
  if (len1 = packet_error) or (len1 = 0) then
    begin //we got an error
      fvio.vio_close;//end_server
      //this may need to be replaced to reconnect
      //rather than server lost and stop
      if fvio.last_errno = ER_NET_PACKET_TOO_LARGE then
        fvio.last_errno:=CR_NET_PACKET_TOO_LARGE
      else
        fvio.last_errno:=CR_SERVER_LOST;
      //maybe we need this in both cases
      fvio.last_error:=client_errors[(fvio.last_errno)-CR_MIN_ERROR];
      result:=packet_error;
      exit;
    end;
  if (pchar(fread_pos)[0] = #255)then //was it an error from server?
    begin
      if (len1 > 3) then
        begin
          p:=pchar(fread_pos+1);
          if (fprotocol_version > 9) then
            begin
              fvio.last_errno:=byte(p[0])+byte(p[1])shl 8;
              p:=p+2;
              len1:=len1-2;
              if fclient_flag and CLIENT_RESERVED = CLIENT_RESERVED then
                if p[0] = '#' then
                  begin
                    { todo store mysql state here }
                    p:= p+6;
                    len1:= len1-6;
                  end;
            end
          else
            begin
              fvio.last_errno:=CR_UNKNOWN_ERROR;
              dec(len1);
            end;
          if len1<MYSQL_ERRMSG_SIZE-1 then //save the error
            fvio.last_error:=copy(p,1,len1)
          else
            fvio.last_error:=copy(p,1,MYSQL_ERRMSG_SIZE-1);
        end
      else
        begin
          fvio.last_errno:=CR_UNKNOWN_ERROR;
          fvio.last_error:=client_errors[(fvio.last_errno)-CR_MIN_ERROR];
        end;
      result:=packet_error;
      exit;
    end;
  result:=len1; //no errors return the size
end;

////////////////////////////////////////////////////////////////////////////////
// writes a packet into the buffer (this will not send over net)
function TMysqlNet.net_write_buff(packet: pchar; len1: Integer): longint;
var
  left_length:longint;
begin
  left_length:=((longint(fbuff)+fmax_packet) - fwrite_pos); //how much space left in buffer?
  while (len1 > left_length) do
    begin
      move(packet[0],pointer(fwrite_pos)^,left_length); //send chunks of buffer size
      if (net_real_write(fbuff,fmax_packet)<>0)then
        begin
          result:=1; //on error stop sending
          exit;
        end;
      fwrite_pos:=longint(fbuff); //move at beginning of the buffer
      packet:=packet+left_length; //move the packet
      len1:=len1-left_length; //left length to send
      left_length:=fmax_packet; //maximum chunk size
    end;
  move(packet[0],pointer(fwrite_pos)^,len1); //move the last part of the packet
  fwrite_pos:=fwrite_pos+len1; // move the write cursor into the buffer to point to the new position (this didnt cleared the buffer)
  result:=0; //no errors
end;

////////////////////////////////////////////////////////////////////////////////
// same as write_buff except we have a command id in front and flush the buffer
// right away
function TMysqlNet.net_write_command(command: char; const packet: pchar;
  len1: cardinal): longint;
var
  buff:array[0..NET_HEADER_SIZE] of char;
begin
  if (len1 >= MAX_PACKET_LENGTH) then
    begin
      last_errno:=ER_NET_PACKET_TOO_LARGE;
      result:=1;
      exit;
    end;
  buff[0]:= chr(len1+1);
  buff[1]:= chr((len1+1) shr 8);
  buff[2]:= chr((len1+1) shr 16);
  if fcompress then //if using compress packet no in header is 0
    buff[3]:= #0
  else
    begin
      buff[3]:=chr(fpkt_nr); //else write the packet no
      if fpkt_nr=255 then
        fpkt_nr:=0
      else
        inc(fpkt_nr); //increment it
    end;
  buff[4]:=command;
  if net_write_buff(buff,5)<>0 then //send the header
    result:=1
  else
    if net_write_buff(packet,len1)<>0 then //send the packet
      result:=1
    else
      if net_flush<>0 then //flush the buffer
        result:=1
      else
        result:=0;
end;

////////////////////////////////////////////////////////////////////////////////
// sets the compress flag (enabled only when using compression)
procedure TMysqlNet.setcompress(const Value: boolean);
begin
  {$IFDEF HAVE_COMPRESS}
  fcompress := Value;
  {$ELSE}
  fcompress:=false;
  {$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
// maps vio last_error to net last_error
procedure TMysqlNet.Setlast_error(const Value: string);
begin
  fvio.last_error := Value;
end;

////////////////////////////////////////////////////////////////////////////////
// maps vio last_errno to net last_errno
procedure TMysqlNet.Setlast_errorno(const Value: cardinal);
begin
  fvio.last_errno:=Value;
end;

////////////////////////////////////////////////////////////////////////////////
// maps vio type to net vio type
function TMysqlNet.GetVioType: TEnumVioType;
begin
  result:=fvio.VIO_type;
end;

{$IFDEF HAVE_SSL}
////////////////////////////////////////////////////////////////////////////////
// tells vio to go to ssl
procedure TMysqlNet.SwitchToSSL(const key_file:pchar;const cert_file:pchar;const ca_file:pchar;const ca_path:pchar;var cipher:pchar; timeout:cardinal);
begin
  fvio.SwitchToSSL(key_file,cert_file,ca_file,ca_path,cipher,timeout);
  if fvio.VIO_type<>VIO_TYPE_SSL then
    net_close; //we can't switch to ssl ... close the net
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
// get it from vio
function TMysqlNet.GetNoTimeOut: Boolean;
begin
  result:=fvio.NoTimeOut;
end;

////////////////////////////////////////////////////////////////////////////////
// make sure it gets to vio
procedure TMysqlNet.SetNoTimeOut(const Value: Boolean);
begin
  fvio.NoTimeOut:=Value;
end;

end.
