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

unit uMysqlNewPassword;

interface

uses
  umysqlsha1, sysutils;

procedure password_hash_stage1(const ato:pchar; const password:pchar);
procedure password_hash_stage2(ato:pchar; const salt:pchar);
procedure password_crypt(from:pchar; ato:pchar; password:pchar; alength:integer);
procedure create_key_from_old_password(const passwd:pchar; key:pchar);
procedure hashPassword(pass:pchar; var res0,res1:longint);
procedure scramble(ato:pchar; const messag:pchar; const password:pchar);
procedure newscramble(_to:pchar; _message:pchar; password:pchar);
function mysql_scramble( pass:string; hashseed:string):pchar;

implementation

type
  TMySalt = record
    s1:cardinal;
    s2:cardinal;
  end;
  TRandStruct = record
    seed1,
    seed2,
    max_value:cardinal;
    max_value_dbl:double;
  end;


procedure password_hash_stage1(const ato:pchar; const password:pchar);
var
  context:TSHA1Context;
  i:integer;
begin
  sha1_reset(context);
  i:=0;
  while password[i]<>#0 do
  begin
    if not (password[i] in [' ',#9]) then
      sha1_input(context,pchar(@password[i]),1);
    inc(i);
  end;
  sha1_result(context,ato);
end;

procedure password_hash_stage2(ato:pchar; const salt:pchar);
var
  context:TSHA1Context;
begin
  sha1_reset(context);
  sha1_input(context,salt, 4);
  sha1_input(context,ato, ctSHA1HashSize);
  sha1_result(context,ato);
end;

procedure password_crypt(from:pchar; ato:pchar; password:pchar; alength:integer);
var
  i:integer;
begin
  i:=0;
  while i<alength do
    begin
      byte(ato[i]):=byte(from[i]) xor byte(password[i]);
      inc(i);
    end;
end;

function char_val(X:char):byte;
begin
  if (x>='0') and (x<='9') then
    result:=ord(x)-ord('0')
  else
    if (x>='A') and (x<='Z') then
      result:=ord(X)-ord('A')+10
    else
      result:=ord(X)-ord('a')+10;
end;

function get_salt_from_password(const password:string):tmysalt;
var
  val:longint;
  i,j,l:integer;
begin
  l:=length(password);
  result.s1:=0;
  result.s2:=0;
  if (l>0)then
    begin
      j:=1;
      val:=0;
      for i:=0 to 7 do
        begin
          val:=(val shl 4)+char_val(password[j]);
          inc(j);
        end;
      result.s1:=val;
      val:=0;
      for i:=0 to 7 do
        begin
          val:=(val shl 4)+char_val(password[j]);
          inc(j);
        end;
      result.s2:=val;
    end;
end;

procedure get_hash_and_password(salt:tmysalt; bin_password:pchar);
var
  context:TSHA1Context;
  va:integer;
  bp:pchar;
  t:integer;
begin
  bp:=bin_password;
  va:= salt.s1;
  for t:=3 downto 0 do
    begin
      bp[t]:= chr(va AND 255);
      va:=va shr 8;
    end;
  bp:=bp+ 4;

  va:= salt.s2;
  for t:=3 downto 0 do
    begin
      bp[t]:= chr(va AND 255);
      va:=va shr 8;
    end;

  sha1_reset(context);
  sha1_input(context,bin_password,8);
  sha1_result(context,bin_password);
end;

procedure create_key_from_old_password(const passwd:pchar; key:pchar);
var
  hash_res: array[0..1] of longint;
  buffer:string[20];
  salt:tmysalt;
begin
  FillChar(buffer,20,0);
  //* At first hash password to the string stored in password */
  hashPassword(passwd, hash_res[0],hash_res[1]);
  buffer:=format('%08x%08x',[hash_res[0],hash_res[1]]);
  //* Now convert it to the salt form */
  salt:=get_salt_from_password(buffer);
  //* Finally get hash and bin password from salt */
  get_hash_and_password(salt,key);
end;

{$Q-}
procedure hashPassword(pass:pchar; var res0,res1:longint);
var
  nr,add,nr2,tmp:int64;
  i:longint;
  e1:int64;
  len:longint;
begin
  nr:=1345345333;
  add:=7;
  nr2:=$12345671;
  len:=length(pass)-1;
  for i:=0 to len do
    begin
      if (Pass[i] = #20) or (Pass[i] = #9)then
        continue;
      tmp := $ff AND byte(Pass[i]);
      e1:=(((nr and 63) +add)*tmp)+(nr shl 8);
      nr:=nr xor e1;
      nr2:=nr2+((nr2 shl 8) xor nr);
      add :=add+tmp;
    end;
  res0 := nr AND $7fffffff;
  res1 := nr2 AND $7fffffff;
end;
{$Q+}

procedure randominit(var rand_st: TRandStruct; seed1, seed2:cardinal);
begin
  rand_st.max_value:= $3FFFFFFF;
  rand_st.max_value_dbl:= rand_st.max_value;
  rand_st.seed1:=seed1 mod rand_st.max_value;
  rand_st.seed2:=seed2 mod rand_st.max_value;
end;

function Floor(X: Extended): longint;
begin
  Result := Trunc(X);
  if (X < 0) and (Result<>X) then
    Result:=Result-1;
end;

{$Q-}
function my_rnd(var rand_st:TRandStruct):double;
begin
  rand_st.seed1:=(rand_st.seed1*3+rand_st.seed2) mod rand_st.max_value;
  rand_st.seed2:=(rand_st.seed1+rand_st.seed2+33) mod rand_st.max_value;
  result:=rand_st.seed1/rand_st.max_value_dbl;
end;
{$Q+}

procedure scramble(ato:pchar; const messag:pchar; const password:pchar);
var
  hash_pass,hash_message:array[0..1] of longint;
  message_buffer:string[9];
  msg:pchar;
  to_start:pchar;
  rand_st:TRandStruct;
  bto:pchar;
  extra:char;
begin
  msg:=@message_buffer[1];
  bto:=ato;
  //* We use special message buffer now as new server can provide longer hash */
  move(messag[0],message_buffer[1],8);
  message_buffer[9]:=#0;

  if (password<>nil)and(password[0]<>#0)then
    begin
      to_start:=ato;
      hashpassword(password,hash_pass[0],hash_pass[1]);
      hashpassword(pchar(@message_buffer[1]), hash_message[0],hash_message[1]);
      randominit(rand_st,hash_pass[0] xor hash_message[0], hash_pass[1] xor hash_message[1]);
      while (msg[0]<>#0)do
        begin
          bto[0]:= chr(floor(my_rnd(rand_st)*31)+64);
          inc(bto);
          inc(msg);
        end;
      extra:=chr(floor(my_rnd(rand_st)*31));
      while (to_start <> bto)do
        begin
    	    byte(to_start[0]):=byte(to_start[0]) xor byte(extra);
          inc(to_Start);
        end;
    end;
  bto[0]:=#0;
end;

procedure newscramble(_to:pchar; _message:pchar; password:pchar);
var
  sha1_context: TSHA1Context;
  hash_stage1,hash_stage2:array [0..ctSHA1HashSize-1] of byte;
begin
  sha1_reset(sha1_context);
  //* stage 1: hash password */
  sha1_input(sha1_context, password, length(password));
  sha1_result(sha1_context, @hash_stage1[0]);
  //* stage 2: hash stage 1; note that hash_stage2 is stored in the database */
  sha1_reset(sha1_context);
  sha1_input(sha1_context, @hash_stage1[0], ctSHA1HashSize);
  sha1_result(sha1_context, @hash_stage2[0]);
  //* create crypt string as sha1(message, hash_stage2) */;
  sha1_reset(sha1_context);
  sha1_input(sha1_context, _message, ctSHA1HashSize);
  sha1_input(sha1_context, @hash_stage2[0], ctSHA1HashSize);
  //* xor allows 'from' and 'to' overlap: lets take advantage of it */
  sha1_result(sha1_context, _to);
  password_crypt(_to, _to, @hash_stage1[0], ctSHA1HashSize);
end;

///////////////////////////////////////////////////////////////////////////////
// encryption of password
function mysql_scramble( pass:string; hashseed:string):pchar;
var hp0,hp1:longint;
    hm0,hm1:longint;
    maxValue,seed, seed2 :int64;
    dRes: double;
    i:longint;
    e:byte;
    len1:longint;
begin
  if(pass = '') or (hashseed='')then
    begin
      result:=nil;
      exit;
    end;
  len1:=length(hashseed)-1;
  result:=stralloc(9);
  hashPassword(pchar(pass),hp0,hp1);
  hashPassword(pchar(hashSeed),hm0,hm1);
  maxValue:= $3FFFFFFF;
  seed  := ( hp0 xor hm0 )mod maxValue ;
  seed2 := ( hp1 xor hm1 )mod maxValue ;
  for i:=0 to len1 do
    begin
      seed  := (seed * 3 + seed2) mod maxValue ;
      seed2 := (seed + seed2 + 33) mod maxValue ;
      dRes := Seed / maxValue;
      result[i] := char( floor( dRes * 31 ) + 64 );
    end;
  seed  := (seed * 3 + seed2) mod maxValue ;
  dRes := Seed / maxValue;
  e := floor( dRes * 31 );
  for i := 0 to len1 do
    result[i] := chr( byte (result[i]) xor e);
  result[len1+1]:=#0; //should not be needed
end;

end.
