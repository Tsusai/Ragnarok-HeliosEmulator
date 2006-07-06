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
{$Q-}{$R-}
unit umysqlsha1;

interface

uses
  sysutils;

const
  ctSHA1HashSize = 20;
  ctSHAKeys: array[0..4] of LongInt =
    (LongInt($67452301),
     LongInt($EFCDAB89),
     LongInt($98BADCFE),
     LongInt($10325476),
     LongInt($C3D2E1F0));

type
  TSHA1Context = record
    FLength: int64;
    FInterimHash: array[0..4] of LongInt;
    FComputed: boolean;
    FCorrupted: boolean;
    FMsgBlockIndex: byte;
    FMsgBlock: array[0..63] of BYTE;
  end;

procedure sha1_reset(var context: TSHA1Context);
procedure sha1_input(var context: TSHA1Context; msgArray :pchar; msgLen:cardinal);
procedure sha1_result(var context: TSHA1Context; msgDigest: pchar);

implementation

procedure sha1_ProcessMessageBlock(var context: TSHA1Context);
const
  ctKeys: array[0..3] of LongInt =
    (LongInt($5A827999),
    LongInt($6ED9EBA1),
    LongInt($8F1BBCDC),
    LongInt($CA62C1D6));
   
var
  i,j:integer;
  temp:longint;
  w: array [0..79] of longint;
  a,b,c,d,e:longint;
begin
  for i:= 0 to 15 do
    begin
      j:= i*4;
      W[i]:= context.FMsgBlock[j] shl 24;
      W[i]:=W[i] or context.FMsgBlock[j+1] shl 16;
      W[i]:=W[i] or context.FMsgBlock[j+2] shl 8;
      W[i]:=W[i] or context.FMsgBlock[j+3];
    end;
  for i:= 16 to 79 do
    begin
      W[i]:=W[i-3] xor W[i-8] xor W[i-14] xor W[i-16];
      W[i]:=(W[i] shl 1) or (W[i] shr 31);
    end;
  A:= context.FInterimHash[0];
  B:= context.FInterimHash[1];
  C:= context.FInterimHash[2];
  D:= context.FInterimHash[3];
  E:= context.FInterimHash[4];
  for i:= 0 to 19 do
    begin
      temp:= ((A shl 5) or (A shr 27))+((B and C)or((not B)and D))+E+W[i]+ctKeys[0];
      E:= D;
      D:= C;
      C:= (B shl 30) or (B shr 2);
      B:= A;
      A:= temp;
    end;
  for i:= 20 to 39 do
    begin
      temp:= ((A shl 5) or (A shr 27))+(B xor C xor D)+E+W[i]+ctKeys[1];
      E:= D;
      D:= C;
      C:= (B shl 30) or (B shr 2);
      B:= A;
      A:= temp;
    end;
  for i:= 40 to 59 do
    begin
      temp:= ((A shl 5) or (A shr 27))+((B and C)or(B and D)or(C and D))+E+W[i]+ctKeys[2];
      E:= D;
      D:= C;
      C:= (B shl 30) or (B shr 2);
      B:= A;
      A:= temp;
    end;
  for i:= 60 to 79 do
    begin
      temp:= ((A shl 5) or (A shr 27))+(B xor C xor D)+E+W[i]+ctKeys[3];
      E:= D;
      D:= C;
      C:= (B shl 30) or (B shr 2);
      B:= A;
      A:= temp;
    end;
  context.FInterimHash[0]:= context.FInterimHash[0]+A;
  context.FInterimHash[1]:= context.FInterimHash[1]+B;
  context.FInterimHash[2]:= context.FInterimHash[2]+C;
  context.FInterimHash[3]:= context.FInterimHash[3]+D;
  context.FInterimHash[4]:= context.FInterimHash[4]+E;
  context.FMsgBlockIndex:= 0;
end;

procedure sha1_reset(var context: TSHA1Context);
begin
  context.FLength:= 0;
  context.FMsgBlockIndex:= 0;
  context.FInterimHash[0]:= ctSHAKeys[0];
  context.FInterimHash[1]:= ctSHAKeys[1];
  context.FInterimHash[2]:= ctSHAKeys[2];
  context.FInterimHash[3]:= ctSHAKeys[3];
  context.FInterimHash[4]:= ctSHAKeys[4];
  context.FComputed:= false;
  context.FCorrupted:= false;
  FillChar(context.FMsgBlock[0], 64, #0);
end;

procedure sha1_input(var context: TSHA1Context; msgArray :pchar; msgLen:cardinal);
begin
  assert(assigned(msgArray), 'Empty array paased to sha1Input');
  if context.FComputed then
    context.FCorrupted:= true;
  if not context.FCorrupted then
    while msgLen>0 do
      begin
        context.FMsgBlock[context.FMsgBlockIndex]:= byte(msgArray[0]);
        inc(context.FMsgBlockIndex);
        context.FLength:= context.FLength+8;
        if context.FMsgBlockIndex=64 then
          sha1_ProcessMessageBlock(context);
        dec(msgLen);
        inc(msgArray);
      end;
end;

procedure sha1_result(var context: TSHA1Context; msgDigest: pchar);
var
  i:integer;
begin
  assert(assigned(msgDigest), 'Empty array passed to sha1Result');
  if not context.FCorrupted then
    begin
      if not context.FComputed then
        begin
          i:=context.FMsgBlockIndex;
          if i>55 then
            begin
              context.FMsgBlock[i]:= $80;
              inc(i);
              FillChar(context.FMsgBlock[i], (64-i), #0);
              context.FMsgBlockIndex:= 64;
              sha1_ProcessMessageBlock(context);
              FillChar(context.FMsgBlock[0], 56, #0);
              context.FMsgBlockIndex:= 56;
            end
          else
            begin
              context.FMsgBlock[i]:= $80;
              inc(i);
              FillChar(context.FMsgBlock[i], (56-i), #0);
              context.FMsgBlockIndex:= 56;
            end;
          context.FMsgBlock[56]:= (context.FLength shr 56);
          context.FMsgBlock[57]:= (context.FLength shr 48);
          context.FMsgBlock[58]:= (context.FLength shr 40);
          context.FMsgBlock[59]:= (context.FLength shr 32);
          context.FMsgBlock[60]:= (context.FLength shr 24);
          context.FMsgBlock[61]:= (context.FLength shr 16);
          context.FMsgBlock[62]:= (context.FLength shr 8) ;
          context.FMsgBlock[63]:= (context.FLength)       ;

          sha1_ProcessMessageBlock(context);

          FillChar(context.FMsgBlock[0], 64, #0);
          context.FLength:= 0;
          context.FComputed:= true;
        end;
      for i := 0 to ctSHA1HashSize -1 do
        byte(msgDigest[i]):= context.FInterimHash[i shr 2]shr(8*(3-(i and 3)));
    end;
end;

end.

