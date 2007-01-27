program HeliosConsole;

{$APPTYPE CONSOLE}

//Console application
(*------------------------------------------------------------------------------
Socket Reading code License:
Copyright (c) 2005-2006, Matthew Mazanec (Tsusai), Kubia System Projects
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

		* Redistributions of source code must retain the above copyright notice,
			this list of conditions and the following disclaimer.
		* Redistributions in binary form must reproduce the above copyright notice,
			this list of conditions and the following disclaimer in the documentation
			and/or other materials provided with the distribution.
		* Neither the name of Kubia Systems nor the names of its contributors may be
			used to endorse or promote products derived from this software without
			specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



TConApplication Console event driven application setup
Copyright (C) 1999-2002 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be><francois.piette@swing.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

(I'll do it i swear!!!!)
------------------------------------------------------------------------------*)

uses
  SysUtils,
  Types,
  Classes,
  Variants,
  QTypes,
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  QStdCtrls,
  QMenus,
  QExtCtrls,
  Sockets,
  QComCtrls;

type
	TConApplication = class(TComponent)
	protected
		FLoginServer : TTcpServer;
		FPort            : String;
		//procedure DoListen;
		//procedure DoClose;
		//procedure DoPort(const Param : String);
		procedure TcpServer1Accept(Sender: TObject;
			ClientSocket: TCustomIpClient);
		function  ProcessCommand(const Command : String): boolean;
	public
		constructor Create(AOwner : TComponent); override;
		destructor  Destroy; override;
		procedure   Execute;
	end;


//Some Idea of a client type class perhaps....
type
	ClientInfo   = class
		PeerIP      : string[15];            { Client IP address }
		HostName    : String[40];            { Hostname }
		Connected,                           { Time of connect }
		LastAction  : TDateTime;             { Time of last transaction }
		Thread      : Pointer;               { Pointer to thread }
	end;

	TBufSize = 0..(High(Word) div 2);

	TBuffer  = array[TBufSize] of Byte;
	TCBuffer = array[TBufSize] of Char;


var
	AppPath : string;
	NowUsers : word;


constructor TConApplication.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
		FLoginServer := TTcpServer.Create(nil);
		FPort            := '6900';
end;

destructor  TConApplication.Destroy;
begin
	if Assigned(FLoginServer) then
			FreeAndNil(FLoginServer);
	inherited Destroy;
end;

procedure TConApplication.Execute;
var
	Command : String;
begin
	FLoginServer.OnAccept := TcpServer1Accept;
	FLoginServer.LocalPort := FPort;
	FLoginServer.Active := true;
	WriteLn('Using Port="', FPort, '"');
	WriteLn('Enter $H to get help, $Q to quit.');
	while TRUE do begin //KEEP ALIVE
		Write('Enter command >');
		ReadLn(Command);
		if (Length(Command) > 0) and (Command[1] = '$') then begin
			if not ProcessCommand(Command) then
			begin
				break;
			end;
		end
		else begin
			WriteLn('Type $H to get help.');
		end;
		//Sleep(1000);
	end;
end;

function TConApplication.ProcessCommand(const Command : String): boolean;
var
	Param : String;
	I     : Integer;
begin
	Result := TRUE;
	if Length(Command) < 2 then
		Exit;
	I := Pos(' ', Command);
	if I > 0 then
		Param := Trim(Copy(Command, I + 1, Length(Command)))
	else
		Param := '';
	try
		case UpperCase(Command[2])[1] of
		'H': WriteLn('Enter command preceded by dollar sign.'#10 +
								 'Any line not beginning by "$" is considered data'#10 +
								 'and send as is to the server.'#10 +
								 'Commands:'#10 +
								 '  H         This help screen'#10 +
								 '  P         Select server port'#10 +
								 '  L         Listen for clients'#10 +
								 '  S         Stop listenning'#10 +
								 '  Q         Return to system'#10);
		//'P': DoPort(Param);
		//'L': DoListen;
		//'S': DoClose;
		'Q': Result := FALSE;
		else
			WriteLn('Unknown command. Type $H to get help.');
		end;
	except
		on E:Exception do WriteLn('Exception ' + E.ClassName + ': ' + E.Message);
	end;
end;

procedure Debug(Line : string);
begin
	Writeln(Line);
end;

procedure WFIFOB(index:word; b:byte; var buf : TBuffer);
begin
	Assert(index <= 32767, 'WFIFOB: index overflow ' + IntToStr(index));
	Move(b, buf[index], 1);
end;

procedure WFIFOW(index:word; w:word; var buf : TBuffer);
begin
	Assert(index <= 32766, 'WFIFOW: index overflow ' + IntToStr(index));
	Move(w, buf[index], 2);
end;

function RFIFOS(index:word; cnt:word; buf : TBuffer; stra : TCBuffer):string;
begin
	Assert(index <= 32767, 'RFIFOS: index overflow ' + IntToStr(index));
	Assert(index + cnt <= 32767, 'RFIFOS: index+cnt overflow ' + IntToStr(index+cnt));
	stra[cnt] := #0;
	Move(buf[index], stra, cnt);
	Result := stra;
end;

//Internet Component Tab, TcpServer (Socket) CLX
procedure FakeLoginServer1(ClientSocket : TCustomIpClient; Plength : integer);
//I get random access violations with this one on shutdown.
//Mad isn't catching to help me.  Possiblity with socket disconnects exception
//But dunno how to catch it
const
	AccName = 'Tsusai';
	AccPass = 'Test';
var
	NameInput,
	PassInput : string;
	Buffer : TBuffer;
	stra   : TCBuffer;
	ERR : byte;
begin
	ClientSocket.ReceiveBuf(Buffer,PLength);
	NameInput := RFIFOS(6,24,Buffer,stra);
	PassInput := RFIFOS(30,24,Buffer,stra);
	Debug('User : ' + NameInput);
	Debug('Pass : ' + PassInput);
	if (AccName = NameInput) and (AccPass = PassInput) then
	begin
		ERR := 7;
	end else
	begin
		ERR := 0;
	end;
	FillChar(Buffer, 23, 0);
	WFIFOW( 0, $006a, Buffer);
	WFIFOB( 2, ERR, Buffer);
	ClientSocket.SendBuf(Buffer, 23);
end;



//Internet Component Tab, TcpServer (Socket) CLX
procedure TConApplication.TcpServer1Accept(Sender: TObject;
	ClientSocket: TCustomIpClient);
var
	lgt : integer;
	buf : array [0..511] of byte;
begin
	//Peek-ing at the socket buffer without modifying or reading it.  Will return
	//a length
	lgt := ClientSocket.PeekBuf(buf, 510);
	Debug(IntToStr(lgt));
	FakeLoginServer1(ClientSocket,lgt);

end;

var
	ConApp : TConApplication;

begin
	try
		ConApp := TConApplication.Create(nil);
		try
			ConApp.Execute;
		finally
			FreeAndNil(ConApp);
		end;
			WriteLn('Good bye...');
	except
		on E:Exception do
			WriteLn('Exception occured. ', E.ClassName, ': ', E.Message);
	end;
end.

