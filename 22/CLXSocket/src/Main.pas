//CLX application
unit Main;
(*------------------------------------------------------------------------------
Copyright (c) 2005, Matthew Mazanec (Tsusai), Kubia System Projects
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
------------------------------------------------------------------------------*)

interface

uses
	SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
	QDialogs, QStdCtrls, QMenus, QExtCtrls, Sockets, QComCtrls;

type
	TMainForm = class(TForm)
		Console: TMemo;
		StatusBar1: TStatusBar;
		Menu: TMainMenu;
		File1: TMenuItem;
		N1: TMenuItem;
		Server1: TMenuItem;
		Start1: TMenuItem;
		Stop1: TMenuItem;
		ProgressBar1: TProgressBar;
		TcpServer1: TTcpServer;
		procedure FormCreate(Sender: TObject);
		procedure TcpServer1Accept(Sender: TObject;
			ClientSocket: TCustomIpClient);
	private
		{ Private declarations }
	public
		{ Public declarations }
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

	procedure Debug(Line : string);

var
	MainForm: TMainForm;
	AppPath : string;
	NowUsers : word;


implementation
uses
	Database;

{$R *.xfm}

procedure Debug(Line : string);
begin
	MainForm.Console.Lines.Add(Line);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
	AppPath := ExtractFilePath(ParamStr(0));
	LoadDatabase;

	//Console.Lines.Add(AppPath);
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
procedure TMainForm.TcpServer1Accept(Sender: TObject;
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

end.
