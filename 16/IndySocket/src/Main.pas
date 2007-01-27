//CLX application with indy
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
	QDialogs, QStdCtrls, QMenus, QExtCtrls, QComCtrls,
	IdBaseComponent, IdComponent, IdTCPServer;

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
		IdTCPServer1: TIdTCPServer;
		procedure FormCreate(Sender: TObject);
		procedure IdTCPServer1Execute(AThread: TIdPeerThread);
		procedure IdTCPServer1Exception(AThread: TIdPeerThread;
			AException: Exception);
		procedure IdTCPServer1Disconnect(AThread: TIdPeerThread);
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

//INDY
procedure FakeLoginServer2(var AThread: TIdPeerThread; PLength : integer);
const
	AccName = 'Tsusai';
	AccPass = 'Test';
var
	NameInput,
	PassInput : string;
	ID : word;
	Buffer : TBuffer;
	stra   : TCBuffer;
	ERR : byte;
begin
	(*AThread.Connection.InputBuffer.Seek(6,soFromBeginning);
	AThread.Connection.InputBuffer.Read(NameInput,24);
	AThread.Connection.InputBuffer.Seek(30,soFromBeginning);
	AThread.Connection.InputBuffer.Read(PassInput,24);*)
	AThread.Connection.ReadBuffer(Buffer,PLength);
	//AThread.Connection.InputBuffer.ReadBuffer(Buffer,PLength);
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
	//Method 1, no buffer, fragmented packet sending perhaps?
	//Might have to snif this....
	//[2005/12/04] - Tsusai: Sniff reveals no breaking, however, if it is
	//disconnected and can't send the WriteBuffer, a socket exception will occur.
	//ALSO LOOKIES! NO @#@#$@#$@#$ POINTERS, AThread.Data is TOBJECT!!!!.

	ID := $006a;
	//AThread.Connection.WriteBuffer(ID,2);
	//AThread.Connection.WriteBuffer(ERR,1);
	//ID := 0;
	//AThread.Connection.WriteBuffer(ID,20);//Fill in the rest of the gap

	//Method 2, Buffer
	FillChar(Buffer, 23, 0);
	WFIFOW( 0, ID, Buffer);
	WFIFOB( 2, ERR, Buffer);
	AThread.Connection.WriteBuffer(Buffer,23);

end;

//INDY
procedure TMainForm.IdTCPServer1Execute(AThread: TIdPeerThread);
var
	PLength : integer;
begin
	if AThread.Connection.Connected then begin
		//Command places the socket information into it's InputBuffer class,
		//returns received length
		PLength := Athread.Connection.ReadFromStack(false);
		if PLength > 0 then
		begin
			Debug(IntToStr(PLength));
			NowUsers := AThread.Connection.Server.Threads.LockList.Count;
			StatusBar1.Panels.Items[0].Text := 'Connected: ' + IntToStr(NowUsers);
			FakeLoginServer2(AThread,PLength);
		end;
	end;
end;

//INDY
procedure TMainForm.IdTCPServer1Exception(AThread: TIdPeerThread;
	AException: Exception);
begin
	if AThread.Connection.Connected then
	begin
		AThread.Connection.Disconnect;
	end;
end;

procedure TMainForm.IdTCPServer1Disconnect(AThread: TIdPeerThread);
begin
	if NowUsers > 0 then
	begin
		Dec(NowUsers);
	end;
	StatusBar1.Panels.Items[0].Text := 'Connected: ' + IntToStr(NowUsers);
	IdTCPServer1.Threads.Remove(AThread);
end;

end.
