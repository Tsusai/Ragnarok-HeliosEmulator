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
	IdBaseComponent, IdComponent, IdTCPServer, IdCustomTCPServer, IdContext,
	IdIOHandler, IdIOHandlerStream, IdGlobal;

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
		procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure IdTCPServer1Exception(AContext: TIdContext;
      AException: Exception);
    procedure IdTCPServer1Disconnect(AContext: TIdContext);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

	TBufSize = 0..(High(Word) div 2);
	TCBuffer = array[TBufSize] of Char;

	procedure Debug(Line : string);

var
	MainForm: TMainForm;
	AppPath : string;
	NowUsers : word;


implementation

{$R *.xfm}

procedure Debug(Line : string);
begin
	MainForm.Console.Lines.Add(Line);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
	AppPath := ExtractFilePath(ParamStr(0));

	//Console.Lines.Add(AppPath);
end;

procedure WFIFOB(index:word; b:byte; var buf : TIdBytes);
begin
	Assert(index <= 32767, 'WFIFOB: index overflow ' + IntToStr(index));
	Move(b, buf[index], 1);
end;

procedure WFIFOW(index:word; w:word; var buf : TIdBytes);
begin
	Assert(index <= 32766, 'WFIFOW: index overflow ' + IntToStr(index));
	Move(w, buf[index], 2);
end;

function RFIFOS(index:word; cnt:word; buf : Tidbytes; stra : TCBuffer):string;
begin
	Assert(index <= 32767, 'RFIFOS: index overflow ' + IntToStr(index));
	Assert(index + cnt <= 32767, 'RFIFOS: index+cnt overflow ' + IntToStr(index+cnt));
	stra[cnt] := #0;
	Move(buf[index], stra, cnt);
	Result := stra;
end;

//INDY
procedure FakeLoginServer2(var AContext: TIdContext; PLength : integer);
const
	AccName = 'Tsusai';
	AccPass = 'Test';
var
	NameInput,
	PassInput : string[24];
	ID : word;
	Buffer : TIdBytes;
	stra   : TCBuffer;
	ERR : byte;
begin
	AContext.Connection.IOHandler.InputBuffer.ExtractToBytes(Buffer,Plength);
	NameInput := RFIFOS(6,24,Buffer,stra);
	PassInput := RFIFOS(30,24,Buffer,stra);
	Debug('RFIFO User : ' + NameInput);
	Debug('RFIFO Pass : ' + PassInput);
	if (AccName = NameInput) and (AccPass = PassInput) then
	begin
		ERR := 7;
	end else
	begin
		ERR := 0;
	end;

	ID := $006a;

	//Method: Buffer
	SetLength(Buffer,23);
	WFIFOW( 0, ID, Buffer);
	WFIFOB( 2, ERR, Buffer);
	AContext.Connection.IOHandler.Write(Buffer);


end;

//INDY
procedure TMainForm.IdTCPServer1Execute(AContext: TIdContext);
var
	PLength : integer;
begin
	if AContext.Connection.Connected then begin
		//Command places the socket information into it's InputBuffer class,
		//returns received length
		PLength := AContext.Connection.IOHandler.InputBuffer.Size;
		//PLength := AContext.Connection..Connection.ReadFromStack(false);
		//AThread.Connection.
		if PLength > 0 then
		begin
			Debug(IntToStr(PLength));
			//NowUsers := AContext..Threads.LockList.Count;
			StatusBar1.Panels.Items[0].Text := 'Connected: ' + IntToStr(NowUsers);
			FakeLoginServer2(AContext,PLength);
		end;
	end;
end;

//INDY
procedure TMainForm.IdTCPServer1Exception(AContext: TIdContext;
	AException: Exception);
begin
	if Acontext.Connection.Connected then
	begin
		AContext.Connection.Disconnect;
	end;
end;

procedure TMainForm.IdTCPServer1Disconnect(AContext: TIdContext);
begin
	if NowUsers > 0 then
	begin
		Dec(NowUsers);
	end;
	StatusBar1.Panels.Items[0].Text := 'Connected: ' + IntToStr(NowUsers);
end;

end.
