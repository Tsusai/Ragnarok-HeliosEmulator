unit Mailbox;

interface
uses
	Contnrs,
	Being;

type
	TMail = class
	public
		ID : LongWord;
		SenderID : LongWord;
		SenderName: String;
		ReceiverID : LongWord;
		ReceiverName : String;
		SendTime : LongWord;
		Title : String;
		Content : String;
		Read : Boolean;
	end;

	TMailBox = class
	private
		fChanged : Boolean;
		fMails : Byte;
		fNewMails : Byte; //How many new mails?
		fBeing : TBeing; //Since we can't loop back refence...
		fMailList : TObjectList;
		function GetValue(Index : Integer) : TMail;
	public
		property Changed : Boolean read fChanged;
		property Mails : Byte read fMails;
		property NewMails : Byte read fNewMails;
		property Item[Index : Integer] : TMail read GetValue;
		procedure LoadMails;
		procedure Clear;
		function Get(const AMailID:LongWord):TMail;
		function Delete(const AMailID:LongWord):Boolean;
		constructor Create(const ABeing:TBeing);
		destructor Destroy; override;
	end;

implementation
uses
	Character,
	ZoneSend,
	PacketTypes;

function TMailBox.GetValue(Index : Integer) : TMail;
begin
	Result := TMail(fMailList[Index]);
end;

procedure TMailBox.LoadMails;
var
	AChara : TCharacter;
begin
	if Changed then
	begin
		AChara := TCharacter(fBeing);
		TThreadLink(AChara.ClientInfo.Data).DatabaseLink.Mail.LoadMails(
			fMailList,
			AChara.ID,
			fMails,
			fNewMails
		);
		fChanged := False;
	end;
end;

procedure TMailBox.Clear;
begin
	fMailList.Clear;
end;

function TMailBox.Get(const AMailID:LongWord):TMail;
var
	AChara : TCharacter;
begin
	AChara := TCharacter(fBeing);
	Result :=
		TThreadLink(AChara.ClientInfo.Data).DatabaseLink.Mail.Get(
			AChara.ID,
			AMailID
		);
	fChanged := True; {Assume player reading an unread message}
end;

function TMailBox.Delete(const AMailID:LongWord):Boolean;
var
	AChara : TCharacter;
begin
	AChara := TCharacter(fBeing);
	Result :=
		TThreadLink(AChara.ClientInfo.Data).DatabaseLink.Mail.Delete(
			AChara.ID,
			AMailID
		);
	fChanged := True;
end;

constructor TMailBox.Create(const ABeing:TBeing);
begin
	{Set to True so we can load a fresh copy}
	fChanged := True;
	fBeing := ABeing;
	fMailList := TObjectList.Create(True);
end;{Create}

destructor TMailBox.Destroy;
begin
	fMailList.Free;
end;
end.