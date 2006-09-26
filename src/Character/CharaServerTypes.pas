(*------------------------------------------------------------------------------
CharaServerTypes
Tsusai 2006

Description:
 The Character Server Type.
 An object type that contains all information about a character server
 This unit is to help for future design of multi server communication IF the
 user were to want to do so.  Else, it would act as an all-in-one.
	Only one type in this unit for the time being.
------------------------------------------------------------------------------*)
unit CharaServerTypes;

interface
type
	TCharaServ = class
  protected
  //
	private
		fIP           : widestring;

		procedure SetIPCardinal(Value : widestring);

	public
		IPCardinal    : cardinal;
		Port          : word;
		ServerName    : string;
		OnlineUsers   : word;
		InternalServer: boolean;

		property IP   : widestring read fIP write SetIPCardinal;

	end;

implementation
uses
	//Helios
	WinLinux;

(*------------------------------------------------------------------------------
TCharaServ.SetIPCardinal

The Ragnarok client does not connect to a server using the plain x.x.x.x IP
string format.  It uses a cardinal form.  Making the IP a property, we are able
to call a function to go ahead and set the Cardinal form at any time.
------------------------------------------------------------------------------*)
procedure TCharaServ.SetIPCardinal(Value : widestring);
begin
	IPCardinal  := GetCardinalFromIPString(Value);
	fIP         := Value;
end; (*proc TCharaServ.SetIPCardinal
------------------------------------------------------------------------------*)

end.

