//------------------------------------------------------------------------------
//CharaServerTypes			                                                UNIT
//------------------------------------------------------------------------------
//	What it does-
//      The Character Server Type.
//    An object type that contains all information about a character server
//    This unit is to help for future design of multi server communication IF
//    the user were to want to do so.  Else, it would act as an all-in-one.
//	  Only one type in this unit for the time being.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
unit CharaServerTypes;

interface
type
//------------------------------------------------------------------------------
//TCharaServ                                                              CLASS
//------------------------------------------------------------------------------
	TCharaServ = class
  protected
  //
	private
		fIP           : string;

		procedure SetIPCardinal(Value : string);

	public
		IPCardinal    : cardinal;
		Port          : word;
		ServerName    : string;
		OnlineUsers   : word;
		InternalServer: boolean;

		property IP   : string read fIP write SetIPCardinal;

	end;
//------------------------------------------------------------------------------


implementation
uses
	//Helios
	WinLinux;

//------------------------------------------------------------------------------
//CharaServerProcess			                                                UNIT
//------------------------------------------------------------------------------
//	What it does-
//      The Ragnarok client does not connect to a server using the plain x.x.x.x
//    IP string format.  It uses a cardinal form.  Making the IP a property, we
//    are able to call a function to go ahead and set the Cardinal form at any
//    time.
//
//	Changes -
//		December 17th, 2006 - RaX - Created Header.
//
//------------------------------------------------------------------------------
procedure TCharaServ.SetIPCardinal(Value : string);
begin
	fIP         := GetIPStringFromHostname(Value);
	IPCardinal  := GetCardinalFromIPString(fIP);
end; //proc SetIPCardinal
//------------------------------------------------------------------------------

end.

