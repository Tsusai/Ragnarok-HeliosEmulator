(*
  LinCRT Kylix unit v 1.0
  Author: Andrei Borovsky, aborovsky@mtu-net.ru
  Copyright (c) 2002 Andrei Borovsky
  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish,
  distribute, distribute with modifications, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Software.
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESSED

  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  IN NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
  DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
  OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
  THE USE OR OTHER DEALINGS IN THE SOFTWARE.

  Except as contained in this notice, the name(s) of the above copyright
  holders shall not be used in advertising or otherwise to promote the
  sale, use or other dealings in this Software without prior written
  authorization.
*)

unit LinCRT;

interface

uses
  Libc, NCurses;

const

// Pseudocharacter values returned by GetKey

  BS = 127; // Backspace
  LF = 10;
  TAB = 9;
  ESC = 27;

  kLArrow = 260;
  kRArrow = 261;
  kDArrow = 258;
  kUArrow = 259;
  kHome = 262;
  kBkSp = 263;  // Another Backspace code. Curses.h
                // says this value is unreliable (true ;-))
  kEnd = 264;
  kF1 = 265;
  kF2 = 266;
  kF3 = 267;
  kF4 = 268;
  kF5 = 269;
  kF6 = 270;
  kF7 = 271;
  kF8 = 272;
  kF9 = 273;
  kF10 = 274;
  kF11 = 275;
  kF12 = 276;
  kDel = 330;
  kIns = 331;
  kPgDn = 338;
  kPgUp = 339;

  mLButtonDown = 501;
  mLButtonUp = 502;
  mLButtonClk = 503;
  mLButtonDblClk = 504;
  mMButtonDown = 505;
  mMButtonUp = 506;
  mMButtonClk = 507;
  mMButtonDblClk = 508;
  mRButtonDown = 509;
  mRButtonUp = 510;
  mRButtonClk = 511;
  mRButtonDblClk = 512;
  mMouseMoved = 561;  // not supported yet

// Color constants

  Black = 0;
  Red = 1;
  Green = 2;
  Brown = 3;
  Blue = 4;
  Magenta = 5;
  Cyan = 6;
  Gray = 7;
  LightGray = 7;
  DarkGray = 7;
  LightRed = 9;
  LightGreen = 10;
  Yellow = 11;
  LightBlue = 12;
  LightMagenta = 13;
  LightCyan = 14;
  White = 15;
  Blink = 128;

// Additional attributes constants
// (as returned by GetAddAttrXY)
  aUnderline = 2;
  aInverse = 4;

type

  TOnTermResizeProc = procedure(DoingInput : Boolean);

var

  StdScr, CurWnd : PWINDOW;
  OnTermResize : TOnTermResizeProc;
  ScrCols, ScrLines : Integer;
  WindMin, WindMax : Integer;
  TextAttr : Byte;
  Inverse, Underline : Boolean;
  ScrollWnd : Boolean;
  BreakOnResize, Broken  : Boolean;

  // Clears up to the end of the current line
  procedure ClrEol;
  // Clears current window
  procedure ClrScr;
  // Deletes current character
  procedure DelChar;
  //  Deletes current line
  procedure DelLine;
  // Finishes with LinCRT
  procedure DoneLinCRT;
  // Returns additional attributes
  function GetAddAttrXY(X, Y : Byte) : Byte;
  // Returns character at the specified position
  function GetCharXY(X, Y : Byte) : Char;
  // Returns character color at the specified position
  function GetColorXY(X, Y : Byte) : Byte;
  // Reads the key from the mouse or keyboard
  function GetKey : Word;
  // Sets the cursor position within the current window
  procedure GotoXY(X, Y : Byte);
  // Initializes the library
  procedure InitLinCRT;
  // Initializes mouse handling
  procedure InitMouse;
  // Inserts character in the current position
  procedure InsChar(Ch : Char);
  // Inserts new line in the current position
  procedure InsLine;
  // Determines if the key has been pressed
  function KeyPressed : Boolean;
  // Finishes mouse handling
  procedure KillMouse;
  // Checks if the mouse event occured within the current winodw
  function MouseTrapped : Boolean;
  // Puts character in the current position
  procedure PutChar(Ch : Char);
  // Puts character in the position X, Y
  procedure PutCharXY(Ch : Char; X, Y : Byte);
  // Reads the key from the keyboard
  function ReadKey : Char;
  // Sets or disables window scrollng
  procedure SetScrollWnd(b : Boolean);
  // Sets text and background colors
  procedure SetColors(Fg, Bg : Byte);
  // Starts the copy of the shell
  procedure ShellOut(Cmd : PChar);
  // Sets background color
  procedure TextBackground(Color : Byte);
  // Sets text color
  procedure TextColor(Color : Byte);
  // Returns the mouse position at the last mouse event
  function WhereMouseXY : Word;
  // Returns the curent X position
  function WhereX : Byte;
  // Returns the curent Y position
  function WhereY : Byte;
  // Opens the new window
  procedure Window(X1, Y1, X2, Y2 : Byte);


implementation

const

  BUF_SIZE = 1024;
  STACK_SIZE = 64;  // This value is the number of color pairs
                    // supported by ncurses

type

  TXtAttr = record
    Fg, Bg,
    PairNum : Byte;
  end;

var

  IOBuf : array[0..BUF_SIZE] of Byte;
  PairStack : array[0..STACK_SIZE-1] of TXtAttr;
  StackHead : Integer;
  MouseEvent : MEVENT;
  SpKey : Byte;
  DoingInput, Intr : Boolean;

// Internal routines

  procedure winch_handler( SigNum : Integer); stdcall;
  begin
    if BreakOnResize then Intr := True;
    endwin;
    wrefresh(StdScr);
    ScrCols := StdScr._maxx;
    ScrLines := StdScr._maxy;
    if CurWnd <> StdScr then delwin(CurWnd);
    endwin;
    wrefresh(StdScr);
    CurWnd :=StdScr;
    if Assigned(OnTermResize) then OnTermResize(DoingInput);
  end;

  function GetPair(Fg, Bg : Byte) : Integer;
  var
    i, Pos : Integer;
  begin
    Pos := -1;
    for i := 0 to StackHead do
    if (PairStack[i].Fg = Fg) and (PairStack[i].Bg = Bg) then
    begin
      Pos := i;
      Break;
    end;
    if Pos < 0 then
    begin
      Pos := StackHead;
      if StackHead < (STACK_SIZE - 1) then
      begin
        Inc(StackHead);
        init_pair(StackHead+1, Fg, Bg);
        PairStack[StackHead].PairNum := StackHead+1;
        PairStack[StackHead].Fg := Fg;
        PairStack[StackHead].Bg := Bg;
        Pos := StackHead;
      end;
    end;
    Result := PairStack[Pos].PairNum;
  end;

  procedure SetAttrs;
  var
    Pn : Integer;
  begin
    Pn := GetPair(TextAttr and 7, (TextAttr and 112) shr 4);
    wattrset(CurWnd, COLOR_PAIR(Pn));
    CurWnd._bkgd := (Pn shl 8) + 32;
    if (TextAttr and 128) <> 0 then
    set_attr(CurWnd, A_BLINK);
    if (TextAttr and 8) <> 0 then
    set_attr(CurWnd, A_BOLD);
    if Underline then set_attr(CurWnd, A_UNDERLINE);
    if Inverse then set_attr(CurWnd, A_REVERSE);
  end;

  function TranslateMEvent : Word;
  begin
    case MouseEvent.bstate of
      BUTTON1_RELEASED : Result := mLButtonUp;
      BUTTON1_PRESSED : Result := mLButtonDown;
      BUTTON1_CLICKED : Result := mLButtonClk;
      BUTTON1_DOUBLE_CLICKED : Result := mLButtonDblClk;
      BUTTON2_RELEASED : Result := mRButtonUp;
      BUTTON2_PRESSED : Result := mRButtonDown;
      BUTTON2_CLICKED : Result := mRButtonClk;
      BUTTON2_DOUBLE_CLICKED : Result := mRButtonDblClk;
      BUTTON3_RELEASED : Result := mMButtonUp;
      BUTTON3_PRESSED : Result := mMButtonDown;
      BUTTON3_CLICKED : Result := mMButtonClk;
      BUTTON3_DOUBLE_CLICKED : Result := mMButtonDblClk;
      REPORT_MOUSE_POSITION : Result := mMouseMoved;
    end;
  end;

  procedure DefineKey(c1, c2, c3 : Byte; Code : Word);
  var
    Seq : array[0..4] of Byte;
  begin
    Seq[0] := 27;
    Seq[1] := c1;
    Seq[2] := c2;
    Seq[3] := c3;
    Seq[4] := 0;
    define_key(@Seq[0], Code);
  end;

(*
 Here we initiate some pseudocharacter values that
 for some reason are not initiated by ncurses itself
 (at leas on my system). You may add/remove some
 key definitions here, if your program requires this.
 Note, that on any terminal there is a sequence that
 will not be enterpreted correctly.
*)

  procedure DefineKeys;
  begin
    DefineKey(91, 91, 65, kF1);
    DefineKey(91, 91, 66, kF2);
    DefineKey(91, 91, 67, kF3);
    DefineKey(91, 91, 68, kF4);
    DefineKey(91, 91, 69, kF5);
    DefineKey(91, 70, 0, kEnd);
    DefineKey(91, 72, 0, kHome);
    DefineKey(91, 49, 126, kHome);
    DefineKey(91, 52, 126, kEnd);
  end;

  function TranslateCodes(C : Word) : Byte;
  begin
    case C of
      kF1 : Result := 59;
      kF2 : Result := 60;
      kF3 : Result := 61;
      kF4 : Result := 62;
      kF5 : Result := 63;
      kF6 : Result := 64;
      kF7 : Result := 65;
      kF8 : Result := 66;
      kF9 : Result := 67;
      kF10 : Result := 68;
      kF11 : Result := 69;
      kF12 : Result := 70;
      kHome : Result := 71;
      kUArrow : Result := 72;
      kPgUp : Result := 73;
      kLArrow : Result := 75;
      kRArrow : Result := 77;
      kEnd : Result := 79;
      kDArrow : Result := 80;
      kPgDn : Result := 81;
      kIns : Result := 82;
      kDel : Result := 83;
      else Result := 0;
    end;
  end;

  function wread(Win : PWINDOW; Buf : PChar; n : Integer) : Integer;
  var
    tx, ty : Word;
    Count, ch : Integer;
  begin
    Intr := False;
    Broken := False;
    noecho;
    cbreak;
    nodelay(Win, True);
    Count := 0;
    tx := Win._curx;
    ty := Win._cury;
    waddch(Win, 32);
    Win._cury := ty;
    Win._curx := tx;
    wrefresh(Win);
    ch := 0;
    while ch <> 10 do
    begin
      if Intr then
      begin
        Result := 0;
        Broken := True;
        Buf[Count] := #0;
        Exit;
      end;
      ch := wgetch(Win);
      case ch of
        32..126, 128..255 :
        begin
          waddch(Win, ch);
          tx := Win._curx;
          ty := Win._cury;
          waddch(Win, 32);
          Win._cury := ty;
          Win._curx := tx;
          wrefresh(Win);
          Buf[Count] := Char(Lo(ch));
          Inc(Count);
          if Count = (n - 1) then
          begin
            Buf[Count] := #0;
            Result := 0;
            Exit;
          end;
        end;
        BS, kBkSp, kLArrow :
        begin
          tx := Win._curx;
          ty := Win._cury;
          if Count > 0 then
          begin
            if tx > 0 then Dec(tx)
            else if ty > 0 then
            begin
              Dec(ty);
              tx := Win._maxx;
            end;
            Win._curx := tx;
            Win._cury := ty;
            waddch(Win, 32);
            Win._curx := tx;
            Win._cury := ty;
            wrefresh(Win);
            Dec(Count);
          end;
        end;
      end;
    end;
    Buf[Count] := #0;
    waddch(Win, 10);
    Result := 0;
  end;

// Text file driver routines

  function NCInOut(var F : Text) : Integer;
  begin
    SetAttrs;
    with TTextRec(F) do
    begin
      if Mode = fmOutput then
      begin
        IOBuf[BufPos] := 0;
        waddstr(CurWnd, @IOBuf[0]);
        BufPos := 0;
        Result := 0;
      end else
      if Mode = fmInput then
      begin
        DoingInput := True;
        echo;
        nocbreak;
        nodelay(CurWnd, False);
        //wgetnstr(CurWnd, @IOBuf[0], BUF_SIZE-2);
        wread(CurWnd, @IOBuf[0], BUF_SIZE-1);
        BufEnd := __strlen(@IOBuf[0])+1;
        if BufEnd < BUF_SIZE then IOBuf[BufEnd-1] := 10;
        BufPos := 0;
        noecho;
        cbreak;
        nodelay(CurWnd, True);
        DoingInput := False;
        Result := 0;
      end;
    end;
  end;

  function NCFlush(var F : Text) : Integer;
  begin
    with TTextRec(F) do
    begin
      if Mode = fmOutput then
      begin
        SetAttrs;
        IOBuf[BufPos] := 0;
        waddstr(CurWnd, @IOBuf[0]);
        wrefresh(CurWnd);
        BufPos := 0;
      end else
      begin
        BufEnd := 0;
        FillChar(IOBuf[0], BUF_SIZE, 0);
        BufPos := 0;
      end;
    end;
    Result := 0;
  end;

  function NCClose(var F : Text) : Integer;
  begin
    TTextRec(F).Mode := fmClosed;
    Result := 0;
  end;

  function NCOpen(var F : Text) : Integer;
  begin
    with TTextRec(F) do
    begin
      if Mode = fmInOut then Mode := fmOutput;
      InOutFunc := @NCInOut;
      FlushFunc := @NCFlush;
      CloseFunc := @NCClose;
      Result:=0;
    end;
    Result := 0;
  end;

  procedure AssignLinCRT(var F : Text);
  begin
    with TTextRec(F) do
    begin
      Mode := fmClosed;
      BufSize := BUF_SIZE;
      BufPtr := @IOBuf[0];
      OpenFunc := @NCOpen;
      InOutFunc := nil;
      FlushFunc := nil;
      CloseFunc := nil;
      Name[0] := #0;
    end;
  end;

// Public LinCRT routines

  procedure ClrEol;
  begin
    SetAttrs;
    wclrtoeol(CurWnd);
    wrefresh(CurWnd);
  end;

  procedure ClrScr;
  begin
    SetAttrs;
    wclear(CurWnd);
  end;

  procedure DelChar;
  begin
    wdelch(CurWnd);
    wrefresh(CurWnd);
  end;

  procedure DelLine;
  begin
    wdeleteln(CurWnd);
    wrefresh(CurWnd);
  end;

  procedure DoneLinCRT;
  begin
    endwin;
  end;

  function GetAddAttrXY(X, Y : Byte) : Byte;
  var
    tx, ty : Integer;
  begin
    tx := CurWnd._curx;
    ty := CurWnd._cury;
    CurWnd._curx := X - 1;
    CurWnd._cury := Y - 1;
    Result := Lo(winch(CurWnd) shr 16);
    CurWnd._curx := tx;
    CurWnd._cury := ty;
  end;

  function GetCharXY(X, Y : Byte) : Char;
  var
    tx, ty : Integer;
  begin
    tx := CurWnd._curx;
    ty := CurWnd._cury;
    CurWnd._curx := X - 1;
    CurWnd._cury := Y - 1;
    Result := Char(Lo(winch(CurWnd)));
    CurWnd._curx := tx;
    CurWnd._cury := ty;
  end;

  function GetColorXY(X, Y : Byte) : Byte;
  var
    tx, ty : Integer;
  begin
    tx := CurWnd._curx;
    ty := CurWnd._cury;
    CurWnd._curx := X - 1;
    CurWnd._cury := Y - 1;
    Result := Lo(winch(CurWnd) shr 8);
    CurWnd._curx := tx;
    CurWnd._cury := ty;
  end;

  function GetKey : Word;
  var
    Key : Integer;
  begin
 // If You have some problems with GetKey,
 // see the note above the DefineKeys procedure
    noecho;
    nodelay(CurWnd, False);
    cbreak;
    Key := wgetch(CurWnd);
    if Key = KEY_MOUSE then
    begin
      getmouse(MouseEvent);
      Key := TranslateMEvent;
    end;
    Result := Key;
  end;

  procedure GotoXY(X, Y : Byte);
  begin
    if (X > 0) and (X < (CurWnd._maxx+2)) then
    CurWnd._curx := X - 1;
    if (Y > 0) and (Y < (CurWnd._maxy+2)) then
    CurWnd._cury := Y - 1;
    wrefresh(CurWnd);
    // wmove(CurWnd, Y-1, X-1);
  end;

  procedure InitLinCRT;
  begin
    StdScr := initscr;
    ScrCols := StdScr._maxx;
    ScrLines := StdScr._maxy;
    CurWnd := StdScr;
    WindMin := Lo(CurWnd._begx) + (Lo(CurWnd._begy) shl 8);
    WindMax := Lo(CurWnd._maxx) + (Lo(CurWnd._maxy) shl 8);
    start_color;
    OnTermResize := nil;
    signal(SIGWINCH, @winch_handler);
    TextAttr := 7;
    Inverse := False;
    Underline := False;
    StackHead := -1;
    BreakOnResize := False;
    Broken := False;
    DoingInput := False;
    ScrollWnd := True;
    AssignLinCRT(Input);
    Reset(Input);
    AssignLinCRT(Output);
    ReWrite(Output);
    keypad(StdScr, True);
    DefineKeys;
    SpKey := 0;
    Intr := False;
  end;

  procedure InitMouse;
  var
    mask, oldmask : Integer;
  begin
    mask := BUTTON1_RELEASED or BUTTON1_PRESSED or BUTTON1_CLICKED or
    BUTTON1_DOUBLE_CLICKED;
    mask := mask or BUTTON2_RELEASED or BUTTON2_PRESSED or BUTTON2_CLICKED or
    BUTTON2_DOUBLE_CLICKED;
    mask := mask or BUTTON3_RELEASED or BUTTON3_PRESSED or BUTTON3_CLICKED or
    BUTTON3_DOUBLE_CLICKED;
//    if TrackMove then mask := mask or REPORT_MOUSE_POSITION;
    mousemask(mask, oldmask);
  end;

  procedure InsChar(Ch : Char);
  begin
    SetAttrs;
    winsch(CurWnd, Integer(Ch));
    wrefresh(CurWnd);
  end;

  procedure InsLine;
  begin
    SetAttrs;
    winsdelln(CurWnd, 1);
    wrefresh(CurWnd);
  end;

  function KeyPressed : Boolean;
  var
    ch : chtype;
  begin
    noecho;
    nodelay(CurWnd, True);
    cbreak;
    Result := False;
    ch := wgetch(CurWnd);
    if ch <> ERR then
    begin
      ungetch(ch);
      Result := True;
    end;
  end;

  procedure KillMouse;
  var
    oldmask : Integer;
  begin
    mousemask(0, oldmask);
  end;

  function MouseTrapped : Boolean;
  begin
    Result := wenclose(CurWnd, MouseEvent.y, MouseEvent.x);
  end;

  procedure PutChar(Ch : Char);
  var
    tx, ty : Integer;
  begin
    scrollok(CurWnd, False);
    tx := CurWnd._curx;
    ty := CurWnd._cury;
    SetAttrs;
    waddch(CurWnd, Byte(Ch));
    CurWnd._curx := tx;
    CurWnd._cury := ty;
    wrefresh(CurWnd);
    scrollok(CurWnd, ScrollWnd);
  end;

  procedure PutCharXY(Ch : Char; X, Y : Byte);
  var
    tx, ty : Integer;
  begin
    scrollok(CurWnd, False);
    tx := CurWnd._curx;
    ty := CurWnd._cury;
    CurWnd._curx := X-1;
    CurWnd._cury := Y-1;
    SetAttrs;
    waddch(CurWnd, Byte(Ch));
    CurWnd._curx := tx;
    CurWnd._cury := ty;
    wrefresh(CurWnd);
    scrollok(CurWnd, ScrollWnd);
  end;

  function ReadKey : Char;
  var
    ch : chtype;
  begin
    if SpKey <> 0 then
    begin
      Result := Char(SpKey);
      SpKey := 0;
      Exit;
    end;
    noecho;
    nodelay(CurWnd, False);
    cbreak;
    ch := wgetch(CurWnd);
    if ch > 255 then
    begin
      SpKey := TranslateCodes(ch);
      ch := 0;
    end;
    Result := Char(Lo(ch));
  end;

  procedure SetColors(Fg, Bg : Byte);
  begin
    TextAttr := TextAttr and (255 - 112);
    TextAttr := TextAttr or (Bg shl 4);
    if Fg = Blink then TextAttr := TextAttr or Blink
    else
    begin
      TextAttr := TextAttr and 127;
      TextAttr := TextAttr and (255 - 15);
      TextAttr := TextAttr or Fg;
    end;
  end;

  procedure SetScrollWnd(b : Boolean);
  begin
    ScrollWnd := b;
    scrollok(CurWnd, ScrollWnd);
  end;

  procedure ShellOut(Cmd : PChar);
  begin
    def_prog_mode;
    endwin;
    Libc.system(Cmd);
    wrefresh(StdScr);
    wrefresh(CurWnd);
  end;

  procedure TextBackground(Color : Byte);
  begin
    TextAttr := TextAttr and (255 - 112);
    TextAttr := TextAttr or (Color shl 4);
  end;

  procedure TextColor(Color : Byte);
  begin
    if Color = Blink then TextAttr := TextAttr or Blink
    else
    begin
      TextAttr := TextAttr and 127;
      TextAttr := TextAttr and (255 - 15);
      TextAttr := TextAttr or Color;
    end;
  end;

  function WhereMouseXY : Word;
  var
    X, Y : Integer;
  begin
    X := MouseEvent.x;
    Y := MouseEvent.y;
    wmouse_trafo(StdScr, Y, X, False);
    Result := Lo(X) +1  + ((Lo(Y) + 1) shl 8);
  end;

  function WhereX : Byte;
  begin
    Result := Lo(CurWnd._curx + 1);
  end;

  function WhereY : Byte;
  begin
    Result := Lo(CurWnd._cury + 1);
  end;

  procedure Window(X1, Y1, X2, Y2 : Byte);
  var
    Tmp : PWINDOW;
  begin
    Tmp := CurWnd;
    CurWnd := newwin(Y2-Y1+1, X2-X1+1, Y1-1, X1-1);
    if CurWnd = nil then CurWnd := Tmp
    else if Tmp <> StdScr then delwin(Tmp);
    WindMin := Lo(CurWnd._begx) + (Lo(CurWnd._begy) shl 8);
    WindMax := Lo(CurWnd._maxx) + (Lo(CurWnd._maxy) shl 8);
    scrollok(CurWnd, ScrollWnd);
    SetAttrs;
    keypad(CurWnd, True)
  end;

initialization
// dummy

finalization

  endwin;

end.
