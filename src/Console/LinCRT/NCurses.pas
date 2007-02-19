(*
  ncurses interface unit

  Parts of this file are translated from ncurses header file curses.h
  Copyright (c) 1998,1999,2000 Free Software Foundation, Inc.
  Translator: Andrei Borovsky - aborovsky@mtu-net.ru

  This traslation is by no means complete.
  I have included here only those ncurses functions that
  are used (or are planned to be used) in my LinCRT unit.
  I have also added here some routines and constants that
  I've found useful with ncurses.
*)

unit NCurses;

interface

const

  ERR = -1;
  {$EXTERNALSYM ERR}
  OK = 0;
  {$EXTERNALSYM OK}
  KEY_MOUSE = $199;
  {$EXTERNALSYM KEY_MOUSE}

  BUTTON1_RELEASED = $1;
  {$EXTERNALSYM BUTTON1_RELEASED}
  BUTTON1_PRESSED = $2;
  {$EXTERNALSYM BUTTON1_PRESSED}
  BUTTON1_CLICKED = $4;
  {$EXTERNALSYM BUTTON1_CLICKED}
  BUTTON1_DOUBLE_CLICKED = $8;
  {$EXTERNALSYM BUTTON1_DOUBLE_CLICKED}
  BUTTON2_RELEASED = $40;
  {$EXTERNALSYM BUTTON2_RELEASED}
  BUTTON2_PRESSED = $80;
  {$EXTERNALSYM BUTTON2_PRESSED}
  BUTTON2_CLICKED = $100;
  {$EXTERNALSYM BUTTON2_CLICKED}
  BUTTON2_DOUBLE_CLICKED = $200;
  {$EXTERNALSYM BUTTON2_DOUBLE_CLICKED}
  BUTTON3_RELEASED = $1000;
  {$EXTERNALSYM BUTTON3_RELEASED}
  BUTTON3_PRESSED = $2000;
  {$EXTERNALSYM BUTTON3_PRESSED}
  BUTTON3_CLICKED = $4000;
  {$EXTERNALSYM BUTTON3_CLICKED}
  BUTTON3_DOUBLE_CLICKED = $8000;
  {$EXTERNALSYM BUTTON3_DOUBLE_CLICKED}
  REPORT_MOUSE_POSITION = $8000000;
  {$EXTERNALSYM REPORT_MOUSE_POSITION}

  COLOR_BLACK = 0;
  {$EXTERNALSYM COLOR_BLACK}
  COLOR_RED = 1;
  {$EXTERNALSYM COLOR_RED}
  COLOR_GREEN = 2;
  {$EXTERNALSYM COLOR_GREEN}
  COLOR_YELLOW = 3;
  {$EXTERNALSYM COLOR_YELLOW}
  COLOR_BLUE = 4;
  {$EXTERNALSYM COLOR_BLUE}
  COLOR_MAGENTA = 5;
  {$EXTERNALSYM COLOR_MAGENTA}
  COLOR_CYAN = 6;
  {$EXTERNALSYM COLOR_CYAN}
  COLOR_WHITE = 7;
  {$EXTERNALSYM COLOR_WHITE}

  A_STANDOUT = 1 shl 16;
  {$EXTERNALSYM A_STANDOUT}
  A_UNDERLINE = 1 shl 17;
  {$EXTERNALSYM A_UNDERLINE}
  A_REVERSE = 1 shl 18;
  {$EXTERNALSYM A_REVERSE}
  A_BLINK = 1 shl 19;
  {$EXTERNALSYM A_BLINK}
  A_DIM = 1 shl 20;
  {$EXTERNALSYM A_DIM}
  A_BOLD = 1 shl 21;
  {$EXTERNALSYM A_BOLD}
  A_ALTCHARSET = 1 shl 22;
  {$EXTERNALSYM A_ALTCHARSET}
  A_INVIS = 1 shl 23;
  {$EXTERNALSYM A_INVIS}

type

  chtype = Integer;
  {$EXTERNALSYM chtype}
  PChtype = ^chtype;
  attr_t = chtype;
  {$EXTERNALSYM attr_t}
  NCURSES_COLOR_T = Word;
  {$EXTERNALSYM NCURSES_COLOR_T}
  NCURSES_SIZE_T = Word;
  {$EXTERNALSYM NCURSES_SIZE_T}
  mmask_t = Integer;
  {$EXTERNALSYM mmask_t}

  MEVENT = record
    id : Word;
    x, y, z : Integer;
    bstate : mmask_t;
  end;
  {$EXTERNALSYM MEVENT}

  ldat = record
    text : PChtype;
    firstchar : NCURSES_SIZE_T;
    lastchar : NCURSES_SIZE_T;
    oldindex : NCURSES_SIZE_T;
  end;
  {$EXTERNALSYM ldat}

  PLdat = ^ldat;

  pdat = record
    _pad_y, _pad_x : NCURSES_SIZE_T;
    _pad_top, _pad_left : NCURSES_SIZE_T;
    _pad_bottom, _pad_right : NCURSES_SIZE_T;
  end;
  {$EXTERNALSYM pdat}

  PWINDOW = ^_win_st;
  _win_st = record
    _cury, _curx : NCURSES_SIZE_T;
    _maxy, _maxx : NCURSES_SIZE_T;
    _begy, _begx : NCURSES_SIZE_T;
    _flags : Word;
    _attrs : attr_t;
    _bkgd : chtype;
    _notimeout : Boolean;
    _clear : Boolean;
    _leaveok : Boolean;
    _scroll : Boolean;
    _idlok : Boolean;
    _idcok : Boolean;
    _immed : Boolean;
    _sync : Boolean;
    _use_keypad : Boolean;
    _delay : Integer;
    _line : PLdat;
    _regtop : NCURSES_SIZE_T;
    _regbottom : NCURSES_SIZE_T;
    _parx : Integer;
    _pary : Integer;
    _parent : PWINDOW;
    _pad : pdat;
    _yoffset : NCURSES_SIZE_T;
  end;
  {$EXTERNALSYM _win_st}

  WINDOW = _win_st;
  {$EXTERNALSYM WINDOW}

  function cbreak : Integer; cdecl;
  {$EXTERNALSYM cbreak}
  function COLOR_PAIR(n : Integer) : Integer;
  function def_prog_mode : Integer; cdecl;
  {$EXTERNALSYM def_prog_mode}
  function define_key(definitions : PChar; keycode : Integer) : Integer; cdecl;
  {$EXTERNALSYM define_key}
  function delwin(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM delwin}
  function echo : Integer; cdecl;
  {$EXTERNALSYM echo}
  function endwin : Integer; cdecl;
  {$EXTERNALSYM endwin}
  function erasechar : Integer; cdecl;
  {$EXTERNALSYM erasechar}
  function getmouse(var event : MEVENT) : Integer; cdecl;
  {$EXTERNALSYM getmouse}
  function getyx(win : PWINDOW; var y, x : Word) : Integer;
  {$EXTERNALSYM getyx}
  function init_pair(pair, f, b : Word) : Integer; cdecl; //i
  {$EXTERNALSYM init_pair}
  function initscr : PWINDOW; cdecl;
  {$EXTERNALSYM initscr}
  function keypad(win : PWINDOW; b : Boolean) : Integer; cdecl;
  {$EXTERNALSYM keypad}
  function leaveok(win : PWINDOW; b : Boolean) : Integer; cdecl;
  {$EXTERNALSYM leaveok}
  function mousemask(newmask : mmask_t; var oldmask : mmask_t) : mmask_t; cdecl;
  {$EXTERNALSYM mousemask}
  function mvwaddch(win : PWINDOW; y, x : Integer; ch : chtype) : Integer; cdecl;
  {$EXTERNALSYM mvwaddch}
  function mvwin(win : PWINDOW; y, x : Integer) : Integer; cdecl;
  {$EXTERNALSYM mvwin}
  function NCURSES_BITS(mask, shift : Integer) : Integer;
  function newwin(nlines, ncols, begin_y, begin_x : Integer) : PWINDOW; cdecl;
  {$EXTERNALSYM newwin}
  function nl : Integer; cdecl;
  {$EXTERNALSYM nl}
  function nocbreak : Integer; cdecl;
  {$EXTERNALSYM nocbreak}
  function nodelay(win : PWINDOW; b : Boolean) : Integer; cdecl;
  {$EXTERNALSYM nodelay}
  function noecho : Integer; cdecl;
  {$EXTERNALSYM noecho}
  function notimeout(win : PWINDOW; b : Boolean) : Integer; cdecl;
  {$EXTERNALSYM notimeout}
  function pechochar(win : PWINDOW; ch : chtype) : Integer; cdecl;
  {$EXTERNALSYM pechochar}
  function resizeterm(lines, columns : Integer) : Integer; cdecl;
  {$EXTERNALSYM resizeterm}
  function scroll(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM scroll}
  function scrollok(win : PWINDOW; b : Boolean) : Integer; cdecl;
  {$EXTERNALSYM scrollok}
  function set_attr(win : PWINDOW; attr : Integer) : Integer;
  function start_color : Integer; cdecl;
  {$EXTERNALSYM start_color}
  function timeout(delay : Integer) : Integer; cdecl;
  {$EXTERNALSYM timeout}
  function ungetch(ch : Integer) : Integer; cdecl;
  {$EXTERNALSYM ungetch}
  function unset_attr(win : PWINDOW; attr : Integer) : Integer;
  function waddch(win : PWINDOW; ch : chtype) : Integer; cdecl;
  {$EXTERNALSYM waddch}
  function waddstr(win : PWINDOW; str : PChar) : Integer; cdecl;
  {$EXTERNALSYM waddstr}
  function wattr_get(win : PWINDOW; var attrs : attr_t; var pair : Word; opts : Pointer) : Integer; cdecl;
  {$EXTERNALSYM wattr_get}
  function wattrset(win : PWINDOW; attrs : Integer) : Integer;
  {$EXTERNALSYM wattrset}
  function wclear(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM wclear}
  function wclrtoeol(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM wclrtoeol}
  function wcolor_set(win : PWINDOW; Colors : Word; opts : Pointer) : Integer; cdecl;
  {$EXTERNALSYM wcolor_set}
  function wdelch(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM wdelch}
  function wdeleteln(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM wdeleteln}
  function wenclose(win : PWindow; y, x : Integer) : Boolean; cdecl;
  {$EXTERNALSYM wenclose}
  function werase(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM werase}
  function wgetch(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM wgetch}
  function wgetnstr(win : PWINDOW; str : PChar; n : Integer) : Integer; cdecl;
  {$EXTERNALSYM wgetnstr}
  function winch(win : PWINDOW) : chtype; cdecl;
  {$EXTERNALSYM winch}
  function winsch(win : PWINDOW; ch : chtype) : Integer; cdecl;
  {$EXTERNALSYM winsch}
  function winsdelln(win : PWINDOW; n : Integer) : Integer; cdecl;
  {$EXTERNALSYM winsdelln}
  function winsstr(win : PWINDOW; str : PChar) : Integer; cdecl;
  {$EXTERNALSYM winsstr}
  function wmouse_trafo(win : PWINDOW; var pY, pX : Integer; to_screen : Boolean) : Boolean; cdecl;
  {$EXTERNALSYM wmouse_trafo}
  function wmove(win : PWINDOW; y, x : Integer) : Integer; cdecl;
  {$EXTERNALSYM wmove}
  function wrefresh(win : PWINDOW) : Integer; cdecl;
  {$EXTERNALSYM wrefresh}

implementation

const

  libname = 'libncurses.so';

  function cbreak : Integer; cdecl; external libname name 'cbreak';
  function def_prog_mode : Integer; cdecl; external libname name 'def_prog_mode';
  function define_key(definitions : PChar; keycode : Integer) : Integer; cdecl; external libname name 'define_key';
  function delwin(win : PWINDOW) : Integer; cdecl; external libname name 'delwin';
  function echo : Integer; cdecl; external libname name 'echo';
  function endwin : Integer; cdecl; external libname name 'endwin';
  function erasechar : Integer; cdecl; external libname name 'erasechar';
  function getmouse(var event : MEVENT) : Integer; cdecl; external libname name 'getmouse';
  function init_pair(pair, f, b : Word) : Integer; cdecl; external libname name 'init_pair';
  function initscr : PWINDOW; cdecl; external libname name 'initscr';
  function keypad(win : PWINDOW; b : Boolean) : Integer; cdecl; external libname name 'keypad';
  function leaveok(win : PWINDOW; b : Boolean) : Integer; cdecl; external libname name 'leaveok';
  function mousemask(newmask : mmask_t; var oldmask : mmask_t) : mmask_t; cdecl; external libname name 'mousemask';
  function mvwaddch(win : PWINDOW; y, x : Integer; ch : chtype) : Integer; cdecl; external libname name 'mvwaddch';
  function mvwin(win : PWINDOW; y, x : Integer) : Integer; cdecl; cdecl; external libname name 'mvwin';
  function newwin(nlines, ncols, begin_y, begin_x : Integer) : PWINDOW; cdecl; external libname name 'newwin';
  function nl : Integer; cdecl; external libname name 'nl';
  function nocbreak : Integer; cdecl; external libname name 'nocbreak';
  function nodelay(win : PWINDOW; b : Boolean) : Integer; cdecl; external libname name 'nodelay';
  function noecho : Integer; cdecl; external libname name 'noecho';
  function notimeout(win : PWINDOW; b : Boolean) : Integer; cdecl; external libname name 'notimeout';
  function pechochar(win : PWINDOW; ch : chtype) : Integer; cdecl; external libname name 'pechochar';
  function resizeterm(lines, columns : Integer) : Integer; cdecl; external libname name 'resizeterm';
  function scroll(win : PWINDOW) : Integer; cdecl; external libname name 'scroll';
  function scrollok(win : PWINDOW; b : Boolean) : Integer; cdecl; external libname name 'scrollok';
  function start_color : Integer; cdecl; external libname name 'start_color';
  function timeout(delay : Integer) : Integer; cdecl; external libname name 'timeout';
  function ungetch(ch : Integer) : Integer; cdecl; cdecl; external libname name 'ungetch';
  function waddch(win : PWINDOW; ch : chtype) : Integer; cdecl; external libname name 'waddch';
  function waddstr(win : PWINDOW; str : PChar) : Integer; cdecl; external libname name 'waddstr';
  function wattr_get(win : PWINDOW; var attrs : attr_t; var pair : Word; opts : Pointer) : Integer; cdecl; external libname name 'wattr_get';
//  function wattrset(win : PWindow; attrs : Integer) : Integer; cdecl; external libname name 'wattrset';
  function wclear(win : PWINDOW) : Integer; cdecl; external libname name 'wclear';
  function wclrtoeol(win : PWINDOW) : Integer; cdecl; external libname name 'wclrtoeol';
  function wcolor_set(win : PWINDOW; Colors : Word; opts: Pointer) : Integer; cdecl; external libname name 'wcolor_set';
  function wdelch(win : PWINDOW) : Integer; cdecl; external libname name 'wdelch';
  function wdeleteln(win :PWINDOW) : Integer; cdecl; external libname name 'wdeleteln';
  function wenclose(win : PWINDOW; y, x : Integer) : Boolean; cdecl; external libname name 'wenclose';
  function werase(win : PWINDOW) : Integer; cdecl; external libname name 'werase';
  function wgetch(win : PWINDOW) : Integer; cdecl; external libname name 'wgetch';
  function wgetnstr(win : PWINDOW; str : PChar; n : Integer) : Integer; cdecl;  external libname name 'wgetnstr';
  function winch(win : PWINDOW) : chtype; cdecl; external libname name 'winch';
  function winsch(win : PWINDOW; ch : chtype) : Integer; cdecl; external libname name 'winsch';
  function winsdelln(win : PWINDOW; n : Integer) : Integer; cdecl; external libname name 'winsdelln';
  function winsstr(win : PWINDOW; str : PChar) : Integer; cdecl; external libname name 'winsstr';
  function wmouse_trafo(win : PWINDOW; var pY, pX : Integer; to_screen : Boolean) : Boolean;  cdecl; external libname name 'wmouse_trafo';
  function wmove(win : PWINDOW; y, x : Integer) : Integer; cdecl; external libname name 'wmove';
  function wrefresh(win : PWINDOW) : Integer; cdecl; external libname name 'wrefresh';

  function getyx;
  begin
    y := win._cury;
    x := win._curx;
    Result := 0;
  end;

  function set_attr;
  begin
    win._attrs := win._attrs or attr;
    Result := 0;
  end;

  function unset_attr;
  begin
    win._attrs := win._attrs and not attr;
    Result := 0;
  end;

  function NCURSES_BITS;
  begin
    Result := mask shl (shift+8);
  end;

  function COLOR_PAIR;
  begin
    Result := NCURSES_BITS(n, 0);
  end;

  function wattrset;
  begin
    win._attrs := attrs;
    Result:=0;
  end;
end.

