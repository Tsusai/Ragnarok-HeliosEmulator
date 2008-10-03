rem ----------------------------------------------------------------
rem Helios DCC32.exe Batch File
rem You must set all the correct paths for this to work!!!!
rem ----------------------------------------------------------------
@echo off

rem ----------------------------------------------------------------
rem Environmental variables
rem ----------------------------------------------------------------
echo Setting enviromental paths

rem ----------------------------------------------------------------
rem Helios Folder with bin, obj, and src folders
rem ----------------------------------------------------------------
set helios=c:\users\tsusai\desktop\delphi\helios

rem ----------------------------------------------------------------
rem Indy 10 library (dcu) files
rem ----------------------------------------------------------------
set indylibdcu=C:\Program Files\CodeGear\RAD Studio\5.0\lib\Indy10

rem ----------------------------------------------------------------
rem Zeoslib.  If installed proper, there are dcu's to use
rem ----------------------------------------------------------------
set zeoslibdcu=C:\Delphi\Zeos\packages\delphi11\build

rem ----------------------------------------------------------------
rem Madcollection.  DeXter D2007's codename.
rem ----------------------------------------------------------------
set madbasic=C:\Program Files\madCollection\madBasic\DeXter
set maddisasm=C:\Program Files\madCollection\madDisAsm\DeXter
set madexcept=C:\Program Files\madCollection\madExcept\DeXter

rem ----------------------------------------------------------------
rem SVN Section.  Uncomment if you want to update first
rem ----------------------------------------------------------------
rem set svndir=c:\
rem cd %helios%
rem %svndir%\svn update .
rem %svndir%\svn update .\Database
rem %svndir%\svn update .\Scripts

rem ----------------------------------------------------------------
rem BUILD IT!!!!!
rem ----------------------------------------------------------------

cd %helios%\src
echo Building
dcc32 -B -U"%indylibdcu%";"%zeoslibdcu%";"%madbasic%";"%maddisasm%";"%madexcept%" Helios.dpr

rem ----------------------------------------------------------------
rem Done, let the user know.  Comment out if you don't want to wait
rem ----------------------------------------------------------------
pause