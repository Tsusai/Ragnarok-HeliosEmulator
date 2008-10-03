rem ----------------------------------------------------------------
rem Helios DCC32.exe Batch File
rem You must set all the correct paths for this to work!!!!
rem ----------------------------------------------------------------

@echo off
echo Setting enviromental paths

rem ----------------------------------------------------------------
rem Helios Source Folder with the .dpr file
rem ----------------------------------------------------------------
set helios=c:\users\tsusai\desktop\delphi\helios\src

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
rem BUILD IT!!!!!
rem ----------------------------------------------------------------

cd %helios%
echo Building
dcc32 -B -U"%indylibdcu%";"%zeoslibdcu%";"%madbasic%";"%maddisasm%";"%madexcept%" Helios.dpr