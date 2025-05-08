@echo off
call ..\work\setenv.bat
if not exist ..\release\ (mkdir ..\release)
if not exist ..\release\x86 (mkdir ..\release\x86)
if not exist lib\ (mkdir lib) else (del /Q lib\*)
%FPCDIR%\fpc.exe -l- -Twin32 -Pi386 -MDelphi -Scghi -CX -O3 -XX -v0 -vm6058,5024 -Filib -Fuunits -Fuunits\* -FuC:\lazarus\lcl\units\i386-win32\win32 -FuC:\lazarus\lcl\units\i386-win32 -FuC:\lazarus\components\freetype\lib\i386-win32 -FuC:\lazarus\components\lazutils\lib\i386-win32 -FuC:\lazarus\packager\units\i386-win32 -Fu. -FUlib -FE..\release\x86 -oBurdockPaint.exe -dLCL -dLCLwin32 BurdockPaint.lpr
rem %FPCDIR%\fpc.exe -l- -Twin32 -Pi386 -MDelphi -Scghi -CX -O3 -XX -vewi -vm6058,5024 -Filib -Fuunits -Fuunits\* -FuC:\lazarus\lcl\units\i386-win32\win32 -FuC:\lazarus\lcl\units\i386-win32 -FuC:\lazarus\components\freetype\lib\i386-win32 -FuC:\lazarus\components\lazutils\lib\i386-win32 -FuC:\lazarus\packager\units\i386-win32 -Fu. -FUlib -FE..\_release_x86 -oBurdockPaint.exe -dLCL -dLCLwin32 BurdockPaint.lpr
RMDIR "lib" /S /Q

