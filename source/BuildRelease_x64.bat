@echo off
call ..\work\setenv.bat
if not exist ..\release\ (mkdir ..\release)
if not exist ..\release\x64 (mkdir ..\release\x64)
if not exist lib\ (mkdir lib) else (del /Q lib\*)
%FPCDIR%\fpc.exe -l- -MDelphi -Scghi -CX -O3 -XX -v0 -vm6058,5024 -Filib -Fuunits -Fuunits\* -FuC:\lazarus\lcl\units\x86_64-win64\win32 -FuC:\lazarus\lcl\units\x86_64-win64 -FuC:\lazarus\components\freetype\lib\x86_64-win64 -FuC:\lazarus\components\lazutils\lib\x86_64-win64 -FuC:\lazarus\packager\units\x86_64-win64 -Fu. -FUlib -FE..\release\x64 -oBurdockPaint.exe -dLCL -dLCLwin32 BurdockPaint.lpr
rem %FPCDIR%\fpc.exe -l- -MDelphi -Scghi -CX -O3 -XX -vewi -vm6058,5024 -Filib -Fuunits -Fuunits\* -FuC:\lazarus\lcl\units\x86_64-win64\win32 -FuC:\lazarus\lcl\units\x86_64-win64 -FuC:\lazarus\components\freetype\lib\x86_64-win64 -FuC:\lazarus\components\lazutils\lib\x86_64-win64 -FuC:\lazarus\packager\units\x86_64-win64 -Fu. -FUlib -FE..\release\x64 -oBurdockPaint.exe -dLCL -dLCLwin32 BurdockPaint.lpr
RMDIR "lib" /S /Q

