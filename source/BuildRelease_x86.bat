@echo off
if not exist lib\ (mkdir lib)
if not exist lib\x86_64-win64\ (mkdir lib\x86_64-win64) else (del /Q lib\x86_64-win64\*)
C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe -l- -Twin32 -Pi386 -MDelphi -Scghi -CX -O3 -XX -v0 -vm6058,5024 -Filib\i386-win32 -Fuunits -Fuunits\* -FuC:\lazarus\lcl\units\i386-win32\win32 -FuC:\lazarus\lcl\units\i386-win32 -FuC:\lazarus\components\freetype\lib\i386-win32 -FuC:\lazarus\components\lazutils\lib\i386-win32 -FuC:\lazarus\packager\units\i386-win32 -Fu. -FUlib\x86_64-win64 -FE..\release\x86 -oBurdockPaint.exe -dLCL -dLCLwin32 BurdockPaint.lpr
rem C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe -Twin32 -Pi386 -MDelphi -Scghi -CX -O3 -XX -l -vewi -vm6058,5024 -Filib\i386-win32 -Fuunits -Fuunits\* -FuC:\lazarus\lcl\units\i386-win32\win32 -FuC:\lazarus\lcl\units\i386-win32 -FuC:\lazarus\components\freetype\lib\i386-win32 -FuC:\lazarus\components\lazutils\lib\i386-win32 -FuC:\lazarus\packager\units\i386-win32 -Fu. -FUlib\x86_64-win64 -FE..\_release_x86 -oBurdockPaint.exe -dLCL -dLCLwin32 BurdockPaint.lpr

