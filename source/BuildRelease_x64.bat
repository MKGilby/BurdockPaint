@echo off
if not exist lib\ (mkdir lib)
if not exist lib\x86_64-win64\ (mkdir lib\x86_64-win64) else (del /Q lib\x86_64-win64\*)
C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe -l- -MDelphi -Scghi -CX -O3 -XX -v0 -vm6058,5024 -Filib\x86_64-win64 -Fuunits -Fuunits\* -FuC:\lazarus\lcl\units\x86_64-win64\win32 -FuC:\lazarus\lcl\units\x86_64-win64 -FuC:\lazarus\components\freetype\lib\x86_64-win64 -FuC:\lazarus\components\lazutils\lib\x86_64-win64 -FuC:\lazarus\packager\units\x86_64-win64 -Fu. -FUlib\x86_64-win64 -FE..\release\x64 -oBurdockPaint.exe -dLCL -dLCLwin32 BurdockPaint.lpr
rem C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe -MDelphi -Scghi -CX -O3 -XX -l -vewi -vm6058,5024 -Filib\x86_64-win64 -Fuunits -Fuunits\* -FuC:\lazarus\lcl\units\x86_64-win64\win32 -FuC:\lazarus\lcl\units\x86_64-win64 -FuC:\lazarus\components\freetype\lib\x86_64-win64 -FuC:\lazarus\components\lazutils\lib\x86_64-win64 -FuC:\lazarus\packager\units\x86_64-win64 -Fu. -FUlib\x86_64-win64 -FE..\release\x64 -oBurdockPaint.exe -dLCL -dLCLwin32 BurdockPaint.lpr

