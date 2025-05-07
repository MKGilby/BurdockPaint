@echo off
call ..\..\work\setenv.bat
if not exist lib\ (mkdir lib) else (del /Q lib\*)
echo Compiling FontBuild2...
%FPCDIR%\fpc.exe -l- -MDelphi -Scghi -CX -O3 -XX -v0 -vm6058,5024 -Filib -Fu..\..\source\units -Fu..\..\source\units\* -Fu. -FUlib -FE..\ -oFontBuild2.exe FontBuild2.lpr

RMDIR "lib" /S /Q
