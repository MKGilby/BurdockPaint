@echo off
echo This script prepares Menu data and constants.
MenuCompiler.exe
bin2inc menu.bin ..\..\source\includes\menu.inc MenuBin /a
del menu.bin
