@echo off
echo Creating backup...
"c:\program files\winrar\winrar" a BurdockPaint -r
timestamp BurdockPaint.rar /d
move *.rar ..\..\Backups
