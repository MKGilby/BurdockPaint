@echo off
echo Creating backup...
"c:\program files\winrar\winrar" a BurdockPaint @backup.txt -r
timestamp BurdockPaint.rar /d
move *.rar ..\..\_Backups
