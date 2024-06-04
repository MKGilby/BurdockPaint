{ -[Name]--------------------------------------------------------------

                             FileBackup Unit

  -[Disclaimer]--------------------------------------------------------

  Written by Szabó 'Gilby' Zsolt
  Copyright 2023 MKSZTSZ

  This unit is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  This unit is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this unit. If not, see <https://www.gnu.org/licenses/>.

  -[Description]-------------------------------------------------------

  Set backup folder, folder max. size, max. retention time, file count
  and add file to backup anytime you want. The file will be copied to
  the set backup folder, expanding its name with current date and time.
  If folder maximum size is set, the folder size will be kept under
  this limit. If retention time set is, the files older that this time
  will be deleted from backup. If file count set, the number of files
  will never exceed this count.

  I used this unit in BurdockPaint to create backups of the project file.

  ---------------------------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby
//     - Initial creation.
//  V1.00a: Gilby
//     - Fix in BackupFile.
//  V1.01: Gilby
//     - Removed unneccessary private variables.
//     - Added BackupOnlyWhenChanged:boolean property. Default: true.
//     - Added BackupFolderFileCount:integer property. Default: 0 (disabled).
//  V1.02: Gilby
//     - No longer using MKToolBox.Replace in timestamp creation.

unit FileBackup;

{$mode Delphi}

interface

uses
  Classes, SysUtils;

type

  { TFileBackup }

  TFileBackup=class
    // Give the backup target path as parameter.
    constructor Create(iTargetPath:string);

    // Backups the specified file to the given backup target path.
    // Before backups it performs a series of checks depending on the
    // set property values.
    procedure BackupFile(pFilename:string);
  private
    fTargetPath:string;

    // Delete oldes file from the backup.
    procedure DeleteOldestFile;

    // Delete all files from the backup.
    procedure DeleteAllFiles;

    // Returns backup folder size in bytes.
    function GetFolderSize:uint64;

    // Return backup folder file count.
    function GetFolderFileCount:integer;

    // Checks backup folder size and removes files until the full backup size
    // with pNewFileSize won't exceed BackupFolderMaxSize (in bytes).
    procedure CheckBackupFolderSize(pNewFileSize:uint64);

    // Checks the age of the files in the backup folder and removes files
    // older than BackupFolderRetentionTime (in days).
    procedure CheckBackupFolderAge;

    // Checks file count in backup folder and removes file until the file count
    // drops below BackupFolderFileCount if needed.
    procedure CheckBackupFileCount;

    // Returns the filename of the last backup of the specified file.
    // If no backup was made for that file, return ''.
    function GetLatestBackupOf(pFilename:string):string;

    // Checks if the two specified files are identical or not (true or false).
    function IsIdentical(pFilename1,pFilename2:string):boolean;
  public
    // True: BackupFile creates backup only if the file changed since the last
    // backup or no backup found. This one is checked first, the others check
    // only if backup needs to be taken.
    // False: BackupFile always creates backup.
    BackupOnlyWhenChanged:boolean;

    // BackupFile checks if AFTER the backup the backup file count exceeds this number.
    // If yes, the oldest file will be deleted until one less file remains than
    // this number.
    // Set to 0 to disable.
    BackupFolderFileCount:integer;

    // BackupFile checks if AFTER the backup the full backup size (in bytes) exceeds
    // this number. If yes, the oldest file will be deleted until the full backup size
    // (including the new file) is below this number.
    // Set to 0 to disable.
    BackupFolderMaxSize:uint64;

    // BackupFile deletes any backup file older (in days) than this number.
    // Set to 0 to disable.
    BackupFolderRetentionTime:integer;
  end;

implementation

uses MKToolBox, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.02';


{ TFileBackup }

constructor TFileBackup.Create(iTargetPath:string);
begin
  fTargetPath:=iTargetPath;
  if not DirectoryExists(iTargetPath) then mkdir(iTargetPath);
  BackupFolderMaxSize:=0;
  BackupFolderRetentionTime:=0;
  BackupFolderFileCount:=0;
  BackupOnlyWhenChanged:=true;
end;

procedure TFileBackup.BackupFile(pFilename:string);
var s:string;needbackup:boolean;i:integer;
begin
  if not fileexists(pFilename) then
    raise Exception.Create(Format('File not found! (%s)',[pFilename]));
  needbackup:=true;
  if BackupOnlyWhenChanged then begin
    s:=GetLatestBackupOf(pFilename);
    if s<>'' then
      needbackup:=not IsIdentical(pFilename,s)
    else
      needbackup:=true;
  end;
  if needbackup then begin
    if BackupFolderRetentionTime>0 then CheckBackupFolderAge;
    if BackupFolderFileCount>0 then CheckBackupFileCount;
    if BackupFolderMaxSize>0 then CheckBackupFolderSize(SizeOfFile(pFilename));
    s:=copy(DateToStr(Date,FS)+TimeToStr(Time,FS),1,19);
    for i:=length(s) downto 1 do
      if s[i] in ['.',':',' '] then delete(s,i,1);
    s:='.'+copy(s,1,8)+'.'+copy(s,9,6);
    CopyFile(pFilename,fTargetPath+'\'+ChangeFileExt(ExtractFileName(pFilename),s+ExtractFileExt(pFilename)));
  end;
end;

procedure TFileBackup.DeleteOldestFile;
var SR:TSearchRec;fOldestTime:TDateTime;fOldestName:String;
begin
  if FindFirst(fTargetPath+'\*.*',faAnyFile-faDirectory,SR)=0 then begin
    fOldestTime:=Now;
    fOldestName:='';
    repeat
      if SR.TimeStamp<fOldestTime then begin
        fOldestTime:=SR.TimeStamp;
        fOldestName:=SR.Name;
      end;
    until FindNext(SR)<>0;
  end;
  FindClose(SR);
  if fOldestName<>'' then DeleteFile(fTargetPath+'\'+fOldestName);
end;

procedure TFileBackup.DeleteAllFiles;
var SR:TSearchRec;
begin
  if FindFirst(fTargetPath+'\*.*',faAnyFile-faDirectory,SR)=0 then begin
    repeat
      DeleteFile(fTargetPath+'\'+SR.Name);
    until FindNext(SR)<>0;
  end;
  FindClose(SR);
end;

function TFileBackup.GetFolderSize:uint64;
var SR:TSearchRec;
begin
  Result:=0;
  if FindFirst(fTargetPath+'\*.*',faAnyFile-faDirectory,SR)=0 then begin
    repeat
      Result+=SR.Size;
    until FindNext(SR)<>0;
  end;
  FindClose(SR);
end;

function TFileBackup.GetFolderFileCount:integer;
var SR:TSearchRec;
begin
  Result:=0;
  if FindFirst(fTargetPath+'\*.*',faAnyFile-faDirectory,SR)=0 then begin
    repeat
      inc(Result);
    until FindNext(SR)<>0;
  end;
  FindClose(SR);
end;

procedure TFileBackup.CheckBackupFolderSize(pNewFileSize:uint64);
begin
  Log.LogStatus('Checking backup folder size...');
  if pNewFileSize<BackupFolderMaxSize then begin
    while GetFolderSize+pNewFileSize>BackupFolderMaxSize do
      DeleteOldestFile;
  end else begin
    DeleteAllFiles;
  end;
end;

procedure TFileBackup.CheckBackupFolderAge;
var SR:TSearchRec;
begin
  if FindFirst(fTargetPath+'\*.*',faAnyFile-faDirectory,SR)=0 then begin
    repeat
      if trunc((Now-SR.TimeStamp)*60*60*24)>BackupFolderRetentionTime then
        DeleteFile(fTargetPath+'\'+SR.Name);
    until FindNext(SR)<>0;
  end;
  FindClose(SR);
end;

procedure TFileBackup.CheckBackupFileCount;
var cnt:integer;
begin
  cnt:=GetFolderFileCount;
  while cnt>=BackupFolderFileCount do begin
    dec(cnt);
    DeleteOldestFile;
  end;
end;

function TFileBackup.GetLatestBackupOf(pFilename:string):string;
var SR:TSearchRec;newest:TDateTime;
begin
  Result:='';
  if FindFirst(fTargetPath+'\*.*',faAnyFile-faDirectory,SR)=0 then begin
    newest:=0;
    repeat
      if SR.TimeStamp>newest then begin
        Result:=fTargetPath+'\'+SR.Name;
        newest:=SR.TimeStamp;
      end;
    until FindNext(SR)<>0;
  end;
  FindClose(SR);
end;

// Taken from SwissDelphiCenter.ch
// https://www.swissdelphicenter.ch/en/showcode.php?id=290
// Method and arguments name changed. An unneeded begin...end removed.
// New line begins moved to the end of previous lines (i use them like this).
function TFileBackup.IsIdentical(pFilename1,pFilename2:string):boolean;
const BlockSize = 65536;
var
  fs1, fs2: TFileStream;
  L1, L2: Integer;
  B1, B2: array[1..BlockSize] of Byte;
begin
  Result := False;
  fs1 := TFileStream.Create(pFilename1, fmOpenRead or fmShareDenyWrite);
  try
    fs2 := TFileStream.Create(pFilename2, fmOpenRead or fmShareDenyWrite);
    try
      if fs1.Size = fs2.Size then begin
        B1[1]:=0;B2[1]:=0;
        while fs1.Position < fs1.Size do begin
          L1 := fs1.Read(B1[1], BlockSize);
          L2 := fs2.Read(B2[1], BlockSize);
          if L1 <> L2 then Exit;
          if not CompareMem(@B1[1], @B2[1], L1) then Exit;
        end;
        Result := True;
      end;
    finally
      fs2.Free;
    end;
  finally
    fs1.Free;
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

