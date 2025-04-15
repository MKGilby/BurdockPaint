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

  Set backup target folder, backup max. size, max. retention time, file count
  and add file to backup anytime you want. The file will be copied to
  the set backup folder, expanding its name with current date and time.
  If backup maximum size is set, the size of backup of one file will be
  kept under this limit. If retention time is set, the backups files older that this time
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
//  V1.03: Gilby - 2025.04.15
//     - BackupFolderFileCount renamed to BackupMaxFileCount.
//     - BackupFolderMaxSize renamed to BackupMaxSize.
//     - BackupFolderRetentionTime renamed to BackupRetentionDays.
//     - Backup retention now applied only to the backup files of the
//       current file to backup.

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
    procedure DeleteOldestFile(pFilename:string);

    // Delete all files from the backup.
    procedure DeleteAllFiles(pFilename:string);

    // Returns backup folder size in bytes.
    function GetFolderSize(pFilename:string):uint64;

    // Return backup folder file count.
    function GetFolderFileCount(pFilename:string):integer;

    // Checks backup folder size and removes files until the full backup size
    // with pNewFileSize won't exceed BackupFolderMaxSize (in bytes).
    procedure CheckBackupFolderSize(pFilename:string);

    // Checks the age of the files in the backup folder and removes files
    // older than BackupFolderRetentionTime (in days).
    procedure CheckBackupFolderAge(pFilename:string);

    // Checks file count in backup folder and removes file until the file count
    // drops below BackupFolderFileCount if needed.
    procedure CheckBackupFileCount(pFilename:string);

    // Returns the filename of the last backup of the specified file.
    // If no backup was made for that file, return ''.
    function GetLatestBackupOf(pFilename:string):string;

    // Checks if the two specified files are identical or not (true or false).
    function IsIdentical(pFilename1,pFilename2:string):boolean;

    // Creates search mask for timestamped versions of pFilename
    // test.png -> test.*.png
    // document.final.txt -> document.final.*.txt
    function CreateSearchMask(pFilename:string):string;
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
    BackupMaxFileCount:integer;

    // BackupFile checks if AFTER the backup the full backup size of this file
    // (in bytes) exceeds this number. If yes, the oldest file will be deleted
    // until the full backup size (including the new file) is below this number.
    // Set to 0 to disable.
    BackupMaxSize:uint64;

    // BackupFile deletes any backup of current file being older than this number.
    // Set to 0 to disable.
    BackupRetentionDays:integer;
  end;

implementation

uses MKToolBox, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.03';


{ TFileBackup }

constructor TFileBackup.Create(iTargetPath:string);
begin
  fTargetPath:=iTargetPath;
  if not DirectoryExists(iTargetPath) then mkdir(iTargetPath);
  BackupMaxSize:=0;
  BackupRetentionDays:=0;
  BackupMaxFileCount:=0;
  BackupOnlyWhenChanged:=true;
end;

procedure TFileBackup.BackupFile(pFilename:string);
var s:string;needbackup:boolean;i:integer;
begin
  Log.LogStatus(Format('Backup file "%s".',[pFilename]));
  if not fileexists(pFilename) then
    raise Exception.Create(Format('File not found! (%s)',[pFilename]));
  needbackup:=true;
  if BackupOnlyWhenChanged then begin
    Log.LogStatus('Checking if file changed since previous backup...');
    s:=GetLatestBackupOf(pFilename);
    if s<>'' then begin
      Log.LogStatus('  Last backup: '+s);
      needbackup:=not IsIdentical(pFilename,s)
    end else begin
      Log.LogStatus('  No previous backup found.');
      needbackup:=true;
    end;
  end;
  if needbackup then begin
    Log.LogStatus('We have to backup the file.');
    Log.LogStatus('Retention settings:');
    if BackupRetentionDays<>0 then begin
      Log.LogStatus(Format('  Keep files for %d day(s).',[BackupRetentionDays]));
      CheckBackupFolderAge(pFilename);
    end;
    if BackupMaxFileCount<>0 then begin
      Log.LogStatus(Format('  Keep last %d files.',[BackupMaxFileCount]));
      CheckBackupFileCount(pFilename);
    end;
    if BackupMaxSize>0 then begin
      Log.LogStatus(Format('  Keep backup size under %d bytes.',[BackupMaxSize]));
      CheckBackupFolderSize(pFilename);
    end;
    s:=copy(DateToStr(Date,FS)+TimeToStr(Time,FS),1,19);
    for i:=length(s) downto 1 do
      if s[i] in ['.',':',' '] then delete(s,i,1);
    s:='.'+copy(s,1,8)+'.'+copy(s,9,6);
    Log.LogStatus(Format('Creating backup as "%s".',[fTargetPath+'\'+ChangeFileExt(ExtractFileName(pFilename),s+ExtractFileExt(pFilename))]));
    CopyFile(pFilename,fTargetPath+'\'+ChangeFileExt(ExtractFileName(pFilename),s+ExtractFileExt(pFilename)));
  end else
    Log.LogStatus('No backup needed.');
end;

procedure TFileBackup.DeleteOldestFile(pFilename:string);
var SR:TSearchRec;fOldestTime:TDateTime;fOldestName:String;
begin
  if FindFirst(fTargetPath+'\'+CreateSearchMask(pFilename),faAnyFile-faDirectory,SR)=0 then begin
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

procedure TFileBackup.DeleteAllFiles(pFilename:string);
var SR:TSearchRec;
begin
  if FindFirst(fTargetPath+'\'+CreateSearchMask(pFilename),faAnyFile-faDirectory,SR)=0 then begin
    repeat
      DeleteFile(fTargetPath+'\'+SR.Name);
    until FindNext(SR)<>0;
  end;
  FindClose(SR);
end;

function TFileBackup.GetFolderSize(pFilename:string):uint64;
var SR:TSearchRec;
begin
  Result:=0;
  if FindFirst(fTargetPath+'\'+CreateSearchMask(pFilename),faAnyFile-faDirectory,SR)=0 then begin
    repeat
      Result+=SR.Size;
    until FindNext(SR)<>0;
  end;
  FindClose(SR);
end;

function TFileBackup.GetFolderFileCount(pFilename:string):integer;
var SR:TSearchRec;
begin
  Result:=0;
  if FindFirst(fTargetPath+'\'+CreateSearchMask(pFilename),faAnyFile-faDirectory,SR)=0 then begin
    repeat
      inc(Result);
    until FindNext(SR)<>0;
  end;
  FindClose(SR);
end;

procedure TFileBackup.CheckBackupFolderSize(pFilename:string);
var newfilesize:uint64;
begin
  Log.LogStatus('Checking backup folder size...');
  newfilesize:=SizeOfFile(pFilename);
  if newfilesize<BackupMaxSize then begin
    while GetFolderSize(pFilename)+newfilesize>BackupMaxSize do
      DeleteOldestFile(pFilename);
  end else begin
    DeleteAllFiles(pFilename);
  end;
end;

procedure TFileBackup.CheckBackupFolderAge(pFilename:string);
var SR:TSearchRec;
begin
  if FindFirst(fTargetPath+'\'+CreateSearchMask(pFilename),faAnyFile-faDirectory,SR)=0 then begin
    repeat
      if trunc((Now-SR.TimeStamp)*60*60*24)>BackupRetentionDays then
        DeleteFile(fTargetPath+'\'+SR.Name);
    until FindNext(SR)<>0;
  end;
  FindClose(SR);
end;

procedure TFileBackup.CheckBackupFileCount(pFilename:string);
var cnt:integer;
begin
  cnt:=GetFolderFileCount(pFilename);
  while cnt>=BackupMaxFileCount do begin
    dec(cnt);
    DeleteOldestFile(pFilename);
  end;
end;

function TFileBackup.GetLatestBackupOf(pFilename:string):string;
var SR:TSearchRec;newest:TDateTime;
begin
  Result:='';
  if FindFirst(fTargetPath+'\'+CreateSearchMask(pFilename),faAnyFile-faDirectory,SR)=0 then begin
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
// New line begins moved to the end of previous lines (I use them like this).
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

function TFileBackup.CreateSearchMask(pFilename:string):string;
begin
  Result:=ChangeFileExt(ExtractFileName(pFilename),'.*'+ExtractFileExt(pFilename));
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

