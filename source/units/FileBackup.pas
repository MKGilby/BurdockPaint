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

  Set backup folder, folder max. size, retention time and add file to
  backup anytime you want. The file will be copied to the set backup
  folder, expanding its name with current date and time.
  If folder maximum size is set, the folder size will be kept under
  this limit. If retention time set is, the files older that this time
  will be deleted from backup.

  I used this unit in BurdockPaint to create backups of the project file.

  ---------------------------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby
//     - Initial creation.
//  V1.00a: Gilby
//     - Fix in BackupFile.

unit FileBackup;

{$mode Delphi}

interface

uses
  Classes, SysUtils;

type

  { TFileBackup }

  TFileBackup=class
    constructor Create(iTargetPath:string);
    procedure BackupFile(pFilename:string);
  private
    fTargetPath:string;
    fFolderMaxSize:uint64;
    fFolderRetentionTime:integer;
    procedure DeleteOldestFile;
    procedure DeleteAllFiles;
    function GetFolderSize:uint64;
    procedure CheckBackupFolderSize(pNewFileSize:uint64);
    procedure CheckBackupFolderAge;
  public
    property BackupFolderMaxSize:uint64 write fFolderMaxSize;
    property BackupFolderRetentionTime:integer write fFolderRetentionTime;
  end;

implementation

uses MKToolBox, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.00a';


{ TFileBackup }

constructor TFileBackup.Create(iTargetPath:string);
begin
  fTargetPath:=iTargetPath;
  if not DirectoryExists(iTargetPath) then mkdir(iTargetPath);
  fFolderMaxSize:=0;
  fFolderRetentionTime:=0;
end;

procedure TFileBackup.BackupFile(pFilename:string);
var s:string;
begin
  if not fileexists(pFilename) then
    raise Exception.Create(Format('File not found! (%s)',[pFilename]));
  if fFolderMaxSize>0 then CheckBackupFolderSize(SizeOfFile(pFilename));
  if fFolderRetentionTime>0 then CheckBackupFolderAge;
  s:=copy(DateToStr(Date,FS)+TimeToStr(Time,FS),1,19);
  s:=replace(replace(replace(s,'.',''),':',''),' ','');
  s:='.'+copy(s,1,8)+'.'+copy(s,9,6);
  CopyFile(pFilename,fTargetPath+'\'+ChangeFileExt(ExtractFileName(pFilename),s+ExtractFileExt(pFilename)));
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

procedure TFileBackup.CheckBackupFolderSize(pNewFileSize:uint64);
begin
  Log.LogStatus('Checking backup folder size...');
  if pNewFileSize<fFolderMaxSize then begin
    while GetFolderSize+pNewFileSize>fFolderMaxSize do
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
      if trunc((Now-SR.TimeStamp)*60*60*24)>fFolderRetentionTime then
        DeleteFile(fTargetPath+'\'+SR.Name);
    until FindNext(SR)<>0;
  end;
  FindClose(SR);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

