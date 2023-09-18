{
  BurdockPaint - Copyright 2023 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of BurdockPaint.

  BurdockPaint is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  BurdockPaint is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  BurdockPaint. If not, see <https://www.gnu.org/licenses/>.
}

// You need to keep the units separated from main project's units, this makes
// sure that deprecated or removed blocks/versions can be read by Project updater.

program ProjectUpdate1;

uses SysUtils, BDPProject, ParametersUnit;

type

  { TMain }

  TMain=class
    procedure Run;
  end;

{ TMain }

procedure TMain.Run;
var P:TBDProject;fn:String;
begin
  writeln('BurdockPaint Project File Updater 1 - (C) 2023 MKSZTSZ');
  writeln;
  writeln('Updates project file from version 1 (paletted) or version 2 (true color)');
  writeln('to the version where blocks are identified by 3+1 char IDs.');
  writeln;

  if Parameters.Count>1 then begin
    fn:=Parameters[1];
    if FileExists(fn) then begin
      writeln('Reading project...');
      P:=TBDProject.CreateFromFile(fn);
      try
        writeln('Cleaning project (removing undo/redo data)...');
        P.Clean;
        writeln('Creating backup...');
        if not FileExists(ChangeFileExt(fn,'.bak')) then begin
          RenameFile(fn,ChangeFileExt(fn,'.bak'));
          writeln('Writing project...');
          P.SaveToFile(fn);
          writeln('Completed.');
        end else writeln('*** Cannot create backup (.bak) file!');
      finally
        P.Free;
      end;
    end else writeln('*** The specified file is not found!');
  end else writeln('*** Give a project file name for parameter!');
end;

begin
  with TMain.Create do try Run; finally Free; end;
end.

