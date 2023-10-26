{
  BurdockPaint - Copyright 2023 MKSZTSZ
  Written by Szab� "Gilby" Zsolt / MKSZTSZ

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

program BurdockPaint;

{$ifndef DEBUG}{$apptype gui}{$endif}

uses
  Interfaces,
  SysUtils,
  BDPMain,
  Logger,
  ARGBImageBMPReaderUnit,
  ARGBImageBMPWriterUnit,
  ARGBImageCELReaderUnit,
  ARGBImagePNGReaderUnit,
  ARGBImagePNGWriterUnit,
  ARGBImageTGAReaderUnit,
  ARGBImageTGAWriterUnit;

const
  VERSION='0.9';
  BDATE={$i %DATE%};

{$R *.res}

begin
  try
    with TMain.Create(VERSION,BDATE) do try
      Run;
    finally
      Free;
    end;
  except
    on e:exception do
      Log.LogError(e.Message);
  end;
end.


