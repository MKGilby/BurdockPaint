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

unit BDPToolFill;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase;

type

  { TBDToolFill }

  TBDToolFill=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
  end;

implementation

uses sdl2, BDPShared, BDPRegion;

{ TBDToolFill }

constructor TBDToolFill.Create;
begin
  inherited ;
  fName:='FILL';
  fHint:=uppercase('Apply ink to all pixels until stopped by a different color.');
  fPinnable:=true;
end;

function TBDToolFill.Click(x,y,button:integer):boolean;
var
  fTempImage:TBDRegion;
  i,j:integer;
  fLeft,fTop,fRight,fBottom:integer;
begin
  if button=SDL_BUTTON_LEFT then begin
    fLeft:=x;
    fTop:=y;
    fRight:=x;
    fBottom:=y;
    // Create temporary image.
    fTempImage:=TBDRegion.Create(Project.CurrentRegion.Width,Project.CurrentRegion.Height);
    // Copy current image to temporary image.
    fTempImage.PutImage(0,0,Project.CurrentRegion);
    // Process Fill on temporary image.
    fTempImage.FloodFill(x,y,POSTPROCESSCOLOR);
    // Scan affected area.
    for j:=0 to fTempImage.Height-1 do
      for i:=0 to fTempImage.Width-1 do
        if uint32((fTempImage.RawData+(j*fTempImage.Width+i)*4)^)=POSTPROCESSCOLOR then begin
          if fLeft>i then fLeft:=i;
          if fRight<i then fRight:=i;
          if fTop>j then fTop:=j;
          if fBottom<j then fBottom:=j;
        end;
    // Create Undo operation with the still unchanged image.
    Project.CurrentImage.RegionUndo.AddImageUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1);
    // Initialize ink with affected area. (Current image still unchanged.)
    ActiveInk.InitializeArea(fLeft,fTop,fRight,fBottom);
    // Copy affected area from temporary image to current image.
    Project.CurrentRegion.PutImagePart(fLeft,fTop,fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1,fTempImage);
    // Free temporory image.
    fTempImage.Free;
    // PostProcess with ink.
    ActiveInk.PostProcess;
    // Add Redo image to undo operation.
    Project.CurrentImage.RegionUndo.AddImageRedoToLastUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1);
    Result:=true;
  end else Result:=false;
end;

end.

