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

unit BDPToolDraw;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase, BDPImage;

type

  { TBDToolDraw }

  TBDToolDraw=class(TBDTool)
    constructor Create; override;
    function MouseDown(x,y,button:integer):boolean; override;
    function MouseUp(x,y,button:integer):boolean; override;
    function MouseMove(x,y,button:integer):boolean; override;
  private
    fTempImage:TBDRegion;
    fLeft,fTop,fRight,fBottom:integer;
    fDown:boolean;
  end;

implementation

uses SDL2, BDPShared;

{ TBDToolDraw }

constructor TBDToolDraw.Create;
begin
  inherited ;
  fName:='DRAW';
  fHint:=uppercase('Freehand draw. Click and move mouse to draw.');
  fPinnable:=true;
end;

function TBDToolDraw.MouseDown(x,y,button:integer):boolean;
begin
  if button=SDL_BUTTON_LEFT then begin
    fTempImage:=TBDRegion.Create(Project.CurrentImage.Width,Project.CurrentImage.Height);
    fTempImage.PutImage(0,0,Project.CurrentImage);
    Project.CurrentImage.PutPixel(x,y,ActiveInk.GetColorAt(x,y));
    Result:=true;
    fLeft:=x;
    fTop:=y;
    fRight:=x;
    fBottom:=y;
    fDown:=true;
  end else begin
    Result:=false;
    fDown:=false;
  end;
end;

function TBDToolDraw.MouseUp(x,y,button:integer):boolean;
begin
  if fDown then begin
    fDown:=false;
    Project.CurrentExtImage.ImageUndo.AddImageUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1,fTempImage);
    FreeAndNil(fTempImage);
    Project.CurrentExtImage.ImageUndo.AddImageRedoToLastUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1);
    Result:=true;
  end else Result:=false;
end;

function TBDToolDraw.MouseMove(x,y,button:integer):boolean;
begin
  if fDown then begin
    Project.CurrentImage.PutPixel(x,y,ActiveInk.GetColorAt(x,y));
    if fLeft>x then fLeft:=x;
    if fTop>y then fTop:=y;
    if fRight<x then fRight:=x;
    if fBottom<y then fBottom:=y;
    Result:=true;
  end else Result:=false;
end;

end.

