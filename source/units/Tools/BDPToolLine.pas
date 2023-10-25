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

unit BDPToolLine;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase;

type

  { TBDToolLine }

  TBDToolLine=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
    function MouseUp(x,y,button:integer):boolean; override;
    procedure Draw; override;
    procedure Clear; override;
  private
    fSX,fSY:integer;
    procedure DrawLineWithInk(x1,y1,x2,y2:integer);
  end;

implementation

uses sdl2, BDPShared;

{ TBDToolLine }

constructor TBDToolLine.Create;
begin
  inherited ;
  fName:='LINE';
  fHint:=uppercase('Click for start then click for the end.');
  fPinnable:=true;
end;

function TBDToolLine.Click(x,y,button:integer):boolean;
var Left,Top,Width,Height:integer;
begin
  if button=SDL_BUTTON_LEFT then begin
    case fState of
      0:begin
          fSX:=x;
          fSY:=y;
          Result:=true;
          fState:=1;
        end;
      1:begin
          if fSX>x then begin
            Left:=x;
            Width:=fSX-x+1;
          end else begin
            Left:=fSX;
            Width:=x-fSX+1;
          end;
          if fSY>y then begin
            Top:=y;
            Height:=fSY-y+1;
          end else begin
            Top:=fSY;
            Height:=y-fSY+1;
          end;
          Project.CurrentImage.RegionUndo.AddImageUndo(Left,Top,Width,Height);
          ActiveInk.InitializeArea(fSX,fSY,x,y);
          if ActiveInk.SupportsOnTheFly then
            DrawLineWithInk(fSX,fSY,x,y)
          else begin
            Project.CurrentRegion.Line(fSX,fSY,fX,fY,POSTPROCESSCOLOR);
            ActiveInk.PostProcess;
          end;
          Project.CurrentImage.RegionUndo.AddImageRedoToLastUndo(Left,Top,Width,Height);
          Result:=true;
          fState:=0;
          InfoBar.ShowText('');
        end;
    end;
  end
  else if Button=SDL_BUTTON_RIGHT then begin
    if fState>0 then begin
      fState:=0;
      Result:=true;
      InfoBar.ShowText('');
    end else Result:=false
  end else Result:=false;
end;

function TBDToolLine.MouseUp(x,y,button:integer):boolean;
begin
  Result:=fState>0;
end;

procedure TBDToolLine.Draw;
var d:integer;
begin
  case fState of
    0:;
    1:begin
        Project.OverlayImage.Line(fSX,fSY,fX,fY,VibroColors.GetColor);
        if (fSX>fX) then begin
          d:=trunc(arctan((fSY-fY)/(fSX-fX))*180/pi)+270;
        end else
        if (fSX<fX) then begin
          d:=trunc(arctan((fSY-fY)/(fSX-fX))*180/pi)+90;
        end else begin
          if (fSY>=fY) then begin
            d:=0;
          end else begin
            d:=180;
          end;
        end;

        InfoBar.ShowText('('+inttostr(fSX)+','+inttostr(fSY)+') '+
          'WI='+inttostr(abs(fSX-fX)+1)+' HE='+inttostr(abs(fSY-fY)+1)+' '+
          '('+inttostr(fX)+','+inttostr(fY)+') '+
          'DEG='+inttostr(d));
      end;
  end;
end;

procedure TBDToolLine.Clear;
begin
  case fState of
    0:;
    1:Project.OverlayImage.Line(fSX,fSY,fX,fY,0);
  end;
end;

// Taken from http://www.efg2.com/Lab/Library/Delphi/Graphics/Bresenham.txt
// Stripped a few comments, variable names changed here and there...
procedure TBDToolLine.DrawLineWithInk(x1,y1,x2,y2:integer);
var
  a,b,d : integer;
  diag_inc, nondiag_inc : integer;
  dx_diag, dx_nondiag, dy_diag, dy_nondiag : integer;
  i,swap,x,y : integer;
begin
  x := x1;
  y := y1;
  {Determine drawing direction and step to the next pixel.}
  a := x2 - x1;
  b := y2 - y1;
  {Determine whether end point lies to right or left of start point.}
  if a < 0 then begin
    a := -a;
    dx_diag := -1;
  end else
    dx_diag := 1;
  {Determine whether end point lies above or below start point.}
  if b < 0 then begin
    b := -b;
    dy_diag := -1
  end else
    dy_diag := 1;
  {Identify octant containing end point.}
  if a < b then begin
    swap := a;
    a := b;
    b := swap;
    dx_nondiag := 0;
    dy_nondiag := dy_diag
  end else begin
    dx_nondiag := dx_diag;
    dy_nondiag := 0
  end;
  d := b + b - a;
  nondiag_inc := b + b;
  diag_inc    := b + b - a - a;
  for i := 0 to a do begin   {draw the a+1 pixels}
    Project.CurrentRegion.PutPixel(x,y,ActiveInk.GetColorAt(x,y));
    if d < 0 then begin
      x := x + dx_nondiag;
      y := y + dy_nondiag;
      d := d + nondiag_inc
    end else begin
      x := x + dx_diag;
      y := y + dy_diag;
      d := d + diag_inc
    end;
  end;
end;

end.

