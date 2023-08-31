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

unit BDPToolCircle;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase;

type

  { TBDToolCircle }

  TBDToolCircle=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
    function MouseUp(x,y,button:integer):boolean; override;
    procedure Draw; override;
    procedure Clear; override;
  private
    fSX,fSY:integer;
    procedure DrawCircleWithInk(cx,cy,r:integer);
    procedure DrawFilledCircleWithInk(cx,cy,r:integer);
  end;

implementation

uses sdl2, BDPShared;

{ TBDToolCircle }

constructor TBDToolCircle.Create;
begin
  inherited ;
  fName:='CIRCLE';
  fHint:=uppercase('Draw a circle. Right click to select method.');
  fPinnable:=true;
end;

function TBDToolCircle.Click(x,y,button:integer):boolean;
var r:integer;
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
          r:=round(sqrt(sqr(fSX-x)+sqr(fSY-y)));
          Project.CurrentExtImage.ImageUndo.AddImageUndo(fSX-r,fSY-r,r*2+1,r*2+1);
          ActiveInk.InitializeArea(fSX-r,fSY-r,fSX+r,fSY+r);
          if ActiveInk.SupportsOnTheFly then begin
            if Settings.FillShapes then
              DrawFilledCircleWithInk(fSX,fSY,r)
            else
              DrawCircleWithInk(fSX,fSY,r)
          end else begin
            if Settings.FillShapes then
              Project.CurrentImage.FilledCircle(fSX,fSY,r,POSTPROCESSCOLOR)
            else
              Project.CurrentImage.Circle(fSX,fSY,r,POSTPROCESSCOLOR);
            ActiveInk.PostProcess;
          end;
          Project.CurrentExtImage.ImageUndo.AddImageRedoToLastUndo(fSX-r,fSY-r,r*2+1,r*2+1);
          InfoBar.ShowText('');
          Result:=true;
          fState:=0;
        end;
    end;
  end
  else if Button=SDL_BUTTON_RIGHT then begin  // Right button
    if fState>0 then begin
      fState:=0;
      InfoBar.ShowText('');
      Result:=true;
    end else Result:=false
  end else Result:=false;
end;

function TBDToolCircle.MouseUp(x,y,button:integer):boolean;
begin
  Result:=fState>0;
end;

procedure TBDToolCircle.Draw;
var r:integer;
begin
  case fState of
    0:;
    1:begin
        r:=round(sqrt(sqr(fSX-fX)+sqr(fSY-fY)));
        Project.OverlayImage.Circle(fSX,fSY,r,VibroColors.GetColor);

        InfoBar.ShowText('('+inttostr(fSX)+','+inttostr(fSY)+') '+
          'RADIUS='+inttostr(r)+' '+
          '('+inttostr(fX)+','+inttostr(fY)+') ');
      end;
  end;
end;

procedure TBDToolCircle.Clear;
var r:integer;
begin
  case fState of
    0:;
    1:begin
        r:=round(sqrt(sqr(fSX-fX)+sqr(fSY-fY)));
        Project.OverlayImage.Circle(fSX,fSY,r,0);
      end;
  end;
end;

// Taken from https://www.geeksforgeeks.org/bresenhams-circle-drawing-algorithm/
procedure TBDToolCircle.DrawCircleWithInk(cx,cy,r:integer);

  procedure PutPixel8(x,y:integer);
  begin
    Project.CurrentImage.PutPixel(cx+x, cy+y, ActiveInk.GetColorAt(cx+x, cy+y));
    Project.CurrentImage.PutPixel(cx-x, cy+y, ActiveInk.GetColorAt(cx-x, cy+y));
    Project.CurrentImage.PutPixel(cx+x, cy-y, ActiveInk.GetColorAt(cx+x, cy-y));
    Project.CurrentImage.PutPixel(cx-x, cy-y, ActiveInk.GetColorAt(cx-x, cy-y));
    Project.CurrentImage.PutPixel(cx+y, cy+x, ActiveInk.GetColorAt(cx+y, cy+x));
    Project.CurrentImage.PutPixel(cx-y, cy+x, ActiveInk.GetColorAt(cx-y, cy+x));
    Project.CurrentImage.PutPixel(cx+y, cy-x, ActiveInk.GetColorAt(cx+y, cy-x));
    Project.CurrentImage.PutPixel(cx-y, cy-x, ActiveInk.GetColorAt(cx-y, cy-x));
  end;

var x,y,d:integer;

begin
  x:=0;
  y:=r;
  d:=3-2*r;
  PutPixel8(x,y);
  while (y>=x) do begin
    inc(x);

    // check for decision parameter and correspondingly update d, x, y
    if d>0 then begin
      dec(y);
      d:=d+4*(x-y)+10;
    end else
      d:=d+4*x+6;
    PutPixel8(x,y);
  end;
end;

// Taken from https://www.geeksforgeeks.org/bresenhams-circle-drawing-algorithm/
// Modified to draw filled circle
procedure TBDToolCircle.DrawFilledCircleWithInk(cx,cy,r:integer);

  procedure HLine(x,y:integer);
  var i:integer;
  begin
    for i:=-x to +x do begin
      Project.CurrentImage.PutPixel(cx+i,cy+y,ActiveInk.GetColorAt(cx+i,cy+y));
      Project.CurrentImage.PutPixel(cx+i,cy-y,ActiveInk.GetColorAt(cx+i,cy-y));
    end;
    for i:=-y to +y do begin
      Project.CurrentImage.PutPixel(cx+i,cy+x,ActiveInk.GetColorAt(cx+i,cy+x));
      Project.CurrentImage.PutPixel(cx+i,cy-x,ActiveInk.GetColorAt(cx+i,cy-x));
    end;
  end;

var x,y,d:integer;

begin
  x:=0;
  y:=r;
  d:=3-2*r;
  HLine(x,y);
  while (y>=x) do begin
    inc(x);

    // check for decision parameter and correspondingly update d, x, y
    if d>0 then begin
      dec(y);
      d:=d+4*(x-y)+10;
    end else
      d:=d+4*x+6;
    HLine(x,y);
  end;
end;

end.

