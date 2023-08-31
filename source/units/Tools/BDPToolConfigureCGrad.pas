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

unit BDPToolConfigureCGrad;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase;

type

  { TBDToolConfigureCGrad }

  TBDToolConfigureCGrad=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
    function MouseUp(x,y,button:integer):boolean; override;
    procedure Draw; override;
    procedure Clear; override;
  private
    fSX,fSY:integer;
  end;

implementation

uses sdl2, BDPShared;

{ TBDToolConfigureCGrad }

constructor TBDToolConfigureCGrad.Create;
begin
  inherited ;
  fName:='CONFCG';
  fHint:=uppercase('Select center then set radius.');
end;

function TBDToolConfigureCGrad.Click(x,y,button:integer):boolean;
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
          Settings.CGradCenterX:=fSX;
          Settings.CGradCenterY:=fSY;
          Settings.CGradRadius:=round(sqrt(sqr(fSX-x)+sqr(fSY-y)));
          InfoBar.ShowText('');
          Result:=true;
          fState:=0;
          MessageQueue.AddMessage(MSG_GETCELFINISHED);
        end;
    end;
  end
  else if Button=SDL_BUTTON_RIGHT then begin  // Right button
    if fState>0 then fState:=0;
    InfoBar.ShowText('');
    Result:=true;
    MessageQueue.AddMessage(MSG_GETCELFINISHED);
  end else Result:=false;
end;

function TBDToolConfigureCGrad.MouseUp(x,y,button:integer):boolean;
begin
  Result:=fState>0;
end;

procedure TBDToolConfigureCGrad.Draw;
var r:integer;
begin
  case fState of
    0:InfoBar.ShowText(Format('%d,%d - CONFIGURE C GRAD - '#132' SELECT CENTER '#133' CANCEL',[fX,fY]));
    1:begin
        r:=round(sqrt(sqr(fSX-fX)+sqr(fSY-fY)));
        Project.OverlayImage.Circle(fSX,fSY,r,VibroColors.GetColor);
        InfoBar.ShowText(
          Format('(%d,%d) R=%d %d,%d - CONFIGURE C GRAD - '#132' SET RADIUS '#133' CANCEL',[fSX,fSY,r,fX,fY]));
      end;
  end;
end;

procedure TBDToolConfigureCGrad.Clear;
begin
  if fState=1 then
    Project.OverlayImage.Circle(fSX,fSY,round(sqrt(sqr(fSX-fX)+sqr(fSY-fY))),0);
end;

end.

