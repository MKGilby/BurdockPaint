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

unit BDPToolConfigureRGrad;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase;

type

  { TBDToolConfigureRGrad }

  TBDToolConfigureRGrad=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
    procedure Draw; override;
    procedure Clear; override;
  end;

implementation

uses sdl2, BDPShared;

{ TBDToolConfigureRGrad }

constructor TBDToolConfigureRGrad.Create;
begin
  inherited Create;
  fName:='CONFRG';
  fHint:=uppercase('Select center.');
end;

function TBDToolConfigureRGrad.Click(x,y,button:integer):boolean;
begin
  if button=SDL_BUTTON_LEFT then begin
    InfoBar.ShowText('');
    Settings.TempRGradCenterX:=x;
    Settings.TempRGradCenterY:=y;
    MessageQueue.AddMessage(MSG_CONFIGRGRADCENTERFINISHED);
    Result:=true;
  end
  else if Button=SDL_BUTTON_RIGHT then begin  // Right button
    InfoBar.ShowText('');
    Settings.TempRGradCenterX:=Settings.RGradCenterX;
    Settings.TempRGradCenterY:=Settings.RGradCenterY;
    MessageQueue.AddMessage(MSG_CONFIGRGRADCENTERFINISHED);
    Result:=true;
  end else Result:=false;
end;

procedure TBDToolConfigureRGrad.Draw;
begin
  Project.OverlayImage.VLine(fX,0,Project.OverlayImage.Height,VibroColors.GetColor);
  Project.OverlayImage.HLine(0,fY,Project.OverlayImage.Width,VibroColors.GetColor);
  InfoBar.ShowText(Format('%d,%d - CONFIGURE R GRAD - '#132' SELECT CENTER '#133' CANCEL',[fX,fY]));
end;

procedure TBDToolConfigureRGrad.Clear;
begin
  Project.OverlayImage.VLine(fX,0,Project.OverlayImage.Height,0);
  Project.OverlayImage.HLine(0,fY,Project.OverlayImage.Width,0);
end;

end.

