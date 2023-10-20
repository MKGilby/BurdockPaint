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

unit BDPInkRGrad;

{$mode Delphi}

interface

uses
  BDPInkBase;

type

  { TBDInkRGrad }

  TBDInkRGrad=class(TBDInk)
    constructor Create; override;
    function GetColorAt(pX,pY:integer):uint32; override;
    procedure Configure; override;
  end;

implementation

uses BDPShared;

{ TBDInkRGrad }

constructor TBDInkRGrad.Create;
begin
  inherited ;
  fName:='R GRAD';
  fHint:='RADIAL GRADIENT. '#132'SELECT '#133'CONFIGURE';
  fSupportsOnTheFly:=true;
end;

function TBDInkRGrad.GetColorAt(pX,pY:integer):uint32;
var d:integer;
begin
  if (Settings.RGradCenterX>pX) then begin
    d:=trunc(arctan((Settings.RGradCenterY-pY)/(Settings.RGradCenterX-pX))*180/pi)+270;
  end else
  if (Settings.RGradCenterX<pX) then begin
    d:=trunc(arctan((Settings.RGradCenterY-pY)/(Settings.RGradCenterX-pX))*180/pi)+90;
  end else begin
    if (Settings.RGradCenterY>=pY) then begin
      d:=0;
    end else begin
      d:=180;
    end;
  end;
  if Settings.DitherGradients then
    Result:=Project.CurrentGradientList.ActiveGradient.GetColorAtDithered(((round((d+Settings.RGradRotation)*Settings.RGradRepetitions)) mod 360)/359)
  else
    Result:=Project.CurrentGradientList.ActiveGradient.GetColorAt(((round((d+Settings.RGradRotation)*Settings.RGradRepetitions)) mod 360)/359)
end;

procedure TBDInkRGrad.Configure;
begin
  MessageQueue.AddMessage(MSG_OPENCONFIGURERGRADDIALOG);
end;

end.

