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

unit BDPInkCGrad;

{$mode Delphi}

interface

uses
  BDPInkBase;

type

  { TBDInkCGrad }

  TBDInkCGrad=class(TBDInk)
    constructor Create; override;
    function GetColorAt(pX,pY:integer):uint32; override;
    procedure Configure; override;
  end;

implementation

uses BDPShared;

// -------------------------------------------------------- [ TBDInkCGrad ] ---

constructor TBDInkCGrad.Create;
begin
  inherited ;
  fName:='C GRAD';
  fHint:='CIRCULAR GRADIENT. '#132'SELECT '#133'CONFIGURE';
  fSupportsOnTheFly:=true;
end;

function TBDInkCGrad.GetColorAt(pX,pY:integer):uint32;
var r:integer;
begin
  if Settings.CGradRadius>1 then begin
    r:=trunc(Sqrt(sqr(px-Settings.CGradCenterX)+sqr(py-Settings.CGradCenterY))) mod Settings.CGradRadius;
    if Settings.DitherGradients then
      Result:=Project.CurrentColorClusters.ActiveColorCluster.
        GetColorAtDithered(r,Settings.CGradRadius,Settings.DitherStrength)
    else
      Result:=Project.CurrentColorClusters.ActiveColorCluster.
        GetColorAt(r,Settings.CGradRadius)
  end else
    Result:=Project.CurrentColorClusters.ActiveColorCluster.GetColorAt(1,2);
end;

procedure TBDInkCGrad.Configure;
begin
  MessageQueue.AddMessage(MSG_TOGGLECONTROLS);
  ActiveTool:=Tools.ItemByName['CONFCG'];
end;

end.

