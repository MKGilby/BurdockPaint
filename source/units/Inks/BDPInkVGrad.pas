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

unit BDPInkVGrad;

{$mode Delphi}

interface

uses
  BDPInkBase;

type

  { TBDInkVGrad }

  TBDInkVGrad=class(TBDInk)
    constructor Create; override;
    function GetColorAt(pX,pY:integer):uint32; override;
  end;

implementation

uses BDPShared;

{ TBDInkVGrad }

constructor TBDInkVGrad.Create;
begin
  inherited ;
  fName:='V GRAD';
  fHint:='VERTICAL GRADIENT.';
  fSupportsOnTheFly:=true;
end;

function TBDInkVGrad.GetColorAt(pX,pY:integer):uint32;
begin
  if fHeight>1 then begin
    if Settings.DitherGradients then
      Result:=Project.CurrentColorClusters.ActiveColorCluster.GetColorAtDithered(pY-fTop,fHeight-1,Settings.DitherStrength)
    else
      Result:=Project.CurrentColorClusters.ActiveColorCluster.GetColorAt(pY-fTop,fHeight-1)
  end else
    Result:=Project.CurrentColorClusters.ActiveColorCluster.GetColorAt(1,2);
end;

end.

