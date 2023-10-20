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

unit BDPInkLGrad;

{$mode Delphi}

interface

uses
  BDPInkBase;

type

  { TBDInkLGrad }

  TBDInkLGrad=class(TBDInk)
    constructor Create; override;
    procedure PostProcess; override;
  private
    function ProcessSegment(i,j:integer):integer;
  end;

implementation

uses BDPShared;

{ TBDInkLGrad }

constructor TBDInkLGrad.Create;
begin
  inherited ;
  fName:='L GRAD';
  fHint:='LINEAR GRADIENT.';
  fSupportsOnTheFly:=false;
end;

function TBDInkLGrad.ProcessSegment(i,j:integer):integer;
var SegmentLeft,SegmentRight:integer;
begin
  SegmentLeft:=i;
  SegmentRight:=i;
  repeat
    inc(SegmentRight);
  until (SegmentRight>=Project.CurrentImage.Width) or (Project.CurrentImage.GetPixel(SegmentRight,j)<>POSTPROCESSCOLOR);
  Result:=SegmentRight-i-1;
  dec(SegmentRight);
  if Result>0 then begin
    if Settings.DitherGradients then begin
      while i<SegmentRight+1 do begin
        Project.CurrentImage.PutPixel(i,j,Project.CurrentGradientList.ActiveGradient.GetColorAtDithered((i-SegmentLeft)/(Result+1)));
        inc(i);
      end;
    end else begin
      while i<SegmentRight+1 do begin
        Project.CurrentImage.PutPixel(i,j,Project.CurrentGradientList.ActiveGradient.GetColorAt((i-SegmentLeft)/(Result+1)));
        inc(i);
      end;
    end;
  end else begin
    Project.CurrentImage.PutPixel(i,j,Project.CurrentGradientList.ActiveGradient.GetColorAt(0.5));
  end;
end;

procedure TBDInkLGrad.PostProcess;
var i,j:integer;
begin
  for j:=fTop to fTop+fHeight-1 do begin
    i:=fLeft;
    while i<=fLeft+fWidth-1 do begin
      if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
        i+=ProcessSegment(i,j);
      inc(i);
    end;
  end;
end;

end.

