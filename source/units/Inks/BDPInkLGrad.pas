{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
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
  until (SegmentRight>=Project.CurrentRegion.Width) or (Project.CurrentRegion.GetPixel(SegmentRight,j)<>POSTPROCESSCOLOR);
  Result:=SegmentRight-i-1;
  dec(SegmentRight);
  if Result>0 then begin
    if Project.CurrentGradientList.ActiveGradient.Dithered then begin
      while i<SegmentRight+1 do begin
        Project.CurrentRegion.PutPixel(i,j,Project.CurrentGradientList.ActiveGradient.GetColorAtDithered((i-SegmentLeft)/(Result+1)));
        inc(i);
      end;
    end else begin
      while i<SegmentRight+1 do begin
        Project.CurrentRegion.PutPixel(i,j,Project.CurrentGradientList.ActiveGradient.GetColorAt((i-SegmentLeft)/(Result+1)));
        inc(i);
      end;
    end;
  end else begin
    Project.CurrentRegion.PutPixel(i,j,Project.CurrentGradientList.ActiveGradient.GetColorAt(0.5));
  end;
end;

procedure TBDInkLGrad.PostProcess;
var i,j:integer;
begin
  for j:=fTop to fTop+fHeight-1 do begin
    i:=fLeft;
    while i<=fLeft+fWidth-1 do begin
      if Project.CurrentRegion.GetPixel(i,j)=POSTPROCESSCOLOR then
        i+=ProcessSegment(i,j);
      inc(i);
    end;
  end;
end;

end.

