{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
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
    if Project.CurrentGradientList.ActiveGradient.Dithered then
      Result:=Project.CurrentGradientList.ActiveGradient.GetColorAtDithered((pY-fTop)/(fHeight-1))
    else
      Result:=Project.CurrentGradientList.ActiveGradient.GetColorAt((pY-fTop)/(fHeight-1))
  end else
    Result:=Project.CurrentGradientList.ActiveGradient.GetColorAt(0.5);
end;

end.

