{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPInkHGrad;

{$mode Delphi}

interface

uses
  BDPInkBase;

type

  { TBDInkHGrad }

  TBDInkHGrad=class(TBDInk)
    constructor Create; override;
    function GetColorAt(pX,pY:integer):uint32; override;
  end;

implementation

uses BDPShared;

{ TBDInkHGrad }

constructor TBDInkHGrad.Create;
begin
  inherited ;
  fName:='H GRAD';
  fHint:='HORIZONTAL GRADIENT.';
  fSupportsOnTheFly:=true;
end;

function TBDInkHGrad.GetColorAt(pX,pY:integer):uint32;
begin
  if fWidth>1 then begin
    if Project.CurrentGradientList.ActiveGradient.Dithered then
      Result:=Project.CurrentGradientList.ActiveGradient.GetColorAtDithered((px-fLeft)/(fWidth-1))
    else
      Result:=Project.CurrentGradientList.ActiveGradient.GetColorAt((px-fLeft)/(fWidth-1))
  end else
    Result:=Project.CurrentGradientList.ActiveGradient.GetColorAt(0.5);
end;

end.

