{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
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

{ TBDInkCGrad }

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
      Result:=Project.CurrentGradientList.ActiveGradient.
        GetColorAtDithered(r/Settings.CGradRadius)
    else
      Result:=Project.CurrentGradientList.ActiveGradient.
        GetColorAt(r/Settings.CGradRadius)
  end else
    Result:=Project.CurrentGradientList.ActiveGradient.GetColorAt(0.5);
end;

procedure TBDInkCGrad.Configure;
begin
  MessageQueue.AddMessage(MSG_TOGGLECONTROLS);
  ActiveTool:=Tools.ItemByName['CONFCG'];
end;

end.

