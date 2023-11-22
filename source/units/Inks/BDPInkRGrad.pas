{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
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

