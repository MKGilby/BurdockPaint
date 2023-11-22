{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPInkRandom;

{$mode Delphi}

interface

uses
  BDPInkBase;

type

  { TBDInkRandom }

  TBDInkRandom=class(TBDInk)
    constructor Create; override;
    function GetColorAt(pX,pY:integer):uint32; override;
  end;

implementation

uses BDPShared;

{ TBDInkRandom }

constructor TBDInkRandom.Create;
begin
  inherited ;
  fName:='RANDOM';
  fHint:='RANDOM COLORS FROM THE SELECTED GRADIENT.';
  fSupportsOnTheFly:=true;
end;

function TBDInkRandom.GetColorAt(pX,pY:integer):uint32;
begin
  Result:=Project.CurrentGradientList.ActiveGradient.GetColorAt(random);
end;

end.

