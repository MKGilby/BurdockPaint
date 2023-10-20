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

