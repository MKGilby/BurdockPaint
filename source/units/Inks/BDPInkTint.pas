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

unit BDPInkTint;

{$mode Delphi}

interface

uses
  BDPInkBase;

type

  { TBDInkTint }

  TBDInkTint=class(TBDInk)
    constructor Create; override;
    function GetColorAt(pX,pY:integer):uint32; override;
    procedure Configure; override;
  end;


implementation

uses BDPShared;

{ TBDInkTint }

constructor TBDInkTint.Create;
begin
  inherited Create;
  fName:='TINT';
  fHint:='TINT WITH THE SELECTED COLOR. '#132'SELECT '#133'CONFIGURE';
  fSupportsOnTheFly:=true;
end;

function TBDInkTint.GetColorAt(pX, pY: integer): uint32;
var c32:uint32;c1,c2:integer;
begin
  c32:=Project.CurrentRegion.GetPixel(pX,pY);
  c1:=(c32 and $FF000000) shr 24;
  c2:=(Settings.ActiveColor and $FF000000) shr 24;
  Result:=((c1+round((c2-c1)*Settings.RealTintStrength)) and $ff) shl 24;
  c1:=(c32 and $FF0000) shr 16;
  c2:=(Settings.ActiveColor and $FF0000) shr 16;
  Result+=((c1+round((c2-c1)*Settings.RealTintStrength)) and $ff) shl 16;
  c1:=(c32 and $FF00) shr 8;
  c2:=(Settings.ActiveColor and $FF00) shr 8;
  Result+=((c1+round((c2-c1)*Settings.RealTintStrength)) and $ff) shl 8;
  c1:=c32 and $FF;
  c2:=Settings.ActiveColor and $FF;
  Result+=(c1+round((c2-c1)*Settings.RealTintStrength)) and $ff;
end;

procedure TBDInkTint.Configure;
begin

end;

end.

