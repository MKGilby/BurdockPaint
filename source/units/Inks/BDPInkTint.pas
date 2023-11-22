{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
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

