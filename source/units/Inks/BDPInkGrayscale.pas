{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPInkGrayscale;

{$mode Delphi}

interface

uses
  BDPInkBase;

type

  { TBDInkGrayscale }

  TBDInkGrayscale=class(TBDInk)
    constructor Create; override;
    function GetColorAt(pX,pY:integer):uint32; override;
    procedure ProcessWithCEL(pX,pY:integer); override;
  end;


implementation

uses BDPShared;

{ TBDInkGrayscale }

constructor TBDInkGrayscale.Create;
begin
  inherited Create;
  fName:='GRAYSC';
  fHint:='GRAYSCALE.';
  fSupportsOnTheFly:=true;
end;

function TBDInkGrayscale.GetColorAt(pX, pY: integer): uint32;
var c32,c:uint32;
begin
  c32:=Project.CurrentRegion.GetPixel(pX,pY);
  c:=trunc(((c32 and $FF0000) shr 16)*0.3+((c32 and $FF00) shr 8)*0.59+(c32 and $FF)*0.11);
  Result:=(c32 and $FF000000)+c<<16+c<<8+c;
end;

procedure TBDInkGrayscale.ProcessWithCEL(pX, pY: integer);
var i,j:integer;c32,c:uint32;
begin
  if Assigned(Project.CELImage) then begin
    for j:=0 to Project.CELImage.Height-1 do
      for i:=0 to Project.CELImage.Width-1 do
        if not Settings.ClearKeyColor or
           (Settings.ClearKeyColor and (Project.CELImage.GetPixel(i,j)<>$FF000000)) then begin
          c32:=Project.CurrentRegion.GetPixel(pX+i,pY+j);
          c:=trunc(((c32 and $FF0000) shr 16)*0.3+((c32 and $FF00) shr 8)*0.59+(c32 and $FF)*0.11);
          Project.CurrentRegion.PutPixel(pX+i,pY+j,(c32 and $FF000000)+c<<16+c<<8+c);
        end;
  end;
end;

end.

