unit BDPColorSelectorUnit;

{$mode Delphi}

interface

uses MKMouse2, ARGBImageUnit;

type

  { TColorSelector }

  TColorSelector=class(TMouseObject)
    constructor Create(iTarget:TARGBImage;iLeft,iTop,iColorCount:integer);
    procedure Draw; override;
  private
    fTarget:TARGBImage;
    fColorCount:integer;
    fSelectedIndex:integer;
    fColors:array of integer;
    fParentX,fParentY:integer;
  published
    property ParentX:integer read fParentX write fParentX;
    property ParentY:integer read fParentY write fParentY;
  end;

implementation

uses BDPSharedUnit;

{ TColorSelector }

constructor TColorSelector.Create(iTarget:TARGBImage;iLeft,iTop,iColorCount:integer);
var i:integer;
begin
  fColorCount:=iColorCount;
  fLeft:=iLeft;
  fTop:=iTop;
  fWidth:=fColorCount*27+6;
  fHeight:=27;
  SetLength(fColors,fColorCount);
  for i:=0 to fColorCount-1 do
    fColors[i]:=i;
  fTarget:=iTarget;
  fSelectedIndex:=0;
end;

procedure TColorSelector.Draw;
var i,x:integer;
begin
  x:=0;
  for i:=0 to fColorCount-1 do begin
    fTarget.Bar(fLeft+x-fParentX,fTop-fParentY,COLORSELECTORBOXSIZE,COLORSELECTORBOXSIZE,OverlayImage.Palette[2]);
    fTarget.Bar(fLeft+x-fParentX+3,fTop-fParentY+3,COLORSELECTORBOXSIZE-6,COLORSELECTORBOXSIZE-6,MainImage.Palette[fColors[i]]);
    x+=COLORSELECTORBOXSIZE-3;
    if i=0 then x+=6;
  end;
  x:=0;
  for i:=0 to fColorCount-1 do begin
    if i=fSelectedIndex then begin
      fTarget.Bar(fLeft+x-fParentX,fTop-fParentY,COLORSELECTORBOXSIZE,COLORSELECTORBOXSIZE,OverlayImage.Palette[5]);
      fTarget.Bar(fLeft+x-fParentX+3,fTop-fParentY+3,COLORSELECTORBOXSIZE-6,COLORSELECTORBOXSIZE-6,MainImage.Palette[fColors[i]]);
    end;
    x+=27;
    if i=0 then x+=6;
  end;
end;



end.

