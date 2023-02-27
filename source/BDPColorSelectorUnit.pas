unit BDPColorSelectorUnit;

{$mode Delphi}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses MKMouse2, ARGBImageUnit;

type

  { TColorSelector }

  TColorSelector=class(TMouseObject)
    constructor Create(iTarget:TARGBImage;iLeft,iTop,iColorCount:integer);
    procedure Draw; override;
    function Click(Sender:TObject;x, y, buttons: integer): boolean;
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

uses BDPSharedUnit, BDPSettingsUnit;

{ TColorSelector }

constructor TColorSelector.Create(iTarget:TARGBImage;iLeft,iTop,iColorCount:integer);
var i:integer;
begin
  inherited Create;
  fColorCount:=iColorCount;
  fLeft:=iLeft;
  fTop:=iTop;
  fWidth:=fColorCount*(COLORSELECTORBOXSIZE-3)+COLORSELECTORGAP;
  fHeight:=COLORSELECTORBOXSIZE;
  SetLength(fColors,fColorCount);
  fSelectedIndex:=0;
  for i:=0 to fColorCount-1 do begin
    fColors[i]:=Settings.SelectedColors[i];
    if fColors[i]=Settings.ActiveColorIndex then fSelectedIndex:=i;
  end;
  fTarget:=iTarget;
  OnClick:=Self.Click;
end;

procedure TColorSelector.Draw;
var i,x:integer;
begin
  x:=0;
  for i:=0 to fColorCount-1 do begin
    fTarget.Bar(fLeft+x-fParentX,fTop-fParentY,COLORSELECTORBOXSIZE,COLORSELECTORBOXSIZE,OverlayImage.Palette[2]);
    fTarget.Bar(fLeft+x-fParentX+3,fTop-fParentY+3,COLORSELECTORBOXSIZE-6,COLORSELECTORBOXSIZE-6,MainImage.Palette[fColors[i]]);
    x+=COLORSELECTORBOXSIZE-3;
    if i=0 then x+=COLORSELECTORGAP;
  end;
  x:=0;
  for i:=0 to fColorCount-1 do begin
    if i=fSelectedIndex then begin
      fTarget.Bar(fLeft+x-fParentX,fTop-fParentY,COLORSELECTORBOXSIZE,COLORSELECTORBOXSIZE,OverlayImage.Palette[5]);
      fTarget.Bar(fLeft+x-fParentX+3,fTop-fParentY+3,COLORSELECTORBOXSIZE-6,COLORSELECTORBOXSIZE-6,MainImage.Palette[fColors[i]]);
    end;
    x+=COLORSELECTORBOXSIZE-3;
    if i=0 then x+=COLORSELECTORGAP;
  end;
end;

function TColorSelector.Click(Sender:TObject; x,y,buttons:integer):boolean;
var i,cx:integer;
begin
  if buttons=1 then begin
    cx:=0;
    x-=Left;
    for i:=0 to fColorCount-1 do begin
      if (cx+3<=x) and (cx+COLORSELECTORBOXSIZE-3>x) then begin
        fSelectedIndex:=i;
        Settings.ActiveColorIndex:=fColors[i];
      end;
      cx+=COLORSELECTORBOXSIZE-3;
      if i=0 then cx+=COLORSELECTORGAP;
    end;
  end else if buttons=3 then begin
    MessageQueue.AddMessage(MSG_ACTIVATEPALETTEEDITOR);
  end;
  Result:=true;
end;



end.

