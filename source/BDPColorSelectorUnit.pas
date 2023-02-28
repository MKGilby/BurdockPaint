unit BDPColorSelectorUnit;

{$mode Delphi}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses MKMouse2, ARGBImageUnit;

type

  { TColorSelector }

  TColorSelector=class(TMouseObject)
    constructor Create(iTarget:TARGBImage;iLeft,iTop:integer);
    procedure Draw; override;
    function Click(Sender:TObject;x, y, buttons: integer): boolean;
    procedure SetSelectedSlotTo(ColorIndex:integer);
  private
    fTarget:TARGBImage;
//    fColorCount:integer;
    fSelectedIndex:integer;
//    fColors:array of integer;
    fParentX,fParentY:integer;
  published
    property ParentX:integer read fParentX write fParentX;
    property ParentY:integer read fParentY write fParentY;
  end;

implementation

uses BDPSharedUnit, BDPSettingsUnit, sdl2;

{ TColorSelector }

constructor TColorSelector.Create(iTarget:TARGBImage;iLeft,iTop:integer);
var i:integer;
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  fWidth:=COLORSELECTORCOLORS*(COLORSELECTORBOXSIZE-3)+COLORSELECTORGAP;
  fHeight:=COLORSELECTORBOXSIZE;
  fSelectedIndex:=0;
  for i:=0 to COLORSELECTORCOLORS-1 do
    if Settings.SelectedColors[i]=Settings.ActiveColorIndex then fSelectedIndex:=i;
  fTarget:=iTarget;
  OnClick:=Self.Click;
end;

procedure TColorSelector.Draw;
var i,x:integer;
begin
  x:=0;
  for i:=0 to COLORSELECTORCOLORS-1 do begin
    fTarget.Bar(fLeft+x-fParentX,fTop-fParentY,COLORSELECTORBOXSIZE,COLORSELECTORBOXSIZE,OverlayImage.Palette[2]);
    fTarget.Bar(fLeft+x-fParentX+3,fTop-fParentY+3,COLORSELECTORBOXSIZE-6,COLORSELECTORBOXSIZE-6,MainImage.Palette[Settings.SelectedColors[i]]);
    x+=COLORSELECTORBOXSIZE-3;
    if i=0 then x+=COLORSELECTORGAP;
  end;
  x:=0;
  for i:=0 to COLORSELECTORCOLORS-1 do begin
    if i=fSelectedIndex then begin
      fTarget.Bar(fLeft+x-fParentX,fTop-fParentY,COLORSELECTORBOXSIZE,COLORSELECTORBOXSIZE,OverlayImage.Palette[5]);
      fTarget.Bar(fLeft+x-fParentX+3,fTop-fParentY+3,COLORSELECTORBOXSIZE-6,COLORSELECTORBOXSIZE-6,MainImage.Palette[Settings.SelectedColors[i]]);
    end;
    x+=COLORSELECTORBOXSIZE-3;
    if i=0 then x+=COLORSELECTORGAP;
  end;
end;

function TColorSelector.Click(Sender:TObject; x,y,buttons:integer):boolean;
var i,cx:integer;
begin
  if buttons=SDL_BUTTON_LEFT then begin
    cx:=0;
    x-=Left;
    for i:=0 to COLORSELECTORCOLORS-1 do begin
      if (cx+3<=x) and (cx+COLORSELECTORBOXSIZE-3>x) then begin
        fSelectedIndex:=i;
        Settings.ActiveColorIndex:=Settings.SelectedColors[i];
      end;
      cx+=COLORSELECTORBOXSIZE-3;
      if i=0 then cx+=COLORSELECTORGAP;
    end;
  end else if buttons=SDL_BUTTON_MIDDLE then begin
    MessageQueue.AddMessage(MSG_ACTIVATEPALETTEEDITOR);
  end else if buttons=SDL_BUTTON_RIGHT then begin
    cx:=0;
    x-=Left;
    for i:=0 to COLORSELECTORCOLORS-1 do begin
      if (cx+3<=x) and (cx+COLORSELECTORBOXSIZE-3>x) then begin
        fSelectedIndex:=i;
        Settings.ActiveColorIndex:=Settings.SelectedColors[i];
      end;
      cx+=COLORSELECTORBOXSIZE-3;
      if i=0 then cx+=COLORSELECTORGAP;
    end;
    ActiveTool:=Tools.ItemByName['PICKCOL'];
  end;
  Result:=true;
end;

procedure TColorSelector.SetSelectedSlotTo(ColorIndex:integer);
begin
  if (fSelectedIndex>=0) and (fSelectedIndex<COLORSELECTORCOLORS) then begin
    Settings.SelectedColors[fSelectedIndex]:=ColorIndex;
    Settings.ActiveColorIndex:=ColorIndex;
  end;
end;



end.

