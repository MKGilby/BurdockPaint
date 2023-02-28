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
    function KeyDown(Sender:TObject;key:integer):boolean;
    procedure SetSelectedSlotTo(ColorIndex:integer);
  private
    fTarget:TARGBImage;
//    fColorCount:integer;
    fSelectedIndex:integer;
//    fColors:array of integer;
    fParentX,fParentY:integer;
    fPickingColor:boolean;
    function GetClickedIndex(x:integer):integer;
  published
    property ParentX:integer read fParentX write fParentX;
    property ParentY:integer read fParentY write fParentY;
  end;

implementation

uses BDPSharedUnit, BDPSettingsUnit, sdl2, BDPKeyMappingUnit;

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
  OnKeyDown:=Self.KeyDown;
  fPickingColor:=false;
end;

procedure TColorSelector.Draw;
var i,x:integer;
begin
  // Draw dark background for all slots
  fTarget.Bar(
    fLeft-fParentX,
    fTop-fParentY,
    COLORSELECTORBOXSIZE,
    COLORSELECTORBOXSIZE,
    OverlayImage.Palette[2]);
  fTarget.Bar(
    fLeft-fParentX+COLORSELECTORGAP+COLORSELECTORBOXSIZE-3,
    fTop-fParentY,
    (COLORSELECTORBOXSIZE-3)*(COLORSELECTORCOLORS-1)+3,
    COLORSELECTORBOXSIZE,
    OverlayImage.Palette[2]);
  // Draw highlights and color boxes
  x:=0;
  for i:=0 to COLORSELECTORCOLORS-1 do begin
    if Settings.SelectedColors[i]=Settings.ActiveColorIndex then begin
      if not fPickingColor then
        fTarget.Bar(fLeft+x-fParentX,fTop-fParentY,COLORSELECTORBOXSIZE,COLORSELECTORBOXSIZE,OverlayImage.Palette[5])
      else
        fTarget.Bar(fLeft+x-fParentX,fTop-fParentY,COLORSELECTORBOXSIZE,COLORSELECTORBOXSIZE,OverlayImage.Palette[VibroColors.GetColorIndex])
    end;
    fTarget.Bar(fLeft+x-fParentX+3,fTop-fParentY+3,COLORSELECTORBOXSIZE-6,COLORSELECTORBOXSIZE-6,MainImage.Palette[Settings.SelectedColors[i]]);
    x+=COLORSELECTORBOXSIZE-3;
    if i=0 then x+=COLORSELECTORGAP;
  end;
end;

function TColorSelector.Click(Sender:TObject; x,y,buttons:integer):boolean;
var i:integer;
begin
  if buttons=SDL_BUTTON_LEFT then begin
    i:=GetClickedIndex(x);
    if i>-1 then begin
      fSelectedIndex:=i;
      Settings.ActiveColorIndex:=Settings.SelectedColors[i];
    end;
  end else if buttons=SDL_BUTTON_MIDDLE then begin
    MessageQueue.AddMessage(MSG_ACTIVATEPALETTEEDITOR);
  end else if buttons=SDL_BUTTON_RIGHT then begin
    i:=GetClickedIndex(x);
    if i>0 then begin
      fSelectedIndex:=i;
      Settings.ActiveColorIndex:=Settings.SelectedColors[i];
      ActiveTool:=Tools.ItemByName['PICKCOL'];
      fPickingColor:=true;
    end;
  end;
  Result:=true;
end;

function TColorSelector.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=false;
  if (key=KeyMap[KEY_GETCOLOR]) then begin
    if not fPickingColor then begin
      MessageQueue.AddMessage(MSG_SELECTCOLOR);
    end;
    Result:=true;
  end;
end;

procedure TColorSelector.SetSelectedSlotTo(ColorIndex:integer);
begin
  if (fSelectedIndex>=0) and (fSelectedIndex<COLORSELECTORCOLORS) then begin
    if (ColorIndex>=0) and (ColorIndex<MainImage.Palette.Size) then begin
      Settings.SelectedColors[fSelectedIndex]:=ColorIndex;
      Settings.ActiveColorIndex:=ColorIndex;
    end;
  end;
  fPickingColor:=false;
end;

function TColorSelector.GetClickedIndex(x:integer):integer;
var cx,i:integer;
begin
  Result:=-1;
  cx:=0;
  x-=Left;
  for i:=0 to COLORSELECTORCOLORS-1 do begin
    if (cx+3<=x) and (cx+COLORSELECTORBOXSIZE-3>x) then begin
      Result:=i;
      break;
    end;
    cx+=COLORSELECTORBOXSIZE-3;
    if i=0 then cx+=COLORSELECTORGAP;
  end;
end;



end.

