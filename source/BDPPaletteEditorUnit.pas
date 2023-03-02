unit BDPPaletteEditorUnit;

{$mode Delphi}

interface

uses
  vcc_Container2, BDPButtonUnit, mk_sdl2, BDPMessageUnit, BDPSliderUnit;

type

  { TBDPaletteEditor }

  TBDPaletteEditor=class(TContainer)
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
    function MouseEnter(Sender:TObject;x,y:integer):boolean;
    function MouseLeave(Sender:TObject;x,y:integer):boolean;
    function MouseMove(Sender:TObject;x,y:integer):boolean;
    function MouseDown(Sender:TObject;x,y,buttons:integer):boolean;
    function Click(Sender:TObject;x,y,buttons: integer):boolean;
    procedure OnSliderRChange(Sender:TObject;newValue:integer);
    procedure OnSliderGChange(Sender:TObject;newValue:integer);
    procedure OnSliderBChange(Sender:TObject;newValue:integer);
    procedure PaletteEditorShow(Sender:TObject);
  private
    fTexture:TStreamingTexture;
    fSliderR,fSliderG,fSliderB:TBDSlider;
  end;

implementation

uses SysUtils, BDPSharedUnit, SDL2;

const
  PALETTEEDITORHEIGHT=300;
  PALETTESOCKETWIDTH=36;
  PALETTESOCKETHEIGHT=26;
  PALETTESOCKETSTOP=87;
  PALETTESOCKETSLEFT=3;


{ TBDPaletteEditor }

constructor TBDPaletteEditor.Create;
begin
  inherited Create;
  fLeft:=0;
  fTop:=WINDOWHEIGHT-PALETTEEDITORHEIGHT;
  fWidth:=WINDOWWIDTH;
  fHeight:=PALETTEEDITORHEIGHT;
  fTexture:=TStreamingTexture.Create(fWidth,fHeight);
  fTexture.ARGBImage.Clear;
  OnMouseEnter:=MouseEnter;
  OnMouseLeave:=MouseLeave;
  OnMouseMove:=MouseMove;
  OnMouseDown:=MouseDown;
  OnClick:=Click;
  OnShow:=PaletteEditorShow;
  fName:='PaletteEditor';

  fSliderR:=TBDSlider.Create(fTexture.ARGBImage,fLeft,fTop);
  with fSliderR do begin
    Left:=COLORSLIDERSLEFT;Top:=Self.fTop+6;MinValue:=0;MaxValue:=255;Position:=32;
    ZIndex:=15;
    Name:='R-slider';
    OnChange:=OnSliderRChange;
  end;
  AddChild(fSliderR);

  fSliderG:=TBDSlider.Create(fTexture.ARGBImage,fLeft,fTop);
  with fSliderG do begin
    Left:=COLORSLIDERSLEFT+COLORSLIDERWIDTH+3;Top:=Self.fTop+6;MinValue:=0;MaxValue:=255;Position:=32;
    ZIndex:=15;
    Name:='G-slider';
    OnChange:=OnSliderGChange;
  end;
  AddChild(fSliderG);

  fSliderB:=TBDSlider.Create(fTexture.ARGBImage,fLeft,fTop);
  with fSliderB do begin
    Left:=COLORSLIDERSLEFT+2*(COLORSLIDERWIDTH+3);Top:=Self.fTop+6;MinValue:=0;MaxValue:=255;Position:=32;
    ZIndex:=15;
    Name:='B-slider';
    OnChange:=OnSliderBChange;
  end;
  AddChild(fSliderB);

end;

destructor TBDPaletteEditor.Destroy;
begin
  if Assigned(fTexture) then FreeAndNil(fTexture);
  inherited Destroy;
end;

procedure TBDPaletteEditor.Draw;
var i:integer;
begin
  if fVisible then begin
    fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,3,OverlayImage.Palette[2]);
    fTexture.ARGBImage.Bar(0,3,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height-3,OverlayImage.Palette[3]);
    fTexture.ARGBImage.Bar(
      PALETTESOCKETSLEFT,
      PALETTESOCKETSTOP,
      PALETTESOCKETWIDTH*32+3,
      PALETTESOCKETHEIGHT*8+3,
      OverlayImage.Palette.Colors[2]);
    fTexture.ARGBImage.bar(
      PALETTESOCKETSLEFT+(Settings.ActiveColorIndex mod 32)*PALETTESOCKETWIDTH,
      PALETTESOCKETSTOP+(Settings.ActiveColorIndex div 32)*PALETTESOCKETHEIGHT,
      PALETTESOCKETWIDTH+3,
      PALETTESOCKETHEIGHT+3,
      OverlayImage.Palette.Colors[5]);

    for i:=0 to 255 do begin
      fTexture.ARGBImage.Bar(
        PALETTESOCKETSLEFT+(i mod 32)*PALETTESOCKETWIDTH+3,
        PALETTESOCKETSTOP+(i div 32)*PALETTESOCKETHEIGHT+3,
        PALETTESOCKETWIDTH-3,
        PALETTESOCKETHEIGHT-3,
        MainImage.Palette.Colors[i]);
    end;
    inherited Draw;
    fTexture.Update;
    PutTexture(fLeft,fTop,fTexture);
  end;
end;

function TBDPaletteEditor.MouseEnter(Sender:TObject; x,y:integer):boolean;
begin
  if fVisible then SDL_ShowCursor(SDL_ENABLE);
  InfoBar.ShowText('');
  Result:=false;
end;

function TBDPaletteEditor.MouseLeave(Sender:TObject; x,y:integer):boolean;
begin
  if fVisible then SDL_ShowCursor(SDL_DISABLE);
  Result:=false;
end;

function TBDPaletteEditor.MouseMove(Sender:TObject; x,y:integer):boolean;
begin
  Result:=true;
end;

function TBDPaletteEditor.MouseDown(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

function TBDPaletteEditor.Click(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=true;
end;

procedure TBDPaletteEditor.OnSliderRChange(Sender:TObject; newValue:integer);
begin
  MainImage.Palette.ColorR[Settings.ActiveColorIndex]:=newValue;
end;

procedure TBDPaletteEditor.OnSliderGChange(Sender:TObject; newValue:integer);
begin
  MainImage.Palette.ColorG[Settings.ActiveColorIndex]:=newValue;
end;

procedure TBDPaletteEditor.OnSliderBChange(Sender:TObject; newValue:integer);
begin
  MainImage.Palette.ColorB[Settings.ActiveColorIndex]:=newValue;
end;

procedure TBDPaletteEditor.PaletteEditorShow(Sender:TObject);
begin
  fSliderR.Position:=MainImage.Palette.ColorR[Settings.ActiveColorIndex];
  fSliderG.Position:=MainImage.Palette.ColorG[Settings.ActiveColorIndex];
  fSliderB.Position:=MainImage.Palette.ColorB[Settings.ActiveColorIndex];
end;

end.

