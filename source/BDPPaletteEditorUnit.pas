unit BDPPaletteEditorUnit;

{$mode Delphi}

interface

uses
  vcc_Container2, BDPButtonUnit, mk_sdl2, BDPMessageUnit;

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
  private
    fTexture:TStreamingTexture;
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
  fName:='PaletteEditor';

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

end.
