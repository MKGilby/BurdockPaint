unit BDPInfoBar;

{$mode Delphi}{$H+}

interface

uses SysUtils, vcc2_VisibleControl;

type

  { TBDInfoBar }

  TBDInfoBar=class(TVisibleControl)
    constructor Create;
    procedure Draw; override;
//    procedure ShowSimpleCoords(x,y:integer;valid:boolean);
    procedure ShowText(text:string);
  private
    fClear:boolean;
    fTop:integer;
    fTextTop:integer;
    procedure Clear;
    procedure fSetTop(aValue:integer);
  public
    property Top:integer read fTop write fSetTop;
  end;

implementation

uses BDPSharedUnit, mk_sdl2, Font2Unit;

{ TBDInfoBar }

constructor TBDInfoBar.Create;
begin
  inherited Create;
  Width:=WINDOWWIDTH;
  Height:=INFOBARHEIGHT;
  fTop:=WINDOWHEIGHT-CONTROLSHEIGHT-INFOBARHEIGHT;
//  SDL_SetTextureAlphaMod(fTexture.Texture,224);
//  SDL_SetTextureBlendMode(fTexture.Texture,SDL_BLENDMODE_BLEND);
  Clear;
  fTexture.Update;
  fClear:=true;
  if fTop=0 then fTextTop:=3 else fTextTop:=6;
end;

procedure TBDInfoBar.Draw;
begin
  if not fClear then PutTexture(0,fTop,fTexture);
end;

procedure TBDInfoBar.ShowText(text:string);
begin
  Clear;
  if text<>'' then begin
    MM.Fonts['Black'].OutText(fTexture.ARGBImage,text,8,fTextTop,mjLeft);
    fClear:=false;
  end else fClear:=true;
  fTexture.Update;
end;

procedure TBDInfoBar.Clear;
begin
  if fTop>0 then begin
    fTexture.ARGBImage.Bar(0,0,fTexture.Width,3,SystemPalette.Colors[2]);
    fTexture.ARGBImage.Bar(0,3,fTexture.Width,fTexture.Height-3,SystemPalette.Colors[3]);
  end else begin
    fTexture.ARGBImage.Bar(0,0,fTexture.Width,fTexture.Height-3,SystemPalette.Colors[3]);
    fTexture.ARGBImage.Bar(0,fTexture.Height-3,fTexture.Width,3,SystemPalette.Colors[2]);
  end;
end;

procedure TBDInfoBar.fSetTop(aValue:integer);
begin
  fTop:=aValue;
  if fTop<0 then fTop:=0
  else if fTop>WINDOWHEIGHT-INFOBARHEIGHT then fTop:=WINDOWHEIGHT-INFOBARHEIGHT;
  if fTop=0 then fTextTop:=3 else fTextTop:=6;
end;

end.

