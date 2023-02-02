unit BDPInfoBarUnit;

{$mode Delphi}{$H+}

interface

uses MK_SDL2;

type

  { TBDInfoBar }

  TBDInfoBar=class
    constructor Create;
    destructor Destroy; override;
    procedure Draw;
    procedure ShowSimpleCoords(x,y:integer;valid:boolean);
    procedure ShowText(text:string);
  private
    fTexture:TStreamingTexture;
    fClear:boolean;
    procedure Clear;
  end;

implementation

uses SysUtils, BDPSharedUnit, SDL2, Font2Unit;

{ TBDInfoBar }

constructor TBDInfoBar.Create;
begin
  fTexture:=TStreamingTexture.Create(WINDOWWIDTH,24);
  SDL_SetTextureAlphaMod(fTexture.Texture,224);
  SDL_SetTextureBlendMode(fTexture.Texture,SDL_BLENDMODE_BLEND);
  Clear;
  fTexture.Update;
  fClear:=true;
end;

destructor TBDInfoBar.Destroy;
begin
  if Assigned(fTexture) then FreeAndNil(fTexture);
  inherited Destroy;
end;

procedure TBDInfoBar.Draw;
begin
  if not fClear then PutTexture(0,0,fTexture);
end;

procedure TBDInfoBar.ShowSimpleCoords(x,y:integer; valid:boolean);
begin
  Clear;
//  MultiFontOutText(fImage,#4'('#0+inttostr(x)+#4','#0+inttostr(y)+#4')',8,3,mjLeft);
  if valid then
    MM.Fonts['Black'].OutText(fTexture.ARGBImage,'('+inttostr(x)+','+inttostr(y)+')',8,3,mjLeft)
  else
    MM.Fonts['Red'].OutText(fTexture.ARGBImage,'('+inttostr(x)+','+inttostr(y)+')',8,3,mjLeft);
  fTexture.Update;
  fClear:=false;
end;

procedure TBDInfoBar.ShowText(text:string);
begin
  Clear;
  if text<>'' then begin
    MM.Fonts['Black'].OutText(fTexture.ARGBImage,text,8,3,mjLeft);
    fClear:=false;
  end else fClear:=true;
  fTexture.Update;
end;

procedure TBDInfoBar.Clear;
begin
  fTexture.ARGBImage.Bar(0,0,fTexture.Width,fTexture.Height-3,OverlayImage.Palette.Colors[3]);
  fTexture.ARGBImage.Bar(0,fTexture.Height-3,fTexture.Width,3,OverlayImage.Palette.Colors[2]);
end;

end.

