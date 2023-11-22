{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPSimpleGradient;

{$mode Delphi}

interface

uses SysUtils, vcc2_VisibleControlStatic, BDPGradient, mk_sdl2;

type

  { TBDSimpleGradient }

  TBDSimpleGradient=class(TVisibleControlStatic)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer;iGradient:TGradient);
    destructor Destroy; override;
    procedure Draw; override;
  protected
    procedure ReDraw; override;
  private
    fGradient:TGradient;
    fColorsWidth:integer;
    fAlphaBack:TTexture;
    fGradientTexture:TStaticTexture;
    function fGetColorsWidth:integer;
    procedure fSetGradient(value:TGradient);
    procedure fSetSelected(value:boolean);
  public
    property ColorsWidth:integer read fGetColorsWidth;
    property Gradient:TGradient write fSetGradient;
    property Selected:boolean read fSelected write fSetSelected;
  end;


implementation

uses BDPShared, SDL2, ARGBImageUnit;

{ TBDSimpleGradient }

constructor TBDSimpleGradient.Create(iLeft,iTop,iWidth,iHeight:integer;iGradient:TGradient);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  fGradient:=iGradient;
  fWidth:=iWidth;
  Height:=iHeight;
  fNeedRedraw:=true;
  Selected:=false;
  fGradientTexture:=nil;
  fAlphaBack:=MM.Textures.ItemByName['AlphaBack'];
//  OnClick:=Click;
end;

destructor TBDSimpleGradient.Destroy;
begin
  if Assigned(fGradientTexture) then fGradientTexture.Free;
  inherited Destroy;
end;

procedure TBDSimpleGradient.Draw;
begin
  inherited Draw;
  if Assigned(fGradient) then begin
    Bar(fLeft+3,fTop+3,fWidth-6,fHeight-6,fAlphaBack);
    PutTexture(fLeft+3,fTop+3,fGradientTexture);
  end;
end;

procedure TBDSimpleGradient.ReDraw;
var i:integer;c:uint32;tmp:TARGBImage;
begin
  if Assigned(fGradient) then begin
    fColorsWidth:=Width-6;
    with fImage do begin
      if not Selected then
        c:=SystemPalette[SYSTEMCOLORDARK]
      else
        c:=SystemPalette[SYSTEMCOLORHIGHLIGHT];
      // Outer border
      Bar(0,0,Width,3,c);
      Bar(0,Height-3,Width,3,c);
      Bar(0,3,3,Height-6,c);
      Bar(Width-3,3,3,Height-6,c);
    end;
  end else
    fImage.Bar(0,0,Width,Height,0);

  if Assigned(fGradientTexture) then FreeAndNil(fGradientTexture);
  tmp:=TARGBImage.Create(Width-6,Height-6);
  try
    if Assigned(fGradient) then begin
      // Gradient bar
      for i:=0 to fColorsWidth-1 do
        tmp.VLine(i,0,Height,fGradient.GetColorAtRaw(i/(fColorsWidth-1)));
    end else
      tmp.Bar(0,0,tmp.Width,tmp.Height,SystemPalette[SYSTEMCOLORTRANSPARENT]);

    fGradientTexture:=TStaticTexture.Create(tmp);
    SDL_SetTextureBlendMode(fGradientTexture.Texture,SDL_BLENDMODE_BLEND);
  finally
    tmp.Free;
  end;
end;

function TBDSimpleGradient.fGetColorsWidth: integer;
begin
  Result:=fColorsWidth+6;
end;

procedure TBDSimpleGradient.fSetGradient(value: TGradient);
begin
  if value<>fGradient then begin
    fGradient:=value;
    Refresh;
  end;
end;

procedure TBDSimpleGradient.fSetSelected(value:boolean);
begin
  if (value<>fSelected) then begin
    fSelected:=value;
    Refresh;
  end;
end;

end.

