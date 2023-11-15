{
  BurdockPaint - Copyright 2023 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of BurdockPaint.

  BurdockPaint is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  BurdockPaint is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  BurdockPaint. If not, see <https://www.gnu.org/licenses/>.
}

unit BDPSimpleGradient;

{$mode Delphi}

interface

uses vcc2_VisibleControl, BDPGradient, mk_sdl2;

type

  { TBDSimpleGradient }

  TBDSimpleGradient=class(TVisibleControl)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer;iGradient:TGradient);
    destructor Destroy; override;
    procedure Draw; override;
  protected
    procedure ReDraw; override;
  private
    fGradient:TGradient;
    fColorsWidth:integer;
    fAlphaBack:TTexture;
    fGradientTexture:TStreamingTexture;
    function fGetColorsWidth:integer;
    procedure fSetGradient(value:TGradient);
    procedure fSetSelected(value:boolean);
    procedure RecreateTexture(Sender:TObject);
  public
    property ColorsWidth:integer read fGetColorsWidth;
    property Gradient:TGradient write fSetGradient;
    property Selected:boolean read fSelected write fSetSelected;
  end;


implementation

uses BDPShared, SDL2;

{ TBDSimpleGradient }

constructor TBDSimpleGradient.Create(iLeft,iTop,iWidth,iHeight:integer;iGradient:TGradient);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  fGradient:=iGradient;
  OnRecreateTexture:=RecreateTexture;
  fWidth:=iWidth;
  Height:=iHeight;
  fNeedRedraw:=true;
  Selected:=false;
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
    PutTexturePart(fLeft+3,fTop+3,0,0,fWidth-6,fHeight-6,fAlphaBack);
    PutTexture(fLeft+3,fTop+3,fGradientTexture);
  end;
end;

procedure TBDSimpleGradient.ReDraw;
var i:integer;c:uint32;
begin
  if Assigned(fTexture) then begin
    if Assigned(fGradient) then begin
      fColorsWidth:=Width-6;
      with fTexture.ARGBImage do begin
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
      fTexture.ARGBImage.Bar(0,0,Width,Height,0);
    fTexture.Update;
  end;
  if Assigned(fGradientTexture) then
    if Assigned(fGradient) then begin
      // Gradient bar
      for i:=0 to fColorsWidth-1 do
        fGradientTexture.ARGBImage.VLine(i,0,Height,fGradient.GetColorAtRaw(i/(fColorsWidth-1)));
      fGradientTexture.Update;
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

procedure TBDSimpleGradient.RecreateTexture(Sender:TObject);
begin
  if Assigned(fGradientTexture) then fGradientTexture.Free;
  // Recreate texture by inner texture
  fGradientTexture:=TStreamingTexture.Create(fTexture.Width-6,fTexture.Height-6);
  SDL_SetTextureBlendMode(fGradientTexture.Texture,SDL_BLENDMODE_BLEND);
end;

end.

