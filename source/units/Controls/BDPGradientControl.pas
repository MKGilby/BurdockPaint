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

unit BDPGradientControl;

{$mode Delphi}

interface

uses vcc2_VisibleControl, BDPGradient, ARGBImageUnit, Font2Unit, mk_sdl2;

type

  { TBDGradient }

  TBDGradient=class(TVisibleControl)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer;iGradient:TGradient);
    destructor Destroy; override;
    procedure Refresh; override;
    procedure Click(Sender:TObject;x,y,button:integer);
    procedure Draw; override;
  protected
    procedure ReDraw; override;
    procedure fSetWidth(value:integer); override;
  private
    fTLImage,fTRImage,fBLImage,fBRImage:TARGBImage;
    fGradient:TGradient;
    fGradientTexture:TStreamingTexture;
    fAlphaBack:TTexture;
    fFont,fFont2:TFont;
    fPingpongSwitchLeft,fReverseSwitchLeft,
    fColorsLeft,fColorsWidth,fArrowLeft:integer;
    procedure fSetGradient(value:TGradient);
    procedure RecreateTexture(Sender:TObject);
  public
    property Gradient:TGradient read fGradient write fSetGradient;
    property Width:integer read fWidth write fSetWidth;
  end;

implementation

uses BDPShared, sdl2;

const
  PINGPONGSWITCHWIDTH=27;
  REVERSESWITCHWIDTH=27;
  ARROWWIDTH=30;

{ TBDGradient }

constructor TBDGradient.Create(iLeft, iTop, iWidth, iHeight: integer;
  iGradient: TGradient);
begin
  fLeft:=iLeft;
  fTop:=iTop;
  fGradient:=iGradient;
  OnRecreateTexture:=RecreateTexture;
  fWidth:=iWidth;
  fNeedRedraw:=true;
  fTLImage:=MM.Images.ItemByName['ArchTopLeft'];
  fTRImage:=MM.Images.ItemByName['ArchTopRight'];
  fBLImage:=MM.Images.ItemByName['ArchBottomLeft'];
  fBRImage:=MM.Images.ItemByName['ArchBottomRight'];
  fFont:=MM.Fonts['Black'];
  fFont2:=MM.Fonts['Red'];
  fPingpongSwitchLeft:=8;
  fReverseSwitchLeft:=fPingpongSwitchLeft+PINGPONGSWITCHWIDTH+3;
  fColorsLeft:=fReverseSwitchLeft+REVERSESWITCHWIDTH+3;
  fArrowLeft:=Width-ARROWWIDTH-3;
  fColorsWidth:=fArrowLeft-fColorsLeft;
  fAlphaBack:=MM.Textures.ItemByName['AlphaBack'];
  Height:=iHeight;
  OnClick:=Click;
end;

destructor TBDGradient.Destroy;
begin
  if Assigned(fGradientTexture) then fGradientTexture.Free;
  inherited Destroy;
end;

procedure TBDGradient.Refresh;
begin
  inherited Refresh;
end;

procedure TBDGradient.Click(Sender:TObject; x,y,button:integer);
begin
  if Assigned(fGradient) then begin
    x-=Left;
    y-=Top;
    if (x>=fPingpongSwitchLeft) and (x<fReverseSwitchLeft) then begin
      fGradient.PingPong:=not fGradient.PingPong;
      fNeedRedraw:=true;
    end else
    if (x>=fReverseSwitchLeft) and (x<fColorsLeft) then begin
      fGradient.Reversed:=not fGradient.Reversed;
      fNeedRedraw:=true;
    end else
    if (x>=fColorsLeft+3) and (x<fArrowLeft) then begin
      if button=SDL_BUTTON_LEFT then
        Settings.ActiveColor:=fGradient.GetColorAt((x-fColorsLeft-3)/(fColorsWidth-3))
      else if (button=SDL_BUTTON_RIGHT) then begin
//        MessageQueue.AddMessage(MSG_ACTIVATEGRADIENTSELECTOR);
      end;
    end else
    if (x>=fArrowLeft) then begin
      MessageQueue.AddMessage(MSG_ACTIVATEGRADIENTSELECTOR);
    end;
  end;
end;

procedure TBDGradient.Draw;
begin
  inherited Draw;
  PutTexturePart(fLeft+fColorsLeft+3,fTop+3,0,0,fColorsWidth-3,fHeight-6,fAlphaBack);
  PutTexture(fLeft+fColorsLeft+3,fTop+3,fGradientTexture);
end;

procedure TBDGradient.ReDraw;
var i,fonttop:integer;color32:uint32;
begin
  if Assigned(fTexture) then begin
    with fTexture.ARGBImage do begin
      // Background
      Bar(0,0,fPingpongSwitchLeft,Height,SystemPalette[SYSTEMCOLORMID]);
      if Assigned(fGradient) then begin
        if fGradient.PingPong then
          Bar(fPingpongSwitchLeft+3,3,PINGPONGSWITCHWIDTH,Height-6,SystemPalette[SYSTEMCOLORLIGHT])
        else
          Bar(fPingpongSwitchLeft+3,3,PINGPONGSWITCHWIDTH,Height-6,SystemPalette[SYSTEMCOLORMID]);
        if fGradient.Reversed then
          Bar(fReverseSwitchLeft+3,3,REVERSESWITCHWIDTH,Height-6,SystemPalette[SYSTEMCOLORLIGHT])
        else
          Bar(fReverseSwitchLeft+3,3,REVERSESWITCHWIDTH,Height-6,SystemPalette[SYSTEMCOLORMID]);
      end;
      Bar(fArrowLeft,0,ARROWWIDTH,Height,SystemPalette[SYSTEMCOLORMID]);
      // Outer border
      Bar(8,0,Width-16,3,SystemPalette[SYSTEMCOLORDARK]);
      Bar(8,Height-3,Width-16,3,SystemPalette[SYSTEMCOLORDARK]);
      Bar(0,8,3,Height-16,SystemPalette[SYSTEMCOLORDARK]);
      Bar(Width-3,8,3,Height-16,SystemPalette[SYSTEMCOLORDARK]);
      // Vertical separator lines
      Bar(fPingpongSwitchLeft,3,3,Height-6,SystemPalette[SYSTEMCOLORDARK]);
      Bar(fReverseSwitchLeft,3,3,Height-6,SystemPalette[SYSTEMCOLORDARK]);
      Bar(fColorsLeft,3,3,Height-6,SystemPalette[SYSTEMCOLORDARK]);
      Bar(fArrowLeft,3,3,Height-6,SystemPalette[SYSTEMCOLORDARK]);
      // Corners
      if Assigned(fTLImage) then
        fTLImage.CopyTo(0,0,fTLImage.Width,fTLImage.Height,0,0,fTexture.ARGBImage,true);
      if Assigned(fTRImage) then
        fTRImage.CopyTo(0,0,fTRImage.Width,fTRImage.Height,fWidth-8,0,fTexture.ARGBImage,true);
      if Assigned(fBLImage) then
        fBLImage.CopyTo(0,0,fBLImage.Width,fBLImage.Height,0,fHeight-8,fTexture.ARGBImage,true);
      if Assigned(fBRImage) then
        fBRImage.CopyTo(0,0,fBRImage.Width,fBRImage.Height,fWidth-8,fHeight-8,fTexture.ARGBImage,true);
      // Flashing if picking
      //if fPicking then
      //  Bar(fColorsLeft,0,fColorsWidth+3,Height,VibroColors.GetColor);
      // Letters and arrow
      if Assigned(fFont) and Assigned(fFont2) then begin
        fonttop:=(Height-15) div 2;
        if (Assigned(fGradient) and fGradient.PingPong) then
          fFont2.OutText(fTexture.ARGBImage,'P',fPingpongSwitchLeft+PINGPONGSWITCHWIDTH div 2+3,fonttop,1)
        else
          fFont.OutText(fTexture.ARGBImage,'P',fPingpongSwitchLeft+PINGPONGSWITCHWIDTH div 2+3,fonttop,1);
        if (Assigned(fGradient) and fGradient.Reversed) then
          fFont2.OutText(fTexture.ARGBImage,'R',fReverseSwitchLeft+REVERSESWITCHWIDTH div 2+3,fonttop,1)
        else
          fFont.OutText(fTexture.ARGBImage,'R',fReverseSwitchLeft+REVERSESWITCHWIDTH div 2+3,fonttop,1);
        fFont.OutText(fTexture.ARGBImage,#130,fArrowLeft+ARROWWIDTH div 2+1,fonttop,1);
      end;
    end;
    fTexture.Update;
  end;
  // Gradient bar
  if Assigned(fGradient) and Assigned(fGradientTexture) then begin
    for i:=0 to fColorsWidth-1-3 do begin
      color32:=fGradient.GetColorAt(i/(fColorsWidth-3));
      fGradientTexture.ARGBImage.VLine(i,0,fGradientTexture.Height,color32);
      if color32=Settings.ActiveColor then begin
        fGradientTexture.ARGBImage.VLine(i,fGradientTexture.Height div 2-3,3,SystemPalette[SYSTEMCOLORLIGHT]);
        fGradientTexture.ARGBImage.VLine(i,fGradientTexture.Height div 2,3,SystemPalette[SYSTEMCOLORBLACK]);
      end;
    end;
    fGradientTexture.Update;
  end;
end;

procedure TBDGradient.fSetGradient(value:TGradient);
begin
  fGradient:=value;
  fNeedRedraw:=true;
end;

procedure TBDGradient.RecreateTexture(Sender:TObject);
begin
  if Assigned(fGradientTexture) then fGradientTexture.Free;
  // Recreate texture by inner texture
  fGradientTexture:=TStreamingTexture.Create(fColorsWidth-3,fTexture.Height-6);
  SDL_SetTextureBlendMode(fGradientTexture.Texture,SDL_BLENDMODE_BLEND);
end;

procedure TBDGradient.fSetWidth(value:integer);
begin
  fArrowLeft:=value-ARROWWIDTH-3;
  fColorsWidth:=fArrowLeft-fColorsLeft;
  // At this point we already have to know fColorsWidth for the new width!
  inherited fSetWidth(value);
end;

end.

