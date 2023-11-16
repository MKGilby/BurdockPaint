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

unit BDPSliders;

{$mode Delphi}

interface

uses SysUtils, vcc2_Slider, Font2Unit, ARGBImageUnit;

type

  { TBDVerticalSlider }

  TBDVerticalSlider=class(TVerticalSlider)
    constructor Create(iLeft,iTop:integer); overload;
    constructor Create(iLeft,iTop,iWidth,iHeight:integer); overload;
  protected
    procedure ReDraw; override;
  private
    fTLImage,fTRImage,fBLImage,fBRImage:TARGBImage;
    fArrowFont,fDisabledArrowFont:TFont;
  public
    ShowNumber:boolean;
  end;

  { TBDHorizontalSlider }

  TBDHorizontalSlider=class(THorizontalSlider)
    constructor Create(iLeft,iTop:integer); overload;
    constructor Create(iLeft,iTop,iWidth,iHeight:integer); overload;
  protected
    procedure ReDraw; override;
  private
    fTLImage,fTRImage,fBLImage,fBRImage:TARGBImage;
    fArrowFont,fDisabledArrowFont:TFont;
  public
    ShowNumber:boolean;
  end;

implementation

uses BDPShared;

const
  SLIDERDEFAULTWIDTH=240;
  SLIDERDEFAULTHEIGHT=33;
  MINIMUMKNOBSIZE:integer=57;

{ TBDVerticalSlider }

constructor TBDVerticalSlider.Create(iLeft,iTop:integer);
begin
  Create(iLeft,iTop,SLIDERDEFAULTWIDTH,SLIDERDEFAULTHEIGHT);
end;

constructor TBDVerticalSlider.Create(iLeft,iTop,iWidth,iHeight:integer);
begin
  inherited Create;
  Left:=iLeft;
  Top:=iTop;
  Height:=iHeight;
  Width:=iWidth;
  DecClickAreaSize:=iWidth;
  IncClickAreaSize:=iWidth;
  Font:=MM.Fonts['LightGray'];
  fArrowFont:=MM.Fonts['Black'];
  fDisabledArrowFont:=MM.Fonts['DarkGray'];
  Visible:=true;
  Enabled:=true;
  fTLImage:=MM.Images.ItemByName['ArchTopLeft'];
  fTRImage:=MM.Images.ItemByName['ArchTopRight'];
  fBLImage:=MM.Images.ItemByName['ArchBottomLeft'];
  fBRImage:=MM.Images.ItemByName['ArchBottomRight'];
  ShowNumber:=true;
  fNeedRedraw:=true;
end;

procedure TBDVerticalSlider.ReDraw;
var knobsize,p:integer;
begin
  if Assigned(fTexture) then begin
    knobsize:=(fSlideAreaSize-6) div (MaxValue-MinValue+1);
    if knobsize<MINIMUMKNOBSIZE then knobsize:=MINIMUMKNOBSIZE;
    with fTexture.ARGBImage do begin
      Bar(3,3,Width-6,Height-6,SystemPalette[SYSTEMCOLORMID]);
      Bar(8,0,Width-16,3,SystemPalette[SYSTEMCOLORDARK]);
      Bar(8,Height-3,fWidth-16,3,SystemPalette[SYSTEMCOLORDARK]);
      Bar(0,8,3,Height-16,SystemPalette[SYSTEMCOLORDARK]);
      Bar(Width-3,8,3,Height-16,SystemPalette[SYSTEMCOLORDARK]);
      Bar(3,fDecClickAreaSize-3,Width-6,3,SystemPalette[SYSTEMCOLORDARK]);
      Bar(3,fDecClickAreaSize+fSlideAreaSize,Width-6,3,SystemPalette[SYSTEMCOLORDARK]);
      if (MinValue<>MaxValue) then
        p:=fDecClickAreaSize+3+((fSlideAreaSize-knobsize-6)*(Position-MinValue) div (MaxValue-MinValue))
      else
        p:=fDecClickAreaSize+3+(fSlideAreaSize-knobsize-6) div 2;
      Bar(6,p,Width-12,knobsize,SystemPalette[SYSTEMCOLORBLACK]);
    end;
    if Assigned(fTLImage) then
      fTLImage.CopyTo(0,0,fTLImage.Width,fTLImage.Height,0,0,fTexture.ARGBImage,true);
    if Assigned(fTRImage) then
      fTRImage.CopyTo(0,0,fTRImage.Width,fTRImage.Height,fWidth-8,0,fTexture.ARGBImage,true);
    if Assigned(fBLImage) then
      fBLImage.CopyTo(0,0,fBLImage.Width,fBLImage.Height,0,fHeight-8,fTexture.ARGBImage,true);
    if Assigned(fBRImage) then
      fBRImage.CopyTo(0,0,fBRImage.Width,fBRImage.Height,fWidth-8,fHeight-8,fTexture.ARGBImage,true);
    if Assigned(fArrowFont) then begin
      if fPosition>fMinValue then
        fArrowFont.OutText(fTexture.ARGBImage,#128,fWidth div 2,(fDecClickAreaSize-Font.Height) div 2+1,mjCenter);
      if fPosition<fMaxValue then
        fArrowFont.OutText(fTexture.ARGBImage,#130,fWidth div 2,fDecClickAreaSize+fSlideAreaSize+(fIncClickAreaSize-Font.Height) div 2+1,mjCenter);
    end;
    if Assigned(fDisabledArrowFont) then begin
      if fPosition=fMinValue then
        fDisabledArrowFont.OutText(fTexture.ARGBImage,#128,fWidth div 2,(fDecClickAreaSize-fDisabledArrowFont.Height) div 2+1,mjCenter);
      if fPosition=fMaxValue then
        fDisabledArrowFont.OutText(fTexture.ARGBImage,#130,fWidth div 2,fDecClickAreaSize+fSlideAreaSize+(fIncClickAreaSize-fDisabledArrowFont.Height) div 2+1,mjCenter);
    end;
    if ShowNumber and Assigned(fFont) then
      fFont.OutText(fTexture.ARGBImage,inttostr(Position),fWidth div 2,p+(knobsize-Font.Height) div 2,mjCenter);
    fTexture.Update;
  end;
end;

{ TBDHorizontalSlider }

constructor TBDHorizontalSlider.Create(iLeft,iTop:integer);
begin
  Create(iLeft,iTop,SLIDERDEFAULTWIDTH,SLIDERDEFAULTHEIGHT);
end;

constructor TBDHorizontalSlider.Create(iLeft,iTop,iWidth,iHeight:integer);
begin
  inherited Create;
  fTLImage:=MM.Images.ItemByName['ArchTopLeft'];
  fTRImage:=MM.Images.ItemByName['ArchTopRight'];
  fBLImage:=MM.Images.ItemByName['ArchBottomLeft'];
  fBRImage:=MM.Images.ItemByName['ArchBottomRight'];
  Left:=iLeft;
  Top:=iTop;
  Height:=iHeight;
  Width:=iWidth;
  DecClickAreaSize:=Height;
  IncClickAreaSize:=Height;
  Font:=MM.Fonts['LightGray'];
  fArrowFont:=MM.Fonts['Black'];
  fDisabledArrowFont:=MM.Fonts['DarkGray'];
  Visible:=true;
  Enabled:=true;
  ShowNumber:=true;
  fNeedRedraw:=true;
end;

procedure TBDHorizontalSlider.ReDraw;
var p,fonttop:integer;
begin
  if Assigned(fTexture) then begin
    with fTexture.ARGBImage do begin
      Bar(3,3,Width-6,Height-6,SystemPalette[SYSTEMCOLORMID]);
      Bar(8,0,Width-16,3,SystemPalette[SYSTEMCOLORDARK]);
      Bar(8,Height-3,fWidth-16,3,SystemPalette[SYSTEMCOLORDARK]);
      Bar(0,8,3,Height-16,SystemPalette[SYSTEMCOLORDARK]);
      Bar(Width-3,8,3,Height-16,SystemPalette[SYSTEMCOLORDARK]);
      Bar(fDecClickAreaSize-3,3,3,Height-6,SystemPalette[SYSTEMCOLORDARK]);
      Bar(fDecClickAreaSize+fSlideAreaSize,3,3,Height-6,SystemPalette[SYSTEMCOLORDARK]);
      if (MaxValue<>MinValue) then
        p:=fDecClickAreaSize+3+((fSlideAreaSize-MINIMUMKNOBSIZE-6)*(Position-MinValue) div (MaxValue-MinValue))
      else
        p:=fDecClickAreaSize+3+((fSlideAreaSize-MINIMUMKNOBSIZE-6) div 2);
      Bar(p,6,MINIMUMKNOBSIZE,Height-12,SystemPalette[SYSTEMCOLORBLACK]);
    end;
    if Assigned(fTLImage) then
      fTLImage.CopyTo(0,0,fTLImage.Width,fTLImage.Height,0,0,fTexture.ARGBImage,true);
    if Assigned(fTRImage) then
      fTRImage.CopyTo(0,0,fTRImage.Width,fTRImage.Height,fWidth-8,0,fTexture.ARGBImage,true);
    if Assigned(fBLImage) then
      fBLImage.CopyTo(0,0,fBLImage.Width,fBLImage.Height,0,fHeight-8,fTexture.ARGBImage,true);
    if Assigned(fBRImage) then
      fBRImage.CopyTo(0,0,fBRImage.Width,fBRImage.Height,fWidth-8,fHeight-8,fTexture.ARGBImage,true);
    fonttop:=(Height-15) div 2;
    if Assigned(fArrowFont) then begin
      if fPosition>fMinValue then
        fArrowFont.OutText(fTexture.ARGBImage,#131,fDecClickAreaSize div 2,fonttop,1);
      if fPosition<fMaxValue then
        fArrowFont.OutText(fTexture.ARGBImage,#129,fDecClickAreaSize+fSlideAreaSize+fIncClickAreaSize div 2,fonttop,1);
    end;
    if Assigned(fDisabledArrowFont) then begin
      if fPosition=fMinValue then
        fDisabledArrowFont.OutText(fTexture.ARGBImage,#131,fDecClickAreaSize div 2,fonttop,1);
      if fPosition=fMaxValue then
        fDisabledArrowFont.OutText(fTexture.ARGBImage,#129,fDecClickAreaSize+fSlideAreaSize+fIncClickAreaSize div 2,fonttop,1);
    end;
    if ShowNumber and Assigned(fFont) then
      fFont.OutText(fTexture.ARGBImage, inttostr(Position), p+MINIMUMKNOBSIZE div 2,fonttop,1);
    fTexture.Update;
  end;
end;

end.

