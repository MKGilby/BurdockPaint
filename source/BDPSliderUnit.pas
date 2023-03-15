unit BDPSliderUnit;

{$mode Delphi}

interface

uses
  vcc2_Slider, ARGBImageUnit, Font2Unit;

type

  { TBDVerticalSlider }

  TBDVerticalSlider=class(TVerticalSlider)
    constructor Create(iLeft,iTop:integer);
    procedure Draw; override;
  private
    fTopImage,fBottomImage:TARGBImage;
    fArrowFont:TFont;
  end;

  { TBDHorizontalSlider }

  TBDHorizontalSlider=class(THorizontalSlider)
    constructor Create(iLeft,iTop:integer);
    procedure Draw; override;
  private
    fLeftImage,fRightImage:TARGBImage;
    fArrowFont:TFont;
  end;

implementation

uses SysUtils, mk_sdl2, BDPSharedUnit;

const
  SLIDERKNOBWIDTH=57;

{ TBDVerticalSlider }

constructor TBDVerticalSlider.Create(iLeft,iTop:integer);
begin
  inherited Create;
  fTopImage:=MM.Images.ItemByName['SliderTop'];
  fBottomImage:=MM.Images.ItemByName['SliderBottom'];
  Left:=iLeft;
  Top:=iTop;
  Height:=COLORSLIDERWIDTH;
  Width:=COLORSLIDERHEIGHT;
  DecClickAreaSize:=COLORSLIDERHEIGHT;
  IncClickAreaSize:=COLORSLIDERHEIGHT;
  Font:=MM.Fonts['LightGray'];
  fArrowFont:=MM.Fonts['Black'];
  Visible:=true;
  Enabled:=true;
end;

procedure TBDVerticalSlider.Draw;
var p:integer;
begin
  if Visible then begin
    with fTexture.ARGBImage do begin
      Bar(0,0,fWidth,fHeight,OverlayImage.Palette[3]);
      Bar(0,8,3,fHeight-16,OverlayImage.Palette[2]);
      Bar(fWidth-3,8,3,fHeight-16,OverlayImage.Palette[2]);
      Bar(3,fDecClickAreaSize-3,fWidth-6,3,OverlayImage.Palette[2]);
      Bar(3,fDecClickAreaSize+fSlideAreaSize,fWidth-6,3,OverlayImage.Palette[2]);
      p:=fDecClickAreaSize+3+((fSlideAreaSize-SLIDERKNOBWIDTH-6)*(Position-MinValue) div (MaxValue-MinValue));
      Bar(6,p,fWidth-12,SLIDERKNOBWIDTH,OverlayImage.Palette[1]);
    end;
    fTopImage.CopyTo(0,0,fTopImage.Width,fTopImage.Height,0,0,fTexture.ARGBImage,true);
    fBottomImage.CopyTo(0,0,fBottomImage.Width,fBottomImage.Height,0,fHeight-8,fTexture.ARGBImage,true);
    fArrowFont.OutText(fTexture.ARGBImage,#128,fWidth div 2,(fDecClickAreaSize-Font.Height) div 2+1,mjCenter);
    fArrowFont.OutText(fTexture.ARGBImage,#130,fWidth div 2,fDecClickAreaSize+fSlideAreaSize+(fIncClickAreaSize-Font.Height) div 2+1,mjCenter);
    fFont.OutText(fTexture.ARGBImage,inttostr(Position),fWidth div 2,p+(SLIDERKNOBWIDTH-Font.Height) div 2,mjCenter);
    fTexture.Update;
    PutTexture(fLeft,fTop,fTexture);
  end;
end;

{ TBDHorizontalSlider }

constructor TBDHorizontalSlider.Create(iLeft,iTop:integer);
begin
  inherited Create;
  fLeftImage:=MM.Images.ItemByName['SliderLeft'];
  fRightImage:=MM.Images.ItemByName['SliderRight'];
  Left:=iLeft;
  Top:=iTop;
  Height:=COLORSLIDERHEIGHT;
  Width:=COLORSLIDERWIDTH;
  DecClickAreaSize:=COLORSLIDERHEIGHT;
  IncClickAreaSize:=COLORSLIDERHEIGHT;
  Font:=MM.Fonts['LightGray'];
  fArrowFont:=MM.Fonts['Black'];
  Visible:=true;
  Enabled:=true;
end;

procedure TBDHorizontalSlider.Draw;
var p:integer;
begin
  if Visible then begin
    with fTexture.ARGBImage do begin
      Bar(0,0,fWidth,fHeight,OverlayImage.Palette[3]);
      Bar(8,0,fWidth-16,3,OverlayImage.Palette[2]);
      Bar(8,fHeight-3,fWidth-16,3,OverlayImage.Palette[2]);
      Bar(fDecClickAreaSize-3,3,3,fHeight-6,OverlayImage.Palette[2]);
      Bar(fDecClickAreaSize+fSlideAreaSize,3,3,fHeight-6,OverlayImage.Palette[2]);
      p:=fDecClickAreaSize+3+((fSlideAreaSize-SLIDERKNOBWIDTH-6)*(Position-MinValue) div (MaxValue-MinValue));
      Bar(p,6,SLIDERKNOBWIDTH,fHeight-12,OverlayImage.Palette[1]);
    end;
    fLeftImage.CopyTo(0,0,fLeftImage.Width,fLeftImage.Height,0,0,fTexture.ARGBImage,true);
    fRightImage.CopyTo(0,0,fRightImage.Width,fRightImage.Height,fWidth-8,0,fTexture.ARGBImage,true);
    fArrowFont.OutText(fTexture.ARGBImage,#131,COLORSLIDERHEIGHT div 2,9,1);
    fArrowFont.OutText(fTexture.ARGBImage,#129,fDecClickAreaSize+fSlideAreaSize+COLORSLIDERHEIGHT div 2,9,1);
    fFont.OutText(fTexture.ARGBImage, inttostr(Position), p+SLIDERKNOBWIDTH div 2,9,1);
    fTexture.Update;
    PutTexture(fLeft,fTop,fTexture);
  end;
end;

end.

