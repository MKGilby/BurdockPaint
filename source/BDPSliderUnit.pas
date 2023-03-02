unit BDPSliderUnit;

{$mode Delphi}

interface

uses
  vcc_ARGBSlider, ARGBImageUnit, Font2Unit;

type

  { TBDSlider }

  TBDSlider=class(TARGBSlider)
    constructor Create(iTarget:TARGBImage;iOfsX:integer=0;iOfsY:integer=0);
    procedure Draw; override;
  private
    fLeftImage,fRightImage:TARGBImage;
    fArrowFont:TFont;
  end;

implementation

uses SysUtils, BDPSharedUnit;

const
  SLIDERKNOBWIDTH=57;

{ TBDSlider }

constructor TBDSlider.Create(iTarget:TARGBImage; iOfsX:integer; iOfsY:integer);
begin
  inherited Create(iTarget,iOfsX,iOfsY);
  fLeftImage:=MM.Images.ItemByName['SliderLeft'];
  fRightImage:=MM.Images.ItemByName['SliderRight'];
  fHeight:=COLORSLIDERHEIGHT;
  Width:=COLORSLIDERWIDTH;
  DecClickAreaSize:=COLORSLIDERHEIGHT;
  IncClickAreaSize:=COLORSLIDERHEIGHT;
  Font:=MM.Fonts['LightGray'];
  fArrowFont:=MM.Fonts['Black'];
end;

procedure TBDSlider.Draw;
var p:integer;
begin
  fTarget.Bar(fParentRelativeLeft+8,fParentrelativeTop,fWidth-16,3,OverlayImage.Palette[2]);
  fTarget.Bar(fParentRelativeLeft+8,fParentrelativeTop+fHeight-3,fWidth-16,3,OverlayImage.Palette[2]);
  fTarget.Bar(fParentRelativeLeft+fDecClickAreaSize-3,fParentrelativeTop+3,3,fHeight-6,OverlayImage.Palette[2]);
  fTarget.Bar(fParentRelativeLeft+fDecClickAreaSize+fSlideAreaSize,fParentrelativeTop+3,3,fHeight-6,OverlayImage.Palette[2]);
  p:=fParentRelativeLeft+fDecClickAreaSize+3+((fSlideAreaSize-SLIDERKNOBWIDTH-6)*(Position-MinValue) div (MaxValue-MinValue));
  fTarget.Bar(p,fParentrelativeTop+6,SLIDERKNOBWIDTH,fHeight-12,OverlayImage.Palette[1]);
  fLeftImage.CopyTo(0,0,fLeftImage.Width,fLeftImage.Height,fParentRelativeLeft,fParentrelativeTop,fTarget,true);
  fRightImage.CopyTo(0,0,fRightImage.Width,fRightImage.Height,fParentRelativeLeft+fWidth-8,fParentrelativeTop,fTarget,true);
  fArrowFont.OutText(fTarget,#131,fParentRelativeLeft+COLORSLIDERHEIGHT div 2,fParentrelativeTop+9,1);
  fArrowFont.OutText(fTarget,#129,fParentRelativeLeft+fDecClickAreaSize+fSlideAreaSize+COLORSLIDERHEIGHT div 2,fParentrelativeTop+9,1);
  fFont.OutText(fTarget, inttostr(Position), p+SLIDERKNOBWIDTH div 2,fParentrelativeTop+9,1);

end;

end.

