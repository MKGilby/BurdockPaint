unit BDPSliderUnit;

{$mode Delphi}

interface

uses
  vcc_ARGBSlider, ARGBImageUnit, Font2Unit;

type

  { TBDVerticalSlider }

  TBDVerticalSlider=class(TARGBVerticalSlider)
    constructor Create(iTarget:TARGBImage;iOfsX:integer=0;iOfsY:integer=0);
    procedure Draw; override;
    function MouseWheel(Sender:TObject;x,y,wheelx,wheely:integer):boolean;
  private
    fTopImage,fBottomImage:TARGBImage;
    fArrowFont:TFont;
  end;

  { TBDHorizontalSlider }

  TBDHorizontalSlider=class(TARGBHorizontalSlider)
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

{ TBDVerticalSlider }

constructor TBDVerticalSlider.Create(iTarget:TARGBImage; iOfsX:integer; iOfsY:integer);
begin
  inherited Create(iTarget,iOfsX,iOfsY);
  fTopImage:=MM.Images.ItemByName['SliderTop'];
  fBottomImage:=MM.Images.ItemByName['SliderBottom'];
  fHeight:=COLORSLIDERWIDTH;
  fWidth:=COLORSLIDERHEIGHT;
  DecClickAreaSize:=COLORSLIDERHEIGHT;
  IncClickAreaSize:=COLORSLIDERHEIGHT;
  Font:=MM.Fonts['LightGray'];
  fArrowFont:=MM.Fonts['Black'];
  Visible:=true;
  Enabled:=true;
  OnMouseWheel:=MouseWheel;
end;

procedure TBDVerticalSlider.Draw;
var p:integer;
begin
  fTarget.Bar(fOnImageLeft,fOnImageTop+8,3,fHeight-16,OverlayImage.Palette[2]);
  fTarget.Bar(fOnImageLeft+fWidth-3,fOnImageTop+8,3,fHeight-16,OverlayImage.Palette[2]);
  fTarget.Bar(fOnImageLeft+3,fOnImageTop+fDecClickAreaSize-3,fWidth-6,3,OverlayImage.Palette[2]);
  fTarget.Bar(fOnImageLeft+3,fOnImageTop+fDecClickAreaSize+fSlideAreaSize,fWidth-6,3,OverlayImage.Palette[2]);
  p:=fOnImageTop+fDecClickAreaSize+3+((fSlideAreaSize-SLIDERKNOBWIDTH-6)*(Position-MinValue) div (MaxValue-MinValue));
  fTarget.Bar(fOnImageLeft+6,p,fWidth-12,SLIDERKNOBWIDTH,OverlayImage.Palette[1]);
  fTopImage.CopyTo(0,0,fTopImage.Width,fTopImage.Height,fOnImageLeft,fOnImageTop,fTarget,true);
  fBottomImage.CopyTo(0,0,fBottomImage.Width,fBottomImage.Height,fOnImageLeft,fOnImageTop+fHeight-8,fTarget,true);
  fArrowFont.OutText(fTarget,#128,fOnImageLeft+fWidth div 2,fOnImageTop+(fDecClickAreaSize-Font.Height) div 2+1,mjCenter);
  fArrowFont.OutText(fTarget,#130,fOnImageLeft+fWidth div 2,fOnImageTop+fDecClickAreaSize+fSlideAreaSize+(fIncClickAreaSize-Font.Height) div 2+1,mjCenter);
  fFont.OutText(fTarget, inttostr(Position),fOnImageLeft+fWidth div 2,p+(SLIDERKNOBWIDTH-Font.Height) div 2,mjCenter);
end;

function TBDVerticalSlider.MouseWheel(Sender: TObject; x, y, wheelx, wheely: integer): boolean;
begin
  if (wheely<0) then begin
    fPosition-=wheely;
    if fPosition>fMaxValue then fPosition:=fMaxValue;
  end else
  if (wheely>0) then begin
    fPosition-=wheely;
    if fPosition<fMinValue then fPosition:=fMinValue;
  end;
  Result:=true;
end;

{ TBDHorizontalSlider }

constructor TBDHorizontalSlider.Create(iTarget:TARGBImage; iOfsX:integer; iOfsY:integer);
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
  Visible:=true;
  Enabled:=true;
end;

procedure TBDHorizontalSlider.Draw;
var p:integer;
begin
  fTarget.Bar(fOnImageLeft+8,fOnImageTop,fWidth-16,3,OverlayImage.Palette[2]);
  fTarget.Bar(fOnImageLeft+8,fOnImageTop+fHeight-3,fWidth-16,3,OverlayImage.Palette[2]);
  fTarget.Bar(fOnImageLeft+fDecClickAreaSize-3,fOnImageTop+3,3,fHeight-6,OverlayImage.Palette[2]);
  fTarget.Bar(fOnImageLeft+fDecClickAreaSize+fSlideAreaSize,fOnImageTop+3,3,fHeight-6,OverlayImage.Palette[2]);
  p:=fOnImageLeft+fDecClickAreaSize+3+((fSlideAreaSize-SLIDERKNOBWIDTH-6)*(Position-MinValue) div (MaxValue-MinValue));
  fTarget.Bar(p,fOnImageTop+6,SLIDERKNOBWIDTH,fHeight-12,OverlayImage.Palette[1]);
  fLeftImage.CopyTo(0,0,fLeftImage.Width,fLeftImage.Height,fOnImageLeft,fOnImageTop,fTarget,true);
  fRightImage.CopyTo(0,0,fRightImage.Width,fRightImage.Height,fOnImageLeft+fWidth-8,fOnImageTop,fTarget,true);
  fArrowFont.OutText(fTarget,#131,fOnImageLeft+COLORSLIDERHEIGHT div 2,fOnImageTop+9,1);
  fArrowFont.OutText(fTarget,#129,fOnImageLeft+fDecClickAreaSize+fSlideAreaSize+COLORSLIDERHEIGHT div 2,fOnImageTop+9,1);
  fFont.OutText(fTarget, inttostr(Position), p+SLIDERKNOBWIDTH div 2,fOnImageTop+9,1);

end;

end.

