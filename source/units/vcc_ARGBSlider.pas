{ -[Name]-----------------------------------------

   MKSZTSZ Visual Component Collection for SDL2

                      Simple Slider for ARGBImage

  ------------------------------------------------

  -[Disclaimer]-----------------------------------

  Written by Gilby/MKSZTSZ                Freeware
  Hungary, 2023

  ------------------------------------------------

  -[Description]----------------------------------

   It is a simple slider (much like a scrollbar).

  ------------------------------------------------
}

unit vcc_ARGBSlider;

{$mode Delphi}

interface

uses
  vcc_SliderLogic, ARGBImageUnit, Font2Unit;

type

  { TARGBSlider }

  TARGBSlider=class(TSliderLogic)
    constructor Create(iTarget:TARGBImage;iOfsX:integer=0;iOfsY:integer=0); overload;
    procedure Draw; override;
  protected
    // Parents top-left position on the screen
    fOfsX,fOfsY,
    // The position ON the ARGBImage (use it to draw, use Left, Top for mouse)
    fParentRelativeLeft,fParentrelativeTop:integer;
    fTarget:TARGBImage;
    fFont:TFont;
    fBorderColor,
    fNormalColor,
    fHighlightColor:uint32;
    procedure fSetLeft(value:integer); override;
    procedure fSetTop(value:integer); override;
  public
    property Font:TFont read fFont write fFont;
    property BorderColor:uint32 read fBorderColor write fBorderColor;
    property NormalColor:uint32 read fNormalColor write fNormalColor;
    property HighlightColor:uint32 read fHighlightColor write fHighlightColor;
  end;

implementation

uses SysUtils;

{ TARGBSlider }

constructor TARGBSlider.Create(iTarget:TARGBImage; iOfsX:integer; iOfsY:integer);
begin
  inherited Create;
  fOfsX:=iOfsX;
  fOfsY:=iOfsY;
  fTarget:=iTarget;
end;

procedure TARGBSlider.Draw;
begin
  fTarget.Bar(
    fParentRelativeLeft,fParentrelativeTop,
    fDecClickAreaSize,fHeight,
    fBorderColor);
  fTarget.Bar(
    fParentRelativeLeft+fDecClickAreaSize,fParentrelativeTop,
    fSlideAreaSize,1,
    fBorderColor);
  fTarget.Bar(
    fParentRelativeLeft+fDecClickAreaSize,fParentrelativeTop+1,
    fSlideAreaSize,fHeight-2,
    fNormalColor);
  fTarget.Bar(
    fParentRelativeLeft+fDecClickAreaSize,fParentrelativeTop+1,
    fSlideAreaSize*(Position-MinValue) div (MaxValue-MinValue),fHeight-2,
    fHighlightColor);
  fTarget.Bar(
    fParentRelativeLeft+fDecClickAreaSize,fParentrelativeTop+fHeight-1,
    fSlideAreaSize,1,
    fBorderColor);
  fTarget.Bar(fParentRelativeLeft+fDecClickAreaSize+fSlideAreaSize,fParentrelativeTop,
    fIncClickAreaSize,fHeight,
    fBorderColor);
  fFont.OutText(fTarget,inttostr(fPosition),fParentRelativeLeft+fWidth div 2,fParentrelativeTop+2,mjCenter);
end;

procedure TARGBSlider.fSetLeft(value:integer);
begin
  inherited fSetLeft(value);
  fParentRelativeLeft:=value-fOfsX;
end;

procedure TARGBSlider.fSetTop(value:integer);
begin
  inherited fSetTop(value);
  fParentrelativeTop:=value-fOfsY;
end;

end.

