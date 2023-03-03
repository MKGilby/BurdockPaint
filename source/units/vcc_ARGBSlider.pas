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

// Version info:
//
//  V1.00: Gilby - 2023.03.02
//    * Initial creation.
//  V1.00: Gilby - 2023.03.03
//    * Renamed to TARGBHorizontalSlider.
//    * Added vertical version.

unit vcc_ARGBSlider;

{$mode Delphi}

interface

uses
  vcc_SliderLogic, ARGBImageUnit, Font2Unit;

type

  { TARGBHorizontalSlider }

  TARGBHorizontalSlider=class(THorizontalSliderLogic)
    constructor Create(iTarget:TARGBImage;iOfsX:integer=0;iOfsY:integer=0); overload;
    procedure Draw; override;
  protected
    // Parents top-left position on the screen
    fOfsX,fOfsY,
    // The position ON the ARGBImage (use it to draw, use Left, Top for mouse)
    fOnImageLeft,fOnImageTop:integer;
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

  { TARGBVerticalSlider }

  TARGBVerticalSlider=class(TVerticalSliderLogic)
    constructor Create(iTarget:TARGBImage;iOfsX:integer=0;iOfsY:integer=0); overload;
    procedure Draw; override;
  protected
    // Parents top-left position on the screen
    fOfsX,fOfsY,
    // The position ON the ARGBImage (use it to draw, use Left, Top for mouse)
    fOnImageLeft,fOnImageTop:integer;
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

uses SysUtils, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.00';

{ TARGBHorizontalSlider }

constructor TARGBHorizontalSlider.Create(iTarget:TARGBImage; iOfsX:integer; iOfsY:integer);
begin
  inherited Create;
  fOfsX:=iOfsX;
  fOfsY:=iOfsY;
  fTarget:=iTarget;
end;

procedure TARGBHorizontalSlider.Draw;
begin
  fTarget.Bar(fOnImageLeft,fOnImageTop,fDecClickAreaSize,fHeight,fBorderColor);
  fTarget.Bar(fOnImageLeft+fDecClickAreaSize,fOnImageTop,fSlideAreaSize,1,fBorderColor);
  fTarget.Bar(fOnImageLeft+fDecClickAreaSize,fOnImageTop+1,fSlideAreaSize,fHeight-2,fNormalColor);
  fTarget.Bar(fOnImageLeft+fDecClickAreaSize,fOnImageTop+1,
    fSlideAreaSize*(Position-MinValue) div (MaxValue-MinValue),fHeight-2,fHighlightColor);
  fTarget.Bar(fOnImageLeft+fDecClickAreaSize,fOnImageTop+fHeight-1,fSlideAreaSize,1,fBorderColor);
  fTarget.Bar(fOnImageLeft+fDecClickAreaSize+fSlideAreaSize,fOnImageTop,
    fIncClickAreaSize,fHeight,fBorderColor);
  fFont.OutText(fTarget,inttostr(fPosition),fOnImageLeft+fWidth div 2,fOnImageTop+2,mjCenter);
end;

procedure TARGBHorizontalSlider.fSetLeft(value:integer);
begin
  inherited fSetLeft(value);
  fOnImageLeft:=value-fOfsX;
end;

procedure TARGBHorizontalSlider.fSetTop(value:integer);
begin
  inherited fSetTop(value);
  fOnImageTop:=value-fOfsY;
end;

{ TARGBVerticalSlider }

constructor TARGBVerticalSlider.Create(iTarget:TARGBImage; iOfsX:integer; iOfsY:integer);
begin
  inherited Create;
  fOfsX:=iOfsX;
  fOfsY:=iOfsY;
  fTarget:=iTarget;
end;

procedure TARGBVerticalSlider.Draw;
begin
  fTarget.Bar(fOnImageLeft,fOnImageTop,fWidth,fDecClickAreaSize,fBorderColor);
  fTarget.Bar(fOnImageLeft,fOnImageTop+fDecClickAreaSize,1,fSlideAreaSize,fBorderColor);
  fTarget.Bar(fOnImageLeft+1,fOnImageTop+fDecClickAreaSize,fWidth-2,fSlideAreaSize,fNormalColor);
  fTarget.Bar(fOnImageLeft+1,fOnImageTop+fDecClickAreaSize,
    fWidth-2,fSlideAreaSize*(Position-MinValue) div (MaxValue-MinValue),fHighlightColor);
  fTarget.Bar(fOnImageLeft+fWidth-1,fOnImageTop+fDecClickAreaSize,1,fSlideAreaSize,fBorderColor);
  fTarget.Bar(fOnImageLeft,fOnImageTop+fDecClickAreaSize+fSlideAreaSize,
    fHeight,fIncClickAreaSize,fBorderColor);
//  fFont.OutText(fTarget,inttostr(fPosition),fOnImageLeft+fWidth div 2,fOnImageTop+2,mjCenter);
end;

procedure TARGBVerticalSlider.fSetLeft(value:integer);
begin
  inherited fSetLeft(value);
  fOnImageLeft:=value-fOfsX;
end;

procedure TARGBVerticalSlider.fSetTop(value:integer);
begin
  inherited fSetTop(value);
  fOnImageTop:=value-fOfsY;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

