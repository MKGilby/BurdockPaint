{ -[Name]-----------------------------------------

   MKSZTSZ Visual Component Collection for SDL2

                                    Simple Slider

  ------------------------------------------------

  -[Disclaimer]-----------------------------------

   Written by Gilby/MKSZTSZ              Freeware
   Hungary, 2023

  ------------------------------------------------

  -[Description]----------------------------------

   It is a simple slider (much like a scrollbar).

  ------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2023.11.20
//    * Initial creation from vcc2_Slider

unit vcc2_SliderStatic;

{$mode Delphi}

interface

uses
  vcc2_SliderLogicStatic, ARGBImageUnit, Font2Unit;

type

  { TARGBHorizontalSlider }

  THorizontalSlider=class(THorizontalSliderLogic)
  protected
    fFont:TFont;
    fBorderColor,
    fNormalColor,
    fHighlightColor:uint32;
    procedure ReDraw; override;
  public
    property Font:TFont read fFont write fFont;
    property BorderColor:uint32 read fBorderColor write fBorderColor;
    property NormalColor:uint32 read fNormalColor write fNormalColor;
    property HighlightColor:uint32 read fHighlightColor write fHighlightColor;
  end;

  { TVerticalSlider }

  TVerticalSlider=class(TVerticalSliderLogic)
  protected
    fFont:TFont;
    fBorderColor,
    fNormalColor,
    fHighlightColor:uint32;
    procedure ReDraw; override;
  public
    property Font:TFont read fFont write fFont;
    property BorderColor:uint32 read fBorderColor write fBorderColor;
    property NormalColor:uint32 read fNormalColor write fNormalColor;
    property HighlightColor:uint32 read fHighlightColor write fHighlightColor;
  end;

implementation

uses SysUtils, mk_sdl2, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.00';

{ TARGBHorizontalSlider }

procedure THorizontalSlider.ReDraw;
begin
  with fImage do begin
    Bar(0,0,fDecClickAreaSize,fHeight,fBorderColor);
    Bar(fDecClickAreaSize,0,fSlideAreaSize,1,fBorderColor);
    Bar(fDecClickAreaSize,1,fSlideAreaSize,fHeight-2,fNormalColor);
    Bar(fDecClickAreaSize,1,
      fSlideAreaSize*(Position-MinValue) div (MaxValue-MinValue),fHeight-2,fHighlightColor);
    Bar(fDecClickAreaSize,fHeight-1,fSlideAreaSize,1,fBorderColor);
    Bar(fDecClickAreaSize+fSlideAreaSize,0,fIncClickAreaSize,fHeight,fBorderColor);
  end;
  if Assigned(fFont) then
  fFont.OutText(fImage,inttostr(fPosition),fWidth div 2,2,mjCenter);
end;

{ TVerticalSlider }

procedure TVerticalSlider.ReDraw;
begin
  with fImage do begin
    Bar(0,0,fWidth,fDecClickAreaSize,fBorderColor);
    Bar(0,fDecClickAreaSize,1,fSlideAreaSize,fBorderColor);
    Bar(1,fDecClickAreaSize,fWidth-2,fSlideAreaSize,fNormalColor);
    Bar(1,fDecClickAreaSize,
      fWidth-2,fSlideAreaSize*(Position-MinValue) div (MaxValue-MinValue),fHighlightColor);
    Bar(fWidth-1,fDecClickAreaSize,1,fSlideAreaSize,fBorderColor);
    Bar(0,0+fDecClickAreaSize+fSlideAreaSize,fHeight,fIncClickAreaSize,fBorderColor);
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

