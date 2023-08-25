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

unit BDPBasicControls;

{$mode Delphi}

interface

uses SysUtils, vcc2_Slider, vcc2_Button, Font2Unit, ARGBImageUnit,
  BDPMessage, vcc2_VisibleControl;

type

  { TBDButton }

  TBDButton=class(TButton)
    constructor Create(iX,iY,iWidth,iHeight:integer;iCaption,iHint:string;
          iAssignedobject:TObject=nil); overload;
    constructor Create; overload;
    procedure MouseEnter(Sender:TObject);
    procedure MouseLeave(Sender:TObject);
    procedure Click(Sender:TObject;x, y, buttons: integer);
  protected
    procedure ReDraw; override;
  private
    fHint:string;
    fAssignedObject:TObject;
    fTLImage,fTRImage,fBLImage,fBRImage:TARGBImage;
    fFont2:TFont;
    fMessage:TMessage;
  published
    property Hint:string read fHint write fHint;
    property AssignedObject:TObject read fAssignedObject write fAssignedObject;
  public
    property Message:TMessage read fMessage write fMessage;
  end;

  { TBDVerticalSlider }

  TBDVerticalSlider=class(TVerticalSlider)
    constructor Create(iLeft,iTop:integer);
  protected
    procedure ReDraw; override;
  private
    fTLImage,fTRImage,fBLImage,fBRImage:TARGBImage;
    fArrowFont:TFont;
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
  end;

  { TBDColorBox }

  TBDColorBox=class(TVisibleControl)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer);
    procedure ColorChanged;
  protected
    procedure ReDraw; override;
  private
    fColor:uint32;
    fTLImage,fTRImage,fBLImage,fBRImage:TARGBImage;
    procedure fSetColor(value:uint32);
  public
    property Color:uint32 read fColor write fSetColor;
  end;

  { TBDHSBox }

  TBDHSBox=class(TVisibleControl)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer);
    procedure SetColor(pH,pS:byte);
    procedure Click(Sender:TObject;x,y,button:integer);
  protected
    procedure ReDraw; override;
  private
    fColor:uint32;
    fX,fY:integer;   // current crosshair center position
    function GetColorByH(pX:integer):uint32;
    function ModifyColorByS(pColor:uint32;pY:integer):uint32;
  public
    property Color:uint32 read fColor;
  end;

  { TBDLightSlider }

  TBDLightSlider=class(TVisibleControl)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer);
  protected
    procedure ReDraw; override;
  private
    fBaseColor:uint32;
    fSelectedL:byte;
    procedure fSetBaseColor(pValue:uint32);
    procedure fSetSelectedL(pValue:byte);
  public
    property BaseColor:uint32 write fSetBaseColor;
    property SelectedL:byte read fSelectedL write fSetSelectedL;
  end;

implementation

uses BDPShared, mk_sdl2;

const
  SLIDERKNOBWIDTH=57;

{ TBDButton }

constructor TBDButton.Create(iX,iY,iWidth,iHeight:integer;
  iCaption,iHint:string; iAssignedobject:TObject);
begin
  Create;
  Left:=iX;
  Top:=iY;
  Width:=iWidth;
  Height:=iHeight;
  TextAlignX:=mjCenter;
  TextOffsetY:=2;
  Font:=MM.Fonts['Black'];
  fFont2:=MM.Fonts['Red'];
  fCaption:=iCaption;
  fName:=fCaption;
  fHint:=iHint;
  fAssignedObject:=iAssignedobject;
  fMessage:=TMessage.Init(MSG_NONE,0,0);
  fTLImage:=MM.Images.ItemByName['ArchTopLeft'];
  fTRImage:=MM.Images.ItemByName['ArchTopRight'];
  fBLImage:=MM.Images.ItemByName['ArchBottomLeft'];
  fBRImage:=MM.Images.ItemByName['ArchBottomRight'];
  fNeedRedraw:=true;
end;

constructor TBDButton.Create;
begin
  inherited Create;
  fSelected:=false;
  OnMouseEnter:=Self.MouseEnter;
  OnMouseLeave:=Self.MouseLeave;
  OnClick:=Self.Click;
end;

procedure TBDButton.MouseEnter(Sender:TObject);
begin
  InfoBar.ShowText(fHint);
end;

procedure TBDButton.MouseLeave(Sender:TObject);
begin
  InfoBar.ShowText('');
end;

procedure TBDButton.Click(Sender:TObject; x,y,buttons:integer);
begin
  case buttons of
    1:begin  // Left click
        if fMessage.TypeID<>MSG_NONE then MessageQueue.AddMessage(fMessage);
      end;
  end;
end;

procedure TBDButton.ReDraw;
begin
  with fTexture.ARGBImage do begin
    Bar(8,0,Width-16,3,SystemPalette[2]);
    Bar(8,Height-3,fWidth-16,3,SystemPalette[2]);
    Bar(0,8,3,Height-16,SystemPalette[2]);
    Bar(Width-3,8,3,Height-16,SystemPalette[2]);
    if fSelected then
      Bar(3,3,Width-6,Height-6,SystemPalette[4])
    else begin
      if fEnabled then
        Bar(3,3,Width-6,Height-6,SystemPalette[3])
      else
        Bar(3,3,Width-6,Height-6,SystemPalette[2]);
    end;
  end;
  if Assigned(fTLImage) then
    fTLImage.CopyTo(0,0,fTLImage.Width,fTLImage.Height,0,0,fTexture.ARGBImage,true);
  if Assigned(fTRImage) then
    fTRImage.CopyTo(0,0,fTRImage.Width,fTRImage.Height,fWidth-8,0,fTexture.ARGBImage,true);
  if Assigned(fBLImage) then
    fBLImage.CopyTo(0,0,fBLImage.Width,fBLImage.Height,0,fHeight-8,fTexture.ARGBImage,true);
  if Assigned(fBRImage) then
    fBRImage.CopyTo(0,0,fBRImage.Width,fBRImage.Height,fWidth-8,fHeight-8,fTexture.ARGBImage,true);
  if not fSelected then begin
    if Assigned(fFont) then
      fFont.OutText(fTexture.ARGBImage,fCaption,fTextAlignPointX-fLeft,fTextAlignPointY+fTextOffsetY-fTop,fTextAlignX)
  end else begin
    if Assigned(fFont2) then
      fFont2.OutText(fTexture.ARGBImage,fCaption,fTextAlignPointX-fLeft,fTextAlignPointY+fTextOffsetY-fTop,fTextAlignX);
  end;
  fTexture.Update;
end;

{ TBDVerticalSlider }

constructor TBDVerticalSlider.Create(iLeft,iTop:integer);
begin
  inherited Create;
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
  fTLImage:=MM.Images.ItemByName['ArchTopLeft'];
  fTRImage:=MM.Images.ItemByName['ArchTopRight'];
  fBLImage:=MM.Images.ItemByName['ArchBottomLeft'];
  fBRImage:=MM.Images.ItemByName['ArchBottomRight'];
  fNeedRedraw:=true;
end;

procedure TBDVerticalSlider.ReDraw;
var p:integer;
begin
  if Assigned(fTexture) then begin
    with fTexture.ARGBImage do begin
      Bar(3,3,Width-6,Height-6,SystemPalette[3]);
      Bar(8,0,Width-16,3,SystemPalette[2]);
      Bar(8,Height-3,fWidth-16,3,SystemPalette[2]);
      Bar(0,8,3,Height-16,SystemPalette[2]);
      Bar(Width-3,8,3,Height-16,SystemPalette[2]);
      Bar(3,fDecClickAreaSize-3,Width-6,3,SystemPalette[2]);
      Bar(3,fDecClickAreaSize+fSlideAreaSize,Width-6,3,SystemPalette[2]);
      p:=fDecClickAreaSize+3+((fSlideAreaSize-SLIDERKNOBWIDTH-6)*(Position-MinValue) div (MaxValue-MinValue));
      Bar(6,p,Width-12,SLIDERKNOBWIDTH,SystemPalette[1]);
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
      fArrowFont.OutText(fTexture.ARGBImage,#128,fWidth div 2,(fDecClickAreaSize-Font.Height) div 2+1,mjCenter);
      fArrowFont.OutText(fTexture.ARGBImage,#130,fWidth div 2,fDecClickAreaSize+fSlideAreaSize+(fIncClickAreaSize-Font.Height) div 2+1,mjCenter);
    end;
    if Assigned(fFont) then
      fFont.OutText(fTexture.ARGBImage,inttostr(Position),fWidth div 2,p+(SLIDERKNOBWIDTH-Font.Height) div 2,mjCenter);
    fTexture.Update;
  end;
end;

{ TBDHorizontalSlider }

constructor TBDHorizontalSlider.Create(iLeft,iTop:integer);
begin
  inherited Create;
  fTLImage:=MM.Images.ItemByName['ArchTopLeft'];
  fTRImage:=MM.Images.ItemByName['ArchTopRight'];
  fBLImage:=MM.Images.ItemByName['ArchBottomLeft'];
  fBRImage:=MM.Images.ItemByName['ArchBottomRight'];
  Left:=iLeft;
  Top:=iTop;
  Height:=COLORSLIDERHEIGHT;
  Width:=COLORSLIDERWIDTH;
  DecClickAreaSize:=COLORSLIDERHEIGHT;
  IncClickAreaSize:=COLORSLIDERHEIGHT;
  Font:=MM.Fonts['LightGray'];
  fArrowFont:=MM.Fonts['Black'];
  fDisabledArrowFont:=MM.Fonts['DarkGray'];
  Visible:=true;
  Enabled:=true;
  fNeedRedraw:=true;
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
  fNeedRedraw:=true;
end;

procedure TBDHorizontalSlider.ReDraw;
var p,fonttop:integer;
begin
  if Assigned(fTexture) then begin
    with fTexture.ARGBImage do begin
      Bar(3,3,Width-6,Height-6,SystemPalette[3]);
      Bar(8,0,Width-16,3,SystemPalette[2]);
      Bar(8,Height-3,fWidth-16,3,SystemPalette[2]);
      Bar(0,8,3,Height-16,SystemPalette[2]);
      Bar(Width-3,8,3,Height-16,SystemPalette[2]);
      Bar(fDecClickAreaSize-3,3,3,Height-6,SystemPalette[2]);
      Bar(fDecClickAreaSize+fSlideAreaSize,3,3,Height-6,SystemPalette[2]);
      if (MaxValue<>MinValue) then
        p:=fDecClickAreaSize+3+((fSlideAreaSize-SLIDERKNOBWIDTH-6)*(Position-MinValue) div (MaxValue-MinValue))
      else
        p:=fDecClickAreaSize+3+((fSlideAreaSize-SLIDERKNOBWIDTH-6) div 2);
      Bar(p,6,SLIDERKNOBWIDTH,Height-12,SystemPalette[1]);
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
    if Assigned(fFont) then
      fFont.OutText(fTexture.ARGBImage, inttostr(Position), p+SLIDERKNOBWIDTH div 2,fonttop,1);
    fTexture.Update;
  end;
end;

{ TBDColorBox }

constructor TBDColorBox.Create(iLeft,iTop,iWidth,iHeight:integer);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  Width:=iWidth;
  Height:=iHeight;
  fTLImage:=MM.Images.ItemByName['ArchTopLeft'];
  fTRImage:=MM.Images.ItemByName['ArchTopRight'];
  fBLImage:=MM.Images.ItemByName['ArchBottomLeft'];
  fBRImage:=MM.Images.ItemByName['ArchBottomRight'];
  fColor:=65535;
  fVisible:=true;

  fNeedRedraw:=true;
end;

procedure TBDColorBox.ColorChanged;
begin
  fNeedRedraw:=true;
end;

procedure TBDColorBox.ReDraw;
begin
  if Assigned(fTexture) then begin
    with fTexture.ARGBImage do begin
      Bar(3,3,Width-6,Height-6,fColor);
      Bar(8,0,Width-16,3,SystemPalette[2]);
      Bar(8,Height-3,fWidth-16,3,SystemPalette[2]);
      Bar(0,8,3,Height-16,SystemPalette[2]);
      Bar(Width-3,8,3,Height-16,SystemPalette[2]);
    end;
    if Assigned(fTLImage) then
      fTLImage.CopyTo(0,0,fTLImage.Width,fTLImage.Height,0,0,fTexture.ARGBImage,true);
    if Assigned(fTRImage) then
      fTRImage.CopyTo(0,0,fTRImage.Width,fTRImage.Height,fWidth-8,0,fTexture.ARGBImage,true);
    if Assigned(fBLImage) then
      fBLImage.CopyTo(0,0,fBLImage.Width,fBLImage.Height,0,fHeight-8,fTexture.ARGBImage,true);
    if Assigned(fBRImage) then
      fBRImage.CopyTo(0,0,fBRImage.Width,fBRImage.Height,fWidth-8,fHeight-8,fTexture.ARGBImage,true);
    fTexture.Update;
  end;
end;

procedure TBDColorBox.fSetColor(value:uint32);
begin
  if (fColor<>value) then begin fColor:=value;fNeedRedraw:=true;end;
end;

{ TBDHSBox }

constructor TBDHSBox.Create(iLeft,iTop,iWidth,iHeight:integer);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  Width:=iWidth;
  Height:=iHeight;
  fX:=0;
  fY:=0;
  fColor:=$ffff0000;
  fVisible:=true;
  fNeedRedraw:=true;
  OnClick:=Click;
end;

procedure TBDHSBox.SetColor(pH,pS:byte);
begin
  fX:=(pH*(fWidth-6) div 255);
  fY:=(pS*(fHeight-6) div 255);
end;

procedure TBDHSBox.Click(Sender:TObject; x,y,button:integer);
begin
  x-=fLeft;y-=fTop;
  if x<3 then x:=3
  else if x>fWidth-3-1 then x:=fWidth-3-1;
  if y<3 then y:=3
  else if y>fHeight-3-1 then y:=fHeight-3-1;
  fX:=x-3;
  fY:=y-3;
  fColor:=ModifyColorByS(GetColorByH(fX+5),fY+5);
  Refresh;
end;

procedure TBDHSBox.ReDraw;
var i,j:integer;c:uint32;
begin
  if Assigned(fTexture) then begin
    with fTexture.ARGBImage do begin
      Bar(0,0,Width,3,SystemPalette[2]);
      Bar(0,Height-3,fWidth,3,SystemPalette[2]);
      Bar(0,3,3,Height-6,SystemPalette[2]);
      Bar(Width-3,3,3,Height-6,SystemPalette[2]);
      for i:=3 to fWidth-3-1 do begin
        c:=GetColorByH(i);
        for j:=3 to fHeight-3-1 do
          PutPixel(i,j,ModifyColorByS(c,j));
      end;
      Bar(3+fX+3,3+fY,4,2,0,0,0);
      Bar(3+fX,3+fY+3,2,4,0,0,0);
      Bar(3+fX-5,3+fY,4,2,0,0,0);
      Bar(3+fX,3+fY-5,2,4,0,0,0);
    end;
    fTexture.Update;
  end;
end;

function TBDHSBox.GetColorByH(pX:integer):uint32;
var w:integer;r,g,b:byte;

  function lerp1(value,lo,hi:integer):integer;
  begin
    Result:=(value-lo)*255 div (hi-lo);
  end;

begin
  w:=fWidth-6;
  pX-=3;
  if pX<0 then pX:=0
  else if pX>w-1 then pX:=w-1;
  if (pX>=0) and (pX<w div 6) then begin
    r:=255;
    g:=lerp1(pX,0,w div 6-1);
    b:=0;
  end else
  if (pX>=w div 6) and (pX<w div 3) then begin
    r:=255-lerp1(pX,w div 6,w div 3-1);
    g:=255;
    b:=0;
  end else
  if (pX>=w div 3) and (pX<w div 2) then begin
    r:=0;
    g:=255;
    b:=lerp1(pX,w div 3,w div 2-1);
  end else
  if (pX>=w div 2) and (pX<w*2 div 3) then begin
    r:=0;
    g:=255-lerp1(pX,w div 2,w*2 div 3-1);
    b:=255;
  end else
  if (pX>=w*2 div 3) and (pX<w*5 div 6) then begin
    r:=lerp1(pX,w*2 div 3,w*5 div 6-1);
    g:=0;
    b:=255;
  end;
  if (pX>=w*5 div 6) and (pX<w) then begin
    r:=255;
    g:=0;
    b:=255-lerp1(pX,w*5 div 6,w-1);
  end;
  Result:=$ff000000+(r<<16)+(g<<8)+b;
end;

function TBDHSBox.ModifyColorByS(pColor:uint32; pY:integer):uint32;
var h:integer;r,g,b:byte;

  function lerp2(value,max,start:integer):integer;
  begin
    Result:=start+value*(128-start) div max;
  end;

begin
  h:=fHeight-6;
  pY-=3;
  if pY<0 then py:=0
  else if pY>h-1 then py:=h-1;
  r:=lerp2(pY,h-1,(pColor and $FF0000)>>16);
  g:=lerp2(pY,h-1,(pColor and $FF00)>>8);
  b:=lerp2(pY,h-1,pColor and $FF);
  Result:=pColor and $FF000000+(r<<16)+(g<<8)+b;
end;

{ TBDLightSlider }

constructor TBDLightSlider.Create(iLeft,iTop,iWidth,iHeight:integer);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  Width:=iWidth;
  Height:=iHeight;
  fBaseColor:=$ff0000ff;  // The redest! :)
  fSelectedL:=128;
  fVisible:=true;
  fNeedRedraw:=true;
end;

procedure TBDLightSlider.ReDraw;
begin
  inherited ReDraw;
end;

procedure TBDLightSlider.fSetBaseColor(pValue:uint32);
begin
  if fBaseColor<>pValue then begin
    fBaseColor:=pValue;
    Refresh;
  end;
end;

procedure TBDLightSlider.fSetSelectedL(pValue:byte);
begin
  if fSelectedL<>pValue then begin;
    fSelectedL:=pValue;
    Refresh;
  end;
end;

end.

