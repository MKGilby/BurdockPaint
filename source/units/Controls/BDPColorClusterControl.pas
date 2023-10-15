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

unit BDPColorClusterControl;

{$mode Delphi}

interface

uses vcc2_VisibleControl, BDPGradient, ARGBImageUnit, Font2Unit;

type

  { TBDColorCluster }

  TBDColorCluster=class(TVisibleControl)
    constructor Create(iLeft,iTop:integer;iColorCluster:TGradient;iRMBPicks:boolean);
    procedure Refresh; override;
    procedure Click(Sender:TObject;x,y,button:integer);
    procedure Draw; override;
  protected
    procedure ReDraw; override;
    procedure fSetWidth(value:integer); override;
  private
    fTLImage,fTRImage,fBLImage,fBRImage:TARGBImage;
    fColorCluster:TGradient;
    fFont,fFont2:TFont;
    fPingpongSwitchLeft,fReverseSwitchLeft,
    fColorsLeft,fColorsWidth,fArrowLeft:integer;
    fRMBPicks:boolean;
    fPicking:boolean;
    procedure fSetColorCluster(value:TGradient);
  public
    property ColorCluster:TGradient read fColorCluster write fSetColorCluster;
    property Width:integer read fWidth write fSetWidth;
  end;

implementation

uses BDPShared, sdl2;

const
  PINGPONGSWITCHWIDTH=27;
  REVERSESWITCHWIDTH=27;
  ARROWWIDTH=30;

{ TBDColorCluster }

constructor TBDColorCluster.Create(iLeft,iTop:integer;iColorCluster:TGradient;
  iRMBPicks:boolean);
begin
  fLeft:=iLeft;
  fTop:=iTop;
  fColorCluster:=iColorCluster;
  Width:=COLORCLUSTERWIDTH;
  Height:=COLORCLUSTERHEIGHT;
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
  OnClick:=Click;
  fRMBPicks:=iRMBPicks;
  fPicking:=false;
end;

procedure TBDColorCluster.Refresh;
begin
  inherited Refresh;
  fPicking:=false;
end;

procedure TBDColorCluster.Click(Sender:TObject; x,y,button:integer);
begin
  if Assigned(fColorCluster) then begin
    x-=Left;
    y-=Top;
    if (x>=fPingpongSwitchLeft) and (x<fReverseSwitchLeft) then begin
      fColorCluster.PingPong:=not fColorCluster.PingPong;
      fNeedRedraw:=true;
    end else
    if (x>=fReverseSwitchLeft) and (x<fColorsLeft) then begin
      fColorCluster.Reversed:=not fColorCluster.Reversed;
      fNeedRedraw:=true;
    end else
    if (x>=fColorsLeft+3) and (x<fArrowLeft) then begin
      if button=SDL_BUTTON_LEFT then
        Settings.ActiveColor:=fColorCluster.GetColorAt(x-fColorsLeft-3,fColorsWidth-3)
      else if (button=SDL_BUTTON_RIGHT) then begin
        if fRMBPicks then begin
          fPicking:=true;
//          MessageQueue.AddMessage(MSG_ACTIVATEPICKCOLORCLUSTER);
        end else begin
          MessageQueue.AddMessage(MSG_ACTIVATECOLORCLUSTEREDITOR);
        end;
      end;
    end else
    if (x>=fArrowLeft) then begin
      MessageQueue.AddMessage(MSG_OPENCOLORCLUSTERDIALOG,fLeft+(fTop<<11)+(fWidth<<22));
    end;
  end;
end;

procedure TBDColorCluster.Draw;
begin
  if fPicking then fNeedRedraw:=true;
  inherited Draw;
end;

procedure TBDColorCluster.ReDraw;
var i,fonttop:integer;color32:uint32;
begin
  if Assigned(fTexture) then begin
    with fTexture.ARGBImage do begin
      // Background
      Bar(0,0,fPingpongSwitchLeft,Height,SystemPalette[SYSTEMCOLORMID]);
      if Assigned(fColorCluster) then begin
        if fColorCluster.PingPong then
          Bar(fPingpongSwitchLeft+3,3,PINGPONGSWITCHWIDTH,Height-6,SystemPalette[SYSTEMCOLORLIGHT])
        else
          Bar(fPingpongSwitchLeft+3,3,PINGPONGSWITCHWIDTH,Height-6,SystemPalette[SYSTEMCOLORMID]);
        if fColorCluster.Reversed then
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
      if fPicking then
        Bar(fColorsLeft,0,fColorsWidth+3,Height,VibroColors.GetColor);
      // Color cluster bar
      if Assigned(fColorCluster) then begin
        for i:=0 to fColorsWidth-1-3 do begin
          color32:=fColorCluster.GetColorAt(i,fColorsWidth-3);
          VLine(fColorsLeft+i+3,3,Height-6,color32);
          if color32=Settings.ActiveColor then begin
            VLine(fColorsLeft+i+3,Height div 2-3,3,SystemPalette[SYSTEMCOLORLIGHT]);
            VLine(fColorsLeft+i+3,Height div 2,3,SystemPalette[SYSTEMCOLORBLACK]);
          end;
        end;
      end;
      // Letters and arrow
      if Assigned(fFont) and Assigned(fFont2) then begin
        fonttop:=(Height-15) div 2;
        if (Assigned(fColorCluster) and fColorCluster.PingPong) then
          fFont2.OutText(fTexture.ARGBImage,'P',fPingpongSwitchLeft+PINGPONGSWITCHWIDTH div 2+3,fonttop,1)
        else
          fFont.OutText(fTexture.ARGBImage,'P',fPingpongSwitchLeft+PINGPONGSWITCHWIDTH div 2+3,fonttop,1);
        if (Assigned(fColorCluster) and fColorCluster.Reversed) then
          fFont2.OutText(fTexture.ARGBImage,'R',fReverseSwitchLeft+REVERSESWITCHWIDTH div 2+3,fonttop,1)
        else
          fFont.OutText(fTexture.ARGBImage,'R',fReverseSwitchLeft+REVERSESWITCHWIDTH div 2+3,fonttop,1);
        fFont.OutText(fTexture.ARGBImage,#130,fArrowLeft+ARROWWIDTH div 2+1,fonttop,1);
      end;
    end;
    fTexture.Update;
  end;
end;

procedure TBDColorCluster.fSetColorCluster(value:TGradient);
begin
  fColorCluster:=value;
  fNeedRedraw:=true;
end;

procedure TBDColorCluster.fSetWidth(value:integer);
begin
  inherited fSetWidth(value);
  fArrowLeft:=Width-ARROWWIDTH-3;
  fColorsWidth:=fArrowLeft-fColorsLeft;
end;

end.

