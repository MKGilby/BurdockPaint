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

unit BDPColorPalette2;

{$mode Delphi}

interface

uses SysUtils, vcc2_ContainerStatic;

// This is the colorpalette tool at the right side of the window.

type

  { TBDColorPalette2 }

  TBDColorPalette2=class(TContainerStatic)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer);
  protected
    procedure ReDraw; override;
//    procedure fSetWidth(value:integer); override;
    procedure fSetHeight(value:integer); override;
  private
    fEntryHeight:integer;
    fPage:integer;
    procedure Click(Sender:TObject;x,y,button:integer);
    procedure MouseWheel(Sender:TObject;x,y,wheelx,wheely:integer);
    procedure MouseMove(Sender:TObject;x,y:integer);
    procedure MouseLeave(Sender:TObject);
  public
//    property Width:integer read fWidth write fSetWidth;
    property Height:integer read fHeight write fSetHeight;
  end;


implementation

uses BDPShared, MKMouse2, sdl2;

{ TBDColorPalette2 }

constructor TBDColorPalette2.Create(iLeft, iTop, iWidth, iHeight: integer);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  Width:=iWidth;
  Height:=iHeight;
  fNeedRedraw:=true;
  ZIndex:=LEVEL1CONTROLS_ZINDEX+10;
  fName:='ColorPalette2';
  OnClick:=Click;
  OnMouseWheel:=MouseWheel;
  OnMouseMove:=MouseMove;
  MouseObjects.Add(Self);
  fEntryHeight:=(fHeight-(12+22+3)) div 16;
  fPage:=0;
end;

procedure TBDColorPalette2.ReDraw;
var i:integer;
begin
  fImage.Bar(0,0,Width,Height,SystemPalette.Colors[SYSTEMCOLORDARK]);
  fImage.Bar(3,3,Width-6,Height-6,SystemPalette.Colors[SYSTEMCOLORMID]);
  fImage.Bar(6,19,Width-12,fEntryHeight*16+3,SystemPalette.Colors[SYSTEMCOLORDARK]);
  if Assigned(Project.CurrentPalette) then begin
    for i:=0 to 15 do begin
      if Project.CurrentPalette.Colors[fPage*16+i]=Settings.ActiveColor then
        fImage.Bar(6,19+i*fEntryHeight,fWidth-12,fEntryHeight+3,SystemPalette.Colors[SYSTEMCOLORHIGHLIGHT]);
      fImage.Bar(9,19+i*fEntryHeight+3,fWidth-18,fEntryHeight-3,
          Project.CurrentPalette.Colors[fPage*16+i]);
    end;
  end;
  MM.Fonts['SmallBlack'].OutText(fImage,inttostr(fPage+1)+'/16',Width div 2,6,1);
end;

{procedure TBDColorPalette2.fSetWidth(value: integer);
begin
  inherited fSetWidth(value);
end;}

procedure TBDColorPalette2.fSetHeight(value: integer);
begin
  inherited fSetHeight(value);
end;

procedure TBDColorPalette2.Click(Sender: TObject; x, y, button: integer);
begin
  x-=Left;
  y-=Top;
  if button=SDL_BUTTON_LEFT then begin
    if (x>=9) and (x<Width-9) then begin
      if (y>=19) and (y<19+fEntryHeight*16) then begin
        Settings.ActiveColor:=Project.CurrentPalette.Colors[fPage*16+((y-19) div fEntryHeight)];
        Refresh;
      end;
    end;
  end;
end;

procedure TBDColorPalette2.MouseWheel(Sender: TObject; x, y, wheelx, wheely: integer);
var pre:integer;
begin
  // If only the y MouseWheel rolled and CrossWheels enabled, use that.
{  if (wheelx=0) and fCrossWheels then wheely:=wheelx;
  if fInvertWheel then wheely:=-wheely;}
  pre:=fPage;
  fPage-=wheely;
  if fPage<0 then fPage:=0
  else if fPage>15 then fPage:=15;
  if (pre<>fPage) then begin
    MouseMove(Sender,x,y);
    fNeedRedraw:=true;
  end;
end;

procedure TBDColorPalette2.MouseMove(Sender:TObject; x,y:integer);
begin
  x-=Left;
  y-=Top;
  if (x>=9) and (x<Width-9) and (y>=19) and (y<19+fEntryHeight*16) then
    ColorUnderMouse:=Project.CurrentPalette.Colors[fPage*16+((y-19) div fEntryHeight)]
  else
    ColorUnderMouse:=POSTPROCESSCOLOR;
end;

procedure TBDColorPalette2.MouseLeave(Sender:TObject);
begin
  ColorUnderMouse:=POSTPROCESSCOLOR;
end;

end.

