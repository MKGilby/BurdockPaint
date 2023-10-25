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

unit BDPCoordinateBox;

{$mode Delphi}

interface

uses SysUtils, vcc2_VisibleControl;

const
  COORDINATEBOXWIDTH=112;
  COORDINATEBOXHEIGHT=96;

type

  { TBDCoordinateBox }

  TBDCoordinateBox=class(TVisibleControl)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer);
    destructor Destroy; override;
    procedure Draw; override;
  protected
    procedure ReDraw; override;
    procedure fSetWidth(value:integer); override;
  private
    fCenterX:integer;
    fCenterA,fCenterR,fCenterG,fCenterB:integer;
    fLeftA,fLeftR,fLeftG,fLeftB:integer;
    fWidthA,fWidthR,fWidthG,fWidthB:integer;
  public
    property Width:integer read fWidth write fSetWidth;
  end;



implementation

uses BDPShared, MKMouse2;

{ TBDCoordinateBox }

constructor TBDCoordinateBox.Create(iLeft,iTop,iWidth,iHeight:integer);
begin
  fLeft:=iLeft;
  fTop:=iTop;
  if iWidth<COORDINATEBOXWIDTH then iWidth:=COORDINATEBOXWIDTH*18;
  if iHeight<COORDINATEBOXHEIGHT then iHeight:=COORDINATEBOXHEIGHT;
  Width:=iWidth;
  Height:=iHeight;
  fNeedRedraw:=true;
  ZIndex:=LEVEL1CONTROLS_ZINDEX+10;
  MouseObjects.Add(Self);
  Visible:=true;
end;

destructor TBDCoordinateBox.Destroy;
begin
  if MouseObjects.IndexOf(Self)>-1 then MouseObjects.Remove(Self);
  inherited Destroy;
end;

procedure TBDCoordinateBox.Draw;
var c:uint32;
begin
  inherited Draw;
  if Visible then begin
    if (DrawAreaX>=0) and (DrawAreaY>=0) then begin
      MM.Fonts['Black'].OutText('X='+inttostr(DrawAreaX),fLeft+fCenterX,fTop+12,1);
      MM.Fonts['Black'].OutText('Y='+inttostr(DrawAreaY),fLeft+fCenterX,fTop+42,1);
      c:=Project.CurrentRegion.GetPixel(DrawAreaX,DrawAreaY);
      MM.Fonts['SmallBlack'].OutText(HexStr((c and $ff000000)>>24,2),fLeft+fCenterA,fTop+77,1);
      MM.Fonts['SmallBlack'].OutText(HexStr((c and $ff0000)>>16,2),fLeft+fCenterR,fTop+77,1);
      MM.Fonts['SmallBlack'].OutText(HexStr((c and $ff00)>>8,2),fLeft+fCenterG,fTop+77,1);
      MM.Fonts['SmallBlack'].OutText(HexStr(c and $ff,2),fLeft+fCenterB,fTop+77,1);
    end else begin
      if (DrawAreaX>=0) then
        MM.Fonts['DarkRed'].OutText('X='+inttostr(DrawAreaX),fLeft+fCenterX,fTop+12,1)
      else
        MM.Fonts['DarkRed'].OutText('X=OUT',fLeft+fCenterX,fTop+12,1);
      if (DrawAreaY>=0) then
        MM.Fonts['DarkRed'].OutText('Y='+inttostr(DrawAreaY),fLeft+fCenterX,fTop+42,1)
      else
        MM.Fonts['DarkRed'].OutText('Y=OUT',fLeft+fCenterX,fTop+42,1);
      MM.Fonts['SmallDarkRed'].OutText('--',fLeft+fCenterA,fTop+77,1);
      MM.Fonts['SmallDarkRed'].OutText('--',fLeft+fCenterR,fTop+77,1);
      MM.Fonts['SmallDarkRed'].OutText('--',fLeft+fCenterG,fTop+77,1);
      MM.Fonts['SmallDarkRed'].OutText('--',fLeft+fCenterB,fTop+77,1);
    end;
  end;
end;

procedure TBDCoordinateBox.ReDraw;
begin
  if Assigned(fTexture) then begin
    with fTexture.ARGBImage do begin
      Bar(0,0,Width,Height,SystemPalette.Colors[SYSTEMCOLORDARK]);
      Bar(fLeftA,67,fWidthA,32,128,128,128);
      Bar(fLeftR,67,fWidthR,32,128,64,64);
      Bar(fLeftG,67,fWidthG,32,64,128,64);
      Bar(fLeftB,67,fWidthB,32,64,64,128);
    end;
    fTexture.Update;
  end;
end;

procedure TBDCoordinateBox.fSetWidth(value:integer);
begin
  inherited fSetWidth(value);
  fCenterX:=Width div 2;
  fLeftA:=0;
  fLeftR:=Width div 4;
  fLeftG:=Width div 2;
  fLeftB:=Width*3 div 4;
  fWidthA:=fLeftR-fLeftA;
  fWidthR:=fLeftG-fLeftR;
  fWidthG:=fLeftB-fLeftG;
  fWidthB:=Width-fLeftB;
  fCenterA:=fLeftA+fWidthA div 2;
  fCenterR:=fLeftR+fWidthR div 2;
  fCenterG:=fLeftG+fWidthG div 2;
  fCenterB:=fLeftB+fWidthB div 2;
end;

end.

