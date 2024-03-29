{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPCoordinateBox;

{$mode Delphi}

interface

uses SysUtils, vcc2_VisibleControlStatic;

const
  COORDINATEBOXWIDTH=112;
  COORDINATEBOXHEIGHT=96;

type

  { TBDCoordinateBox }

  TBDCoordinateBox=class(TVisibleControlStatic)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer);
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

uses BDPShared, MKMouse2, ARGBImageUnit, mk_sdl2;

{ TBDCoordinateBox }

constructor TBDCoordinateBox.Create(iLeft,iTop,iWidth,iHeight:integer);
begin
  fLeft:=iLeft;
  fTop:=iTop;
  if iWidth<COORDINATEBOXWIDTH then iWidth:=COORDINATEBOXWIDTH{*18};  // ???
  if iHeight<COORDINATEBOXHEIGHT then iHeight:=COORDINATEBOXHEIGHT;
  Width:=iWidth;
  Height:=iHeight;
  fNeedRedraw:=true;
  ZIndex:=LEVEL1CONTROLS_ZINDEX+10;
  MouseObjects.Add(Self);
  Visible:=true;
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
      c:=ColorUnderMouse;
      if c=POSTPROCESSCOLOR then begin
        MM.Fonts['SmallDarkRed'].OutText('--',fLeft+fCenterA,fTop+77,1);
        MM.Fonts['SmallDarkRed'].OutText('--',fLeft+fCenterR,fTop+77,1);
        MM.Fonts['SmallDarkRed'].OutText('--',fLeft+fCenterG,fTop+77,1);
        MM.Fonts['SmallDarkRed'].OutText('--',fLeft+fCenterB,fTop+77,1);
      end else begin
        MM.Fonts['SmallBlack'].OutText(HexStr((c and $ff000000)>>24,2),fLeft+fCenterA,fTop+77,1);
        MM.Fonts['SmallBlack'].OutText(HexStr((c and $ff0000)>>16,2),fLeft+fCenterR,fTop+77,1);
        MM.Fonts['SmallBlack'].OutText(HexStr((c and $ff00)>>8,2),fLeft+fCenterG,fTop+77,1);
        MM.Fonts['SmallBlack'].OutText(HexStr(c and $ff,2),fLeft+fCenterB,fTop+77,1);
      end;
    end;
  end;
end;

procedure TBDCoordinateBox.ReDraw;
begin
  with fImage do begin
    Bar(0,0,Width,Height,SystemPalette.Colors[SYSTEMCOLORDARK]);
    Bar(fLeftA,67,fWidthA,32,128,128,128);
    Bar(fLeftR,67,fWidthR,32,128,64,64);
    Bar(fLeftG,67,fWidthG,32,64,128,64);
    Bar(fLeftB,67,fWidthB,32,64,64,128);
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

