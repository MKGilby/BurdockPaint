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

unit BDPColorPalette;

{$mode Delphi}

interface

uses
  vcc2_VisibleControl, BDPPalette;

type

  { TBDColorPalette }

  TBDColorPalette=class(TVisibleControl)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer);
    procedure ReDraw; override;
  private
    fEntryWidth,fEntryHeight:integer;
    fPalette:TBDPalette;
    procedure fSetPalette(pPalette:TBDPalette);
  public
    property Palette:TBDPalette write fSetPalette;
  end;

implementation

uses BDPShared;

const
  PALETTEHORIZONTALENTRYCOUNT=8;
  PALETTEVERTICALENTRYCOUNT=8;

{ TBDColorPalette }

constructor TBDColorPalette.Create(iLeft, iTop, iWidth, iHeight: integer);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  Width:=iWidth;
  Height:=iHeight;
  fEntryWidth:=(fWidth-3) div PALETTEHORIZONTALENTRYCOUNT;
  fEntryHeight:=(fHeight-3) div PALETTEVERTICALENTRYCOUNT;
  fPalette:=nil;
  fNeedRedraw:=true;
end;

procedure TBDColorPalette.ReDraw;
var i,j:integer;
begin
  if Assigned(fTexture) then begin
    fTexture.ARGBImage.Bar(0,0,Width,Height,SystemPalette.Colors[SYSTEMCOLORDARK]);
    if Assigned(fPalette) then begin
      for j:=0 to PALETTEVERTICALENTRYCOUNT-1 do
        for i:=0 to PALETTEHORIZONTALENTRYCOUNT-1 do begin
          fTexture.ARGBImage.Bar(i*fEntryWidth+3,j*fEntryHeight+3,fEntryWidth-3,fEntryHeight-3,
            fPalette.Colors[j*PALETTEHORIZONTALENTRYCOUNT+i]);
        end;
    end;
    fTexture.Update;
  end;
end;

procedure TBDColorPalette.fSetPalette(pPalette:TBDPalette);
begin
  fPalette:=pPalette;
  if fPalette.Size<(PALETTEHORIZONTALENTRYCOUNT*PALETTEVERTICALENTRYCOUNT) then
    fPalette.Resize(64);
  Refresh;
end;

end.

