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

unit BDPInkSoften;

{$mode Delphi}

interface

uses BDPInkBase, BDPRegion;

type

  { TBDInkSoften }

  TBDInkSoften=class(TBDInk)
    constructor Create; override;
    procedure InitializeAreaWH(pLeft,pTop,pWidth,pHeight:integer); override;
    procedure InitializeArea(pX1,pY1,pX2,pY2:integer); override;
    function GetColorAt(pX,pY:integer):uint32; override;
    procedure PostProcess; override;
  private
    fTempImage:TBDRegion;
  end;

implementation

uses BDPShared;

{ TBDInkSoften }

constructor TBDInkSoften.Create;
begin
  inherited Create;
  fName:='SOFTEN';
  fHint:='AVERAGES PIXEL COLORS WITH NEIGHBOURS.';
  fSupportsOnTheFly:=false;
end;

procedure TBDInkSoften.InitializeAreaWH(pLeft,pTop,pWidth,pHeight:integer);
begin
  inherited ;
  fTempImage:=TBDRegion.Create(fWidth,fHeight);
  fTempImage.PutImagePart(0,0,fLeft,fTop,fWidth,fHeight,Project.CurrentImage);
end;

procedure TBDInkSoften.InitializeArea(pX1,pY1,pX2,pY2:integer);
begin
  inherited ;
  fTempImage:=TBDRegion.Create(fWidth,fHeight);
  fTempImage.PutImagePart(0,0,fLeft,fTop,fWidth,fHeight,Project.CurrentImage);
end;

function TBDInkSoften.GetColorAt(pX,pY:integer):uint32;
var r,g,b,c:integer;p:uint32;
begin
  pX-=fLeft;pY-=fTop;
  c:=1;
  p:=fTempImage.GetPixel(px,py);
  r:=(p and $FF0000)>>16;
  g:=(p and $FF00)>>8;
  b:=(p and $FF);
  if px>0 then begin
    p:=fTempImage.GetPixel(px-1,py);
    r+=(p and $FF0000)>>16;
    g+=(p and $FF00)>>8;
    b+=(p and $FF);
    inc(c);
  end;
  if px<fTempImage.Width-1 then begin
    p:=fTempImage.GetPixel(px+1,py);
    r+=(p and $FF0000)>>16;
    g+=(p and $FF00)>>8;
    b+=(p and $FF);
    inc(c);
  end;
  if py>0 then begin
    p:=fTempImage.GetPixel(px,py-1);
    r+=(p and $FF0000)>>16;
    g+=(p and $FF00)>>8;
    b+=(p and $FF);
    inc(c);
  end;
  if py<fTempImage.Height-1 then begin
    p:=fTempImage.GetPixel(px,py+1);
    r+=(p and $FF0000)>>16;
    g+=(p and $FF00)>>8;
    b+=(p and $FF);
    inc(c);
  end;
  Result:=($FF000000)+
          (((r div c) and $ff)<<16)+
          (((g div c) and $ff)<<8)+
          (((b div c) and $ff));
end;

procedure TBDInkSoften.PostProcess;
var i,j:integer;
begin
  for j:=fTop to fTop+fHeight-1 do
    for i:=fLeft to fLeft+fWidth-1 do
      if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
        Project.CurrentImage.PutPixel(i,j,GetColorAt(i,j));
  if Assigned(fTempImage) then fTempImage.Free;
end;

end.

