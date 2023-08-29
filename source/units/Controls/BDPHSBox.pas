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

unit BDPHSBox;

{$mode Delphi}

interface

uses SysUtils, vcc2_VisibleControl;

type

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

implementation

uses BDPShared;

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

end.

