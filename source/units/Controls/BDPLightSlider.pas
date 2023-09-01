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

unit BDPLightSlider;

{$mode Delphi}

interface

uses vcc2_VisibleControl, MKMouse2;

type

  { TBDLightSlider }

  TBDLightSlider=class(TVisibleControl)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer);
  protected
    procedure ReDraw; override;
  private
    fBaseColor:uint32;
    fX:integer;  // position of the small arrow
    fIsMouseDown:boolean;
    procedure MouseDown(Sender:TObject;x,y,button:integer);
    procedure MouseMove(Sender:TObject;x,y:integer);
    procedure MouseUp(Sender:TObject;x,y,button:integer);
    procedure MouseLeave(Sender:TObject);
    function GetColorByL(value:integer):uint32;
    procedure fSetBaseColor(pValue:uint32);
    function fGetL:byte;
    procedure fSetL(pValue:byte);
    function GetColor:uint32;
  public
    OnChange:TSimpleEvent;
    property BaseColor:uint32 write fSetBaseColor;
    property L:byte read fGetL write fSetL;
    property Color:uint32 read GetColor;
  end;

implementation

uses BDPShared;

{ TBDLightSlider }

constructor TBDLightSlider.Create(iLeft,iTop,iWidth,iHeight:integer);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  Width:=iWidth;
  Height:=iHeight;
  fBaseColor:=$ffff0000;  // The redest! :)
  fX:=Width div 2;
  fVisible:=true;
  fNeedRedraw:=true;
  OnMouseDown:=MouseDown;
  OnMouseUp:=MouseUp;
  OnMouseMove:=MouseMove;
  OnMouseLeave:=MouseLeave;
  fIsMouseDown:=false;
end;

procedure TBDLightSlider.ReDraw;
var i:integer;c:uint32;
begin
  if Assigned(fTexture) then begin
    with fTexture.ARGBImage do begin
      Bar(0,0,Width,3,SystemPalette[3]);  // The arrow area
      HLine(fX-2,0,5,SystemPalette[1]);  // The arrow
      HLine(fX-1,1,3,SystemPalette[1]);
      PutPixel(fX,2,SystemPalette[1]);
      Bar(0,3,Width,3,SystemPalette[2]);  // Border
      Bar(0,Height-3,fWidth,3,SystemPalette[2]);
      Bar(0,6,3,Height-9,SystemPalette[2]);
      Bar(Width-3,6,3,Height-9,SystemPalette[2]);
      // Color area
      for i:=3 to fWidth-3-1 do begin
        c:=GetColorByL((i-3)*256 div (fWidth-6));
        VLine(i,6,Height-9,c);
      end;
    end;
    fTexture.Update;
  end;
end;

procedure TBDLightSlider.MouseDown(Sender:TObject; x,y,button:integer);
begin
  x-=fLeft;
  if x<3 then x:=3
  else if x>=fWidth-3 then x:=fWidth-4;
  fX:=x;
  fIsMouseDown:=true;
  Refresh;
  if Assigned(OnChange) then OnChange(Sender);
end;

procedure TBDLightSlider.MouseMove(Sender:TObject; x,y:integer);
begin
  if fIsMouseDown then begin
    x-=fLeft;
    if x<3 then x:=3
    else if x>=fWidth-3 then x:=fWidth-4;
    fX:=x;
    Refresh;
    if Assigned(OnChange) then OnChange(Sender);
  end;
end;

procedure TBDLightSlider.MouseUp(Sender:TObject; x,y,button:integer);
begin
  fIsMouseDown:=false;
end;

procedure TBDLightSlider.MouseLeave(Sender:TObject);
begin
  fIsMouseDown:=false;
end;

function TBDLightSlider.GetColorByL(value:integer):uint32;
var r,g,b:byte;
begin
  Result:=(fBaseColor and $ff000000);
  r:=(fBaseColor and $ff0000)>>16;
  g:=(fBaseColor and $ff00)>>8;
  b:=fBaseColor and $ff;
  if value<=128 then begin
    Result+=
      ((r*value div 128) and $ff)<<16+
      ((g*value div 128) and $ff)<<8+
      ((b*value div 128) and $ff);
  end else begin
    value:=value-128;
    Result+=
      ((r+(255-r)*value div 127) and $ff)<<16+
      ((g+(255-g)*value div 127) and $ff)<<8+
      ((b+(255-b)*value div 127) and $ff);
  end;
end;

procedure TBDLightSlider.fSetBaseColor(pValue:uint32);
begin
  if fBaseColor<>pValue then begin
    fBaseColor:=pValue;
    Refresh;
  end;
end;

function TBDLightSlider.fGetL:byte;
begin
  Result:=(fX-3)*255 div (fWidth-6);
end;

procedure TBDLightSlider.fSetL(pValue:byte);
begin
  pValue:=3+pValue*(fWidth-6) div 255;
  if fX<>pValue then begin;
    fX:=pValue;
    Refresh;
  end;
end;

function TBDLightSlider.GetColor:uint32;
begin
  Result:=GetColorByL((fX-3)*256 div (fWidth-6));
end;

end.

