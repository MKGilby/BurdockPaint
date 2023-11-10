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

unit BDPSimpleGradient;

{$mode Delphi}

interface

uses vcc2_VisibleControl, BDPGradient;

type

  { TBDSimpleGradient }

  TBDSimpleGradient=class(TVisibleControl)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer;iGradient:TGradient);
  protected
    procedure ReDraw; override;
  private
    fGradient:TGradient;
    fColorsWidth:integer;
    function fGetColorsWidth:integer;
    procedure fSetGradient(value:TGradient);
    procedure fSetSelected(value:boolean);
  public
    property ColorsWidth:integer read fGetColorsWidth;
    property Gradient:TGradient write fSetGradient;
    property Selected:boolean read fSelected write fSetSelected;
  end;


implementation

uses BDPShared;

{ TBDSimpleGradient }

constructor TBDSimpleGradient.Create(iLeft,iTop,iWidth,iHeight:integer;iGradient:TGradient);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  fGradient:=iGradient;
  Width:=iWidth;
  Height:=iHeight;
  fNeedRedraw:=true;
  Selected:=false;
//  OnClick:=Click;
end;

procedure TBDSimpleGradient.ReDraw;
var i:integer;c:uint32;
begin
  if Assigned(fTexture) then begin
    if Assigned(fGradient) then begin
      fColorsWidth:=Width-6;
      with fTexture.ARGBImage do begin
        if not Selected then
          c:=SystemPalette[SYSTEMCOLORDARK]
        else
          c:=SystemPalette[SYSTEMCOLORHIGHLIGHT];
        // Outer border
        Bar(0,0,Width,3,c);
        Bar(0,Height-3,Width,3,c);
        Bar(0,3,3,Height-6,c);
        Bar(Width-3,3,3,Height-6,c);
        // Gradient bar
        for i:=0 to fColorsWidth-1 do
          VLine(i+3,3,Height-6,fGradient.GetColorAt(i/(fColorsWidth-1)));
      end;
    end else
      fTexture.ARGBImage.Bar(0,0,Width,Height,0);
    fTexture.Update;
  end;
end;

function TBDSimpleGradient.fGetColorsWidth: integer;
begin
  Result:=fColorsWidth+6;
end;

procedure TBDSimpleGradient.fSetGradient(value: TGradient);
begin
  if value<>fGradient then begin
    fGradient:=value;
    Refresh;
  end;
end;

procedure TBDSimpleGradient.fSetSelected(value:boolean);
begin
  if (value<>fSelected) then begin
    fSelected:=value;
    Refresh;
  end;
end;

end.

