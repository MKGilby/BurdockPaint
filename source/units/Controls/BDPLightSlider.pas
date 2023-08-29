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

uses vcc2_VisibleControl;

type

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

