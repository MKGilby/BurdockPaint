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

unit BDPColorSelector;

{$mode Delphi}

interface

uses vcc2_VisibleControl, BDPColorCluster;

type

  { TBDColorSelector }

  TBDColorSelector=class(TVisibleControl)
    constructor Create(iLeft,iTop,fWidth,fHeight:integer);
    destructor Destroy; override;
    procedure Click(Sender:TObject;x, y, buttons: integer);
    function KeyDown(Sender:TObject;key:integer):boolean;
  protected
    procedure ReDraw; override;
    procedure fSetWidth(value:integer); override;
    procedure fSetHeight(value:integer); override;
  private
    fLeftClusterLeft,fLeftClusterWidth:integer;
    fMainColorLeft:integer;
    fRightClusterLeft,fRightClusterWidth:integer;
    fRightColorLeft:integer;
    fLeftColorCluster,fRightColorCluster:TColorCluster;
    procedure RecalculatePositions;
  public
    property Width:integer read fWidth write fSetWidth;
    property Height:integer read fHeight write fSetHeight;
  end;

implementation

uses BDPShared, BDPSettings, sdl2, mk_sdl2, BDPKeyMapping;

{ TBDColorSelector }

constructor TBDColorSelector.Create(iLeft,iTop,fWidth,fHeight:integer);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  Width:=fWidth;
  Height:=fHeight;
  OnClick:=Self.Click;
  OnKeyDown:=Self.KeyDown;
  fLeftColorCluster:=TColorCluster.Create(
    Settings.ColorSelectorLeftColor,
    Settings.ColorSelectorMainColor);
  fRightColorCluster:=TColorCluster.Create(
    Settings.ColorSelectorMainColor,
    Settings.ColorSelectorRightColor);
end;

destructor TBDColorSelector.Destroy;
begin
  if Assigned(fLeftColorCluster) then fLeftColorCluster.Free;
  if Assigned(fRightColorCluster) then fRightColorCluster.Free;
  inherited Destroy;
end;

procedure TBDColorSelector.Click(Sender:TObject; x,y,buttons:integer);
begin
  if buttons=SDL_BUTTON_LEFT then begin
  end else if buttons=SDL_BUTTON_MIDDLE then begin
  end else if buttons=SDL_BUTTON_RIGHT then begin
  end;
end;

function TBDColorSelector.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=false;
  if (key=KeyMap[KEY_GETCOLOR]) then begin
    MessageQueue.AddMessage(MSG_SELECTCOLOR);
    Result:=true;
  end;
end;

procedure TBDColorSelector.ReDraw;
var i:integer;
begin
  with fTexture.ARGBImage do begin
    Bar(0,0,fTexture.Width,fTexture.Height,SystemPalette[2]);
    Bar(3,3,Height-6,Height-6,Settings.ColorSelectorLeftColor);
    Bar(fMainColorLeft+3,3,Height-6,Height-6,Settings.ColorSelectorMainColor);
    Bar(fRightColorLeft+3,3,Height-6,Height-6,Settings.ColorSelectorRightColor);
    for i:=0 to fLeftClusterWidth-1 do
      VLine(fLeftClusterLeft+i,3,Height-6,fLeftColorCluster.GetColorAt(i,fLeftClusterWidth));
    for i:=0 to fRightClusterWidth-1 do
      VLine(fRightClusterLeft+i,3,Height-6,fRightColorCluster.GetColorAt(i,fRightClusterWidth));
  end;
  fTexture.Update;
end;

procedure TBDColorSelector.RecalculatePositions;
begin
  fLeftClusterLeft:=Height;
  fLeftClusterWidth:=(Width-3*Height) div 2;
  fMainColorLeft:=fLeftClusterLeft+fLeftClusterWidth;
  fRightClusterLeft:=fMainColorLeft+Height;
  fRightClusterWidth:=(Width-3*Height)-fLeftClusterWidth;
  fRightColorLeft:=fRightClusterLeft+fRightClusterWidth;
end;

procedure TBDColorSelector.fSetWidth(value:integer);
begin
  inherited fSetWidth(value);
  RecalculatePositions;
end;

procedure TBDColorSelector.fSetHeight(value:integer);
begin
  inherited fSetHeight(value);
  RecalculatePositions;
end;

end.

