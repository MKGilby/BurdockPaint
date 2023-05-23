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

uses vcc2_VisibleControl;

type

  { TBDColorSelector }

  TBDColorSelector=class(TVisibleControl)
    constructor Create(iLeft,iTop:integer);
    procedure Draw; override;
    procedure Click(Sender:TObject;x, y, buttons: integer);
    function KeyDown(Sender:TObject;key:integer):boolean;
    procedure SetSelectedSlotTo(ColorIndex:integer);
  private
    fSelectedIndex:integer;
    fPickingColor:boolean;
    function GetClickedIndex(x:integer):integer;
  end;

implementation

uses BDPShared, BDPSettings, sdl2, mk_sdl2, BDPKeyMapping;

{ TBDColorSelector }

constructor TBDColorSelector.Create(iLeft,iTop:integer);
var i:integer;
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  Width:=COLORSELECTORCOLORS*(COLORSELECTORBOXSIZE-3)+COLORSELECTORGAP+3;
  Height:=COLORSELECTORBOXSIZE;
  fSelectedIndex:=0;
  for i:=0 to COLORSELECTORCOLORS-1 do
    if Settings.SelectedColors[i]=Settings.ActiveColorIndex then fSelectedIndex:=i;
  OnClick:=Self.Click;
  OnKeyDown:=Self.KeyDown;
  fTexture.ARGBImage.Bar(0,0,fTexture.Width,fTexture.Height,SystemPalette[3]);
  fPickingColor:=false;
end;

procedure TBDColorSelector.Draw;
var i,x:integer;
begin
  // Draw dark background for all slots
  fTexture.ARGBImage.Bar(0,0,COLORSELECTORBOXSIZE,COLORSELECTORBOXSIZE,SystemPalette[2]);
  fTexture.ARGBImage.Bar(
    COLORSELECTORGAP+COLORSELECTORBOXSIZE-3,
    0,
    (COLORSELECTORBOXSIZE-3)*(COLORSELECTORCOLORS-1)+3,
    COLORSELECTORBOXSIZE,
    SystemPalette[2]);
  // Draw highlights and color boxes
  x:=0;
  for i:=0 to COLORSELECTORCOLORS-1 do begin
    if not fPickingColor then begin
      if Settings.SelectedColors[i]=Settings.ActiveColorIndex then begin
        fTexture.ARGBImage.Bar(x,0,COLORSELECTORBOXSIZE,COLORSELECTORBOXSIZE,SystemPalette[5])
      end
    end else begin
      if i=fSelectedIndex then
        fTexture.ARGBImage.Bar(x,0,COLORSELECTORBOXSIZE,COLORSELECTORBOXSIZE,SystemPalette[VibroColors.GetColorIndex]);
    end;
    fTexture.ARGBImage.Bar(x+3,3,COLORSELECTORBOXSIZE-6,COLORSELECTORBOXSIZE-6,Project.CurrentImage.Palette[Settings.SelectedColors[i]]);
    x+=COLORSELECTORBOXSIZE-3;
    if i=0 then x+=COLORSELECTORGAP;
  end;
  fTexture.Update;
  PutTexture(fLeft,fTop,fTexture);
end;

procedure TBDColorSelector.Click(Sender:TObject; x,y,buttons:integer);
var i:integer;
begin
  if buttons=SDL_BUTTON_LEFT then begin
    i:=GetClickedIndex(x);
    if i>-1 then begin
      fSelectedIndex:=i;
      Settings.ActiveColorIndex:=Settings.SelectedColors[i];
    end;
  end else if buttons=SDL_BUTTON_MIDDLE then begin
    MessageQueue.AddMessage(MSG_ACTIVATEPALETTEEDITOR);
  end else if buttons=SDL_BUTTON_RIGHT then begin
    i:=GetClickedIndex(x);
    if i>-1 then begin
      fSelectedIndex:=i;
      Settings.ActiveColorIndex:=Settings.SelectedColors[i];
      ActiveTool:=Tools.ItemByName['PICKCOLCS'];
      fPickingColor:=true;
    end;
  end;
end;

function TBDColorSelector.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=false;
  if (key=KeyMap[KEY_GETCOLOR]) then begin
    if not fPickingColor then begin
      MessageQueue.AddMessage(MSG_SELECTCOLOR);
    end;
    Result:=true;
  end;
end;

procedure TBDColorSelector.SetSelectedSlotTo(ColorIndex:integer);
begin
  if (fSelectedIndex>=0) and (fSelectedIndex<COLORSELECTORCOLORS) then begin
    if (ColorIndex>=0) and (ColorIndex<Project.CurrentImage.Palette.Size) then begin
      Settings.SelectedColors[fSelectedIndex]:=ColorIndex;
      Settings.ActiveColorIndex:=ColorIndex;
    end;
  end;
  fPickingColor:=false;
end;

function TBDColorSelector.GetClickedIndex(x:integer):integer;
var cx,i:integer;
begin
  Result:=-1;
  cx:=0;
  x-=Left;
  for i:=0 to COLORSELECTORCOLORS-1 do begin
    if (cx+3<=x) and (cx+COLORSELECTORBOXSIZE-3>x) then begin
      Result:=i;
      break;
    end;
    cx+=COLORSELECTORBOXSIZE-3;
    if i=0 then cx+=COLORSELECTORGAP;
  end;
end;



end.

