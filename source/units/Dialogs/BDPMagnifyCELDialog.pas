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

unit BDPMagnifyCELDialog;

{$mode Delphi}

interface

uses
  SysUtils, BDPButton, BDPModalDialog;

type

  { TBDMagnifyCELDialog }

  TBDMagnifyCELDialog=class(TBDModalDialog)
    constructor Create;
    procedure ReDraw; override;
    function KeyDown(Sender:TObject;key:integer):boolean;
    procedure MagnifyButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure OKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure CancelButtonClick(Sender:TObject;x,y,buttons:integer);
  private
    fMagnifyButtons:array[0..2] of TBDButton;
  end;

implementation

uses BDPShared, MKMouse2, SDL2;

const
  MAGNIFYDIALOGWIDTH=480;
  MAGNIFYDIALOGHEIGHT=128;
  XBUTTONWIDTH=63;

{ TBDMagnifyCELDialog }

constructor TBDMagnifyCELDialog.Create;
const MAGNIFIES:array[0..2] of integer=(2,3,5);
var atmb:TBDButton;i:integer;
begin
  inherited Create(MAGNIFYDIALOGWIDTH,MAGNIFYDIALOGHEIGHT);
  Refresh;
  for i:=0 to 2 do begin
    fMagnifyButtons[i]:=TBDButton.Create(
      fLeft+(MAGNIFYDIALOGWIDTH div 4)*(i+1)-XBUTTONWIDTH div 2,
      fTop+48,
      XBUTTONWIDTH, NORMALBUTTONHEIGHT,
      inttostr(MAGNIFIES[i])+'X','');
    fMagnifyButtons[i].ZIndex:=MODALDIALOG_ZINDEX+1;
    fMagnifyButtons[i].Tag:=i;
    fMagnifyButtons[i].OnClick:=MagnifyButtonClick;
    if i=0 then fMagnifyButtons[i].Selected:=true;
    AddChild(fMagnifyButtons[i]);
  end;
  atmB:=TBDButton.Create(
    fLeft+MAGNIFYDIALOGWIDTH div 3-NORMALBUTTONWIDTH div 2,
    fTop+84,
    NORMALBUTTONWIDTH, NORMALBUTTONHEIGHT,
    'OK','MAGNIFY CEL');
  atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
  atmB.OnClick:=OKButtonClick;
  AddChild(atmB);
  atmB:=TBDButton.Create(
    fLeft+(MAGNIFYDIALOGWIDTH div 3*2)-NORMALBUTTONWIDTH div 2,
    fTop+84,
    NORMALBUTTONWIDTH, NORMALBUTTONHEIGHT,
    'CANCEL','DON''T MAGNIFY CEL');
  atmB.OnClick:=CancelButtonClick;
  atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);
  OnKeyDown:=KeyDown;
  MouseObjects.Add(Self);
end;

procedure TBDMagnifyCELDialog.ReDraw;
begin
  if Assigned(fTexture) then begin
    fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,3,SystemPalette[SYSTEMCOLORDARK]);
    fTexture.ARGBImage.Bar(0,0,3,fTexture.ARGBImage.Height,SystemPalette[SYSTEMCOLORDARK]);
    fTexture.ARGBImage.Bar(fTexture.ARGBImage.Width-3,0,3,fTexture.ARGBImage.Height,SystemPalette[SYSTEMCOLORDARK]);
    fTexture.ARGBImage.Bar(0,fTexture.ARGBImage.Height-3,fTexture.ARGBImage.Width,3,SystemPalette[SYSTEMCOLORDARK]);
    fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,SystemPalette[SYSTEMCOLORMID]);
    MM.Fonts['Black'].OutText(fTexture.ARGBImage,'MAGNIFY CEL',MAGNIFYDIALOGWIDTH div 2,16,1);
    fTexture.Update;
  end;
end;

function TBDMagnifyCELDialog.KeyDown(Sender:TObject; key:integer):boolean;
begin
  if key=SDL_SCANCODE_2 then begin
    fMagnifyButtons[0].Selected:=true;
    fMagnifyButtons[1].Selected:=false;
    fMagnifyButtons[2].Selected:=false;
  end else
  if key=SDL_SCANCODE_3 then begin
    fMagnifyButtons[0].Selected:=false;
    fMagnifyButtons[1].Selected:=true;
    fMagnifyButtons[2].Selected:=false;
  end else
  if key=SDL_SCANCODE_5 then begin
    fMagnifyButtons[0].Selected:=false;
    fMagnifyButtons[1].Selected:=false;
    fMagnifyButtons[2].Selected:=true;
  end else
  if key=SDL_SCANCODE_ESCAPE then begin
    Self.Hide;
  end else
  if key=SDL_SCANCODE_RETURN then begin
    if fMagnifyButtons[0].Selected then Project.CELImage.Magnify(2)
    else if fMagnifyButtons[1].Selected then Project.CELImage.Magnify(3)
    else if fMagnifyButtons[2].Selected then Project.CELImage.Magnify(5);
    MessageQueue.AddMessage(MSG_SHOWCEL);
    Self.Hide;
  end;
  Result:=true;
end;

procedure TBDMagnifyCELDialog.MagnifyButtonClick(Sender:TObject;x,y,buttons:integer);
var i:integer;
begin
  for i:=0 to 2 do fMagnifyButtons[i].Selected:=false;
  (Sender as TBDButton).Selected:=true;
end;

procedure TBDMagnifyCELDialog.OKButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  if fMagnifyButtons[0].Selected then Project.CELImage.Magnify(2)
  else if fMagnifyButtons[1].Selected then Project.CELImage.Magnify(3)
  else if fMagnifyButtons[2].Selected then Project.CELImage.Magnify(5);
  MessageQueue.AddMessage(MSG_SHOWCEL);
  Self.Hide;
end;

procedure TBDMagnifyCELDialog.CancelButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Self.Hide;
end;

end.

