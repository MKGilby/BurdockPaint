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

unit BDPRotateCELDialog;

{$mode Delphi}

interface

uses
  SysUtils, BDPButton, BDPModalDialog;

type

  { TBDRotateCELDialog }

  TBDRotateCELDialog=class(TBDModalDialog)
    constructor Create;
    procedure ReDraw; override;
    function KeyDown(Sender:TObject;key:integer):boolean;
    procedure RotateButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure OKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure CancelButtonClick(Sender:TObject;x,y,buttons:integer);
  private
    fRotateButtons:array[0..2] of TBDButton;
  end;

implementation

uses BDPShared, MKMouse2, sdl2;

const
  ROTATEDIALOGWIDTH=512;
  ROTATEDIALOGHEIGHT=128;

{ TBDRotateCELDialog }

constructor TBDRotateCELDialog.Create;
const ROTATES:array[0..2] of integer=(90,180,270);
var atmb:TBDButton;i:integer;
begin
  inherited Create(ROTATEDIALOGWIDTH,ROTATEDIALOGHEIGHT);
  fName:='Rotate CEL';
  Refresh;
  for i:=0 to 2 do begin
    fRotateButtons[i]:=TBDButton.Create(
      fLeft+(ROTATEDIALOGWIDTH div 4)*(i+1)-NORMALBUTTONWIDTH div 2,
      fTop+48,
      NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
      inttostr(ROTATES[i])+'°','');
    fRotateButtons[i].ZIndex:=MODALDIALOG_ZINDEX+1;
    fRotateButtons[i].Tag:=i;
    fRotateButtons[i].OnClick:=RotateButtonClick;
    if i=0 then fRotateButtons[i].Selected:=true;
    AddChild(fRotateButtons[i]);
  end;
  atmB:=TBDButton.Create(
    fLeft+ROTATEDIALOGWIDTH div 3-NORMALBUTTONWIDTH div 2,
    fTop+84,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'OK','ROTATE CEL');
  atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
  atmB.OnClick:=OKButtonClick;
  AddChild(atmB);
  atmB:=TBDButton.Create(
    fLeft+(ROTATEDIALOGWIDTH div 3*2)-NORMALBUTTONWIDTH div 2,
    fTop+84,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'CANCEL','DON''T ROTATE CEL');
  atmB.OnClick:=CancelButtonClick;
  atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);
  OnKeyDown:=KeyDown;
  MouseObjects.Add(Self);
end;

procedure TBDRotateCELDialog.ReDraw;
begin
  if Assigned(fTexture) then begin
    fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,3,SystemPalette[SYSTEMCOLORDARK]);
    fTexture.ARGBImage.Bar(0,0,3,fTexture.ARGBImage.Height,SystemPalette[SYSTEMCOLORDARK]);
    fTexture.ARGBImage.Bar(fTexture.ARGBImage.Width-3,0,3,fTexture.ARGBImage.Height,SystemPalette[SYSTEMCOLORDARK]);
    fTexture.ARGBImage.Bar(0,fTexture.ARGBImage.Height-3,fTexture.ARGBImage.Width,3,SystemPalette[SYSTEMCOLORDARK]);
    fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,SystemPalette[SYSTEMCOLORMID]);
    MM.Fonts['Black'].OutText(fTexture.ARGBImage,'ROTATE CEL',ROTATEDIALOGWIDTH div 2,16,1);
    fTexture.Update;
  end;
end;

function TBDRotateCELDialog.KeyDown(Sender:TObject; key:integer):boolean;
begin
  if key=SDL_SCANCODE_1 then begin
    fRotateButtons[0].Selected:=true;
    fRotateButtons[1].Selected:=false;
    fRotateButtons[2].Selected:=false;
  end else
  if key=SDL_SCANCODE_2 then begin
    fRotateButtons[0].Selected:=false;
    fRotateButtons[1].Selected:=true;
    fRotateButtons[2].Selected:=false;
  end else
  if key=SDL_SCANCODE_3 then begin
    fRotateButtons[0].Selected:=false;
    fRotateButtons[1].Selected:=false;
    fRotateButtons[2].Selected:=true;
  end else
  if key=SDL_SCANCODE_ESCAPE then begin
    Self.Hide;
  end else
  if key=SDL_SCANCODE_RETURN then begin
    if fRotateButtons[0].Selected then Project.CELImage.Rotate(1)
    else if fRotateButtons[1].Selected then Project.CELImage.Rotate(2)
    else if fRotateButtons[2].Selected then Project.CELImage.Rotate(3);
    MessageQueue.AddMessage(MSG_SHOWCEL);
    Self.Hide;
  end;
  Result:=true;
end;

procedure TBDRotateCELDialog.RotateButtonClick(Sender:TObject; x,y,buttons:integer);
var i:integer;
begin
  for i:=0 to 2 do fRotateButtons[i].Selected:=false;
  (Sender as TBDButton).Selected:=true;
end;

procedure TBDRotateCELDialog.OKButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  if fRotateButtons[0].Selected then Project.CELImage.Rotate(1)
  else if fRotateButtons[1].Selected then Project.CELImage.Rotate(2)
  else if fRotateButtons[2].Selected then Project.CELImage.Rotate(3);
  MessageQueue.AddMessage(MSG_SHOWCEL);
  Self.Hide;
end;

procedure TBDRotateCELDialog.CancelButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Self.Hide;
end;

end.

