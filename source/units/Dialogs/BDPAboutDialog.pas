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

unit BDPAboutDialog;

{$mode Delphi}

interface

uses
  BDPModalDialog;

type

  { TBDAboutDialog }

  TBDAboutDialog=class(TBDModalDialog)
    constructor Create;
    procedure ReDraw; override;
    procedure OKButtonClick(Sender:TObject;x,y,buttons:integer);
  end;

implementation

uses BDPShared, BDPButton, MKMouse2;

const
  SPLASHSCREENWIDTH=640;
  SPLASHSCREENHEIGHT=160;

{ TBDAboutDialog }

constructor TBDAboutDialog.Create;
var atmB:TBDButton;
begin
  inherited Create(SPLASHSCREENWIDTH,SPLASHSCREENHEIGHT);
  fName:='SplashScreen';
  Refresh;
  atmB:=TBDButton.Create(
    fLeft+SPLASHSCREENWIDTH-NORMALBUTTONWIDTH-6-16,
    fTop+SPLASHSCREENHEIGHT-44,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'OK','CLOSE DIALOG');
  atmB.OnClick:=OKButtonClick;
  atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);
  Visible:=false;
  MouseObjects.Add(Self);
end;

procedure TBDAboutDialog.ReDraw;
begin
  if Assigned(fTexture) then begin
    fTexture.ARGBImage.Bar(0,0,fTexture.ARGBImage.Width,fTexture.ARGBImage.Height,SystemPalette[2]);
    fTexture.ARGBImage.Bar(3,3,fTexture.ARGBImage.Width-6,fTexture.ARGBImage.Height-6,SystemPalette[3]);
    MM.Fonts['LogoFont'].OutText(fTexture.ARGBImage,'BURDoCK PAINT',80,24,0);
    MM.Fonts['DarkGray'].OutText(fTexture.ARGBImage,'CODE: GILBY/MKSZTSZ',80,56,0);
    MM.Fonts['DarkGray'].OutText(fTexture.ARGBImage,'LICENSED UNDER GNU GPL 3',80,88,0);
    MM.Fonts['DarkGray'].OutText(fTexture.ARGBImage,'COPYRIGHT 2023 MKSZTSZ',80,120,0);
    MM.Images.ItemByName['Burdock'].CopyTo(0,0,46,52,16,(SPLASHSCREENHEIGHT-52) div 2,fTexture.ARGBImage,true);
    fTexture.Update;
  end;
end;

procedure TBDAboutDialog.OKButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Self.Hide;
end;

end.

