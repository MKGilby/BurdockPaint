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
  ABOUTDIALOGWIDTH=640;
  ABOUTDIALOGHEIGHT=160+MODALDIALOGCAPTIONHEIGHT-3;

{ TBDAboutDialog }

constructor TBDAboutDialog.Create;
var atmB:TBDButton;
begin
  inherited Create(ABOUTDIALOGWIDTH,ABOUTDIALOGHEIGHT);
  fName:='SplashScreen';
  Visible:=false;
  fNeedRedraw:=true;
  Caption:='ABOUT';
  atmB:=TBDButton.Create(
    fLeft+ABOUTDIALOGWIDTH-NORMALBUTTONWIDTH-6-16,
    fTop+ABOUTDIALOGHEIGHT-44,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'OK','CLOSE DIALOG');
  atmB.OnClick:=OKButtonClick;
  atmB.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);
  MouseObjects.Add(Self);
end;

procedure TBDAboutDialog.ReDraw;
begin
  inherited ReDraw;
  MM.Fonts['LogoFont'].OutText(fImage,'BURDoCK PAINT',80,42,0);
  MM.Fonts['DarkGray'].OutText(fImage,'CODE: GILBY/MKSZTSZ',80,74,0);
  MM.Fonts['DarkGray'].OutText(fImage,'LICENSED UNDER GNU GPL 3',80,106,0);
  MM.Fonts['DarkGray'].OutText(fImage,'COPYRIGHT 2023 MKSZTSZ',80,138,0);
  MM.Images.ItemByName['Burdock'].CopyTo(0,0,46,52,16,(ABOUTDIALOGHEIGHT-52) div 2,fImage,true);
end;

procedure TBDAboutDialog.OKButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Self.Hide;
end;

end.

