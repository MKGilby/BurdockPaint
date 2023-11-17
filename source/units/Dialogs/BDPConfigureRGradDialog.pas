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

unit BDPConfigureRGradDialog;

{$mode Delphi}

interface

uses
  SysUtils, BDPSliders, BDPModalDialog;

type

  { TBDConfigureRGradDialog }

  TBDConfigureRGradDialog=class(TBDModalDialog)
    constructor Create;
    procedure ReDraw; override;
    procedure OKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure CenterButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure CancelButtonClick(Sender:TObject;x,y,buttons:integer);
  private
    fRepetitionSlider:TBDHorizontalSlider;
    fRotationSlider:TBDHorizontalSlider;
    procedure Show(Sender:TObject);
  end;

implementation

uses BDPButton, BDPShared, MKMouse2;

const
  CONFIGURERGRADDIALOGWIDTH=480;
  CONFIGURERGRADDIALOGHEIGHT=256+MODALDIALOGCAPTIONHEIGHT-3;

{ TBDConfigureRGradDialog }

constructor TBDConfigureRGradDialog.Create;
const SLIDERWIDTH=300;
      SLIDERHEIGHT=33;
var atmB:TBDButton;
begin
  inherited Create(CONFIGURERGRADDIALOGWIDTH,CONFIGURERGRADDIALOGHEIGHT);
  fName:='ConfigureRGradDialog';
  Caption:='CONFIGURE RGRAD PARAMETERS';

  fRepetitionSlider:=TBDHorizontalSlider.Create(
    fLeft+(CONFIGURERGRADDIALOGWIDTH-SLIDERWIDTH) div 2, fTop+66, SLIDERWIDTH, SLIDERHEIGHT);
  fRepetitionSlider.ZIndex:=MODALDIALOG_ZINDEX+1;
  fRepetitionSlider.Name:='RGradRepSlider';
  fRepetitionSlider.MinValue:=1;
  fRepetitionSlider.MaxValue:=64;
  fRepetitionSlider.Position:=Settings.RGradRepetitions;
  AddChild(fRepetitionSlider);

  fRotationSlider:=TBDHorizontalSlider.Create(
    fLeft+(CONFIGURERGRADDIALOGWIDTH-SLIDERWIDTH) div 2, fTop+146, SLIDERWIDTH, SLIDERHEIGHT);
  fRotationSlider.ZIndex:=MODALDIALOG_ZINDEX+1;
  fRotationSlider.Name:='RGradRotSlider';
  fRotationSlider.MinValue:=0;
  fRotationSlider.MaxValue:=359;
  fRotationSlider.Position:=Settings.RGradRotation;
  AddChild(fRotationSlider);

  atmB:=TBDButton.Create(
    fLeft+(CONFIGURERGRADDIALOGWIDTH div 2-NORMALBUTTONWIDTH div 2),
    fTop+194,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'CENTER','SET RADIAL GRADIENT CENTER POINT');
  atmb.OnClick:=CenterButtonClick;
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);

  atmB:=TBDButton.Create(
    fLeft+(CONFIGURERGRADDIALOGWIDTH div 3-NORMALBUTTONWIDTH div 2),
    fTop+234,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'OK','APPLY VALUES');
  atmb.OnClick:=OKButtonClick;
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);

  atmB:=TBDButton.Create(
    fLeft+(CONFIGURERGRADDIALOGWIDTH div 3*2-NORMALBUTTONWIDTH div 2),
    fTop+234,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'CANCEL','CLOSE DIALOG');
  atmb.OnClick:=CancelButtonClick;
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);

  OnShow:=Show;
  Visible:=false;
  MouseObjects.Add(Self);
end;

procedure TBDConfigureRGradDialog.ReDraw;
begin
  inherited ReDraw;
  MM.Fonts['Black'].OutText(fImage,'REPETITIONS',CONFIGURERGRADDIALOGWIDTH div 2,34,1);
  MM.Fonts['Black'].OutText(fImage,'ROTATION °',CONFIGURERGRADDIALOGWIDTH div 2,114,1);
end;

procedure TBDConfigureRGradDialog.OKButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Settings.RGradRepetitions:=fRepetitionSlider.Position;
  Settings.RGradRotation:=fRotationSlider.Position;
  Self.Hide;
  MouseObjects.Remove(Self);
  FreeAndNil(Self);
end;

procedure TBDConfigureRGradDialog.CenterButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Self.Hide;
  MessageQueue.AddMessage(MSG_CONFIGRGRADCENTER);
end;

procedure TBDConfigureRGradDialog.CancelButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Self.Hide;
  MouseObjects.Remove(Self);
  FreeAndNil(Self);
end;

procedure TBDConfigureRGradDialog.Show(Sender:TObject);
begin
  fRotationSlider.Position:=Settings.RGradRotation;
  fRepetitionSlider.Position:=Settings.RGradRepetitions;
end;

end.

