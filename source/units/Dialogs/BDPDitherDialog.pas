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

unit BDPDitherDialog;

{$mode Delphi}

interface

uses
  BDPSliders, BDPModalDialog;

type

  { TBDDitherDialog }

  TBDDitherDialog=class(TBDModalDialog)
    constructor Create;
    procedure ReDraw; override;
    procedure OKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure CancelButtonClick(Sender:TObject;x,y,buttons:integer);
  private
    fSlider:TBDHorizontalSlider;
  end;

implementation

uses BDPButton, BDPShared, MKMouse2;

const
  DITHERDIALOGWIDTH=512;
  DITHERDIALOGHEIGHT=144+MODALDIALOGCAPTIONHEIGHT-3;

{ TBDDitherDialog }

constructor TBDDitherDialog.Create;
const SLIDERWIDTH=360;
      SLIDERHEIGHT=33;
var atmB:TBDButton;
begin
  inherited Create(DITHERDIALOGWIDTH,DITHERDIALOGHEIGHT);
  fName:='DitherDialog';
  Caption:='CONFIGURE DITHER PARAMETERS';

  fSlider:=TBDHorizontalSlider.Create(
    fLeft+(DITHERDIALOGWIDTH-SLIDERWIDTH) div 2, fTop+66, SLIDERWIDTH, SLIDERHEIGHT);
  fSlider.ZIndex:=MODALDIALOG_ZINDEX+1;
  fSlider.Name:='DitherSlider';
  fSlider.MinValue:=1;
  fSlider.MaxValue:=100;
  fSlider.Position:=Settings.DitherStrength;
  AddChild(fSlider);

  atmB:=TBDButton.Create(
    fLeft+(DITHERDIALOGWIDTH div 3-NORMALBUTTONWIDTH div 2),
    fTop+114,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'OK','APPLY DITHER STRENGTH');
  atmb.OnClick:=OKButtonClick;
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);

  atmB:=TBDButton.Create(
    fLeft+(DITHERDIALOGWIDTH div 3*2-NORMALBUTTONWIDTH div 2),
    fTop+114,
    NORMALBUTTONWIDTH,NORMALBUTTONHEIGHT,
    'CANCEL','CLOSE DIALOG');
  atmb.OnClick:=CancelButtonClick;
  atmb.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(atmB);


  Visible:=false;
  MouseObjects.Add(Self);
end;

procedure TBDDitherDialog.ReDraw;
begin
  inherited ReDraw;
  if Assigned(fTexture) then begin
    MM.Fonts['Black'].OutText(fTexture.ARGBImage,'DITHER STRENGTH',DITHERDIALOGWIDTH div 2,34,1);
    fTexture.Update;
  end;
end;

procedure TBDDitherDialog.OKButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Settings.DitherStrength:=fSlider.Position;
  Self.Hide;
end;

procedure TBDDitherDialog.CancelButtonClick(Sender:TObject; x,y,buttons:integer);
begin
  Self.Hide;
end;

end.

