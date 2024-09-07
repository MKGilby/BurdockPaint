{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

// Generated on 2024.09.07

unit BDPDitherDialog;

{$mode delphi}

interface

uses
  SysUtils, BDPModalDialog, BDPButton, BDPCheckBox, BDPSliders;

type

  { TBDDitherDialog }

  TBDDitherDialog=class(TBDModalDialog)
    constructor Create;
    procedure ReDraw; override;
  private
    ConfDitherBandCheckbox:TBDCheckBox;
    ConfDitherBandSlider:TBDHorizontalSlider;
    ConfDitherSlider:TBDHorizontalSlider;
    ConfDitherUseDitherCheckbox:TBDCheckBox;
    fConfDitherCancelButton:TBDButton;
    fConfDitherOKButton:TBDButton;
    procedure SaveSettings;
    procedure Show(Sender:TObject);
    procedure ConfDitherOKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure ConfDitherCancelButtonClick(Sender:TObject;x,y,buttons:integer);
  end;

implementation

uses BDPShared, MKMouse2;

{ TBDDitherDialog }

constructor TBDDitherDialog.Create;
begin
  inherited Create(489,321);
  fName:='BDPDitherDialog';
  fCaption:='CONFIGURE DITHER PARAMETERS';
  OnShow:=Show;
  Visible:=false;
  MouseObjects.Add(Self);

  ConfDitherUseDitherCheckbox:=TBDCheckBox.Create(fLeft+84,fTop+30,321,27,'DITHER GRADIENTS','... OR NOT');
  with ConfDitherUseDitherCheckbox do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='ConfDitherUseDitherCheckbox';
  end;
  AddChild(ConfDitherUseDitherCheckbox);

  ConfDitherSlider:=TBDHorizontalSlider.Create(fLeft+84,fTop+93,320,33);
  with ConfDitherSlider do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='ConfDitherSlider';
    MinValue:=0;
    MaxValue:=100;
    Position:=Settings.DitherStrength;
  end;
  AddChild(ConfDitherSlider);

  ConfDitherBandCheckbox:=TBDCheckBox.Create(fLeft+75,fTop+153,339,27,'USE COLOR BANDING','... OR NOT');
  with ConfDitherBandCheckbox do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='ConfDitherBandCheckbox';
  end;
  AddChild(ConfDitherBandCheckbox);

  ConfDitherBandSlider:=TBDHorizontalSlider.Create(fLeft+84,fTop+216,320,33);
  with ConfDitherBandSlider do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='ConfDitherBandSlider';
    MinValue:=2;
    MaxValue:=64;
    Position:=Settings.DitherColorBandCount;
  end;
  AddChild(ConfDitherBandSlider);

  fConfDitherOKButton:=TBDButton.Create(fLeft+113,fTop+276,127,27,'OK','APPLY VALUES');
  fConfDitherOKButton.OnClick:=ConfDitherOKButtonClick;
  fConfDitherOKButton.Name:='ConfDitherOKButton';
  fConfDitherOKButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fConfDitherOKButton);

  fConfDitherCancelButton:=TBDButton.Create(fLeft+249,fTop+276,127,27,'CANCEL','CLOSE DIALOG');
  fConfDitherCancelButton.OnClick:=ConfDitherCancelButtonClick;
  fConfDitherCancelButton.Name:='ConfDitherCancelButton';
  fConfDitherCancelButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fConfDitherCancelButton);
end;

procedure TBDDitherDialog.Redraw;
begin
  inherited ReDraw;
  MM.Fonts['Black'].OutText(fImage,'DITHER STRENGTH',244,66,1);
  MM.Fonts['Black'].OutText(fImage,'COLOR BAND COUNT',244,189,1);
end;

procedure TBDDitherDialog.SaveSettings;
begin
  Settings.DitherColorBandCount:=ConfDitherBandSlider.Position;
  Settings.DitherColorBanding:=ConfDitherBandCheckbox.Selected;
  Settings.DitherGradients:=ConfDitherUseDitherCheckbox.Selected;
  Settings.DitherStrength:=ConfDitherSlider.Position;
end;

procedure TBDDitherDialog.ConfDitherOKButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  SaveSettings;
  Hide;
end;

procedure TBDDitherDialog.ConfDitherCancelButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Hide;
end;

procedure TBDDitherDialog.Show(Sender:TObject);
begin
  ConfDitherBandCheckbox.Selected:=Settings.DitherColorBanding;
  ConfDitherBandSlider.Position:=Settings.DitherColorBandCount;
  ConfDitherSlider.Position:=Settings.DitherStrength;
  ConfDitherUseDitherCheckbox.Selected:=Settings.DitherGradients;
end;

end.
