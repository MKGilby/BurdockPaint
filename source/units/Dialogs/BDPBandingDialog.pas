{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

// Generated on 2025.05.06

unit BDPBandingDialog;

{$mode delphi}

interface

uses
  SysUtils, BDPModalDialog, BDPButton, BDPCheckBox, BDPSliders;

type

  { TBDBandingDialog }

  TBDBandingDialog=class(TBDModalDialog)
    constructor Create;
    procedure ReDraw; override;
  private
    ConfBandingBandCheckbox:TBDCheckBox;
    ConfBandingBandSlider:TBDHorizontalSlider;
    fConfBandingCancelButton:TBDButton;
    fConfBandingOKButton:TBDButton;
    procedure SaveSettings;
    procedure Show(Sender:TObject);
    procedure ConfBandingOKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure ConfBandingCancelButtonClick(Sender:TObject;x,y,buttons:integer);
  end;

implementation

uses BDPShared, MKMouse2;

{ TBDBandingDialog }

constructor TBDBandingDialog.Create;
begin
  inherited Create(507,198);
  fName:='BDPBandingDialog';
  fCaption:='CONFIGURE BANDING PARAMETERS';
  OnShow:=Show;
  Visible:=false;
  MouseObjects.Add(Self);

  ConfBandingBandCheckbox:=TBDCheckBox.Create(fLeft+84,fTop+30,339,27,'USE COLOR BANDING','... OR NOT');
  with ConfBandingBandCheckbox do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='ConfBandingBandCheckbox';
  end;
  AddChild(ConfBandingBandCheckbox);

  ConfBandingBandSlider:=TBDHorizontalSlider.Create(fLeft+93,fTop+93,320,33);
  with ConfBandingBandSlider do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='ConfBandingBandSlider';
    MinValue:=2;
    MaxValue:=64;
    Position:=Project.CurrentGradientList.ActiveGradient.ColorBandCount;
  end;
  AddChild(ConfBandingBandSlider);

  fConfBandingOKButton:=TBDButton.Create(fLeft+122,fTop+153,127,27,'OK','APPLY VALUES');
  fConfBandingOKButton.OnClick:=ConfBandingOKButtonClick;
  fConfBandingOKButton.Name:='ConfBandingOKButton';
  fConfBandingOKButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fConfBandingOKButton);

  fConfBandingCancelButton:=TBDButton.Create(fLeft+258,fTop+153,127,27,'CANCEL','CLOSE DIALOG');
  fConfBandingCancelButton.OnClick:=ConfBandingCancelButtonClick;
  fConfBandingCancelButton.Name:='ConfBandingCancelButton';
  fConfBandingCancelButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fConfBandingCancelButton);
end;

procedure TBDBandingDialog.Redraw;
begin
  inherited ReDraw;
  MM.Fonts['Black'].OutText(fImage,'COLOR BAND COUNT',253,66,1);
end;

procedure TBDBandingDialog.SaveSettings;
begin
  Project.CurrentGradientList.ActiveGradient.ColorBandCount:=ConfBandingBandSlider.Position;
  Project.CurrentGradientList.ActiveGradient.ColorBanding:=ConfBandingBandCheckbox.Selected;
end;

procedure TBDBandingDialog.ConfBandingOKButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  SaveSettings;
  Hide;
end;

procedure TBDBandingDialog.ConfBandingCancelButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Hide;
end;

procedure TBDBandingDialog.Show(Sender:TObject);
begin
  ConfBandingBandCheckbox.Selected:=Project.CurrentGradientList.ActiveGradient.ColorBanding;
  ConfBandingBandSlider.Position:=Project.CurrentGradientList.ActiveGradient.ColorBandCount;
end;

end.
