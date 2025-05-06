{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

// Generated on 2025.05.06

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
  inherited Create(489,198);
  fName:='BDPDitherDialog';
  fCaption:='CONFIGURE DITHER PARAMETERS';
  OnShow:=Show;
  Visible:=false;
  MouseObjects.Add(Self);

  ConfDitherUseDitherCheckbox:=TBDCheckBox.Create(fLeft+93,fTop+30,303,27,'DITHER GRADIENT','... OR NOT');
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
    Position:=Project.CurrentGradientList.ActiveGradient.DitherStrength;
  end;
  AddChild(ConfDitherSlider);

  fConfDitherOKButton:=TBDButton.Create(fLeft+113,fTop+153,127,27,'OK','APPLY VALUES');
  fConfDitherOKButton.OnClick:=ConfDitherOKButtonClick;
  fConfDitherOKButton.Name:='ConfDitherOKButton';
  fConfDitherOKButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fConfDitherOKButton);

  fConfDitherCancelButton:=TBDButton.Create(fLeft+249,fTop+153,127,27,'CANCEL','CLOSE DIALOG');
  fConfDitherCancelButton.OnClick:=ConfDitherCancelButtonClick;
  fConfDitherCancelButton.Name:='ConfDitherCancelButton';
  fConfDitherCancelButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fConfDitherCancelButton);
end;

procedure TBDDitherDialog.Redraw;
begin
  inherited ReDraw;
  MM.Fonts['Black'].OutText(fImage,'DITHER STRENGTH',244,66,1);
end;

procedure TBDDitherDialog.SaveSettings;
begin
  Project.CurrentGradientList.ActiveGradient.Dithered:=ConfDitherUseDitherCheckbox.Selected;
  Project.CurrentGradientList.ActiveGradient.DitherStrength:=ConfDitherSlider.Position;
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
  ConfDitherSlider.Position:=Project.CurrentGradientList.ActiveGradient.DitherStrength;
  ConfDitherUseDitherCheckbox.Selected:=Project.CurrentGradientList.ActiveGradient.Dithered;
end;

end.
