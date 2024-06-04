{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

// Generated on 2024.06.04

unit BDPConfigureTintDialog;

{$mode delphi}

interface

uses
  SysUtils, BDPModalDialog, BDPButton, BDPCheckBox, BDPSliders;

type

  { TBDConfigureTintDialog }

  TBDConfigureTintDialog=class(TBDModalDialog)
    constructor Create;
    procedure ReDraw; override;
  private
    ConfTintSlider:TBDHorizontalSlider;
    ConfTintCheckbox:TBDCheckBox;
    procedure SaveSettings;
    procedure Show(Sender:TObject);
    procedure ConfTintOKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure ConfTintCancelButtonClick(Sender:TObject;x,y,buttons:integer);
  end;

implementation

uses BDPShared, MKMouse2;

{ TBDConfigureTintDialog }

constructor TBDConfigureTintDialog.Create;
var tmp:TBDButton;
begin
  inherited Create(453,180);
  fName:='BDPConfigureTintDialog';
  fCaption:='CONFIGURE TINT PARAMETERS';
  OnShow:=Show;
  Visible:=false;
  MouseObjects.Add(Self);

  ConfTintSlider:=TBDHorizontalSlider.Create(fLeft+66,fTop+57,320,33);
  with ConfTintSlider do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='ConfTintSlider';
    MinValue:=0;
    MaxValue:=100;
    Position:=Settings.TintStrength;
  end;
  AddChild(ConfTintSlider);

  ConfTintCheckbox:=TBDCheckBox.Create(fLeft+12,fTop+99,429,27,'PUTCEL USE CEL AS MASK','... NOT AS COLOR SOURCE');
  with ConfTintCheckbox do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='ConfTintCheckbox';
  end;
  AddChild(ConfTintCheckbox);

  tmp:=TBDButton.Create(fLeft+95,fTop+135,127,27,'OK','APPLY VALUES');
  tmp.OnClick:=ConfTintOKButtonClick;
  tmp.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(tmp);

  tmp:=TBDButton.Create(fLeft+231,fTop+135,127,27,'CANCEL','CLOSE DIALOG');
  tmp.OnClick:=ConfTintCancelButtonClick;
  tmp.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(tmp);

end;

procedure TBDConfigureTintDialog.Redraw;
begin
  inherited ReDraw;
  MM.Fonts['Black'].OutText(fImage,'TINT STRENGTH',226,30,1);
end;

procedure TBDConfigureTintDialog.SaveSettings;
begin
  Settings.TintStrength:=ConfTintSlider.Position;
  Settings.TintCELAsMask:=ConfTintCheckbox.Selected;
end;

procedure TBDConfigureTintDialog.ConfTintOKButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  SaveSettings;
  Hide;
end;

procedure TBDConfigureTintDialog.ConfTintCancelButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Hide;
end;

procedure TBDConfigureTintDialog.Show(Sender:TObject);
begin
  ConfTintSlider.Position:=Settings.TintStrength;
  ConfTintCheckbox.Selected:=Settings.TintCELAsMask;
end;

end.
