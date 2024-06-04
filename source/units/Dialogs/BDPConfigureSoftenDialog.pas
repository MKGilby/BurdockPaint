{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

// Generated on 2024.06.04

unit BDPConfigureSoftenDialog;

{$mode delphi}

interface

uses
  SysUtils, BDPModalDialog, BDPButton, BDPCheckBox, BDPSliders;

type

  { TBDConfigureSoftenDialog }

  TBDConfigureSoftenDialog=class(TBDModalDialog)
    constructor Create;
    procedure ReDraw; override;
  private
    ConfSoftenSlider:TBDHorizontalSlider;
    ConfSoftenCheckbox:TBDCheckBox;
    procedure SaveSettings;
    procedure Show(Sender:TObject);
    procedure ConfSoftenOKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure ConfSoftenCancelButtonClick(Sender:TObject;x,y,buttons:integer);
  end;

implementation

uses BDPShared, MKMouse2;

{ TBDConfigureSoftenDialog }

constructor TBDConfigureSoftenDialog.Create;
var tmp:TBDButton;
begin
  inherited Create(489,180);
  fName:='BDPConfigureSoftenDialog';
  fCaption:='CONFIGURE SOFTEN PARAMETERS';
  OnShow:=Show;
  Visible:=false;
  MouseObjects.Add(Self);

  ConfSoftenSlider:=TBDHorizontalSlider.Create(fLeft+84,fTop+57,320,33);
  with ConfSoftenSlider do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='ConfSoftenSlider';
    MinValue:=1;
    MaxValue:=16;
    Position:=Settings.SoftenCenterWeight;
  end;
  AddChild(ConfSoftenSlider);

  ConfSoftenCheckbox:=TBDCheckBox.Create(fLeft+12,fTop+99,465,27,'SOFTEN ALPHA CHANNEL TOO','... OR NOT');
  with ConfSoftenCheckbox do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='ConfSoftenCheckbox';
  end;
  AddChild(ConfSoftenCheckbox);

  tmp:=TBDButton.Create(fLeft+113,fTop+135,127,27,'OK','APPLY VALUES');
  tmp.OnClick:=ConfSoftenOKButtonClick;
  tmp.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(tmp);

  tmp:=TBDButton.Create(fLeft+249,fTop+135,127,27,'CANCEL','CLOSE DIALOG');
  tmp.OnClick:=ConfSoftenCancelButtonClick;
  tmp.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(tmp);

end;

procedure TBDConfigureSoftenDialog.Redraw;
begin
  inherited ReDraw;
  MM.Fonts['Black'].OutText(fImage,'CENTER PIXEL WEIGHT',244,30,1);
end;

procedure TBDConfigureSoftenDialog.SaveSettings;
begin
  Settings.SoftenCenterWeight:=ConfSoftenSlider.Position;
  Settings.SoftenAlphaToo:=ConfSoftenCheckbox.Selected;
end;

procedure TBDConfigureSoftenDialog.ConfSoftenOKButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  SaveSettings;
  Hide;
end;

procedure TBDConfigureSoftenDialog.ConfSoftenCancelButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Hide;
end;

procedure TBDConfigureSoftenDialog.Show(Sender:TObject);
begin
  ConfSoftenSlider.Position:=Settings.SoftenCenterWeight;
  ConfSoftenCheckbox.Selected:=Settings.SoftenAlphaToo;
end;

end.
