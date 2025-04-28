{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

// Generated on 2025.04.28

unit BDPConfigureSepDialog;

{$mode delphi}

interface

uses
  SysUtils, BDPModalDialog, BDPButton, BDPCheckBox;

type

  { TBDConfigureSepDialog }

  TBDConfigureSepDialog=class(TBDModalDialog)
    constructor Create;
    procedure ReDraw; override;
  private
    ConfSepBoxedCheckbox:TBDCheckBox;
    fConfSepCancelButton:TBDButton;
    fConfSepOKButton:TBDButton;
    procedure SaveSettings;
    procedure Show(Sender:TObject);
    procedure ConfSepOKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure ConfSepCancelButtonClick(Sender:TObject;x,y,buttons:integer);
  end;

implementation

uses BDPShared, MKMouse2;

{ TBDConfigureSepDialog }

constructor TBDConfigureSepDialog.Create;
begin
  inherited Create(651,156);
  fName:='BDPConfigureSepDialog';
  fCaption:='CONFIGURE SEPARATE COLORS PARAMETERS';
  OnShow:=Show;
  Visible:=false;
  MouseObjects.Add(Self);

  ConfSepBoxedCheckbox:=TBDCheckBox.Create(fLeft+264,fTop+57,123,27,'BOXED','DRAW A BOX FIRST AND APPLY SEP ONLY INSIDE.');
  with ConfSepBoxedCheckbox do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='ConfSepBoxedCheckbox';
  end;
  AddChild(ConfSepBoxedCheckbox);

  fConfSepOKButton:=TBDButton.Create(fLeft+194,fTop+111,127,27,'OK','APPLY VALUES');
  fConfSepOKButton.OnClick:=ConfSepOKButtonClick;
  fConfSepOKButton.Name:='ConfSepOKButton';
  fConfSepOKButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fConfSepOKButton);

  fConfSepCancelButton:=TBDButton.Create(fLeft+330,fTop+111,127,27,'CANCEL','CLOSE DIALOG');
  fConfSepCancelButton.OnClick:=ConfSepCancelButtonClick;
  fConfSepCancelButton.Name:='ConfSepCancelButton';
  fConfSepCancelButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fConfSepCancelButton);
end;

procedure TBDConfigureSepDialog.Redraw;
begin
  inherited ReDraw;
  MM.Fonts['Black'].OutText(fImage,'DRAW MODE',325,30,1);
end;

procedure TBDConfigureSepDialog.SaveSettings;
begin
  Settings.SepBoxed:=ConfSepBoxedCheckbox.Selected;
end;

procedure TBDConfigureSepDialog.ConfSepOKButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  SaveSettings;
  Hide;
end;

procedure TBDConfigureSepDialog.ConfSepCancelButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Hide;
end;

procedure TBDConfigureSepDialog.Show(Sender:TObject);
begin
  ConfSepBoxedCheckbox.Selected:=Settings.SepBoxed;
end;

end.
