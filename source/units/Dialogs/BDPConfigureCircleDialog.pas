{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

// Generated on 2025.04.28

unit BDPConfigureCircleDialog;

{$mode delphi}

interface

uses
  SysUtils, BDPModalDialog, BDPButton;

type

  { TBDConfigureCircleDialog }

  TBDConfigureCircleDialog=class(TBDModalDialog)
    constructor Create;
    procedure ReDraw; override;
  private
    fConfCircleBXButton:TBDButton;
    fConfCircleCancelButton:TBDButton;
    fConfCircleCRButton:TBDButton;
    fConfCircleOKButton:TBDButton;
    fTemp01:integer;
    procedure SaveSettings;
    procedure Show(Sender:TObject);
    procedure ConfCircleCRButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure ConfCircleBXButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure ConfCircleOKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure ConfCircleCancelButtonClick(Sender:TObject;x,y,buttons:integer);
  end;

implementation

uses BDPShared, MKMouse2;

{ TBDConfigureCircleDialog }

constructor TBDConfigureCircleDialog.Create;
begin
  inherited Create(489,156);
  fName:='BDPConfigureCircleDialog';
  fCaption:='CONFIGURE CIRCLE PARAMETERS';
  OnShow:=Show;
  Visible:=false;
  MouseObjects.Add(Self);

  fConfCircleCRButton:=TBDButton.Create(fLeft+113,fTop+57,127,27,'CENTER','CLICK ON CENTER THEN CLICK TO SET RADIUS');
  fConfCircleCRButton.OnClick:=ConfCircleCRButtonClick;
  fConfCircleCRButton.Name:='ConfCircleCRButton';
  fConfCircleCRButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fConfCircleCRButton);

  fConfCircleBXButton:=TBDButton.Create(fLeft+249,fTop+57,127,27,'BOXED','DRAW BOUNDING BOX');
  fConfCircleBXButton.OnClick:=ConfCircleBXButtonClick;
  fConfCircleBXButton.Name:='ConfCircleBXButton';
  fConfCircleBXButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fConfCircleBXButton);

  fConfCircleOKButton:=TBDButton.Create(fLeft+113,fTop+111,127,27,'OK','APPLY VALUES');
  fConfCircleOKButton.OnClick:=ConfCircleOKButtonClick;
  fConfCircleOKButton.Name:='ConfCircleOKButton';
  fConfCircleOKButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fConfCircleOKButton);

  fConfCircleCancelButton:=TBDButton.Create(fLeft+249,fTop+111,127,27,'CANCEL','CLOSE DIALOG');
  fConfCircleCancelButton.OnClick:=ConfCircleCancelButtonClick;
  fConfCircleCancelButton.Name:='ConfCircleCancelButton';
  fConfCircleCancelButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fConfCircleCancelButton);
end;

procedure TBDConfigureCircleDialog.Redraw;
begin
  inherited ReDraw;
  MM.Fonts['Black'].OutText(fImage,'DRAW MODE',244,30,1);
end;

procedure TBDConfigureCircleDialog.SaveSettings;
begin
  Settings.CircleMode:=fTemp01;
end;

procedure TBDConfigureCircleDialog.ConfCircleCRButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  fTemp01:=0;
  fConfCircleCRButton.Selected:=true;
  fConfCircleBXButton.Selected:=false;
end;

procedure TBDConfigureCircleDialog.ConfCircleBXButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  fTemp01:=1;
  fConfCircleBXButton.Selected:=true;
  fConfCircleCRButton.Selected:=false;
end;

procedure TBDConfigureCircleDialog.ConfCircleOKButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  SaveSettings;
  Hide;
end;

procedure TBDConfigureCircleDialog.ConfCircleCancelButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Hide;
end;

procedure TBDConfigureCircleDialog.Show(Sender:TObject);
begin
  fConfCircleBXButton.Selected:=(Settings.CircleMode=1);
  fConfCircleCRButton.Selected:=(Settings.CircleMode=0);
end;

end.
