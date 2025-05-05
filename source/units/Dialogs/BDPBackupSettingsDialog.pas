{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

// Generated on 2025.05.05

unit BDPBackupSettingsDialog;

{$mode delphi}

interface

uses
  SysUtils, BDPModalDialog, BDPButton, BDPSliders;

type

  { TBDBackupSettingsDialog }

  TBDBackupSettingsDialog=class(TBDModalDialog)
    constructor Create;
    procedure ReDraw; override;
  private
    BackupIntervalSlider:TBDHorizontalSlider;
    fBackupSettingsCancelButton:TBDButton;
    fBackupSettingsOKButton:TBDButton;
    procedure SaveSettings;
    procedure Show(Sender:TObject);
    procedure BackupSettingsOKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure BackupSettingsCancelButtonClick(Sender:TObject;x,y,buttons:integer);
  end;

implementation

uses BDPShared, MKMouse2;

{ TBDBackupSettingsDialog }

constructor TBDBackupSettingsDialog.Create;
begin
  inherited Create(411,162);
  fName:='BDPBackupSettingsDialog';
  fCaption:='BACKUP SETTINGS';
  OnShow:=Show;
  Visible:=false;
  MouseObjects.Add(Self);

  BackupIntervalSlider:=TBDHorizontalSlider.Create(fLeft+45,fTop+57,320,33);
  with BackupIntervalSlider do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='BackupIntervalSlider';
    MinValue:=30;
    MaxValue:=600;
    Position:=Settings.BackupInterval;
  end;
  AddChild(BackupIntervalSlider);

  fBackupSettingsOKButton:=TBDButton.Create(fLeft+74,fTop+117,127,27,'OK','APPLY VALUES');
  fBackupSettingsOKButton.OnClick:=BackupSettingsOKButtonClick;
  fBackupSettingsOKButton.Name:='BackupSettingsOKButton';
  fBackupSettingsOKButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fBackupSettingsOKButton);

  fBackupSettingsCancelButton:=TBDButton.Create(fLeft+210,fTop+117,127,27,'CANCEL','CLOSE DIALOG');
  fBackupSettingsCancelButton.OnClick:=BackupSettingsCancelButtonClick;
  fBackupSettingsCancelButton.Name:='BackupSettingsCancelButton';
  fBackupSettingsCancelButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fBackupSettingsCancelButton);
end;

procedure TBDBackupSettingsDialog.Redraw;
begin
  inherited ReDraw;
  MM.Fonts['Black'].OutText(fImage,'BACKUP INTERVAL (SECS)',205,30,1);
end;

procedure TBDBackupSettingsDialog.SaveSettings;
begin
  Settings.BackupInterval:=BackupIntervalSlider.Position;
end;

procedure TBDBackupSettingsDialog.BackupSettingsOKButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  SaveSettings;
  Hide;
end;

procedure TBDBackupSettingsDialog.BackupSettingsCancelButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  Hide;
end;

procedure TBDBackupSettingsDialog.Show(Sender:TObject);
begin
  BackupIntervalSlider.Position:=Settings.BackupInterval;
end;

end.
