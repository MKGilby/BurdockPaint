{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

// Generated on 2025.05.06

unit BDPBackupSettingsDialog;

{$mode delphi}

interface

uses
  SysUtils, BDPModalDialog, BDPButton, BDPCheckBox, BDPSliders;

type

  { TBDBackupSettingsDialog }

  TBDBackupSettingsDialog=class(TBDModalDialog)
    constructor Create;
    procedure ReDraw; override;
  private
    BackupCheckbox:TBDCheckBox;
    BackupDaysSlider:TBDHorizontalSlider;
    BackupFileCountSlider:TBDHorizontalSlider;
    BackupIntervalSlider:TBDHorizontalSlider;
    BackupLimitDays:TBDCheckBox;
    BackupLimitFileCount:TBDCheckBox;
    BackupLimitSize:TBDCheckBox;
    BackupMaxSizeSlider:TBDHorizontalSlider;
    fBackupSettingsCancelButton:TBDButton;
    fBackupSettingsOKButton:TBDButton;
    fBackupUnitKBButton:TBDButton;
    fBackupUnitMBButton:TBDButton;
    fTemp01:integer;
    procedure SaveSettings;
    procedure Show(Sender:TObject);
    procedure BackupUnitKBButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure BackupUnitMBButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure BackupSettingsOKButtonClick(Sender:TObject;x,y,buttons:integer);
    procedure BackupSettingsCancelButtonClick(Sender:TObject;x,y,buttons:integer);
  end;

implementation

uses BDPShared, MKMouse2;

{ TBDBackupSettingsDialog }

constructor TBDBackupSettingsDialog.Create;
begin
  inherited Create(591,522);
  fName:='BDPBackupSettingsDialog';
  fCaption:='BACKUP SETTINGS';
  OnShow:=Show;
  Visible:=false;
  MouseObjects.Add(Self);

  BackupCheckbox:=TBDCheckBox.Create(fLeft+81,fTop+30,429,27,'CREATE PROJECT BACKUPS','CHECK TO CREATE PERIODIC BACKUPS OF THE PROJECT FILE.');
  with BackupCheckbox do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='BackupCheckbox';
  end;
  AddChild(BackupCheckbox);

  BackupIntervalSlider:=TBDHorizontalSlider.Create(fLeft+135,fTop+93,320,33);
  with BackupIntervalSlider do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='BackupIntervalSlider';
    MinValue:=30;
    MaxValue:=600;
    Position:=Settings.BackupInterval;
  end;
  AddChild(BackupIntervalSlider);

  BackupLimitSize:=TBDCheckBox.Create(fLeft+63,fTop+153,465,27,'LIMIT BACKUP FOLDER SIZE','CHECK TO LIMIT THE SIZE OF THE BACKUP FOLDER BY REMOVING OLDEST FILES.');
  with BackupLimitSize do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='BackupLimitSize';
  end;
  AddChild(BackupLimitSize);

  BackupMaxSizeSlider:=TBDHorizontalSlider.Create(fLeft+135,fTop+189,320,33);
  with BackupMaxSizeSlider do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='BackupMaxSizeSlider';
    MinValue:=1;
    MaxValue:=1024;
    Position:=Settings.BackupFolderMaxSize;
  end;
  AddChild(BackupMaxSizeSlider);

  fBackupUnitKBButton:=TBDButton.Create(fLeft+164,fTop+231,127,27,'KB','BACKUP LIMIT SIZE IS IN KILOBYTES.');
  fBackupUnitKBButton.OnClick:=BackupUnitKBButtonClick;
  fBackupUnitKBButton.Name:='BackupUnitKBButton';
  fBackupUnitKBButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fBackupUnitKBButton);

  fBackupUnitMBButton:=TBDButton.Create(fLeft+300,fTop+231,127,27,'MB','BACKUP LIMIT SIZE IS IN MEGABYTES.');
  fBackupUnitMBButton.OnClick:=BackupUnitMBButtonClick;
  fBackupUnitMBButton.Name:='BackupUnitMBButton';
  fBackupUnitMBButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fBackupUnitMBButton);

  BackupLimitDays:=TBDCheckBox.Create(fLeft+9,fTop+285,573,27,'DELETE FILES OLDER THAN (DAYS)','CHECK TO DELETE FILES OLDER THAN THE SET DAYS FROM BACKUP.');
  with BackupLimitDays do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='BackupLimitDays';
  end;
  AddChild(BackupLimitDays);

  BackupDaysSlider:=TBDHorizontalSlider.Create(fLeft+135,fTop+321,320,33);
  with BackupDaysSlider do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='BackupDaysSlider';
    MinValue:=1;
    MaxValue:=365;
    Position:=Settings.BackupFolderRetentionTime;
  end;
  AddChild(BackupDaysSlider);

  BackupLimitFileCount:=TBDCheckBox.Create(fLeft+99,fTop+381,393,27,'LIMIT MAX FILE COUNT','CHECK TO DELETE OLDEST FILES WHEN FILE COUNT EXCEEDS SET VALUE.');
  with BackupLimitFileCount do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='BackupLimitFileCount';
  end;
  AddChild(BackupLimitFileCount);

  BackupFileCountSlider:=TBDHorizontalSlider.Create(fLeft+135,fTop+417,320,33);
  with BackupFileCountSlider do begin
    ZIndex:=MODALDIALOG_ZINDEX+1;
    Name:='BackupFileCountSlider';
    MinValue:=1;
    MaxValue:=1024;
    Position:=Settings.BackupFolderMaxFileCount;
  end;
  AddChild(BackupFileCountSlider);

  fBackupSettingsOKButton:=TBDButton.Create(fLeft+164,fTop+477,127,27,'OK','APPLY VALUES');
  fBackupSettingsOKButton.OnClick:=BackupSettingsOKButtonClick;
  fBackupSettingsOKButton.Name:='BackupSettingsOKButton';
  fBackupSettingsOKButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fBackupSettingsOKButton);

  fBackupSettingsCancelButton:=TBDButton.Create(fLeft+300,fTop+477,127,27,'CANCEL','CLOSE DIALOG');
  fBackupSettingsCancelButton.OnClick:=BackupSettingsCancelButtonClick;
  fBackupSettingsCancelButton.Name:='BackupSettingsCancelButton';
  fBackupSettingsCancelButton.ZIndex:=MODALDIALOG_ZINDEX+1;
  AddChild(fBackupSettingsCancelButton);
end;

procedure TBDBackupSettingsDialog.Redraw;
begin
  inherited ReDraw;
  MM.Fonts['Black'].OutText(fImage,'BACKUP INTERVAL (SECS)',295,66,1);
end;

procedure TBDBackupSettingsDialog.SaveSettings;
begin
  Settings.BackupFolderMaxFileCount:=BackupFileCountSlider.Position;
  Settings.BackupFolderMaxSize:=BackupMaxSizeSlider.Position;
  Settings.BackupFolderRetentionTime:=BackupDaysSlider.Position;
  Settings.BackupInterval:=BackupIntervalSlider.Position;
  Settings.BackupUnitSize:=fTemp01;
  Settings.CreateBackups:=BackupCheckbox.Selected;
  Settings.LimitBackupFolderMaxFileCount:=BackupLimitFileCount.Selected;
  Settings.LimitBackupFolderMaxSize:=BackupLimitSize.Selected;
  Settings.LimitBackupFolderRetentionTime:=BackupLimitDays.Selected;
end;

procedure TBDBackupSettingsDialog.BackupUnitKBButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  fTemp01:=0;
  fBackupUnitKBButton.Selected:=true;
  fBackupUnitMBButton.Selected:=false;
end;

procedure TBDBackupSettingsDialog.BackupUnitMBButtonClick(Sender:TObject;x,y,buttons:integer);
begin
  fTemp01:=1;
  fBackupUnitMBButton.Selected:=true;
  fBackupUnitKBButton.Selected:=false;
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
  BackupCheckbox.Selected:=Settings.CreateBackups;
  BackupDaysSlider.Position:=Settings.BackupFolderRetentionTime;
  BackupFileCountSlider.Position:=Settings.BackupFolderMaxFileCount;
  BackupIntervalSlider.Position:=Settings.BackupInterval;
  BackupLimitDays.Selected:=Settings.LimitBackupFolderRetentionTime;
  BackupLimitFileCount.Selected:=Settings.LimitBackupFolderMaxFileCount;
  BackupLimitSize.Selected:=Settings.LimitBackupFolderMaxSize;
  BackupMaxSizeSlider.Position:=Settings.BackupFolderMaxSize;
  fBackupUnitKBButton.Selected:=(Settings.BackupUnitSize=0);
  fBackupUnitMBButton.Selected:=(Settings.BackupUnitSize=1);
  fTemp01:=Settings.BackupUnitSize;
end;

end.
