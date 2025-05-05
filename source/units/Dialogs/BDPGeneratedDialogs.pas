{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

// Generated on 2025.05.05

unit BDPGeneratedDialogs;

{$mode Delphi}

interface

uses
  BDPBackupSettingsDialog, BDPBandingDialog, BDPDitherDialog,
  BDPImageResizeDialog, BDPConfigureSoftenDialog, BDPConfigureTintDialog,
  BDPConfigureCircleDialog, BDPConfigureSepDialog;

var
  fBackupSettingsDialog:TBDBackupSettingsDialog;
  fBandingDialog:TBDBandingDialog;
  fDitherDialog:TBDDitherDialog;
  fImageResizeDialog:TBDImageResizeDialog;
  fConfigureSoftenDialog:TBDConfigureSoftenDialog;
  fConfigureTintDialog:TBDConfigureTintDialog;
  fConfigureCircleDialog:TBDConfigureCircleDialog;
  fConfigureSepDialog:TBDConfigureSepDialog;

procedure CreateDialogs;
procedure DestroyDialogs;

implementation

procedure CreateDialogs;
begin
  fBackupSettingsDialog:=TBDBackupSettingsDialog.Create;
  fBandingDialog:=TBDBandingDialog.Create;
  fDitherDialog:=TBDDitherDialog.Create;
  fImageResizeDialog:=TBDImageResizeDialog.Create;
  fConfigureSoftenDialog:=TBDConfigureSoftenDialog.Create;
  fConfigureTintDialog:=TBDConfigureTintDialog.Create;
  fConfigureCircleDialog:=TBDConfigureCircleDialog.Create;
  fConfigureSepDialog:=TBDConfigureSepDialog.Create;
end;

procedure DestroyDialogs;
begin
  fConfigureSepDialog.Free;
  fConfigureCircleDialog.Free;
  fConfigureTintDialog.Free;
  fConfigureSoftenDialog.Free;
  fImageResizeDialog.Free;
  fDitherDialog.Free;
  fBandingDialog.Free;
  fBackupSettingsDialog.Free;
end;

end.
