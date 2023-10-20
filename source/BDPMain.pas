{
  BurdockPaint - Copyright 2023 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of BurdockPaint.

  BurdockPaint is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  BurdockPaint is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  BurdockPaint. If not, see <https://www.gnu.org/licenses/>.
}

unit BDPMain;

{$mode Delphi}{$H+}
{$define LimitFPS}

interface

uses SysUtils, mk_sdl2, Dialogs, FileBackup, BDPMessage, BDPMenu,
  BDPControls, BDPDrawArea, BDPColorEditor,
  BDPMagnifyCELDialog, BDPRotateCELDialog, BDPAboutDialog,
  BDPMessageBox, BDPDitherDialog, BDPConfigureRGradDialog, BDPCoordinateBox,
  BDPGradientEditor;

type

  { TMain }

  TMain=class
    constructor Create(iVersion,iBuildDate:string);
    destructor Destroy; override;
    procedure Run;
  private
    fMainWindow:TWindow;
    fMainMenu:TMainMenu;
    fAboutDialog:TBDAboutDialog;
    fControls:TBDControls;
    fDrawArea:TBDDrawArea;
    fMagnifyDialog:TBDMagnifyCELDialog;
    fRotateDialog:TBDRotateCELDialog;
    fDitherDialog:TBDDitherDialog;
    fConfigureRGradDialog:TBDConfigureRGradDialog;
    fCoordinateBox:TBDCoordinateBox;
    fGradientEditor:TBDGradientEditor;

    fBackup:TFileBackup;
    fOpenCELDialog,
    fOpenProjectDialog:TOpenDialog;
    fSaveCELDialog,
    fSaveProjectDialog:TSaveDialog;
    procedure HideMainControls;
    procedure ShowMainControls;
    function CreateOpenDialog(pName,pTitle,pFilter:string):TOpenDialog;
    function CreateSaveDialog(pName,pTitle,pFilter:string):TSaveDialog;
    procedure ConfigRGradCenter;
    procedure ConfigRGradCenterFinished;
    procedure OpenProject;
    procedure SaveProject;
    procedure SaveClearProject;
    procedure NewImage;
    procedure DuplicateImage;
    procedure RemoveImage;
    procedure ClearImage;
    procedure GetCEL;
    procedure GetCELFinished;
    procedure PutCEL;
    procedure ShowCEL;
    procedure ToggleControls;
    procedure ReleaseCEL;
    procedure FlipCEL(msg:TMessage);
    procedure OpenCEL;
    procedure SaveCEL;
    procedure ColorEditorResp(msg:TMessage);
    procedure SelectColor;

  end;

implementation

uses Classes, SDL2, BDPShared, MKToolbox, MKStream, MKMouse2, Logger,
  MAD4MidLevelUnit, ParametersUnit, BDPKeyMapping, BDPSettings, BDPImage,
  BDPProject;


{ TMain }

constructor TMain.Create(iVersion,iBuildDate:string);
begin
{$IFDEF DEBUG}
  // Set logging level
  Log.SetLogLevel(llAll);
{$ELSE}
// Set logging level
  Log.SetLogLevel(llStatus);
{$ENDIF}

  MKStreamOpener.AddDirectory('.',0);
  SDL_SetHint(SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH, '1');

  fMainWindow:=TWindow.Create(
    SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED,
    WINDOWWIDTH,
    WINDOWHEIGHT,
    Format('Burdock Paint V%s (%s)',[iVersion,replace(iBuildDate,'/','.')]));

  SetFPS(60);

  Log.LogStatus('Loading settings...');
  Settings:=TSettings.Create;
  Settings.LoadFromFile(SETTINGSFILE);


  if (Parameters.Count=2) and (FileExists(Parameters[1])) then begin
    TEMPPROJECTFILE:=Parameters[1];
    PROJECTBASEPATH:=ExtractFileDir(TEMPPROJECTFILE);
    fBackup:=TFileBackup.Create(PROJECTBASEPATH+'\backups');
    fBackup.BackupFolderMaxSize:=Settings.BackupFolderMaxSize;
    fBackup.BackupFolderRetentionTime:=Settings.BackupFolderRetentionTime;
    fBackup.BackupFile(TEMPPROJECTFILE);
  end else begin
    PROJECTBASEPATH:=ExtractFileDir(Parameters[0]);
    fBackup:=TFileBackup.Create(PROJECTBASEPATH+'\backups');
    fBackup.BackupFolderMaxSize:=Settings.BackupFolderMaxSize;
    fBackup.BackupFolderRetentionTime:=Settings.BackupFolderRetentionTime;
  end;

  LoadAssets;

  fMainMenu:=TMainMenu.Create(MenuBin);
  fAboutDialog:=TBDAboutDialog.Create;
  fControls:=TBDControls.Create;
  fDrawArea:=TBDDrawArea.Create;
  ColorEditor:=TBDColorEditor.Create;
  if not Assigned(Project.CELImage) then fMainMenu.DisableCELSubMenusWithActiveCEL;
  // To enable/disable Image/Remove menuitem and set Controls image slider
  MessageQueue.AddMessage(MSG_PROJECTIMAGECOUNTCHANGED,Project.Images.Count);
  fMagnifyDialog:=TBDMagnifyCELDialog.Create;
  fRotateDialog:=TBDRotateCELDialog.Create;
  fDitherDialog:=TBDDitherDialog.Create;
  fConfigureRGradDialog:=TBDConfigureRGradDialog.Create;
  fCoordinateBox:=TBDCoordinateBox.Create(
    WINDOWWIDTH-COORDINATEBOXWIDTH-24,WINDOWHEIGHT-COORDINATEBOXHEIGHT,COORDINATEBOXWIDTH+24,COORDINATEBOXHEIGHT);
  fGradientEditor:=TBDGradientEditor.Create;
  MouseObjects.List;

  fOpenCELDialog:=CreateOpenDialog('OpenCELDialog','Open CEL','All supported file|*.bdc;*.cel;*.png;*.tga|CEL files|*.bdc|Legacy CEL files|*.cel|PNG files|*.png|TGA files|*.tga');
  fOpenProjectDialog:=CreateOpenDialog('OpenProjectDialog','Open Project','Project files|*.bpprj');
  fSaveCELDialog:=CreateSaveDialog('SaveCELDialog','Save CEL','CEL files|*.bdc|PNG files|*.png|TGA files|*.tga');
  fSaveProjectDialog:=CreateSaveDialog('SaveProjectDialog','Save Project','Project files|*.bpprj');
end;

destructor TMain.Destroy;
begin
  if Assigned(fOpenProjectDialog) then fOpenProjectDialog.Free;
  if Assigned(fSaveProjectDialog) then fSaveProjectDialog.Free;
  if Assigned(fSaveCELDialog) then fSaveCELDialog.Free;
  if Assigned(fOpenCELDialog) then fOpenCELDialog.Free;
  if Assigned(fGradientEditor) then fGradientEditor.Free;
  if Assigned(fCoordinateBox) then fCoordinateBox.Free;
  if Assigned(fConfigureRGradDialog) then fConfigureRGradDialog.Free;
  if Assigned(fDitherDialog) then fDitherDialog.Free;
  if Assigned(fRotateDialog) then fRotateDialog.Free;
  if Assigned(fMagnifyDialog) then fMagnifyDialog.Free;
  if Assigned(ColorEditor) then ColorEditor.Free;
  if Assigned(fDrawArea) then fDrawArea.Free;
  if Assigned(fControls) then fControls.Free;
  if Assigned(fAboutDialog) then fAboutDialog.Free;
  if Assigned(fMainMenu) then fMainMenu.Free;
  FreeAssets;
  if Assigned(fBackup) then fBackup.Free;
  if Assigned(Settings) then begin
    Settings.SaveToFile(SETTINGSFILE);
    Settings.Free;
  end;
  if Assigned(fMainWindow) then fMainWindow.Free;
  inherited Destroy;
end;

procedure TMain.Run;
var
  msg:TMessage;
  mres,quit:boolean;
  PrevBackupTick:uint64;
begin
  quit:=false;
  PrevBackupTick:=0;
//  fQuitWindow.Visible:=true;
//  MouseObjects.List;
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,48,12,24,255);
    SDL_RenderClear(fMainWindow.Renderer);

    MouseObjects.Draw;
    InfoBar.Draw;
    MM.Fonts['Pinky'].OutText('FPS: '+st(fps,3,'0'),WINDOWWIDTH-141,3,0);
  {$ifndef LimitFPS} FlipNoLimit; {$else} Flip; {$endif}
    while MessageQueue.HasNewMessage do begin
      msg:=MessageQueue.GetNextMessage;
      mres:=fControls.ProcessMessage(msg);
      if not mres then mres:=ColorEditor.ProcessMessage(msg);
      if not mres and fMainMenu.Visible then mres:=fMainMenu.ProcessMessage(msg);
      if not mres then
        case msg.TypeID of
          MSG_OPENABOUTDIALOG:           fAboutDialog.Show;
          MSG_QUIT:                      quit:=(msg.DataInt=1);
          MSG_OPENDITHERDIALOG:          fDitherDialog.Show;
          MSG_OPENCONFIGURERGRADDIALOG:  fConfigureRGradDialog.Show;
          MSG_CONFIGRGRADCENTER:         ConfigRGradCenter;
          MSG_CONFIGRGRADCENTERFINISHED: ConfigRGradCenterFinished;
          MSG_OPENPROJECT:               OpenProject;
          MSG_SAVEPROJECT:               SaveProject;
          MSG_SAVECLEARPROJECT:          SaveClearProject;
          MSG_NEWIMAGE:                  NewImage;
          MSG_DUPLICATEIMAGE:            DuplicateImage;
          MSG_REMOVEIMAGE:               RemoveImage;
          MSG_CLEARIMAGE:                ClearImage;
          MSG_SETTOOLSMENU:              fMainMenu.SetToolsMenuStates;
          MSG_SETINKSMENU:               fMainMenu.SetInksMenuStates;
          MSG_GETCEL:                    GetCEL;
          MSG_GETCELFINISHED:            GetCELFinished;
          MSG_PUTCEL:                    PutCEL;
          MSG_SHOWCEL:                   ShowCEL;
          MSG_RESTORECONTROLS:           ShowMainControls;
          MSG_TOGGLECONTROLS:            ToggleControls;
          MSG_RELEASECEL:                ReleaseCEL;
          MSG_OPENROTATECELDIALOG:       fRotateDialog.Show;
          MSG_FLIPCEL:                   FlipCEL(msg);
          MSG_OPENMAGNIFYCELDIALOG:      fMagnifyDialog.Show;
          MSG_OPENCEL:                   OpenCEL;
          MSG_SAVECEL:                   SaveCEL;
          MSG_OPENCOLOREDITOR:           ColorEditor.Show;
          MSG_COLOREDITORRESP:           ColorEditorResp(msg);
          MSG_SELECTCOLOR:               SelectColor;
          MSG_ACTIVATEGRADIENTEDITOR:fGradientEditor.Show;
          MSG_GRADIENTEDITORRESPONSE:fGradientEditor.Hide;
        end;
    end;  // while MessageQueue.HasNewMessage
    HandleMessages;
    quit:=quit or Terminate;
//    fControls.SetMouseCoords(fDrawArea.FrameX,fDrawArea.FrameY);
    if keys[KeyMap[KEY_QUIT]] then begin
      keys[KeyMap[KEY_QUIT]]:=false;
      quit:=MessageBox('CONFIRM','EXIT BURDOCK PAINT?','^YES;^NO')=0
    end;
    if GetTickCount64-PrevBackupTick>Settings.BackupIntervalTicks then begin
//      Project.SaveToFile(TEMPPROJECTFILE);
      fBackup.BackupFile(TEMPPROJECTFILE);
      PrevBackupTick:=GetTickCount64;
    end;
  until quit;
end;

procedure TMain.HideMainControls;
begin
  fControls.Hide;
  fMainMenu.Hide;
  InfoBar.Top:=0;
end;

procedure TMain.ShowMainControls;
begin
  fControls.Show;
  fMainMenu.Show;
  InfoBar.Top:=WINDOWHEIGHT-CONTROLSHEIGHT-INFOBARHEIGHT;
end;

function TMain.CreateOpenDialog(pName,pTitle,pFilter:string):TOpenDialog;
begin
  Result:=TOpenDialog.Create(nil);
  with Result do begin
    Filter:=pFilter;
    FilterIndex:=0;
    Name:=pName;
    Title:=pTitle;
    InitialDir:=PROJECTBASEPATH;
  end;
end;

function TMain.CreateSaveDialog(pName,pTitle,pFilter:string):TSaveDialog;
begin
  Result:=TSaveDialog.Create(nil);
  with Result do begin
    Filter:=pFilter;
    FilterIndex:=0;
    Name:=pName;
    Title:=pTitle;
    InitialDir:=PROJECTBASEPATH;
  end;
end;

procedure TMain.ConfigRGradCenter;
begin
  HideMainControls;
  ActiveTool:=Tools.ItemByName['CONFRG'];
  ActiveTool.Initialize;
end;

procedure TMain.ConfigRGradCenterFinished;
begin
  ShowMainControls;
  Settings.RGradCenterX:=Settings.TempRGradCenterX;
  Settings.RGradCenterY:=Settings.TempRGradCenterY;
  fConfigureRGradDialog.Show;
end;

procedure TMain.OpenProject;
begin
  if fOpenProjectDialog.Execute then begin
    try
      Project.Free;
      Project:=TBDProject.CreateFromFile(fOpenProjectDialog.FileName);
      MessageQueue.AddMessage(MSG_PROJECTIMAGECOUNTCHANGED,Project.Images.Count);
    except
      on e:Exception do begin
        Log.LogError(e.message);
        MessageBox('ERROR',e.Message);
      end;
    end;
  end;
end;

procedure TMain.SaveProject;
begin
  if fSaveProjectDialog.Execute then begin
    try
      Project.SaveToFile(fSaveProjectDialog.FileName);
      MessageBox('INFORMATION','Project saved successfully.');
    except
      on e:Exception do begin
        Log.LogError(e.message);
        MessageBox('ERROR',e.Message);
      end;
    end;
  end;
end;

procedure TMain.SaveClearProject;
begin
  if fSaveProjectDialog.Execute then begin
    try
      Project.Clean;
      Project.SaveToFile(fSaveProjectDialog.FileName);
      MessageBox('INFORMATION','Project saved successfully.');
    except
      on e:Exception do begin
        Log.LogError(e.message);
        MessageBox('ERROR',e.Message);
      end;
    end;
  end;
end;

procedure TMain.NewImage;
begin
  case MessageBox('NEW IMAGE','Where to place new image?','First;Last;Insert;Cancel') of
    0:begin
        Project.Images.Insert(0,TBDExtendedImage.Create);
        Project.CurrentImageIndex:=0;
        MessageQueue.AddMessage(MSG_PROJECTIMAGECOUNTCHANGED,Project.Images.Count);
      end;
    1:begin
        Project.Images.Add(TBDExtendedImage.Create);
        Project.CurrentImageIndex:=Project.Images.Count-1;
        MessageQueue.AddMessage(MSG_PROJECTIMAGECOUNTCHANGED,Project.Images.Count);
      end;
    2:begin
        Project.Images.Insert(Project.CurrentImageIndex,TBDExtendedImage.Create);
        MessageQueue.AddMessage(MSG_PROJECTIMAGECOUNTCHANGED,Project.Images.Count);
      end;
  end;
end;

procedure TMain.DuplicateImage;
var Xs:TMemoryStream;
begin
  case MessageBox('DUPLICATE IMAGE','Where to place duplicated image?','First;Last;Insert;Cancel') of
    0:begin
        Xs:=TMemoryStream.Create;
        try
          Project.CurrentImage.SaveToStream(Xs);
          Xs.Position:=0;
          Project.Images.Insert(0,TBDExtendedImage.CreateFromStream(Xs));
        finally
          Xs.Free;
        end;
        Project.CurrentImageIndex:=0;
        MessageQueue.AddMessage(MSG_PROJECTIMAGECOUNTCHANGED,Project.Images.Count);
      end;
    1:begin
        Xs:=TMemoryStream.Create;
        try
          Project.CurrentImage.SaveToStream(Xs);
          Xs.Position:=0;
          Project.Images.Add(TBDExtendedImage.CreateFromStream(Xs));
        finally
          Xs.Free
        end;
        Project.CurrentImageIndex:=Project.Images.Count-1;
        MessageQueue.AddMessage(MSG_PROJECTIMAGECOUNTCHANGED,Project.Images.Count);
      end;
    2:begin
        Xs:=TMemoryStream.Create;
        try
          Project.CurrentImage.SaveToStream(Xs);
          Xs.Position:=0;
          Project.Images.Insert(Project.CurrentImageIndex,TBDExtendedImage.CreateFromStream(Xs));
        finally
          Xs.Free;
        end;
        MessageQueue.AddMessage(MSG_PROJECTIMAGECOUNTCHANGED,Project.Images.Count);
      end;
  end;
end;

procedure TMain.RemoveImage;
begin
  if (Project.Images.Count>1) then begin
    if MessageBox('REMOVE IMAGE','Really remove image from project?','^Yes;^No')=0 then begin
      Project.Images.Delete(Project.CurrentImageIndex);
      if Project.CurrentImageIndex>=Project.Images.Count then
        Project.CurrentImageIndex:=Project.Images.Count-1;
      MessageQueue.AddMessage(MSG_PROJECTIMAGECOUNTCHANGED,Project.Images.Count);
    end;
  end;
end;

procedure TMain.ClearImage;
begin
  Project.CurrentExtImage.ImageUndo.AddImageUndo(0,0,Project.CurrentImage.Width,Project.CurrentImage.Height);
  Project.CurrentImage.Bar(0,0,Project.CurrentImage.Width,Project.CurrentImage.Height,$FF000000);
  Project.CurrentExtImage.ImageUndo.AddImageRedoToLastUndo(0,0,Project.CurrentImage.Width,Project.CurrentImage.Height);
end;

procedure TMain.GetCEL;
var mx,my:integer;
begin
  HideMainControls;
  SDL_ShowCursor(SDL_DISABLE);
  ActiveTool:=Tools.ItemByName['GETCEL'];
  SDL_GetMouseState(@mx,@my);
  ActiveTool.Move(fDrawArea.MouseXToFrame(mx),fDrawArea.MouseYToFrame(my));
end;

procedure TMain.PutCEL;
var mx,my:integer;
begin
  HideMainControls;
  ActiveTool:=Tools.ItemByName['PUTCEL'];
  ActiveTool.Initialize;
  SDL_GetMouseState(@mx,@my);
  ActiveTool.Move(fDrawArea.MouseXToFrame(mx),fDrawArea.MouseYToFrame(my));
end;

procedure TMain.GetCELFinished;
begin
  fMainMenu.EnableCELSubMenusWithActiveCEL;
  MessageQueue.AddMessage(MSG_RESTORECONTROLS);
end;

procedure TMain.ShowCEL;
begin
  HideMainControls;
  ActiveTool:=Tools.ItemByName['SHOWCEL'];
  ActiveTool.Initialize;
end;

procedure TMain.ToggleControls;
begin
//  if not fPaletteEditor.Visible then
    if fControls.Visible then
      HideMainControls
    else
      ShowMainControls;
end;

procedure TMain.ReleaseCEL;
begin
  if Assigned(Project.CELImage) then begin
    Project.CELImage.Free;
    Project.CELImage:=nil;
  end;
  fMainMenu.DisableCELSubMenusWithActiveCEL;
end;

procedure TMain.FlipCEL(msg:TMessage);
begin
  if msg.DataInt=0 then Project.CELImage.FlipV
  else if msg.DataInt=1 then Project.CelImage.FlipH
  else raise Exception.Create('Invalid FLIPCEL message parameter!');
  MessageQueue.AddMessage(MSG_SHOWCEL);
end;

procedure TMain.OpenCEL;
begin
  if fOpenCELDialog.Execute then begin
    if not assigned(Project.CELImage) then Project.CELImage:=TBDRegion.Create(16,16);
    if UpperCase(ExtractFileExt(fOpenCELDialog.FileName))='.BDC' then
      Project.CELImage.LoadFromFile(fOpenCELDialog.FileName)
    else
      Project.CELImage.ReadFile(fOpenCELDialog.FileName);
    Project.CELImage.Left:=0;
    Project.CELImage.Top:=0;
    fMainMenu.EnableCELSubMenusWithActiveCEL;
    MessageQueue.AddMessage(MSG_SHOWCEL);
  end;
end;

procedure TMain.SaveCEL;
begin
  if fSaveCELDialog.Execute then begin
    try
      if uppercase(ExtractFileExt(fSaveCELDialog.FileName))='.BDC' then
        Project.CELImage.SaveToFile(fSaveCELDialog.FileName)
      else
        Project.CELImage.WriteFile(fSaveCELDialog.FileName,copy(ExtractFileExt(fSaveCELDialog.FileName),2));
    except
      on e:Exception do
        Log.LogError(e.message);
    end;
    MessageQueue.AddMessage(MSG_SHOWCEL);
  end;
end;

procedure TMain.ColorEditorResp(msg: TMessage);
begin
  case msg.DataInt of
    PARM_COL_SELECTOR_LEFT,
    PARM_COL_SELECTOR_MAIN,
    PARM_COL_SELECTOR_RIGHT:fControls.SetColor(msg.DataInt,msg.DataUInt32);
    PARM_COL_GRADEDIT_LEFT,
    PARM_COL_GRADEDIT_RIGHT,
    PARM_COL_GRADEDIT_COLOR3,
    PARM_COL_GRADEDIT_COLOR4,
    PARM_COL_GRADEDIT_COLOR5:begin
      fGradientEditor.SetColor(msg.DataInt,msg.DataUInt32);
      fGradientEditor.Show;
    end;
  end;
  InfoBar.Top:=WINDOWHEIGHT-CONTROLSHEIGHT-INFOBARHEIGHT;
end;

procedure TMain.SelectColor;
var mx,my:integer;
begin
  SDL_GetMouseState(@mx,@my);
  mx:=fDrawArea.MouseXToFrame(mx);
  my:=fDrawArea.MouseYToFrame(my);
  if (mx>=0) and (mx<Project.CurrentImage.Width) and (my>=0) and (my<Project.CurrentImage.Height) then
    Settings.ActiveColor:=Project.CurrentImage.GetPixel(mx,my);
end;


end.

