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

uses SysUtils, mk_sdl2, Dialogs, BDPControls, BDPDrawArea,
  BDPModalDialogs, BDPPaletteEditor, BDPMenu, BDPMessage;

type

  { TMain }

  TMain=class
    constructor Create(iVersion,iBuildDate:string);
    destructor Destroy; override;
    procedure Run;
  private
    fMainWindow:TWindow;
    fControls:TBDControls;
    fDrawArea:TBDDrawArea;
    fPaletteEditor:TBDPaletteEditor;
    fSplashScreen:TBDAboutDialog;
    fMainMenu:TMainMenu;
    fMagnifyDialog:TBDMagnifyCELDialog;
    fRotateDialog:TBDRotateCELDialog;
    fDitherDialog:TBDDitherDialog;
    fSelectColorClusterDialog:TBDSelectColorClusterDialog;
    fConfigureRGradDialog:TBDConfigureRGradDialog;
    fOpenCELDialog,
    fOpenProjectDialog:TOpenDialog;
    fSaveCELDialog,
    fSaveProjectDialog,
    fExportCELDialog:TSaveDialog;
    procedure HideMainControls;
    procedure ShowMainControls;
    function CreateOpenDialog(pName,pTitle,pFilter:string):TOpenDialog;
    function CreateSaveDialog(pName,pTitle,pFilter:string):TSaveDialog;
    procedure ToggleControls;
    procedure ActivatePaletteEditor;
    procedure DeactivatePaletteEditor;
    procedure ShowCEL;
    procedure GetCELFinished;
    procedure SelectColor;
    procedure OpenProject;
    procedure SaveProject;
    procedure SaveClearProject;
    procedure NewImage;
    procedure DuplicateImage;
    procedure RemoveImage;
    procedure ClearImage;
    procedure GetCEL;
    procedure PutCEL;
    procedure ReleaseCEL;
    procedure FlipCEL(msg:TMessage);
    procedure OpenCEL;
    procedure SaveCEL;
    procedure ExportCEL;
    procedure RampCluster;
    procedure OpenColorClusterDialog(msg:TMessage);
    procedure ColorClusterDialogResp(msg:TMessage);
    procedure ConfigRGradCenter;
    procedure ConfigRGradCenterFinished;
  end;

implementation

uses Classes, SDL2, BDPShared, MKToolbox, MKStream, MKMouse2, Logger,
  BDPKeyMapping, BDPTools, MAD4MidLevelUnit, BDPImage,
  BDPProject, ParametersUnit;


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

  if (Parameters.Count=2) and (FileExists(Parameters[1])) then begin
    TEMPPROJECTFILE:=Parameters[1];
    BackupFile(TEMPPROJECTFILE);
    PROJECTBASEPATH:=ExtractFileDir(TEMPPROJECTFILE);
  end else PROJECTBASEPATH:=ExtractFileDir(Parameters[0]);

  LoadAssets;

  fDrawArea:=TBDDrawArea.Create;
  fControls:=TBDControls.Create;
  fPaletteEditor:=TBDPaletteEditor.Create;
  fSplashScreen:=TBDAboutDialog.Create;
  fMainMenu:=TMainMenu.Create(MenuBin);
  if not Assigned(Project.CELImage) then fMainMenu.DisableCELSubMenusWithActiveCEL;
  // To enable/disable Image/Remove menuitem.
  fMainMenu.ProcessMessage(TMessage.Init(MSG_PROJECTIMAGECOUNTCHANGED,Project.Images.Count));
  fMagnifyDialog:=TBDMagnifyCELDialog.Create;
  fRotateDialog:=TBDRotateCELDialog.Create;
  fDitherDialog:=TBDDitherDialog.Create;
  fSelectColorClusterDialog:=TBDSelectColorClusterDialog.Create;
  fConfigureRGradDialog:=TBDConfigureRGradDialog.Create;
  MouseObjects.List;

  fOpenCELDialog:=CreateOpenDialog('OpenCELDialog','Open CEL','CEL files|*.bdc|Legacy CEL files|*.cel');
  fOpenProjectDialog:=CreateOpenDialog('OpenProjectDialog','Open Project','Project files|*.bpprj');
  fSaveCELDialog:=CreateSaveDialog('SaveCELDialog','Save CEL','CEL files|*.bdc');
  fSaveProjectDialog:=CreateSaveDialog('SaveProjectDialog','Save Project','Project files|*.bpprj');
  fExportCELDialog:=CreateSaveDialog('ExportCELDialog','Export CEL to ...','PNG files|*.png|TGA files|*.tga');
end;

destructor TMain.Destroy;
begin
  if Assigned(fExportCELDialog) then FreeAndNil(fExportCELDialog);
  if Assigned(fOpenProjectDialog) then FreeAndNil(fOpenProjectDialog);
  if Assigned(fSaveProjectDialog) then FreeAndNil(fSaveProjectDialog);
  if Assigned(fSaveCELDialog) then FreeAndNil(fSaveCELDialog);
  if Assigned(fOpenCELDialog) then FreeAndNil(fOpenCELDialog);
  if Assigned(fConfigureRGradDialog) then fConfigureRGradDialog.Free;
  if Assigned(fSelectColorClusterDialog) then fSelectColorClusterDialog.Free;
  if Assigned(fDitherDialog) then FreeAndNil(fDitherDialog);
  if Assigned(fRotateDialog) then FreeAndNil(fRotateDialog);
  if Assigned(fMagnifyDialog) then FreeAndNil(fMagnifyDialog);
  if Assigned(fMainMenu) then FreeAndNil(fMainMenu);
  if Assigned(fSplashScreen) then FreeAndNil(fSplashScreen);
  if Assigned(fPaletteEditor) then FreeAndNil(fPaletteEditor);
  if Assigned(fControls) then FreeAndNil(fControls);
  if Assigned(fDrawArea) then FreeAndNil(fDrawArea);
  FreeAssets;
  if Assigned(fMainWindow) then FreeAndNil(fMainWindow);
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
//    MM.Fonts['Pinky'].OutText('LOI: '+st(MouseObjects.LastOverIndex,3,'0'),WINDOWWIDTH-282-16,3,0);
  {$ifndef LimitFPS} FlipNoLimit; {$else} Flip; {$endif}
    while MessageQueue.HasNewMessage do begin
      msg:=MessageQueue.GetNextMessage;
      mres:=false;
      if fControls.Visible then mres:=fControls.ProcessMessage(msg);
      if not mres and fPaletteEditor.Visible then mres:=fPaletteEditor.ProcessMessage(msg);
      if not mres and fMainMenu.Visible then mres:=fMainMenu.ProcessMessage(msg);
      if not mres then
        case msg.TypeID of
          MSG_QUIT:                      quit:=(msg.DataInt=1);
          MSG_TOGGLECONTROLS:            ToggleControls;
          MSG_ACTIVATEPALETTEEDITOR:     ActivatePaletteEditor;
          MSG_DEACTIVATEPALETTEEDITOR:   DeactivatePaletteEditor;
          MSG_OPENDITHERDIALOG:          fDitherDialog.Show;
          MSG_SETTOOLSMENU:              fMainMenu.SetToolsMenuStates;
          MSG_SETINKSMENU:               fMainMenu.SetInksMenuStates;
          MSG_RESTORECONTROLS:           ShowMainControls;
          MSG_SHOWCEL:                   ShowCEL;
          MSG_GETCELFINISHED:            GetCELFinished;
          MSG_SELECTCOLOR:               SelectColor;
          MSG_OPENPROJECT:               OpenProject;
          MSG_SAVEPROJECT:               SaveProject;
          MSG_SAVECLEARPROJECT:          SaveClearProject;
          MSG_NEWIMAGE:                  NewImage;
          MSG_DUPLICATEIMAGE:            DuplicateImage;
          MSG_REMOVEIMAGE:               RemoveImage;
          MSG_CLEARIMAGE:                ClearImage;
          MSG_GETCEL:                    GetCEL;
          MSG_PUTCEL:                    PutCEL;
          MSG_RELEASECEL:                ReleaseCEL;
          MSG_OPENROTATECELDIALOG:       fRotateDialog.Show;
          MSG_FLIPCEL:                   FlipCEL(msg);
          MSG_OPENMAGNIFYCELDIALOG:      fMagnifyDialog.Show;
          MSG_OPENCEL:                   OpenCEL;
          MSG_SAVECEL:                   SaveCEL;
          MSG_EXPORTCEL:                 ExportCEL;
          MSG_RAMPCLUSTER:               RampCluster;
          MSG_OPENABOUTDIALOG:           fSplashScreen.Show;
          MSG_OPENCOLORCLUSTERDIALOG:    OpenColorClusterDialog(msg);
          MSG_COLORCLUSTERDIALOGRESP:    ColorClusterDialogResp(msg);
          MSG_OPENCONFIGURERGRADDIALOG:  fConfigureRGradDialog.Show;
          MSG_CONFIGRGRADCENTER:         ConfigRGradCenter;
          MSG_CONFIGRGRADCENTERFINISHED: ConfigRGradCenterFinished;
        end;
    end;  // while MessageQueue.HasNewMessage
    HandleMessages;
    quit:=quit or Terminate;
    fControls.SetMouseCoords(fDrawArea.FrameX,fDrawArea.FrameY);
    if keys[KeyMap[KEY_QUIT]] then begin
      keys[KeyMap[KEY_QUIT]]:=false;
      quit:=MessageBox('EXIT BURDOCK PAINT?','^YES;^NO')=0
    end;
    if GetTickCount64-PrevBackupTick>Settings.BackupIntervalTicks then begin
      Project.SaveToFile(TEMPPROJECTFILE);
      BackupFile(TEMPPROJECTFILE);
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

procedure TMain.ToggleControls;
begin
  if not fPaletteEditor.Visible then
    if fControls.Visible then
      HideMainControls
    else
      ShowMainControls;
end;

procedure TMain.ActivatePaletteEditor;
begin
  fControls.Hide;
  fPaletteEditor.Show;
  fMainMenu.DisableItem('PROJECT');
  fMainMenu.DisableItem('IMAGE');
  fMainMenu.DisableItem('CEL');
  fMainMenu.DisableItem('TOOLS');
  fMainMenu.DisableItem('INKS');
  fMainMenu.EnableItem('CLUSTER');
  InfoBar.Top:=WINDOWHEIGHT-PALETTEEDITORHEIGHT-INFOBARHEIGHT;
end;

procedure TMain.DeactivatePaletteEditor;
begin
  fPaletteEditor.Hide;
  fControls.Show;
  fMainMenu.EnableItem('PROJECT');
  fMainMenu.EnableItem('IMAGE');
  fMainMenu.EnableItem('CEL');
  fMainMenu.EnableItem('TOOLS');
  fMainMenu.EnableItem('INKS');
  fMainMenu.DisableItem('CLUSTER');
  InfoBar.Top:=WINDOWHEIGHT-CONTROLSHEIGHT-INFOBARHEIGHT;
end;

procedure TMain.ShowCEL;
begin
  HideMainControls;
  ActiveTool:=Tools.ItemByName['SHOWCEL'];
  ActiveTool.Initialize;
end;

procedure TMain.GetCELFinished;
begin
  fMainMenu.EnableCELSubMenusWithActiveCEL;
  MessageQueue.AddMessage(MSG_RESTORECONTROLS);
end;

procedure TMain.SelectColor;
var mx,my:integer;
begin
  SDL_GetMouseState(@mx,@my);
  mx:=fDrawArea.MouseXToFrame(mx);
  my:=fDrawArea.MouseYToFrame(my);
  if (mx>=0) and (mx<Project.CurrentImage.Width) and (my>=0) and (my<Project.CurrentImage.Height) then
    Settings.ActiveColorIndex:=Project.CurrentImage.GetPixel(mx,my);
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
        MessageBox(e.Message);
      end;
    end;
  end;
end;

procedure TMain.SaveProject;
begin
  if fSaveProjectDialog.Execute then begin
    try
      Project.SaveToFile(fSaveProjectDialog.FileName);
      MessageBox('Project saved successfully.');
    except
      on e:Exception do begin
        Log.LogError(e.message);
        MessageBox(e.Message);
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
      MessageBox('Project saved successfully.');
    except
      on e:Exception do begin
        Log.LogError(e.message);
        MessageBox(e.Message);
      end;
    end;
  end;
end;

procedure TMain.NewImage;
begin
  case MessageBox('Where to place new image?','First;Last;Insert;Cancel') of
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
  case MessageBox('Where to place duplicated image?','First;Last;Insert;Cancel') of
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
    if MessageBox('Really remove image from project?','^Yes;^No')=0 then begin
      Project.Images.Delete(Project.CurrentImageIndex);
      if Project.CurrentImageIndex>=Project.Images.Count then
        Project.CurrentImageIndex:=Project.Images.Count-1;
      MessageQueue.AddMessage(MSG_PROJECTIMAGECOUNTCHANGED,Project.Images.Count);
    end;
  end;
end;

procedure TMain.ClearImage;
begin
  Project.CurrentImage.ImageUndo.AddImageUndo(0,0,Project.CurrentImage.Width,Project.CurrentImage.Height);
  Project.CurrentImage.Bar(0,0,Project.CurrentImage.Width,Project.CurrentImage.Height,Settings.SelectedColors[0]);
  Project.CurrentImage.ImageUndo.AddImageRedoToLastUndo(0,0,Project.CurrentImage.Width,Project.CurrentImage.Height);
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
    if not assigned(Project.CELImage) then Project.CELImage:=TBDImage.Create(16,16);
    if UpperCase(ExtractFileExt(fOpenCELDialog.FileName))='.CEL' then
      Project.CELImage.ImportCEL(fOpenCELDialog.FileName)
    else
      Project.CELImage.LoadFromFile(fOpenCELDialog.FileName);
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
      Project.CELImage.SaveToFile(fSaveCELDialog.FileName);
    except
      on e:Exception do
        Log.LogError(e.message);
    end;
    MessageQueue.AddMessage(MSG_SHOWCEL);
  end;
end;

procedure TMain.ExportCEL;
begin
  if fExportCELDialog.Execute then begin
    try
      Project.CELImage.ExportTo(fExportCELDialog.FileName,copy(ExtractFileExt(fExportCELDialog.FileName),2));
    except
      on e:Exception do
        Log.LogError(e.message);
    end;
    MessageQueue.AddMessage(MSG_SHOWCEL);
  end;
end;

procedure TMain.RampCluster;
begin
  with Project.CurrentImage do begin
    with ColorClusters.ActiveColorCluster do
      PaletteUndo.AddPaletteUndo(
        min(StartIndex,EndIndex),
        max(StartIndex,EndIndex)-min(StartIndex,EndIndex)+1
      );
    Project.CurrentImage.Palette.Ramp(Project.CurrentImage.ColorClusters.ActiveColorCluster);
    PaletteUndo.AddPaletteRedoToLastUndo;
  end;
  MessageQueue.AddMessage(MSG_PALETTEPICKEDCOLOR);
end;

procedure TMain.OpenColorClusterDialog(msg:TMessage);
begin
  fSelectColorClusterDialog.SetPositionAndWidth(msg.DataInt and $07ff,(msg.DataInt and $003ff800)>>11,(msg.DataInt and $ffc00000)>>22);
  fSelectColorClusterDialog.Show;
end;

procedure TMain.ColorClusterDialogResp(msg:TMessage);
begin
  if msg.DataInt=-2 then begin
    fSelectColorClusterDialog.Refresh;
  end else begin
    if msg.DataInt>-1 then begin
      Project.CurrentImage.ColorClusters.ActiveIndex:=msg.DataInt;
      MessageQueue.AddMessage(MSG_ACTIVECOLORCLUSTERCHANGED);
    end;
    fSelectColorClusterDialog.Hide;
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

end.

