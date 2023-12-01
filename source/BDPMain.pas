{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPMain;

{$mode Delphi}{$H+}
{$define LimitFPS}

interface

uses SysUtils, mk_sdl2, Dialogs, FileBackup, BDPMessage, BDPMenu,
  BDPControls, BDPDrawArea, BDPColorEditor, BDPMagnifyCELDialog,
  BDPRotateCELDialog, BDPAboutDialog, BDPMessageBox, BDPDitherDialog,
  BDPConfigureRGradDialog, BDPCoordinateBox, BDPGradientEditor, BDPColorPalette2,
  BDPGradientSelector, BDPConfigureTintDialog;

type

  { TMain }

  TMain=class
    constructor Create(iVersion,iBuildDate:string);
    destructor Destroy; override;
    procedure Run;
  private
    fQuit:boolean;
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
    fColorEditor:TBDColorEditor;
    fGradientEditor:TBDGradientEditor;
    fColorPalette:TBDColorPalette2;
    fGradientSelector:TBDGradientSelector;
    fConfigureTintDialog:TBDConfigureTintDialog;

    fBackup:TFileBackup;
    fOpenCELDialog,
    fOpenProjectDialog:TOpenDialog;
    fSaveCELDialog,
    fSaveProjectDialog:TSaveDialog;
    procedure ProcessMessages;
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
    procedure GradientEditorResp(msg:TMessage);

  end;

implementation

uses Classes, SDL2, BDPShared, MKToolbox, MKStream, MKMouse2, Logger,
  MAD4MidLevelUnit, ParametersUnit, BDPKeyMapping, BDPSettings, BDPRegion,
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
    fBackup.BackupFolderFileCount:=Settings.BackupFolderMaxFileCount;
    fBackup.BackupFile(TEMPPROJECTFILE);
  end else begin
    PROJECTBASEPATH:=ExtractFileDir(Parameters[0]);
    fBackup:=TFileBackup.Create(PROJECTBASEPATH+'\backups');
    fBackup.BackupFolderMaxSize:=Settings.BackupFolderMaxSize;
    fBackup.BackupFolderRetentionTime:=Settings.BackupFolderRetentionTime;
    fBackup.BackupFolderFileCount:=Settings.BackupFolderMaxFileCount;
  end;

  Log.Trace('Before assets: '+inttostr(GetHeapStatus.TotalAllocated));

  LoadAssets;

  Log.Trace('After assets: '+inttostr(GetHeapStatus.TotalAllocated));
  fMainMenu:=TMainMenu.Create(MenuBin);
  Log.Trace('After MainMenu: '+inttostr(GetHeapStatus.TotalAllocated));
  fAboutDialog:=TBDAboutDialog.Create;
  Log.Trace('After AboutDialog: '+inttostr(GetHeapStatus.TotalAllocated));
  fControls:=TBDControls.Create;
  Log.Trace('After Controls: '+inttostr(GetHeapStatus.TotalAllocated));
  fDrawArea:=TBDDrawArea.Create;
  Log.Trace('After DrawArea: '+inttostr(GetHeapStatus.TotalAllocated));
  fColorEditor:=TBDColorEditor.Create;
  Log.Trace('After ColorEditor: '+inttostr(GetHeapStatus.TotalAllocated));
  if not Assigned(Project.CELImage) then fMainMenu.DisableCELSubMenusWithActiveCEL;
  // To enable/disable Image/Remove menuitem and set Controls image slider
  MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED,Project.Images.Count);
  fMagnifyDialog:=TBDMagnifyCELDialog.Create;
  Log.Trace('After MagnifyDialog: '+inttostr(GetHeapStatus.TotalAllocated));
  fRotateDialog:=TBDRotateCELDialog.Create;
  Log.Trace('After RotateDialog: '+inttostr(GetHeapStatus.TotalAllocated));
  fDitherDialog:=TBDDitherDialog.Create;
  Log.Trace('After DitherDialog: '+inttostr(GetHeapStatus.TotalAllocated));
  fConfigureRGradDialog:=TBDConfigureRGradDialog.Create;
  Log.Trace('After ConfigureRGradDialog: '+inttostr(GetHeapStatus.TotalAllocated));
  fCoordinateBox:=TBDCoordinateBox.Create(
    WINDOWWIDTH-COORDINATEBOXWIDTH-24,WINDOWHEIGHT-COORDINATEBOXHEIGHT,COORDINATEBOXWIDTH+24,COORDINATEBOXHEIGHT);
  Log.Trace('After CoordinateBox: '+inttostr(GetHeapStatus.TotalAllocated));
  fGradientEditor:=TBDGradientEditor.Create;
  Log.Trace('After GradientEditor: '+inttostr(GetHeapStatus.TotalAllocated));
  fColorPalette:=TBDColorPalette2.Create(WINDOWWIDTH-70,TOPMENUHEIGHT,70,WINDOWHEIGHT-TOPMENUHEIGHT-CONTROLSHEIGHT);
  Log.Trace('After ColorPalette: '+inttostr(GetHeapStatus.TotalAllocated));
  fGradientSelector:=TBDGradientSelector.Create;
  Log.Trace('After GradientSelector: '+inttostr(GetHeapStatus.TotalAllocated));
  fConfigureTintDialog:=TBDConfigureTintDialog.Create;
  Log.Trace('After ConfigureTintDialog: '+inttostr(GetHeapStatus.TotalAllocated));
  MouseObjects.List;

  fOpenCELDialog:=CreateOpenDialog('OpenCELDialog','Open CEL','All supported file|*.bdc;*.cel;*.png;*.tga;*.bmp|CEL files|*.bdc|Legacy CEL files|*.cel|PNG files|*.png|TGA files|*.tga|BMP files|*.bmp');
  fOpenProjectDialog:=CreateOpenDialog('OpenProjectDialog','Open Project','Project files|*.bpprj');
  fSaveCELDialog:=CreateSaveDialog('SaveCELDialog','Save CEL','PNG files|*.png|TGA files|*.tga|BMP files|*.bmp');
  fSaveProjectDialog:=CreateSaveDialog('SaveProjectDialog','Save Project','Project files|*.bpprj');
  Log.Trace('After FCL dialogs: '+inttostr(GetHeapStatus.TotalAllocated));
  fQuit:=false;
end;

destructor TMain.Destroy;
begin
  if Assigned(fOpenProjectDialog) then fOpenProjectDialog.Free;
  if Assigned(fSaveProjectDialog) then fSaveProjectDialog.Free;
  if Assigned(fSaveCELDialog) then fSaveCELDialog.Free;
  if Assigned(fOpenCELDialog) then fOpenCELDialog.Free;
  if Assigned(fConfigureTintDialog) then fConfigureTintDialog.Free;
  if Assigned(fGradientSelector) then fGradientSelector.Free;
  if Assigned(fColorPalette) then fColorPalette.Free;
  if Assigned(fGradientEditor) then fGradientEditor.Free;
  if Assigned(fCoordinateBox) then fCoordinateBox.Free;
  if Assigned(fConfigureRGradDialog) then fConfigureRGradDialog.Free;
  if Assigned(fDitherDialog) then fDitherDialog.Free;
  if Assigned(fRotateDialog) then fRotateDialog.Free;
  if Assigned(fMagnifyDialog) then fMagnifyDialog.Free;
  if Assigned(fColorEditor) then fColorEditor.Free;
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
  PrevBackupTick:uint64;
begin
  PrevBackupTick:=0;
  ProcessMessages;
//  fQuitWindow.Visible:=true;
//  MouseObjects.List;
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,48,12,24,255);
    SDL_RenderClear(fMainWindow.Renderer);

    MouseObjects.Draw;
    InfoBar.Draw;
    MM.Fonts['DarkRed'].OutText('FPS: '+st(fps,3,'0'),WINDOWWIDTH-141,3,0);
    {$ifndef LimitFPS} FlipNoLimit; {$else} Flip; {$endif}
    ProcessMessages;
    HandleMessages;
    fQuit:=fQuit or Terminate;
    if keys[KeyMap[KEY_QUIT]] then begin
      keys[KeyMap[KEY_QUIT]]:=false;
      fQuit:=MessageBox('CONFIRM','EXIT BURDOCK PAINT?','^YES;^NO')=0
    end;
    if GetTickCount64-PrevBackupTick>Settings.BackupIntervalTicks then begin
      Project.SaveToFile(TEMPPROJECTFILE);
      fBackup.BackupFile(TEMPPROJECTFILE);
      PrevBackupTick:=GetTickCount64;
    end;
  until fQuit;
end;

procedure TMain.ProcessMessages;
var msg:TMessage;mres:boolean;
begin
  while MessageQueue.HasNewMessage do begin
    msg:=MessageQueue.GetNextMessage;
    mres:=fControls.ProcessMessage(msg);
    if not mres then mres:=fDrawArea.ProcessMessage(msg);
    if not mres then mres:=fColorEditor.ProcessMessage(msg);
    if not mres then mres:=fGradientEditor.ProcessMessage(msg);
    if not mres and fMainMenu.Visible then mres:=fMainMenu.ProcessMessage(msg);
    if not mres then
      case msg.TypeID of
        MSG_OPENABOUTDIALOG:           fAboutDialog.Show;
        MSG_QUIT:                      fQuit:=(msg.DataInt=1);
        MSG_OPENDITHERDIALOG:          fDitherDialog.Show;
        MSG_OPENCONFIGURERGRADDIALOG:  fConfigureRGradDialog.Show;
        MSG_CONFIGRGRADCENTER:         ConfigRGradCenter;
        MSG_CONFIGRGRADCENTERFINISHED: ConfigRGradCenterFinished;
        MSG_OPENCONFIGURETINTDIALOG:   fConfigureTintDialog.Show;
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
        MSG_OPENCOLOREDITOR:           fColorEditor.Show;
        MSG_COLOREDITORRESP:           ColorEditorResp(msg);
        MSG_SELECTCOLOR:               SelectColor;
        MSG_ACTIVATEGRADIENTEDITOR:    fGradientEditor.Show;
        MSG_GRADIENTEDITORRESPONSE:    GradientEditorResp(msg);
        MSG_ACTIVEIMAGECHANGED:        fColorPalette.Refresh;
        MSG_ACTIVATEGRADIENTSELECTOR:  fGradientSelector.Show;
        MSG_PALETTECHANGED:            fColorPalette.Refresh;
      end;
  end;  // while MessageQueue.HasNewMessage
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
      MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED,Project.Images.Count);
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
        Project.Images.Insert(0,TBDImage.Create);
        Project.CurrentImageIndex:=0;
        MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED,Project.Images.Count);
      end;
    1:begin
        Project.Images.Add(TBDImage.Create);
        Project.CurrentImageIndex:=Project.Images.Count-1;
        MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED,Project.Images.Count);
      end;
    2:begin
        Project.Images.Insert(Project.CurrentImageIndex,TBDImage.Create);
        MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED,Project.Images.Count);
      end;
  end;
end;

procedure TMain.DuplicateImage;
var i:integer;Xs:TMemoryStream;
begin
  i:=MessageBox('DUPLICATE IMAGE','Where to place duplicated image?','First;Last;Insert;Cancel');
  if i in [0..2] then begin
    Xs:=TMemoryStream.Create;
    try
      Project.CurrentImage.SaveToStream(Xs);
      Xs.Position:=0;
      case i of
        0:begin
            Project.Images.Insert(0,TBDImage.CreateFromStream(Xs));
            Project.CurrentImageIndex:=0;
          end;
        1:begin
            Project.Images.Add(TBDImage.CreateFromStream(Xs));
            Project.CurrentImageIndex:=Project.Images.Count-1;
          end;
        2:begin
            Project.Images.Insert(Project.CurrentImageIndex,TBDImage.CreateFromStream(Xs));
          end;
      end;
    finally
      Xs.Free;
    end;
    MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED,Project.Images.Count);
  end;
end;

procedure TMain.RemoveImage;
begin
  if (Project.Images.Count>1) then begin
    if MessageBox('REMOVE IMAGE','Really remove image from project?','^Yes;^No')=0 then begin
      Project.Images.Delete(Project.CurrentImageIndex);
      if Project.CurrentImageIndex>=Project.Images.Count then
        Project.CurrentImageIndex:=Project.Images.Count-1;
      MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED,Project.Images.Count);
    end;
  end;
end;

procedure TMain.ClearImage;
begin
  Project.CurrentImage.RegionUndo.AddImageUndo(0,0,Project.CurrentRegion.Width,Project.CurrentRegion.Height);
  Project.CurrentRegion.Bar(0,0,Project.CurrentRegion.Width,Project.CurrentRegion.Height,$FF000000);
  Project.CurrentImage.RegionUndo.AddImageRedoToLastUndo(0,0,Project.CurrentRegion.Width,Project.CurrentRegion.Height);
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
  if (mx>=0) and (mx<Project.CurrentRegion.Width) and (my>=0) and (my<Project.CurrentRegion.Height) then
    Settings.ActiveColor:=Project.CurrentRegion.GetPixel(mx,my);
end;

procedure TMain.GradientEditorResp(msg:TMessage);
begin
  case msg.DataInt of
    PARM_GRAD_SELECTOR:begin
      if msg.DataUInt32<9999 then begin
        fGradientSelector.SetGradient(msg.DataUInt32);
        MessageQueue.AddMessage(MSG_ACTIVEGRADIENTCHANGED);
      end;
      fGradientSelector.Show;
    end;
  end;
end;


end.

