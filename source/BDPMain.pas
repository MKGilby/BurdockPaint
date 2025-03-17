{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPMain;

{$mode Delphi}{$H+}
{define LimitFPS}

interface

uses SysUtils, mk_sdl2, Dialogs, FileBackup, BDPMessage, BDPMenu,
  BDPControls, BDPDrawArea, BDPColorEditor, BDPMagnifyCELDialog,
  BDPRotateCELDialog, BDPAboutDialog, BDPMessageBox, BDPDitherDialog,
  BDPConfigureRGradDialog, BDPCoordinateBox, BDPGradientEditor, BDPColorPalette2,
  BDPGradientSelector, BDPConfigureTintDialog, BDPConfigureSoftenDialog,
  BDPConfigureCircleDialog, BDPImageResizeDialog, BDPConfigureSepDialog;

type

  { TMain }

  TMain=class
    constructor Create(iVersion,iBuildDate:string);
    destructor Destroy; override;
    procedure Run;
  private
    fWindowBaseTitle:string;
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
    fConfigureSoftenDialog:TBDConfigureSoftenDialog;
    fConfigureCircleDialog:TBDConfigureCircleDialog;
    fImageResizeDialog:TBDImageResizeDialog;
    fConfigureSepDialog:TBDConfigureSepDialog;

    fBackup:TFileBackup;
    fOpenDialog:TOpenDialog;
    fSaveCELDialog,
    fSaveProjectDialog,
    fSaveImageDialog:TSaveDialog;
    procedure ProcessMessages;
    procedure HideMainControls;
    procedure ShowMainControls;
    function CreateOpenDialog(pName,pTitle,pFilter:string):TOpenDialog;
    function CreateSaveDialog(pName,pTitle,pFilter:string):TSaveDialog;
    procedure ConfigRGradCenter;
    procedure ConfigRGradCenterFinished;
    procedure OpenProject;
    procedure SaveProjectAs;
    procedure SaveProject;
    procedure CleanProject;
    procedure NewImage;
    procedure DuplicateImage;
    procedure OpenImage;
    procedure SaveImage;
    procedure RemoveImage;
    procedure ClearImage;
    procedure CropImage;
    procedure ResizeImage;
    procedure ResizeImageFinished;
    procedure GetCEL;
    procedure GetCELFinished;
    procedure PutCEL;
    procedure ClipCEL;
    procedure ShowCEL;
    procedure ToggleControls;
    procedure ReleaseCEL;
    procedure FlipCEL(msg:TMessage);
    procedure GrayscaleCEL;
    procedure OpenCEL;
    procedure SaveCEL;
    procedure ColorEditorResp(msg:TMessage);
    procedure SelectColor;
    procedure GradientEditorResp(msg:TMessage);

  end;

implementation

uses Classes, SDL2, BDPShared, MKToolbox, MKStream, MKMouse2, Logger,
  MAD4MidLevelUnit, ParametersUnit, BDPKeyMapping, BDPSettings, BDPRegion,
  BDPProject, ARGBImageUnit;


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
  SDL_SetHint(SDL_HINT_RENDER_VSYNC, '1');

  Log.LogStatus('Loading settings...');
  Settings:=TSettings.Create;
  Settings.LoadFromFile(SETTINGSFILE);
  fWindowBaseTitle:=Format('Burdock Paint V%s (%s) - ',[iVersion,StringReplace(iBuildDate,'/','.',[rfReplaceAll])]);

  fMainWindow:=TWindow.Create(
    SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED,
    Settings.WindowWidth,
    Settings.WindowHeight,
    fWindowBaseTitle);

  SetFPS(60);

  if (Parameters.Count=2) and (FileExists(Parameters[1])) then begin
    ProjectFilename:=Parameters[1];
    PROJECTBASEPATH:=ExtractFileDir(ProjectFilename);
    fBackup:=TFileBackup.Create(PROJECTBASEPATH+'\backups');
    fBackup.BackupFolderMaxSize:=Settings.BackupFolderMaxSize;
    fBackup.BackupFolderRetentionTime:=Settings.BackupFolderRetentionTime;
    fBackup.BackupFolderFileCount:=Settings.BackupFolderMaxFileCount;
    fBackup.BackupFile(ProjectFilename);
  end else begin
    ProjectFilename:=TEMPPROJECTFILENAME;
    PROJECTBASEPATH:=ExtractFileDir(Parameters[0]);
    fBackup:=TFileBackup.Create(PROJECTBASEPATH+'\backups');
    fBackup.BackupFolderMaxSize:=Settings.BackupFolderMaxSize;
    fBackup.BackupFolderRetentionTime:=Settings.BackupFolderRetentionTime;
    fBackup.BackupFolderFileCount:=Settings.BackupFolderMaxFileCount;
  end;
  fMainWindow.Title:=fWindowBaseTitle+ExtractFileName(ProjectFilename);

  Log.Trace('Before assets: '+inttostr(GetHeapStatus.TotalAllocated));

  LoadAssets;

  Log.Trace('After assets: '+inttostr(GetHeapStatus.TotalAllocated));
  fMainMenu:=TMainMenu.Create(MenuBin);
  Log.Trace('After MainMenu: '+inttostr(GetHeapStatus.TotalAllocated));
  fAboutDialog:=TBDAboutDialog.Create;
  Log.Trace('After AboutDialog: '+inttostr(GetHeapStatus.TotalAllocated));
  fControls:=TBDControls.Create(0,Settings.WindowHeight-CONTROLSHEIGHT,Settings.WindowWidth-COORDINATEBOXWIDTH,CONTROLSHEIGHT);
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
    Settings.WindowWidth-COORDINATEBOXWIDTH-24,Settings.WindowHeight-COORDINATEBOXHEIGHT,COORDINATEBOXWIDTH+24,COORDINATEBOXHEIGHT);
  Log.Trace('After CoordinateBox: '+inttostr(GetHeapStatus.TotalAllocated));
  fGradientEditor:=TBDGradientEditor.Create;
  Log.Trace('After GradientEditor: '+inttostr(GetHeapStatus.TotalAllocated));
  fColorPalette:=TBDColorPalette2.Create(Settings.WindowWidth-70,TOPMENUHEIGHT,70,Settings.WindowHeight-TOPMENUHEIGHT-CONTROLSHEIGHT);
  Log.Trace('After ColorPalette: '+inttostr(GetHeapStatus.TotalAllocated));
  fGradientSelector:=TBDGradientSelector.Create;
  Log.Trace('After GradientSelector: '+inttostr(GetHeapStatus.TotalAllocated));
  fConfigureTintDialog:=TBDConfigureTintDialog.Create;
  Log.Trace('After ConfigureTintDialog: '+inttostr(GetHeapStatus.TotalAllocated));
  fConfigureSoftenDialog:=TBDConfigureSoftenDialog.Create;
  Log.Trace('After ConfigureSoftenDialog: '+inttostr(GetHeapStatus.TotalAllocated));
  fConfigureCircleDialog:=TBDConfigureCircleDialog.Create;
  Log.Trace('After ConfigureCircleDialog: '+inttostr(GetHeapStatus.TotalAllocated));
  fImageResizeDialog:=TBDImageResizeDialog.Create;
  Log.Trace('After ImageResizeDialog: '+inttostr(GetHeapStatus.TotalAllocated));
  fConfigureSepDialog:=TBDConfigureSepDialog.Create;
  Log.Trace('After ConfigureSepDialog: '+inttostr(GetHeapStatus.TotalAllocated));
  MouseObjects.List;

  fOpenDialog:=TOpenDialog.Create(nil);
  fOpenDialog.Name:='OpenDialog';
  fOpenDialog.InitialDir:=PROJECTBASEPATH;
//  fOpenCELDialog:=CreateOpenDialog('OpenCELDialog','Open CEL','All supported file|*.bdc;*.cel;*.png;*.tga;*.bmp|CEL files|*.bdc|Legacy CEL files|*.cel|PNG files|*.png|TGA files|*.tga|BMP files|*.bmp');
//  fOpenProjectDialog:=CreateOpenDialog('OpenProjectDialog','Open Project','Project files|*.bpprj');
  fSaveCELDialog:=CreateSaveDialog('SaveCELDialog','Save CEL','PNG files|*.png|TGA files|*.tga|BMP files|*.bmp');
  fSaveProjectDialog:=CreateSaveDialog('SaveProjectDialog','Save Project','Project files|*.bpprj');
  fSaveImageDialog:=CreateSaveDialog('SaveImageDialog','Save Image','PNG files|*.png|TGA files|*.tga|BMP files|*.bmp');
  Log.Trace('After FCL dialogs: '+inttostr(GetHeapStatus.TotalAllocated));
  fQuit:=false;
end;

destructor TMain.Destroy;
begin
  if Assigned(fSaveImageDialog) then fSaveImageDialog.Free;
  if Assigned(fOpenDialog) then fOpenDialog.Free;
//  if Assigned(fOpenProjectDialog) then fOpenProjectDialog.Free;
  if Assigned(fSaveProjectDialog) then fSaveProjectDialog.Free;
  if Assigned(fSaveCELDialog) then fSaveCELDialog.Free;
//  if Assigned(fOpenCELDialog) then fOpenCELDialog.Free;
  fConfigureSepDialog.Free;
  fImageResizeDialog.Free;
  fConfigureCircleDialog.Free;
  fConfigureSoftenDialog.Free;
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
  fControls.Show;
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,48,12,24,255);
    SDL_RenderClear(fMainWindow.Renderer);

    MouseObjects.Draw;
    InfoBar.Draw;
    MM.Fonts['DarkRed'].OutText('FPS: '+st(fps,3,'0'),Settings.WindowWidth-141,3,0);
    {$ifndef LimitFPS} FlipNoLimit; {$else} Flip; {$endif}
    HandleMessages;
    ProcessMessages;
    fQuit:=fQuit or Terminate;
    if keys[KeyMap[KEY_QUIT]] then begin
      keys[KeyMap[KEY_QUIT]]:=false;
      fQuit:=MessageBox('CONFIRM','EXIT BURDOCK PAINT?','^YES;^NO')=0
    end;
    if GetTickCount64-PrevBackupTick>Settings.BackupIntervalTicks then begin
      Project.SaveToFile(ProjectFilename);
      fBackup.BackupFile(ProjectFilename);
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
        MSG_CONFIGRGRADCENTERRESP:     ConfigRGradCenterFinished;
        MSG_OPENCONFIGURETINTDIALOG:   fConfigureTintDialog.Show;
        MSG_OPENCONFIGURESOFTENDIALOG: fConfigureSoftenDialog.Show;
        MSG_OPENCONFIGURECIRCLEDIALOG: fConfigureCircleDialog.Show;
        MSG_OPENCONFIGURESEPDIALOG:    fConfigureSepDialog.Show;
        MSG_OPENPROJECT:               OpenProject;
        MSG_SAVEPROJECT:               SaveProject;
        MSG_SAVEPROJECTAS:             SaveProjectAs;
        MSG_CLEANPROJECT:              CleanProject;
        MSG_NEWIMAGE:                  NewImage;
        MSG_DUPLICATEIMAGE:            DuplicateImage;
        MSG_OPENIMAGE:                 OpenImage;
        MSG_SAVEIMAGE:                 SaveImage;
        MSG_REMOVEIMAGE:               RemoveImage;
        MSG_CLEARIMAGE:                ClearImage;
        MSG_CROPIMAGE:                 CropImage;
        MSG_RESIZEIMAGE:               ResizeImage;
        MSG_RESIZEIMAGECLOSED:         ResizeImageFinished;
        MSG_SETTOOLSMENU:              fMainMenu.SetToolsMenuStates;
        MSG_SETINKSMENU:               fMainMenu.SetInksMenuStates;
        MSG_GETCEL:                    GetCEL;
        MSG_GETCELFINISHED:            GetCELFinished;
        MSG_PUTCEL:                    PutCEL;
        MSG_CLIPCEL:                   ClipCEL;
        MSG_SHOWCEL:                   ShowCEL;
        MSG_RELEASECEL:                ReleaseCEL;
        MSG_OPENROTATECELDIALOG:       if Assigned(Project.CELImage) then fRotateDialog.Show;
        MSG_FLIPCEL:                   FlipCEL(msg);
        MSG_OPENMAGNIFYCELDIALOG:      if Assigned(Project.CELImage) then fMagnifyDialog.Show;
        MSG_GRAYSCALECEL:              GrayscaleCEL;
        MSG_OPENCEL:                   OpenCEL;
        MSG_SAVECEL:                   SaveCEL;
        MSG_RESTORECONTROLS:           ShowMainControls;
        MSG_TOGGLECONTROLS:            ToggleControls;
        MSG_OPENCOLOREDITOR:           fColorEditor.Show;
        MSG_COLOREDITORRESP:           ColorEditorResp(msg);
        MSG_SELECTCOLOR:               SelectColor;
        MSG_OPENGRADIENTEDITOR:        fGradientEditor.Show;
        MSG_GRADIENTEDITORRESP:        GradientEditorResp(msg);
        MSG_ACTIVEIMAGECHANGED:        fColorPalette.Refresh;
        MSG_OPENGRADIENTSELECTOR:      fGradientSelector.Show;
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
  InfoBar.Top:=Settings.WindowHeight-CONTROLSHEIGHT-INFOBARHEIGHT;
end;

function TMain.CreateOpenDialog(pName,pTitle,pFilter:string):TOpenDialog;
begin
  Result:=TOpenDialog.Create(nil);
  Result.Name:='OpenDialog';
  Result.InitialDir:=PROJECTBASEPATH;
{  with Result do begin
    Filter:=pFilter;
    FilterIndex:=0;
    Name:=pName;
    Title:=pTitle;
    InitialDir:=PROJECTBASEPATH;
  end;}
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
  fOpenDialog.Title:='Open Project';
  fOpenDialog.Filter:='Project files|*.bpprj';
  fOpenDialog.FilterIndex:=0;
  if fOpenDialog.Execute then begin
    try
      Project.Free;
      ProjectFilename:=fOpenDialog.FileName;
      fMainWindow.Title:=fWindowBaseTitle+ExtractFileName(ProjectFilename);
      Project:=TBDProject.CreateFromFile(ProjectFilename);
      MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED,Project.Images.Count);
    except
      on e:Exception do begin
        Log.LogError(e.message);
        MessageBox('ERROR',e.Message);
      end;
    end;
  end;
end;

procedure TMain.SaveProjectAs;
begin
  if fSaveProjectDialog.Execute then begin
    try
      ProjectFilename:=fSaveProjectDialog.FileName;
      fMainWindow.Title:=fWindowBaseTitle+ExtractFileName(ProjectFilename);
      Project.SaveToFile(ProjectFilename);
      MessageBox('INFORMATION','Project saved successfully.');
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
  if ProjectFilename=TEMPPROJECTFILENAME then
    SaveProjectAs
  else begin
    try
      Project.SaveToFile(ProjectFilename);
    except
      on e:Exception do begin
        Log.LogError(e.message);
        MessageBox('ERROR',e.Message);
      end;
    end;
  end;
end;

procedure TMain.CleanProject;
begin
  if MessageBox('CLEAN PROJECT','Really want to remove undo and CEL data?','Yes;No')=0 then begin
   Project.Clean;
   MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED);
   fMainMenu.DisableCELSubMenusWithActiveCEL;
  end;
end;

procedure TMain.NewImage;
var i:integer;
begin
  i:=MessageBox('NEW IMAGE','Where to place new image?','First;Last;Before;After;Cancel');
  if i in [0..3] then begin
    case i of
      0:begin
          Project.Images.Insert(0,TBDImage.Create);
          Project.CurrentImageIndex:=0;
        end;
      1:begin
          Project.Images.Add(TBDImage.Create);
          Project.CurrentImageIndex:=Project.Images.Count-1;
        end;
      2:Project.Images.Insert(Project.CurrentImageIndex,TBDImage.Create);
      3:begin
          if Project.CurrentImageIndex=Project.Images.Count-1 then begin
            Project.Images.Add(TBDImage.Create);
            Project.CurrentImageIndex:=Project.Images.Count-1;
          end else begin
            Project.Images.Insert(Project.CurrentImageIndex+1,TBDImage.Create);
            Project.CurrentImageIndex:=Project.CurrentImageIndex+1;
          end;
        end;
    end;
    MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED,Project.Images.Count);
  end;
end;

procedure TMain.DuplicateImage;
var i:integer;Xs:TMemoryStream;
begin
  i:=MessageBox('DUPLICATE IMAGE','Where to place duplicated image?','First;Last;Before;After;Cancel');
  if i in [0..3] then begin
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
        2:Project.Images.Insert(Project.CurrentImageIndex,TBDImage.CreateFromStream(Xs));
        3:begin
            if Project.CurrentImageIndex=Project.Images.Count-1 then begin
              Project.Images.Add(TBDImage.CreateFromStream(Xs));
              Project.CurrentImageIndex:=Project.Images.Count-1;
            end else begin
              Project.Images.Insert(Project.CurrentImageIndex+1,TBDImage.CreateFromStream(Xs));
              Project.CurrentImageIndex:=Project.CurrentImageIndex+1;
            end;
          end;
      end;
    finally
      Xs.Free;
    end;
    MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED,Project.Images.Count);
  end;
end;

procedure TMain.OpenImage;
var i:integer;img:TBDImage;tmp:TARGBImage;
begin
  fOpenDialog.Filter:='All supported formats|*.png;*.tga;*.bmp;*.gif|PNG files|*.png|TGA files|*.tga|BMP files|*.bmp|GIF files|*.gif';
  fOpenDialog.FilterIndex:=0;
  fOpenDialog.Title:='Open Image';
  if fOpenDialog.Execute then begin
    i:=MessageBox('OPEN IMAGE','Where to place opened image?','First;Last;Before;After;Cancel');
    if i in [0..3] then begin
      tmp:=TARGBImage.Create(fOpenDialog.FileName);
      try
        img:=TBDImage.Create(tmp.Width,tmp.Height);
        img.Region.PutImage(0,0,tmp);
      finally
        tmp.Free;
      end;

      case i of
        0:begin
            Project.Images.Insert(0,img);
            Project.CurrentImageIndex:=0;
          end;
        1:begin
            Project.Images.Add(img);
            Project.CurrentImageIndex:=Project.Images.Count-1;
          end;
        2:Project.Images.Insert(Project.CurrentImageIndex,img);
        3:begin
            if Project.CurrentImageIndex=Project.Images.Count-1 then begin
              Project.Images.Add(img);
              Project.CurrentImageIndex:=Project.Images.Count-1;
            end else begin
              Project.Images.Insert(Project.CurrentImageIndex+1,img);
              Project.CurrentImageIndex:=Project.CurrentImageIndex+1;
            end;
          end;
      end;
      MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED,Project.Images.Count);
    end;
  end;
end;

procedure TMain.SaveImage;
begin
  if fSaveImageDialog.Execute then begin
    try
      Project.CurrentRegion.WriteFile(fSaveImageDialog.FileName,copy(ExtractFileExt(fSaveImageDialog.FileName),2));
      MessageBox('INFORMATION','Image saved successfully.');
    except
      on e:Exception do begin
        Log.LogError(e.message);
        MessageBox('ERROR',e.Message);
      end;
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

procedure TMain.CropImage;
begin
  if MessageBox('CUT IMAGE','Really cut image to smallest possible?','^Yes;^No')=0 then begin
    Project.CurrentImage.RegionUndo.AddResizeUndo;
    Project.CurrentRegion.Crop(0,0,0,255);
    Project.CurrentImage.RegionUndo.AddResizeRedo;
    MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED);
  end;
end;

procedure TMain.ResizeImage;
begin
  Settings.TempInt01:=Project.CurrentRegion.Width;
  Settings.TempInt02:=Project.CurrentRegion.Height;
  fImageResizeDialog.Show;
end;

procedure TMain.ResizeImageFinished;
begin
  // Do a backup since this operation is not undoable
  Project.SaveToFile(ProjectFilename);
  fBackup.BackupFile(ProjectFilename);
  Project.CurrentRegion.Resize(Settings.TempInt01,Settings.TempInt02);
  Project.CurrentImage.ClearUndoData;
  MessageQueue.AddMessage(MSG_ACTIVEIMAGECHANGED);
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

procedure TMain.ClipCEL;
var i,j,x1,y1,x2,y2,w,h:integer;p:pointer;
begin
  x1:=Project.CurrentRegion.Width;
  y1:=Project.CurrentRegion.Height;
  x2:=0;
  y2:=0;
  p:=Project.CurrentRegion.Rawdata;
  for j:=0 to Project.CurrentRegion.Height-1 do
    for i:=0 to Project.CurrentRegion.Width-1 do begin
      if (byte(p^)<>0) or (byte((p+1)^)<>0) or (byte((p+2)^)<>0) or (byte((p+3)^)<>255) then begin
        if i<x1 then x1:=i;
        if i>x2 then x2:=i;
        if j<y1 then y1:=j;
        if j>y2 then y2:=j;
      end;
      inc(p,4);
    end;
  w:=x2-x1+1;
  h:=y2-y1+1;
  if (w>0) and (h>0) then begin
    if assigned(Project.CELImage) then Project.CELImage.Free;
    Project.CELImage:=TBDRegion.Create(w,h);
    Project.CELImage.Left:=x1;
    Project.CELImage.Top:=y1;
    Project.CELImage.PutImagePart(0,0,x1,y1,w,h,Project.CurrentRegion);
    fMainMenu.EnableCELSubMenusWithActiveCEL;
    ShowCEL;
  end;
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

procedure TMain.GrayscaleCEL;
begin
  if Assigned(Project.CELImage) then begin
    Project.CELImage.Grayscale;
    MessageQueue.AddMessage(MSG_SHOWCEL);
  end;
end;

procedure TMain.OpenCEL;
begin
  fOpenDialog.Filter:='All supported files|*.bdc;*.cel;*.png;*.tga;*.bmp|CEL files|*.bdc|Legacy CEL files|*.cel|PNG files|*.png|TGA files|*.tga|BMP files|*.bmp';
  fOpenDialog.FilterIndex:=0;
  fOpenDialog.Title:='Open CEL';
  if fOpenDialog.Execute then begin
    if not assigned(Project.CELImage) then Project.CELImage:=TBDRegion.Create(16,16);
    if UpperCase(ExtractFileExt(fOpenDialog.FileName))='.BDC' then
      Project.CELImage.LoadFromFile(fOpenDialog.FileName)
    else
      Project.CELImage.ReadFile(fOpenDialog.FileName);
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

procedure TMain.ToggleControls;
begin
//  if not fPaletteEditor.Visible then
    if fControls.Visible then
      HideMainControls
    else
      ShowMainControls;
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
  InfoBar.Top:=Settings.WindowHeight-CONTROLSHEIGHT-INFOBARHEIGHT;
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

