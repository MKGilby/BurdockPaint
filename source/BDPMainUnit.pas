unit BDPMainUnit;

{$mode Delphi}{$H+}

interface

uses SysUtils, mk_sdl2, Dialogs, BDPControlsUnit, BDPDrawAreaUnit,
  BDPModalDialogsUnit, BDPPaletteEditorUnit, BDPMenuUnit;

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
    fQuitWindow:TConfirmQuitDialog;
    fMainMenu:TMainMenu;
    fMagnifyDialog:TMagnifyCELDialog;
    fRotateDialog:TRotateCELDialog;
    fOpenDialog:TOpenDialog;
    fSaveDialog:TSaveDialog;
    procedure HideMainControls;
    procedure ShowMainControls;
  end;

implementation

uses SDL2, BDPSharedUnit, MKToolbox, MKStream, MKMouse2, Logger,
  BDPMessageUnit, BDPKeyMappingUnit, BDPToolsUnit, MAD4MidLevelUnit, BDPImageUnit;

{ TMain }

constructor TMain.Create(iVersion,iBuildDate:string);
{$IFNDEF DEBUG}var MAD4:TMAD4MidLevel;{$ENDIF}
begin
  // Set data directory path to allow running without datafile
  MKStreamOpener.AddDirectory('..\data',100);
{$IFDEF DEBUG}
  // Set logging level
  Log.SetLogLevel(llAll);
{$ELSE}
// Set logging level
  Log.SetLogLevel(llStatus);
// Try to mount the datafile.
  try
    MAD4:=TMAD4MidLevel.Create(DATAFILE);
    MKStreamOpener.AddOtherSource(MAD4, 0);
  except
    on exception do ;
  end;
{$ENDIF}

  MKStreamOpener.AddDirectory('.',0);

  fMainWindow:=TWindow.Create(
    SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED,
    WINDOWWIDTH,
    WINDOWHEIGHT,
    Format('Burdock Paint V%s (%s)',[iVersion,replace(iBuildDate,'/','.')]));

  SetFPS(60);

  LoadAssets;

  fDrawArea:=TBDDrawArea.Create;
  fControls:=TBDControls.Create;
  fPaletteEditor:=TBDPaletteEditor.Create;
  fQuitWindow:=TConfirmQuitDialog.Create;
  fSplashScreen:=TBDAboutDialog.Create;
  fMainMenu:=TMainMenu.Create('menu.bin');
  if not Assigned(Project.CELImage) then fMainMenu.DisableCELSubMenusWithActiveCEL;
  fMagnifyDialog:=TMagnifyCELDialog.Create;
  fRotateDialog:=TRotateCELDialog.Create;
  MouseObjects.Sort;
  MouseObjects.List;

  fOpenDialog:=TOpenDialog.Create(nil);
  fOpenDialog.Filter:='CEL files|*.bdc|Legacy CEL files|*.cel';
  fOpenDialog.FilterIndex:=0;
  fOpenDialog.Name:='OpenDialog';
  fOpenDialog.Title:='Open file';
  fOpenDialog.InitialDir:=ExtractFilePath(ParamStr(0));

  fSaveDialog:=TSaveDialog.Create(nil);
  fSaveDialog.Filter:='CEL files|*.bdc';
  fSaveDialog.FilterIndex:=0;
  fSaveDialog.Name:='SaveDialog';
  fSaveDialog.Title:='Save file';
  fSaveDialog.InitialDir:=ExtractFilePath(ParamStr(0));;
end;

destructor TMain.Destroy;
begin
  if Assigned(fSaveDialog) then FreeAndNil(fSaveDialog);
  if Assigned(fOpenDialog) then FreeAndNil(fOpenDialog);
  if Assigned(fRotateDialog) then FreeAndNil(fRotateDialog);
  if Assigned(fMagnifyDialog) then FreeAndNil(fMagnifyDialog);
  if Assigned(fMainMenu) then FreeAndNil(fMainMenu);
  if Assigned(fSplashScreen) then FreeAndNil(fSplashScreen);
  if Assigned(fQuitWindow) then FreeAndNil(fQuitWindow);
  if Assigned(fPaletteEditor) then FreeAndNil(fPaletteEditor);
  if Assigned(fControls) then FreeAndNil(fControls);
  if Assigned(fDrawArea) then FreeAndNil(fDrawArea);
  FreeAssets;
  if Assigned(fMainWindow) then FreeAndNil(fMainWindow);
  inherited Destroy;
end;

procedure TMain.Run;
var msg:TMessage;mres,quit:boolean;mx,my:integer;
begin
  quit:=false;
//  fQuitWindow.Visible:=true;
  MouseObjects.List;
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,48,12,24,255);
    SDL_RenderClear(fMainWindow.Renderer);

    MouseObjects.Draw;
    InfoBar.Draw;
    MM.Fonts['Pinky'].OutText('FPS: '+st(fps,3,'0'),WINDOWWIDTH-141,3,0);
    MM.Fonts['Pinky'].OutText('LOI: '+st(MouseObjects.LastOverIndex,3,'0'),WINDOWWIDTH-282-16,3,0);
    FlipNoLimit;
    while MessageQueue.HasNewMessage do begin
      msg:=MessageQueue.GetNextMessage;
      mres:=false;
      if fControls.Visible then mres:=fControls.ProcessMessage(msg);
      if not mres and fPaletteEditor.Visible then mres:=fPaletteEditor.ProcessMessage(msg);
      if not mres then begin
        case msg.TypeID of
          MSG_TOGGLECONTROLS:begin
            if not fPaletteEditor.Visible then
              if fControls.Visible then
                HideMainControls
              else
                ShowMainControls;
          end;
          MSG_ACTIVATEPALETTEEDITOR:begin
            fControls.Hide;
            fPaletteEditor.Show;
            InfoBar.Top:=WINDOWHEIGHT-PALETTEEDITORHEIGHT-INFOBARHEIGHT;
          end;
          MSG_DEACTIVATEPALETTEEDITOR:begin
            fPaletteEditor.Hide;
            fControls.Show;
            InfoBar.Top:=WINDOWHEIGHT-CONTROLSHEIGHT-INFOBARHEIGHT;
          end;
          MSG_QUIT:begin
            if msg.DataInt=0 then fQuitWindow.Visible:=false
            else quit:=true;
          end;
          MSG_SELECTCOLOR:begin
            SDL_GetMouseState(@mx,@my);
            mx:=fDrawArea.MouseXToFrame(mx);
            my:=fDrawArea.MouseYToFrame(my);
            if (mx>=0) and (mx<Project.CurrentImage.Width) and (my>=0) and (my<Project.CurrentImage.Height) then
              Settings.ActiveColorIndex:=Project.CurrentImage.GetPixel(mx,my);
          end;
          MSG_LOADCEL:begin
            if fOpenDialog.Execute then begin
              if not assigned(Project.CELImage) then Project.CELImage:=TBDImage.Create(16,16);
              if UpperCase(ExtractFileExt(fOpenDialog.FileName))='.CEL' then
                Project.CELImage.ImportCEL(fOpenDialog.FileName)
              else
                Project.CELImage.LoadFromFile(fOpenDialog.FileName);
              Project.CELImage.Left:=0;
              Project.CELImage.Top:=0;
              fMainMenu.EnableCELSubMenusWithActiveCEL;
              MessageQueue.AddMessage(MSG_SHOWCEL);
            end;
          end;
          MSG_CLEARIMAGE:begin
            Project.CurrentImage.ImageUndo.AddImageUndo(0,0,Project.CurrentImage.Width,Project.CurrentImage.Height);
            Project.CurrentImage.Bar(0,0,Project.CurrentImage.Width,Project.CurrentImage.Height,Settings.SelectedColors[0]);
            Project.CurrentImage.ImageUndo.AddImageRedoToLastUndo(0,0,Project.CurrentImage.Width,Project.CurrentImage.Height);
          end;
          MSG_RELEASECEL:begin
            if Assigned(Project.CELImage) then begin
              Project.CELImage.Free;
              Project.CELImage:=nil;
            end;
            fMainMenu.DisableCELSubMenusWithActiveCEL;
          end;
          MSG_GETCEL:begin
            HideMainControls;
            SDL_ShowCursor(SDL_DISABLE);
            ActiveTool:=Tools.ItemByName['GETCEL'];
            SDL_GetMouseState(@mx,@my);
            ActiveTool.Move(fDrawArea.MouseXToFrame(mx),fDrawArea.MouseYToFrame(my));
          end;
          MSG_RESTORECONTROLS:begin
            ShowMainControls;
          end;
          MSG_GETCELFINISHED:begin
            fMainMenu.EnableCELSubMenusWithActiveCEL;
            MessageQueue.AddMessage(MSG_RESTORECONTROLS);
          end;
          MSG_FLIPCEL:begin
            if msg.DataInt=0 then Project.CELImage.FlipV
            else if msg.DataInt=1 then Project.CelImage.FlipH
            else raise Exception.Create('Invalid FLIPCEL message parameter!');
            MessageQueue.AddMessage(MSG_SHOWCEL);
          end;
          MSG_SHOWCEL:begin
            HideMainControls;
            ActiveTool:=Tools.ItemByName['SHOWCEL'];
            ActiveTool.Initialize;
          end;
          MSG_OPENMAGNIFYCELDIALOG:begin
            fMagnifyDialog.Show;
          end;
          MSG_MAGNIFYCELRESP:begin
            fMagnifyDialog.Hide;
            if msg.DataInt in [2..8] then begin
              Project.CELImage.Magnify(msg.DataInt);
              MessageQueue.AddMessage(MSG_SHOWCEL);
            end;
          end;
          MSG_OPENROTATECELDIALOG:begin
            fRotateDialog.Show;
          end;
          MSG_ROTATECELRESP:begin
            fRotateDialog.Hide;
            if msg.DataInt in [1..3] then begin
              Project.CELImage.Rotate(msg.DataInt);
              MessageQueue.AddMessage(MSG_SHOWCEL);
            end;
          end;
          MSG_OPENABOUTDIALOG:begin
            fSplashScreen.Show;
          end;
          MSG_ABOUTRESP:begin
            fSplashScreen.Hide;
          end;
          MSG_PUTCEL:begin
            HideMainControls;
            ActiveTool:=Tools.ItemByName['PUTCEL'];
            ActiveTool.Initialize;
            SDL_GetMouseState(@mx,@my);
            ActiveTool.Move(fDrawArea.MouseXToFrame(mx),fDrawArea.MouseYToFrame(my));
          end;
          MSG_SAVECEL:begin
            if fSaveDialog.Execute then begin
              try
                Project.CELImage.SaveToFile(fSaveDialog.FileName);
              except
                on e:Exception do
                  Log.LogError(e.message);
              end;
              MessageQueue.AddMessage(MSG_SHOWCEL);
            end;
          end;
        end;
      end;
    end;
    HandleMessages;
    quit:=quit or Terminate;
    fControls.SetMouseCoords(fDrawArea.FrameX,fDrawArea.FrameY);
    if keys[KeyMap[KEY_QUIT]] then begin
      fQuitWindow.Visible:=true;
      keys[KeyMap[KEY_QUIT]]:=false;
    end;
    if keys[KeyMap[KEY_GETCEL]] then begin
      if ActiveTool.Pinnable then begin  // Not GetCEL or PutCEL
        HideMainControls;
        ActiveTool:=Tools.ItemByName['GETCEL'];
        SDL_GetMouseState(@mx,@my);
        ActiveTool.Move(fDrawArea.MouseXToFrame(mx),fDrawArea.MouseYToFrame(my));
      end else begin
        ShowMainControls;
      end;
      keys[KeyMap[KEY_GETCEL]]:=false;
    end;
    if keys[KeyMap[KEY_PUTCEL]] then begin
      if ActiveTool.Pinnable and (Assigned(Project.CELImage)) then begin  // Not GetCEL or PutCEL
        HideMainControls;
        ActiveTool:=Tools.ItemByName['PUTCEL'];
        ActiveTool.Initialize;
        SDL_GetMouseState(@mx,@my);
        ActiveTool.Move(fDrawArea.MouseXToFrame(mx),fDrawArea.MouseYToFrame(my));
      end else begin
        ShowMainControls;
      end;
      keys[KeyMap[KEY_PUTCEL]]:=false;
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

end.

