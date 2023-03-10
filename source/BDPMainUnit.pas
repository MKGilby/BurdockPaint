unit BDPMainUnit;

{$mode Delphi}{$H+}

interface

uses mk_sdl2, BDPControlsUnit, BDPDrawAreaUnit, BDPConfirmQuitUnit,
  BDPSplashScreenUnit, BDPPaletteEditorUnit, BDPMenuUnit, Dialogs;

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
    fSplashScreen:TBDSplashScreen;
    fQuitWindow:TConfirmQuitWindow;
    fMainMenu:TMainMenu;
    fOpenDialog:TOpenDialog;
  end;

implementation

uses SysUtils, SDL2, BDPSharedUnit, MKToolbox, MKStream, MKMouse2, Logger,
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
// Try to mount the main executable, it should contain the datafile at the end.
  try
    MAD4:=TMAD4MidLevel.Create(paramstr(0));
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
  fDrawArea.ZIndex:=0;
  MouseObjects.Add(fDrawArea);
  fControls:=TBDControls.Create;
  fControls.ZIndex:=10;
  MouseObjects.Add(fControls);
  fPaletteEditor:=TBDPaletteEditor.Create;
  fPaletteEditor.ZIndex:=10;
  fPaletteEditor.Visible:=false;
  MouseObjects.Add(fPaletteEditor);
  fQuitWindow:=TConfirmQuitWindow.Create;
  fQuitWindow.ZIndex:=MaxLongint-1;
  fQuitWindow.Visible:=false;
  MouseObjects.Add(fQuitWindow);
  if Settings.ShowSplash then begin
    fSplashScreen:=TBDSplashScreen.Create;
    fSplashScreen.ZIndex:=MaxLongint-1;
    MouseObjects.Add(fSplashScreen);
  end;
  fMainMenu:=TMainMenu.Create;
  fMainMenu.ZIndex:=10;
  fMainMenu.Visible:=true;
  MouseObjects.Add(fMainMenu);
  MouseObjects.Sort;
  fOpenDialog:=TOpenDialog.Create(nil);
  fOpenDialog.Filter:='CEL files|*.bdc|Legacy CEL files|*.cel';
  fOpenDialog.FilterIndex:=0;
  fOpenDialog.Name:='OpenDialog';
  fOpenDialog.Title:='Open file';
  fOpenDialog.InitialDir:=ExtractFilePath(ParamStr(0));
end;

destructor TMain.Destroy;
begin
  if Assigned(fOpenDialog) then FreeAndNil(fOpenDialog);
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
      mres:=fControls.ProcessMessage(msg);
      if not mres then mres:=fPaletteEditor.ProcessMessage(msg);
      if not mres then begin
        case msg.TypeID of
          MSG_ACTIVATEPALETTEEDITOR:begin
            fControls.Hide;
            fPaletteEditor.Show;
          end;
          MSG_DEACTIVATEPALETTEEDITOR:begin
            fPaletteEditor.Hide;
            fControls.Show;
          end;
          MSG_QUIT:begin
            if msg.DataInt=0 then fQuitWindow.Visible:=false
            else quit:=true;
          end;
          MSG_SELECTCOLOR:begin
            SDL_GetMouseState(@mx,@my);
            mx:=fDrawArea.MouseXToFrame(mx);
            my:=fDrawArea.MouseYToFrame(my);
            if (mx>=0) and (mx<MainImage.Width) and (my>=0) and (my<MainImage.Height) then
              Settings.ActiveColorIndex:=MainImage.GetPixel(mx,my);
          end;
          MSG_LOADCEL:begin
            if fOpenDialog.Execute then begin
              if not assigned(CELImage) then CELImage:=TBDImage.Create(16,16);
              CELImage.ImportCEL(fOpenDialog.FileName);
            end;
          end;
          MSG_CLEARPICTURE:begin
            UndoSystem.AddImageUndo(0,0,MainImage.Width,MainImage.Height);
            MainImage.Bar(0,0,MainImage.Width,MainImage.Height,Settings.SelectedColors[0]);
            UndoSystem.AddImageRedoToLastUndo(0,0,MainImage.Width,MainImage.Height);
          end;
          MSG_RELEASECEL:begin
            if Assigned(CELImage) then FreeAndNil(CELImage);
          end;
          MSG_GETCEL:begin
            fControls.Hide;
            fMainMenu.Hide;
            SDL_ShowCursor(SDL_DISABLE);
            ActiveTool:=Tools.ItemByName['GETCEL'];
            ActiveTool.Move(fDrawArea.FrameX,fDrawArea.FrameY);
          end;
          MSG_GETCELFINISHED:begin
            fControls.Show;
            fMainMenu.Show;
          end;
        end;
      end;
    end;
    HandleMessages;
    fControls.SetMouseCoords(fDrawArea.FrameX,fDrawArea.FrameY);
    if keys[KeyMap[KEY_QUIT]] then begin
      fQuitWindow.Visible:=true;
      keys[KeyMap[KEY_QUIT]]:=false;
    end;
    if keys[KeyMap[KEY_GETCEL]] then begin
      if ActiveTool.Pinnable then begin  // Not GetCEL or PutCEL
        fControls.Visible:=false;
        ActiveTool:=Tools.ItemByName['GETCEL'];
        SDL_GetMouseState(@mx,@my);
        ActiveTool.Move(fDrawArea.MouseXToFrame(mx),fDrawArea.MouseYToFrame(my));
      end else begin
        fControls.Visible:=true;
        fControls.ActivateToolButton(-1);
      end;
      keys[KeyMap[KEY_GETCEL]]:=false;
    end;
    if keys[KeyMap[KEY_PUTCEL]] then begin
      if ActiveTool.Pinnable and (Assigned(CELImage)) then begin  // Not GetCEL or PutCEL
        fControls.Visible:=false;
        ActiveTool:=Tools.ItemByName['PUTCEL'];
        TBDToolPutCel(ActiveTool).Initialize;
        SDL_GetMouseState(@mx,@my);
        ActiveTool.Move(fDrawArea.MouseXToFrame(mx),fDrawArea.MouseYToFrame(my));
      end else begin
        fControls.Visible:=true;
        fControls.ActivateToolButton(-1);
      end;
      keys[KeyMap[KEY_PUTCEL]]:=false;
    end;
{    if keys[KeyMap[KEY_GETCOLOR]] then begin
      SDL_GetMouseState(@mx,@my);
      mx:=fDrawArea.MouseXToFrame(mx);
      my:=fDrawArea.MouseYToFrame(my);
      if (mx>=0) and (mx<MainImage.Width) and (my>=0) and (my<MainImage.Height) then
        Settings.ActiveColorIndex:=MainImage.GetPixel(mx,my);
      keys[KeyMap[KEY_GETCOLOR]]:=false;
    end;}
  until quit;
end;

end.

