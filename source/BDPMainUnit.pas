unit BDPMainUnit;

{$mode Delphi}{$H+}

interface

uses mk_sdl2, BDPControlsUnit, BDPDrawAreaUnit, BDPConfirmQuitUnit;

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
    fQuitWindow:TConfirmQuitWindow;
  end;

implementation

uses SysUtils, SDL2, BDPSharedUnit, MKToolbox, MKStream, MKMouse2, Logger,
  BDPMessageUnit, BDPKeyMappingUnit;

{ TMain }

constructor TMain.Create(iVersion,iBuildDate:string);
begin
  // Set data directory path to allow running without datafile
  MKStreamOpener.AddDirectory('..\data',100);
{$IFDEF DEBUG}
  // Set logging level
  Log.SetLogLevel(llAll);
{$ELSE}
// Set logging level
  Log.SetLogLevel(llStatus);
{$ENDIF}

  MKStreamOpener.AddDirectory('.',0);

  fMainWindow:=TWindow.Create(
    SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED,
    WINDOWWIDTH,
    WINDOWHEIGHT,
    Format('BurdockPaint V%s (%s)',[iVersion,replace(iBuildDate,'/','.')]));

  SetFPS(60);

  LoadAssets;
  fDrawArea:=TBDDrawArea.Create;
  fDrawArea.ZIndex:=0;
  MouseObjects.Add(fDrawArea);
  fControls:=TBDControls.Create;
  fControls.ZIndex:=10;
  MouseObjects.Add(fControls);
  fQuitWindow:=TConfirmQuitWindow.Create;
  fQuitWindow.ZIndex:=MaxLongint-1;
  fQuitWindow.Visible:=false;
  MouseObjects.Add(fQuitWindow);
  MouseObjects.Sort;
end;

destructor TMain.Destroy;
begin
  if Assigned(fQuitWindow) then FreeAndNil(fQuitWindow);
  if Assigned(fControls) then FreeAndNil(fControls);
  if Assigned(fDrawArea) then FreeAndNil(fDrawArea);
  FreeAssets;
  if Assigned(fMainWindow) then FreeAndNil(fMainWindow);
  inherited Destroy;
end;

procedure TMain.Run;
var msg:TMessage;quit:boolean;mx,my:integer;
begin
  quit:=false;
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,48,12,24,255);
    SDL_RenderClear(fMainWindow.Renderer);

    MouseObjects.Draw;
    InfoBar.Draw;
    MM.Fonts['Black'].OutText('FPS: '+st(fps,3,'0'),WINDOWWIDTH-141,3,0);
    FlipNoLimit;
    while MessageQueue.HasNewMessage do begin
      msg:=MessageQueue.GetNextMessage;
      case msg.TypeID of
        MSG_TOGGLECONTROLS:fControls.Visible:=not fControls.Visible;
        MSG_ACTIVATETOOL:begin
          fControls.ActivateToolButton(msg.DataInt);
          SDL_GetMouseState(@mx,@my);
          ActiveTool.Move(fDrawArea.MouseXToFrame(mx),fDrawArea.MouseYToFrame(my));
        end;
        MSG_ACTIVATEINK:fControls.ActivateInkButton(msg.DataInt);
        MSG_QUIT:begin
          if msg.DataInt=0 then fQuitWindow.Visible:=false
          else quit:=true;
        end;
        MSG_GETCELFINISHED:begin
          fControls.Visible:=true;
          fControls.ActivateToolButton(-1);  // Puts the already selected tool into ActiveTool
        end;
      end;
    end;
    HandleMessages;
    if keys[KeyMap[KEY_QUIT]] then begin
      fQuitWindow.Visible:=true;
      keys[KeyMap[KEY_QUIT]]:=false;
    end;
    if keys[KeyMap[KEY_GETCEL]] then begin
      fControls.Visible:=false;
      ActiveTool:=Tools.ItemByName['GETCEL'];
      SDL_GetMouseState(@mx,@my);
      ActiveTool.Move(fDrawArea.MouseXToFrame(mx),fDrawArea.MouseYToFrame(my));
      keys[KeyMap[KEY_GETCEL]]:=false;
    end;
  until quit;
end;

end.

