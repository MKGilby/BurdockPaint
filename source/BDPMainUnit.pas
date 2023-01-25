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
  BDPMessageUnit;

{ TMain }

constructor TMain.Create(iVersion,iBuildDate:string);
begin
{$IFDEF DEBUG}
  // Set data directory path to allow running without datafile
  MKStreamOpener.AddDirectory('..\data',100);
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
var msg:TMessage;quit:boolean;
begin
  quit:=false;
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,48,12,24,255);
    SDL_RenderClear(fMainWindow.Renderer);

    MouseObjects.Draw;
    Flip;
    while MessageQueue.HasNewMessage do begin
      msg:=MessageQueue.GetNextMessage;
      case msg.TypeID of
        MSG_TOGGLECONTROLS:fControls.Visible:=not fControls.Visible;
        MSG_ACTIVATETOOL:fControls.ActivateToolButton(msg.DataInt);
        MSG_ACTIVATEINK:fControls.ActivateInkButton(msg.DataInt);
        MSG_QUIT:begin
          if msg.DataInt=0 then fQuitWindow.Visible:=false
          else quit:=true;
        end;
      end;
    end;
    HandleMessages;
    if keys[SDL_SCANCODE_Q] then begin
      fQuitWindow.Visible:=true;
      keys[SDL_SCANCODE_Q]:=false;
    end;
  until keys[SDL_SCANCODE_ESCAPE] or quit;
end;

end.

