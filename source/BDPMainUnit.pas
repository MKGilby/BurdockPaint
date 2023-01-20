unit BDPMainUnit;

{$mode Delphi}{$H+}

interface

uses mk_sdl2, BDPControlsUnit, BDPDrawAreaUnit;

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
  fControls.ZIndex:=1;
  MouseObjects.Add(fControls);
  MouseObjects.Sort;
end;

destructor TMain.Destroy;
begin
  if Assigned(fControls) then FreeAndNil(fControls);
  if Assigned(fDrawArea) then FreeAndNil(fDrawArea);
  FreeAssets;
  if Assigned(fMainWindow) then FreeAndNil(fMainWindow);
  inherited Destroy;
end;

procedure TMain.Run;
var msg:TMessage;
begin
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,48,12,24,255);
    SDL_RenderClear(fMainWindow.Renderer);

    MouseObjects.Draw;
    InfoBar.ShowSimpleCoords(MouseX,MouseY,true);
    InfoBar.Draw;
    Flip;
    while MessageQueue.HasNewMessage do begin
      msg:=MessageQueue.GetNextMessage;
      case msg.TypeID of
        MSG_TOGGLECONTROLS:fControls.Visible:=not fControls.Visible;
      end;
    end;
    HandleMessages;
  until keys[SDL_SCANCODE_ESCAPE];
end;

end.

