unit BPDError;

{$mode Delphi}

interface

uses
  Classes, SysUtils, mk_sdl2, Font2Unit;

type

  { TBDError }

  TBDError=class
    constructor Create(iMessage:string);
    destructor Destroy; override;
    procedure Run;
  private
    fWindow:TWindow;
    fFont:TFont;
    fLines:TStringList;
  end;

implementation

uses SDL2, BDPShared;

{ TBDError }

constructor TBDError.Create(iMessage:string);
var i,mx:integer;
begin
  SDL_SetHint(SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH, '1');
  SDL_SetHint(SDL_HINT_RENDER_VSYNC, '1');

  fLines:=TStringList.Create;
  fLines.AddDelimitedText(uppercase(iMessage),#10,true);
  mx:=0;
  for i:=0 to fLines.Count-1 do
    if length(fLines[i])>mx then mx:=length(fLines[i]);
  fWindow:=TWindow.Create(
    SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED,
    mx*18+6,
    fLines.Count*24+6,
    'Error!');
  fFont:=LoadSystemFontEx(4,4,4,FONT_CREATE_TEXTURE);
end;

destructor TBDError.Destroy;
begin
  fLines.Free;
  fFont.Free;
  fWindow.Free;
  inherited Destroy;
end;

procedure TBDError.Run;
var Quit:boolean;top,i:integer;
begin
  Quit:=false;
//  top:=(200-(fLines.Count*20)) div 2;
  top:=6;
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,$9A,$9A,$9A,255);
    SDL_RenderClear(fWindow.Renderer);

    for i:=0 to fLines.Count-1 do
      fFont.OutText(fLines[i],fWindow.Width div 2,top+i*24,mjCenter);
    FlipNoLimit;
    HandleMessages;
    Quit:=Quit or Terminate;
    if keys[SDL_SCANCODE_ESCAPE] then Quit:=true;
  until Quit;
end;

end.

