unit BDPDrawAreaUnit;

{$mode Delphi}

interface

uses
  Classes, MKMouse2, mk_sdl2;

type

  TMousePanning=(mpIdle, mpWaitMove, mpPanning, mpFinished);

  { TBDDrawArea }

  TBDDrawArea=class(TMouseObject)
    constructor Create;
    destructor Destroy; override;
    procedure CenterImage;
    procedure Draw; override;
    procedure Click(Sender:TObject;x,y,buttons:integer);
    procedure MouseDown(Sender:TObject;x,y,buttons:integer);
    procedure MouseUp(Sender:TObject;x,y,buttons:integer);
    procedure MouseMove(Sender:TObject;x,y:integer);
    procedure MouseWheel(Sender:TObject;x,y,wheelx,wheely:integer);
    procedure MouseEnter(Sender:TObject);
    procedure MouseLeave(Sender:TObject);
    function KeyDown(Sender:TObject;key:integer):boolean;
    function KeyUp(Sender:TObject;key:integer):boolean;
    function MouseXToFrame(x:integer):integer;
    function MouseYToFrame(y:integer):integer;
  private
    fTexture:TStreamingTexture;
    fZoomTop,fZoomLeft:integer;
    fZoomLevel:integer;  // 1..4
    fZoomTimes:integer;  // 1,2,4,8
    fCursorX,fCursorY:integer;
    fFrameX,fFrameY:integer;
    fPanDir,fPanFase:integer;
    fMousePanning:TMousePanning;
    fPanX,fPanY,fPanX2,fPanY2:integer;
  public
    property FrameX:integer read fFrameX;
    property FrameY:integer read fFrameY;
  end;


implementation

uses SysUtils, BDPSharedUnit, sdl2, BDPKeyMappingUnit, BDPToolsUnit;

{ TBDDrawArea }

constructor TBDDrawArea.Create;
begin
  inherited Create;
  SetBoundsWH(0,0,WINDOWWIDTH,WINDOWHEIGHT);
  fZoomLevel:=Settings.Zoom;
  fZoomTimes:=1<<(fZoomLevel-1);
  fZoomLeft:=Settings.ZoomLeft;
  fZoomTop:=Settings.ZoomTop;
  fPanDir:=0;
  fCursorX:=-99999;
  fCursorY:=-99999;
  fMousePanning:=mpIdle;
  fName:='DrawArea';
  OnClick:=Self.Click;
  OnMouseDown:=Self.MouseDown;
  OnMouseUp:=Self.MouseUp;
  OnMouseMove:=Self.MouseMove;
  OnMouseEnter:=Self.MouseEnter;
  OnMouseLeave:=Self.MouseLeave;
  OnMouseWheel:=Self.MouseWheel;
  OnKeyDown:=Self.KeyDown;
  OnKeyUp:=Self.KeyUp;
  fTexture:=TStreamingTexture.Create(WINDOWWIDTH,WINDOWHEIGHT);
  SDL_SetTextureBlendMode(fTexture.Texture,SDL_BLENDMODE_BLEND);
  ZIndex:=DRAWAREA_ZINDEX;
  MouseObjects.Add(Self);
end;

destructor TBDDrawArea.Destroy;
begin
  if Assigned(fTexture) then FreeAndNil(fTexture);
  Settings.Zoom:=fZoomLevel;
  Settings.ZoomLeft:=fZoomLeft;
  Settings.ZoomTop:=fZoomTop;
  inherited Destroy;
end;

procedure TBDDrawArea.CenterImage;
begin
  if not(fZoomLevel in [1..4]) then exit;
  fZoomLeft:=((MainImage.Width*fZoomTimes)-WindowWidth) div 2 div fZoomTimes;
  fZoomTop:=((MainImage.Height*fZoomTimes)-WindowHeight) div 2 div fZoomTimes;
end;

procedure TBDDrawArea.Draw;
begin
  MainImage.RenderToTexture(fTexture,0,0,WINDOWWIDTH,WINDOWHEIGHT,fZoomLeft,fZoomTop,fZoomLevel);
  ActiveTool.Draw;
  if (ActiveTool.Name='PUTCEL') or (ActiveTool.Name='SHOWCEL') then
    CELHelperImage.RenderToTextureAsOverlay(fTexture,0,0,WINDOWWIDTH,WINDOWHEIGHT,fZoomLeft,fZoomTop,fZoomLevel);
  OverlayImage.RenderToTextureAsOverlay(fTexture,0,0,WINDOWWIDTH,WINDOWHEIGHT,fZoomLeft,fZoomTop,fZoomLevel);
  fTexture.Update;
  PutTexture(0,0,fTexture);
  ActiveTool.Clear;
  if SDL_ShowCursor(SDL_QUERY)=SDL_DISABLE then
    Cursor.Draw(fCursorX,fCursorY,fZoomLevel);
  if fPanDir<>0 then begin
    if (fPanFase=0) or ((fPanFase>20) and (fPanFase mod 2=0)) then begin
      case fPanDir of
        1:dec(fZoomTop);
        2:inc(fZoomLeft);
        3:inc(fZoomTop);
        4:dec(fZoomLeft);
      end;
    end;
    inc(fPanFase);
  end;

end;

procedure TBDDrawArea.Click(Sender:TObject; x,y,buttons:integer);
begin
  if fMousePanning<>mpFinished then  // Only call tools click on appropiate panning states
    ActiveTool.Click(MouseXToFrame(x),MouseYToFrame(y),buttons);
end;

procedure TBDDrawArea.MouseDown(Sender:TObject; x,y,buttons:integer);
var mx,my:integer;
begin
  fMousePanning:=mpIdle;  // To stop panning if you press other button
  mx:=MouseXToFrame(x);
  my:=MouseYToFrame(y);
  if not ActiveTool.MouseDown(mx,my,buttons) then begin
    if buttons=SDL_BUTTON_RIGHT then begin
      fMousePanning:=mpWaitMove;
      fPanX:=fCursorX;
      fPanY:=fCursorY;
      fPanX2:=fZoomLeft;
      fPanY2:=fZoomTop;
    end;
  end;
end;

procedure TBDDrawArea.MouseUp(Sender:TObject; x,y,buttons:integer);
var mx,my:integer;
begin
  mx:=MouseXToFrame(x);
  my:=MouseYToFrame(y);
  if not ActiveTool.MouseUp(mx,my,buttons) then begin
    if buttons=SDL_BUTTON_RIGHT then begin
      if fMousePanning=mpWaitMove then begin
        MessageQueue.AddMessage(MSG_TOGGLECONTROLS);
        fMousePanning:=mpIdle;
      end
      else fMousePanning:=mpFinished;
    end;
  end else fMousePanning:=mpIdle;
end;

procedure TBDDrawArea.MouseMove(Sender:TObject; x,y:integer);
var buttons:integer;
begin
  buttons:=SDL_GetMouseState(nil,nil);
  fCursorX:=x;
  fCursorY:=y;
  fFrameX:=MouseXToFrame(x);
  fFrameY:=MouseYToFrame(y);
  ActiveTool.Move(fFrameX,fFrameY);
  if not ActiveTool.MouseMove(fFrameX,fFrameY,buttons) then begin
    if fMousePanning=mpWaitMove then fMousePanning:=mpPanning;
    if fMousePanning=mpPanning then begin
      fZoomLeft:=fPanX2+(fPanX-x) div fZoomTimes;
      fZoomTop:=fPanY2+(fPanY-y) div fZoomTimes;
    end;
  end;
end;

procedure TBDDrawArea.MouseWheel(Sender:TObject; x,y,wheelx,wheely:integer);
var mx,my:integer;
begin
  if (wheely<0) and (fZoomLevel>1) then begin
    mx:=MouseXToFrame(x);
    my:=MouseYToFrame(y);
    dec(fZoomLevel);
    fZoomTimes:=1<<(fZoomLevel-1);
    fZoomLeft:=mx-(x div fZoomTimes);
    fZoomTop:=my-(y div fZoomTimes);
  end
  else if (wheely>0) and (fZoomLevel<4) then begin
    mx:=MouseXToFrame(x);
    my:=MouseYToFrame(y);
    inc(fZoomLevel);
    fZoomTimes:=1<<(fZoomLevel-1);
    fZoomLeft:=mx-(x div fZoomTimes);
    fZoomTop:=my-(y div fZoomTimes);
  end;
end;

procedure TBDDrawArea.MouseEnter(Sender:TObject);
begin
  SDL_ShowCursor(SDL_DISABLE);
end;

procedure TBDDrawArea.MouseLeave(Sender:TObject);
begin
  SDL_ShowCursor(SDL_ENABLE);
  if ActiveTool.Name='SELCOL' then TBDToolSelectColor(ActiveTool).SetColor(-1);
end;

function TBDDrawArea.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=false;
  if (key=KeyMap[KEY_PANNINGUP1]) or (key=KeyMap[KEY_PANNINGUP2]) then begin
    fPanDir:=1;
    fPanFase:=0;
    Result:=true;
  end;
  if (key=KeyMap[KEY_PANNINGRIGHT1]) or (key=KeyMap[KEY_PANNINGRIGHT2]) then begin
    fPanDir:=2;
    fPanFase:=0;
    Result:=true;
  end;
  if (key=KeyMap[KEY_PANNINGDOWN1]) or (key=KeyMap[KEY_PANNINGDOWN2]) then begin
    fPanDir:=3;
    fPanFase:=0;
    Result:=true;
  end;
  if (key=KeyMap[KEY_PANNINGLEFT1]) or (key=KeyMap[KEY_PANNINGLEFT2]) then begin
    fPanDir:=4;
    fPanFase:=0;
    Result:=true;
  end;
  if ((key=KeyMap[KEY_ZOOMIN1]) or (key=KeyMap[KEY_ZOOMIN2])) and (fZoomLevel>1) then begin
    dec(fZoomLevel);
    fZoomTimes:=1<<(fZoomLevel-1);
    Result:=true;
  end;
  if ((key=KeyMap[KEY_ZOOMOUT1]) or (key=KeyMap[KEY_ZOOMOUT2])) and (fZoomLevel<4) then begin
    inc(fZoomLevel);
    fZoomTimes:=1<<(fZoomLevel-1);
    Result:=true;
  end;
  if (key=KeyMap[KEY_CENTER]) then begin
    CenterImage;
    Result:=true;
  end;
end;

function TBDDrawArea.KeyUp(Sender:TObject; key:integer):boolean;
begin
  Result:=false;
  if (((key=KeyMap[KEY_PANNINGUP1]) or (key=KeyMap[KEY_PANNINGUP2])) and (fPanDir=1)) or
     (((key=KeyMap[KEY_PANNINGRIGHT1]) or (key=KeyMap[KEY_PANNINGRIGHT2])) and (fPanDir=2)) or
     (((key=KeyMap[KEY_PANNINGDOWN1]) or (key=KeyMap[KEY_PANNINGDOWN2])) and (fPanDir=3)) or
     (((key=KeyMap[KEY_PANNINGLEFT1]) or (key=KeyMap[KEY_PANNINGLEFT2])) and (fPanDir=4)) then begin
    fPanDir:=0;
    Result:=true;
  end;
end;

function TBDDrawArea.MouseXToFrame(x:integer):integer;
begin
  Result:=x div fZoomTimes+fZoomLeft;
end;

function TBDDrawArea.MouseYToFrame(y:integer):integer;
begin
  Result:=y div fZoomTimes+fZoomTop;
end;

end.

