unit BDPDrawAreaUnit;

{$mode Delphi}

interface

uses
  Classes, MKMouse2, mk_sdl2;

type

  { TBDDrawArea }

  TBDDrawArea=class(TMouseObject)
    constructor Create;
    destructor Destroy; override;
    procedure CenterImage;
    procedure Draw; override;
    function Click(Sender:TObject;{%H-}x,{%H-}y,{%H-}buttons:integer):boolean;
    function MouseDown(Sender:TObject;{%H-}x,{%H-}y,buttons:integer):boolean;
    function MouseUp(Sender:TObject;{%H-}x,{%H-}y,buttons:integer):boolean;
    function MouseMove(Sender:TObject;x,y:integer):boolean;
    function MouseWheel(Sender:TObject;x,y,{%H-}wheelx,wheely:integer):boolean;
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
    fMousePanning:integer;  // 0 - false, 1 - waiting for move or mouseup, 2 - was move, really panning
    fPanX,fPanY,fPanX2,fPanY2:integer;
  end;


implementation

uses SysUtils, BDPSharedUnit, sdl2, BDPKeyMappingUnit;

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
  fMousePanning:=0;
  fName:='DrawArea';
  OnClick:=Self.Click;
  OnMouseDown:=Self.MouseDown;
  OnMouseUp:=Self.MouseUp;
  OnMouseMove:=Self.MouseMove;
  OnMouseWheel:=Self.MouseWheel;
  OnKeyDown:=Self.KeyDown;
  OnKeyUp:=Self.KeyUp;
  fTexture:=TStreamingTexture.Create(WINDOWWIDTH,WINDOWHEIGHT);
  SDL_SetTextureBlendMode(fTexture.Texture,SDL_BLENDMODE_BLEND);
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
//var mx,my:integer;
begin
  MainImage.RenderToTexture(fTexture,0,0,WINDOWWIDTH,WINDOWHEIGHT,fZoomLeft,fZoomTop,fZoomLevel);
  ActiveTool.Draw;
//  InfoBar.Draw;
  if ActiveTool.Name='PUTCEL' then
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

{procedure TBDDrawArea.Draw;
var mx,my:integer;
begin
  MainImage.RenderToScreen(0,0,WINDOWWIDTH,WINDOWHEIGHT,fZoomLeft,fZoomTop,fZoomLevel);
  InfoBar.Draw;
  ActiveTool.Draw;
  OverlayImage.RenderToScreenAsOverlay(0,0,WINDOWWIDTH,WINDOWHEIGHT,fZoomLeft,fZoomTop,fZoomLevel);
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

end;}

function TBDDrawArea.Click(Sender:TObject; x,y,buttons:integer):boolean;
begin
  Result:=false;
end;

function TBDDrawArea.MouseDown(Sender:TObject; x,y,buttons:integer):boolean;
var mx,my:integer;
begin
  fMousePanning:=0;  // To stop panning if you press other button
  mx:=MouseXToFrame(x);
  my:=MouseYToFrame(y);
//  InfoBar.ShowSimpleCoords(mx,my,(mx>=0) and (mx<MainImage.Width) and (my>=0) and (my<MainImage.Height));
//  Log.LogDebug(Format('mx=%d, my=%d, buttons=%d',[mx,my,buttons]));
  Result:=false;
  Result:=ActiveTool.MouseDown(mx,my,buttons);
  if not Result then begin
    case buttons of
      3:begin   // Right button
          fMousePanning:=1;
          fPanX:=fCursorX;
          fPanY:=fCursorY;
          fPanX2:=fZoomLeft;
          fPanY2:=fZoomTop;
        end;
    end;
    Result:=true;
  end;
end;

function TBDDrawArea.MouseUp(Sender:TObject; x,y,buttons:integer):boolean;
var mx,my:integer;
begin
  mx:=MouseXToFrame(x);
  my:=MouseYToFrame(y);
  Result:=ActiveTool.MouseUp(mx,my,buttons);
  if not Result then begin
    Result:=ActiveTool.Click(mx,my,buttons);
    if not Result then begin
      if buttons=3 then begin   // Right button
        if fMousePanning=1 then MessageQueue.AddMessage(MSG_TOGGLECONTROLS);
        fMousePanning:=0;
      end;
      Result:=true;
    end else fMousePanning:=0;
  end;
end;

function TBDDrawArea.MouseMove(Sender:TObject; x,y:integer):boolean;
var buttons:integer;
begin
  buttons:=SDL_GetMouseState(nil,nil);
  fCursorX:=x;
  fCursorY:=y;
  fFrameX:=MouseXToFrame(x);
  fFrameY:=MouseYToFrame(y);
  ActiveTool.Move(fFrameX,fFrameY);
  MessageQueue.AddMessage(MSG_MOUSECOORDS,'',(fFrameX and $7fff)+(fFrameY and $7fff)<<16);
//  InfoBar.ShowSimpleCoords(fFrameX,fFrameY,not((fFrameX<0) or (fFrameX>=MainImage.Width) or (fFrameY<0) or (fFrameY>=MainImage.Height)));
  Result:=ActiveTool.MouseMove(fFrameX,fFrameY,buttons);
  if not Result then begin
    if fMousePanning=1 then fMousePanning:=2;
    if fMousePanning=2 then begin
      fZoomLeft:=fPanX2+(fPanX-x) div fZoomTimes;
      fZoomTop:=fPanY2+(fPanY-y) div fZoomTimes;
    end;
    Result:=true;
  end;
end;

function TBDDrawArea.MouseWheel(Sender:TObject; x,y,wheelx,wheely:integer):boolean;
var mx,my:integer;
begin
  Result:=true;
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

