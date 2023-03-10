{
 ------------------------------------------------------------------
 Written by Gilby/MKSZTSZ
 Hungary, 2023
 Freeware
 ------------------------------------------------------------------

// Version info:
//   V1.00 - 2021.01.30 - Gilby
//     * Initial creation from MKMouse
//   V1.01 - 2022.10.15 - Gilby
//     * Replaced Lists.TGenericList with fgl.TFPGObjectList
//   V1.02 - 2023.01.16 - Gilby
//     * BUGFIX in TMouseObjects.Create
//     * BUGFIX in TMouseObjects.HandleEvent
//   V1.03 - 2023.01.19-20 - Gilby
//     * Added ZIndex property to TMouseObject
//     * Added Sort method to TMouseObjects to sort objects in ZOrder (ascending)
//     * Added OnMouseWheel event
//     * TMouseEvent is splitted to TMouseMotionEvent ant TMouseButtonEvent
//   V1.03a - 2023.01.25 - Gilby
//     * Removed unneccessary commented lines.
//     * Changed Log.Trace to Log.LogDebug.
//   V1.04 - 2023.02.26 - Gilby
//     * Added Enabled property to TMouseObject.
//   V1.05 - 2023.03.02 - Gilby
//     * Added Show/Hide methods to TMouseObject.
//     * Added OnShow and OnHide events.
//   V1.06 - 2023.03.06 - Gilby
//     * OnMouseWheel events are passed only objects under the mouse.
//   V1.07 - 2023.03.09 - Gilby
//     * MouseObjects.Sort fix.
//     * Making OnMouseEnter and OnMouseLeave better.
//   V1.08 - 2023.03.10 - Gilby
//     * Making OnClick better.
//       Only call OnClick when MouseDown and MouseUp occurs over the same control.
}

{$ifdef fpc}
  {$mode delphi}
{$endif}

unit MKMouse2;

interface

uses
  Classes, SDL2, StackUnit, fgl;

type
  TSimpleEvent=procedure(Sender:TObject) of object;
  TMouseButtonEvent=function(Sender:TObject;x,y,buttons:integer):boolean of object;
  TMouseMotionEvent=function(Sender:TObject;x,y:integer):boolean of object;
  TMouseWheelEvent=function(Sender:TObject;x,y,wheelx,wheely:integer):boolean of object;
  TKeyEvent=function(Sender:TObject;key:integer):boolean of object;

  { TMouseObject }

  TMouseObject=class
    constructor Create;
    procedure SetBounds(x1,y1,x2,y2:integer);
    procedure SetBoundsWH(x,y,width,height:integer);
    function HandleEvent(Event:PSDL_Event):boolean; virtual;
    procedure Draw; virtual; abstract;
    function IsOver(x,y:integer):boolean;
    procedure Show;
    procedure Hide;
  public
    OnMouseDown:TMouseButtonEvent;
    OnMouseUp:TMouseButtonEvent;
    OnClick:TMouseButtonEvent;
    OnMouseMove:TMouseMotionEvent;
    OnMouseEnter:TSimpleEvent;
    OnMouseLeave:TSimpleEvent;
    OnMouseWheel:TMouseWheelEvent;
    OnKeyDown:TKeyEvent;
    OnKeyUp:TKeyEvent;
    OnShow:TSimpleEvent;
    OnHide:TSimpleEvent;
  protected
    fLeft,fTop,fWidth,fHeight,fZIndex:integer;
    over:boolean;
    keyhandled:boolean;
    fName:string;
    fSelected, fClicked, fVisible, fEnabled:boolean;
    procedure fSetVisible(value:boolean); virtual;
  private
    fTag:integer;
    procedure fSetWidth(value:integer);
    procedure fSetHeight(value:integer);
  public
    property Clicked:boolean read fClicked;
    // Not all object will use this, but needed for radiogroup.
    property Selected:boolean read fSelected write fSelected;
    property Name:string read fName write fName;
    property Left:integer read fLeft write fLeft;
    property Top:integer read fTop write fTop;
    property Width:integer read fWidth write fSetWidth;
    property Height:integer read fHeight write fSetHeight;
    property ZIndex:integer read fZIndex write fZIndex;
    property Tag:integer read fTag write fTag;
    property Visible:boolean read fVisible write fSetVisible;
    property Enabled:boolean read fEnabled write fEnabled;
  end;

  { *** You must add your mouse objects to this to handle events... *** }

  { TMouseObjects }

  TMouseObjects=class(TFPGObjectList<TMouseObject>)
    constructor Create;
    destructor Destroy; override;
    procedure Draw;
    procedure Delete(index:integer);
    procedure Remove(Item:pointer);
    function HandleEvent(Event:PSDL_Event):boolean;
    procedure NewSession;
    procedure EndSession;
    procedure List;
    procedure Sort;
  private
    fStack:TStack;
    fTop:integer;
    fSoftDelete:boolean;
    fLastOverIndex,
    fLastMouseDownIndex:integer;
  public
    property LastOverIndex:integer read fLastOverIndex;
  end;

var
  MouseObjects : TMouseObjects;

implementation

uses SysUtils, Logger, MK_SDL2;

const 
  Fstr={$I %FILE%}+', ';
  Version='1.08';

constructor TMouseObjects.Create;
begin
  inherited Create;
  fStack:=TStack.Create;
  fTop:=0;
  fSoftDelete:=false;
  fLastOverIndex:=-1;
  fLastMouseDownIndex:=-1;
end;

destructor TMouseObjects.Destroy;
begin
  FreeAndNil(fStack);
  inherited ;
end;

procedure TMouseObjects.Delete(index:integer);
begin
  if fSoftDelete then begin
    Items[index]:=nil;
  end else begin
    inherited ;
  end;
end;

procedure TMouseObjects.Remove(Item:pointer);
begin
  if IndexOf(Item)>-1 then Delete(IndexOf(Item));
end;

function TMouseObjects.HandleEvent(Event:PSDL_Event):boolean;
var i,overindex:integer;
begin
  fSoftDelete:=true;
  Result:=false;
  Log.LogDebug('MouseObjects.HandleEvent starts...');
  Log.IncreaseIndent(2);
  case Event^.Type_ of
    SDL_MOUSEBUTTONDOWN:Log.LogDebug('MouseDown');
    SDL_MOUSEBUTTONUP:Log.LogDebug('MouseUp');
    SDL_MOUSEMOTION:Log.LogDebug('MouseMotion');
    SDL_KEYDOWN:Log.LogDebug('KeyDown');
    SDL_KEYUP:Log.LogDebug('KeyUp');
    SDL_MOUSEWHEEL:Log.LogDebug('MouseWheel');
  end;
  overindex:=-1;
  if Count>0 then begin
    i:=Count-1;
    while (i>=fTop) and (i<Count) and not(Result) do begin
      Log.LogDebug('Passing event to object number '+inttostr(i)+' ('+Self[i].Name+')');
      if (overindex=-1) and (Self[i].Visible) and (Self[i].IsOver(Event^.motion.x,Event^.motion.y)) then begin
        overindex:=i;
        if Event^.type_=SDL_MOUSEBUTTONDOWN then fLastMouseDownIndex:=i;
      end;
      if Self[i]<>nil then
        Result:=Self[i].HandleEvent(Event);
      dec(i);
    end;
    if Result then begin
      if Self[i+1]<>nil then
        Log.LogDebug('Event handled by object number '+inttostr(i+1)+' ('+Self[i+1].Name+')')
      else
        Log.LogDebug('Event handled by an object that invalidated itself.');
    end else Log.LogDebug('Event not handled by any objects.');

    // This part checks if the control under the mouse is changed and
    // call OnMouseLeave and OnMouseEnter accordingly.
    if (Event^.type_=SDL_MOUSEMOTION) and (overindex<>fLastOverIndex) then begin
      if fLastOverIndex>-1 then
        if Assigned(Self[fLastOverIndex].OnMouseLeave) then
          Self[fLastOverIndex].OnMouseLeave(Self[fLastOverIndex]);
      if overindex>-1 then
        if Assigned(Self[overindex].OnMouseEnter) then
          Self[overindex].OnMouseEnter(Self[overindex]);
      fLastOverIndex:=overindex;
    end;
    if (Event^.type_=SDL_MOUSEBUTTONUP) and (fLastMouseDownIndex>-1) then begin
      if (overindex=fLastMouseDownIndex) then begin
        if Assigned(Self[overindex].OnClick) then begin
          Log.LogDebug('Click on object number '+inttostr(overindex)+' ('+Self[overindex].Name+')');
          Self[overindex].OnClick(Self[overindex],Event^.button.x,Event^.button.y,Event^.button.button);
        end;
      end;
      fLastMouseDownIndex:=-1;
    end;
  end;
  fSoftDelete:=false;
  for i:=Count-1 downto fTop do
    if Self[i]=nil then Delete(i);
  Log.DecreaseIndent(2);
end;

procedure TMouseObjects.Draw;
var i:integer;
begin
  for i:=fTop to Count-1 do Self[i].Draw;
end;

procedure TMouseObjects.NewSession;
begin
  fStack.Push2(fTop);
  fTop:=Count;
end;

procedure TMouseObjects.EndSession;
var i:integer;
begin
  for i:=Count-1 downto fTop do Delete(i);
  fTop:=fStack.Pop2;
end;

procedure TMouseObjects.List;
const Istr=Fstr+'TMouseObjects.List';
var i:integer;
begin
  Log.LogDebug(Format('Mouse objects listing starts... (fTop=%d, Count=%d)',[fTop,Count]),Istr);
  for i:=fTop to Count-1 do begin
    if Self[i]<>nil then
      Log.LogDebug(inttostr(i)+'. '+Self[i].fName,Istr)
    else
      Log.LogDebug(inttostr(i)+'. <nil>',Istr);
  end;
  Log.LogDebug('Mouse objects listing ends.',Istr);
end;

procedure TMouseObjects.Sort;
var i,j:integer;
begin
  // Sort ascending, the events are distributed from the last to the first object.
  // So bigger Zindex means getting the event earlier.
  // Yep, it's only a quick bubblesort solution.
  // I will improve it if there will be speed problems.
  for i:=0 to Count-2 do
    for j:=Count-2 downto i do
      if Self[j].ZIndex>Self[j+1].ZIndex then Exchange(j,j+1);
end;

constructor TMouseObject.Create;
begin
  fLeft:=-1; // To show that object coordinates are not set.
  over:=false;
  fVisible:=true;
  fEnabled:=true;
  OnMouseDown:=nil;
  OnMouseUp:=nil;
  OnClick:=nil;
  OnMouseMove:=nil;
  OnMouseEnter:=nil;
  OnMouseLeave:=nil;
  OnMouseWheel:=nil;
  OnKeyDown:=nil;
  OnKeyUp:=nil;
  OnShow:=nil;
  OnHide:=nil;
end;

procedure TMouseObject.SetBounds(x1,y1,x2,y2:integer);
var i:integer;
begin
  if x1>x2 then begin
    i:=x1;x1:=x2;x2:=i;
  end;
  if y1>y2 then begin
    i:=y1;y1:=y2;y2:=i;
  end;
  fLeft:=x1;
  fTop:=y1;
//  fRight:=x2;
//  fBottom:=y2;
  fWidth:=x2-x1+1;
  fHeight:=y2-y1+1;
end;

procedure TMouseObject.SetBoundsWH(x,y,width,height:integer);
begin
  if Width<0 then Width:=32;
  if Height<0 then Height:=32;
  fLeft:=x;
  fTop:=y;
  fWidth:=width;
  fHeight:=height;
//  fRight:=x+width-1;
//  fBottom:=y+height-1;
end;

function TMouseObject.IsOver(x,y:integer):boolean;
begin
//  Log.LogStatus(Format('%s.IsOver(%d,%d) Bounds: %d,%d,%d,%d',[name,x,y,fLeft,fTop,fLeft+fWidth,fTop+fHeight]));
  Result:=(x>=fLeft) and (x<fLeft+fWidth) and (y>=fTop) and (y<fTop+fHeight);
//  if Result then Log.LogStatus('Result=true') else Log.LogStatus('Result=false');
end;

procedure TMouseObject.Show;
begin
  if not Self.Visible then begin
    Visible:=true;
    if Assigned(OnShow) then OnShow(Self);
  end;
end;

procedure TMouseObject.Hide;
begin
  if Visible then begin
    Visible:=false;
    if Assigned(OnHide) then OnHide(Self);
  end;
end;

function TMouseObject.HandleEvent(Event:PSDL_Event):boolean;
var nx,ny:integer;
begin
  Log.LogDebug('HandleEvent with object '+fName);
  Result:=false;
  if not fVisible then begin
    Log.LogDebug('Not visible.');
    exit;
  end;
  if not fEnabled then begin
    Log.LogDebug('Not enabled.');
    exit;
  end;
  SDL_GetMouseState(@nx,@ny);
//  with Event^.Button do begin
//    nx:=x;
//    ny:=y;
//  end;
  case Event^.Type_ of
    SDL_MOUSEBUTTONDOWN:with Event^.Button do begin
      Log.LogDebug('Event: MouseButtonDown');
      if IsOver(nx,ny) and Assigned(OnMouseDown) then begin
        Result:=OnMouseDown(Self,nx,ny,Button);
      end;
    end;
    SDL_MOUSEBUTTONUP:with Event^.Button do begin
      Log.LogDebug('Event: MouseButtonUp');
      if IsOver(nx,ny) then begin
        if Assigned(OnMouseUp) then Result:=OnMouseUp(Self,nx,ny,Button);
{        if Assigned(OnClick) then
          if OnClick(Self,nx,ny,Button) then Result:=true;}
      end;
    end;
    SDL_MOUSEMOTION:with Event^.Button do begin
      Log.LogDebug('Event: MouseMotion');
      if IsOver(nx,ny) and Assigned(OnMouseMove) then Result:=OnMouseMove(Self,nx,ny);
    end;
    SDL_MOUSEWHEEL:begin
      if IsOver(nx,ny) and Assigned(OnMouseWheel) then Result:=OnMouseWheel(Self,nx,ny,Event.wheel.x,Event.wheel.y);
    end;
    SDL_KEYDOWN:begin
      if Assigned(OnKeyDown) then Result:=OnKeyDown(Self,Event.Key.keysym.scancode);
    end;
    SDL_KEYUP:begin
      if Assigned(OnKeyUp) then Result:=OnKeyUp(Self,Event.Key.keysym.scancode);
    end;
  end;
  if result then
    Log.LogDebug('HandleEvent result is true.')
  else
    Log.LogDebug('HandleEvent result is false.');
end;

procedure TMouseObject.fSetWidth(value:integer);
begin
  if value<0 then value:=32;
  if fLeft+value>=PrimaryWindow.Window.w then value:=PrimaryWindow.Window.w-fLeft;
  fWidth:=value;
end;

procedure TMouseObject.fSetHeight(value:integer);
begin
  if value<0 then value:=32;
  if fTop+value>=PrimaryWindow.Window.h then value:=PrimaryWindow.Window.h-fTop;
  fHeight:=value;
end;

procedure TMouseObject.fSetVisible(value:boolean);
begin
  if fVisible<>value then fVisible:=value;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  MouseObjects:=TMouseObjects.Create;
  MouseObjects.FreeObjects:=false;
  RegisterEventHandler(MouseObjects.HandleEvent);

finalization
  if Assigned(MouseObjects) then MouseObjects.Free;

end.

