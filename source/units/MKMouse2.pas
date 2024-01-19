{ -[Name]--------------------------------------------------------------

                              MKMouse2 Unit

  -[Disclaimer]--------------------------------------------------------

  Written by Gilby
  Copyright 2021-2023 MKSZTSZ

  This unit is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  This unit is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this unit. If not, see <https://www.gnu.org/licenses/>.

  -[Description]-------------------------------------------------------

  Mouse objects, event handling, etc. (with mk_sdl2)

  ---------------------------------------------------------------------
}

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
//   V1.09 - 2023.03.11-14 - Gilby
//     * Rework of Event handling. No handleevent will be passed for MouseObjects,
//       only the appropiate On... event handler will be called if assigned.
//     * Expanded MouseObjects.List with coordinates and Visible/Enabled properties.
//   V1.10 - 2023.03.15 - Gilby
//     * Changes to make compatible with the new SDL2.
//   V1.10a - 2023.03.21 - Gilby
//     * Following field name changes in SDL2.
//   V1.11 - 2023.03.23 - Gilby
//     * Not visible objects won't get draw called.
//   V1.12 - 2023.04.04 - Gilby
//     * Mouse event proc types are procedures now.
//   V1.12a - 2023.04.24 - Gilby
//     * Added ZIndex to MouseObjects.List.
//   V1.13 - 2023.04.25 - Gilby
//     * Better logging of MouseObjects event handling.
//   V1.14 - 2023.05.25 - Gilby
//     * Removed SoftDelete feature.
//   V1.15 - 2023.05.31 - Gilby
//     * MouseObjects.Sort uses then internal sorting feature of TFPGObjectList.
//     * MouseObjects.Add inserts object to the sorted place (by ZIndex), no
//       need to call Sort. You need Sort when an already added object's
//       ZIndex value is changed.
//   V1.15a - 2023.06.07 - Gilby
//     * Only mouse and keyup keydown events are consumed.
//   V1.16 - 2024.01.19 - Gilby
//     * Added TValueChangeEvent.

{$ifdef fpc}
  {$mode delphi}
{$endif}

unit MKMouse2;

interface

uses
  Classes, SDL2, StackUnit, fgl, BinarySearchUnit;

type
  TSimpleEvent=procedure(Sender:TObject) of object;
  TMouseButtonEvent=procedure(Sender:TObject;x,y,buttons:integer) of object;
  TMouseMotionEvent=procedure(Sender:TObject;x,y:integer) of object;
  TMouseWheelEvent=procedure(Sender:TObject;x,y,wheelx,wheely:integer) of object;
  TKeyEvent=function(Sender:TObject;key:integer):boolean of object;
  TValueChangeEvent=procedure(Sender:TObject;oldValue,newValue:integer) of object;

  { TMouseObject }

  TMouseObject=class
    constructor Create;
    procedure SetBounds(x1,y1,x2,y2:integer);
    procedure SetBoundsWH(x,y,width,height:integer);
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
    fTag:integer;
    procedure fSetWidth(value:integer); virtual;
    procedure fSetHeight(value:integer); virtual;
    procedure fSetVisible(value:boolean); virtual;
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
    procedure Add(Item:TMouseObject);
    procedure Draw;
    procedure Remove(Item:TMouseObject);
    function HandleEvent(Event:PSDL_Event):boolean;
    procedure NewSession;
    procedure EndSession;
    procedure List;
    procedure ListVisible;
    procedure Sort;
  private
    fStack:TStack;
    fTop:integer;
    fLastOverIndex,
    fLastMouseDownIndex:integer;
    function MouseObjectCompare(index:integer;Item:pointer):Integer;
  public
    property LastOverIndex:integer read fLastOverIndex;
  end;

var
  MouseObjects : TMouseObjects;

implementation

uses SysUtils, Logger, MK_SDL2;

const 
  Fstr={$I %FILE%}+', ';
  Version='1.16';

constructor TMouseObjects.Create;
begin
  inherited Create;
  fStack:=TStack.Create;
  fTop:=0;
  fLastOverIndex:=-1;
  fLastMouseDownIndex:=-1;
end;

destructor TMouseObjects.Destroy;
begin
  FreeAndNil(fStack);
  inherited ;
end;

function TMouseObjects.MouseObjectCompare(index:integer;Item:pointer):Integer;
begin
  Result:=TMouseObject(Item).ZIndex-Self[index].ZIndex;
end;

procedure TMouseObjects.Add(Item:TMouseObject);
var i:integer;
begin
  if Self.Count>2 then begin
    i:=BinaryInsert(0,Count-1,MouseObjectCompare,Item);
    Insert(i,Item);
  end else if Self.Count=1 then begin
    if Item.ZIndex>=Self[0].ZIndex then
      Inherited Add(Item)
    else
      Insert(0,Item);
  end else Inherited Add(Item);
end;

procedure TMouseObjects.Remove(Item:TMouseObject);
begin
  if IndexOf(Item)>-1 then Delete(IndexOf(Item));
end;

function TMouseObjects.HandleEvent(Event:PSDL_Event):boolean;
var i,overindex,mx,my:integer;
begin
  if fLastOverIndex>Count-1 then fLastOverIndex:=-1;
  if fLastMouseDownIndex>Count-1 then fLastMouseDownIndex:=-1;
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
    SDL_QUITEV:Log.LogDebug('Quit');
  end;
  overindex:=-1;
  if Count>0 then begin
    i:=Count-1;
    // Key events are passed for each control until someone "eats" it.
    if (Event^.type_=SDL_KEYDOWN) or (Event^.type_=SDL_KEYUP) then begin
      while (i>=fTop) and (i<Count) and not(Result) do begin
        Log.LogDebug('Trying object number '+inttostr(i)+' ('+Self[i].Name+')');
        if Assigned(Self[i]) and (Self[i].Visible) and (Self[i].Enabled) then begin
          if (Event^.type_=SDL_KEYDOWN) and Assigned(Self[i].OnKeyDown) then
            Result:=Self[i].OnKeyDown(Self[i],Event^.key.keysym.scancode)
          else if (Event^.type_=SDL_KEYUP) and Assigned(Self[i].OnKeyUp) then
            Result:=Self[i].OnKeyUp(Self[i],Event^.key.keysym.scancode);
        end;
        dec(i);
      end;
    end else begin
      if (Event^.type_=SDL_MOUSEBUTTONDOWN) or (Event^.type_=SDL_MOUSEBUTTONUP) or
         (Event^.type_=SDL_MOUSEMOTION) then begin
        mx:=event.motion.x;
        my:=event.motion.y
      end else
      if (Event^.type_=SDL_MOUSEWHEEL) then begin
        mx:=event.wheel.mouseX;
        my:=event.wheel.mouseY;
      end;

      // Mouse events are passed only for the visible control under the mouse.
      if (Event^.type_=SDL_MOUSEBUTTONDOWN) or (Event^.type_=SDL_MOUSEBUTTONUP) or
         (Event^.type_=SDL_MOUSEMOTION) or (Event^.type_=SDL_MOUSEWHEEL) then begin
        while (i>=fTop) and (i<Count) and (overindex=-1) do begin
          if Assigned(Self[i]) and Self[i].Visible then begin
//            Log.LogDebug('Trying object number '+inttostr(i)+' ('+Self[i].Name+')');
            if (overindex=-1) and (Self[i].IsOver(mx,my)) then begin
              overindex:=i;
              if Event^.type_=SDL_MOUSEBUTTONDOWN then fLastMouseDownIndex:=i;
            end;
          end;
          dec(i);
        end;
        if overindex>-1 then begin
          Log.LogDebug(Format('Over %d. %s',[overindex,Self[overindex].Name]));
          case Event^.type_ of
            SDL_MOUSEBUTTONDOWN:
              if Assigned(Self[overindex].OnMouseDown) then begin
                Log.LogDebug('Calling OnMouseDown...');
                Self[overindex].OnMouseDown(Self[overindex],mx,my,Event.button.button);
              end;
            SDL_MOUSEBUTTONUP:
              if Assigned(Self[overindex].OnMouseUp) then begin
                Log.LogDebug('Calling OnMouseUp...');
                Self[overindex].OnMouseUp(Self[overindex],mx,my,Event.button.button);
              end;
            SDL_MOUSEMOTION:
              if Assigned(Self[overindex].OnMouseMove) then begin
                Log.LogDebug('Calling OnMouseMove...');
                Self[overindex].OnMouseMove(Self[overindex],mx,my);
              end;
            SDL_MOUSEWHEEL:
              if Assigned(Self[overindex].OnMouseWheel) then begin
                Log.LogDebug('Calling OnMouseWheel...');
                Self[overindex].OnMouseWheel(Self[overindex],mx,my,Event^.wheel.x,Event^.wheel.y);
              end;
          end;
        end;
        Result:=true;
      end;

      // This part checks if the control under the mouse is changed and
      // call OnMouseLeave and OnMouseEnter accordingly.
      if (Event^.type_=SDL_MOUSEMOTION) and (overindex<>fLastOverIndex) then begin
        Log.LogDebug('Object changed under the cursor!');
        if fLastOverIndex>-1 then begin
          Log.LogDebug(Format('  From %d. %s',[fLastOverIndex,Self[fLastOverIndex].Name]));
          if Assigned(Self[fLastOverIndex].OnMouseLeave) then begin
            Log.LogDebug('Calling OnMouseLeave for '+Self[fLastOverIndex].Name+'...');
            Self[fLastOverIndex].OnMouseLeave(Self[fLastOverIndex]);
          end;
        end;
        if overindex>-1 then begin
          Log.LogDebug(Format('  To %d. %s',[overindex,Self[overindex].Name]));
          if Assigned(Self[overindex].OnMouseEnter) then begin
            Log.LogDebug('Calling OnMouseEnter for '+Self[overindex].Name+'...');
            Self[overindex].OnMouseEnter(Self[overindex]);
          end;
        end;
        fLastOverIndex:=overindex;
      end;

      // This part checks if a correct click occured and calls OnClick if assigned.
      // (Click=MouseDown and MouseUp over the same control.)
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
  end;
  Log.DecreaseIndent(2);
end;

procedure TMouseObjects.Draw;
var i:integer;
begin
  for i:=fTop to Count-1 do begin
    if Assigned(Self[i]) then begin
      if Self[i].Visible then Self[i].Draw;
    end;
  end;
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
    if Assigned(Self[i]) then with Self[i] do begin
      Log.LogDebug(Format('%d. %s (%d,%d,%d,%d) Z:%d',[i,fName,fLeft,fTop,fWidth,fHeight,fZIndex]));
      if Visible then Log.LogDebug('  Visible.') else Log.LogDebug('  Not visible.');
      if Enabled then Log.LogDebug('  Enabled.') else Log.LogDebug('  Not enabled.');
    end else
      Log.LogDebug(inttostr(i)+'. <nil>');
  end;
  Log.LogDebug('Mouse objects listing ends.',Istr);
end;

procedure TMouseObjects.ListVisible;
const Istr=Fstr+'TMouseObjects.ListVisible';
var i:integer;
begin
  Log.LogDebug(Format('Mouse objects listing starts (VISIBLE only)... (fTop=%d, Count=%d)',[fTop,Count]),Istr);
  for i:=fTop to Count-1 do begin
    if Assigned(Self[i]) and (Self[i].Visible) then with Self[i] do begin
      Log.LogDebug(Format('%d. %s (%d,%d,%d,%d) Z:%d',[i,fName,fLeft,fTop,fWidth,fHeight,fZIndex]));
      if Enabled then Log.LogDebug('  Enabled.') else Log.LogDebug('  Not enabled.');
    end else
      Log.LogDebug(inttostr(i)+'. <hidden>');
  end;
  Log.LogDebug('Mouse objects listing ends.',Istr);
end;

function MouseObjectCompare2(const Item1,Item2:TMouseObject):Integer;
begin
  Result:=Item1.ZIndex-Item2.ZIndex;
end;

procedure TMouseObjects.Sort;
begin
  // Sort ascending, draw occurs from the last to the first object.
  // So bigger Zindex means getting drawn later.
  inherited Sort(MouseObjectCompare2);
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

procedure TMouseObject.fSetWidth(value:integer);
begin
  if value<0 then value:=32;
  if fLeft+value>=PrimaryWindow.Width then value:=PrimaryWindow.Width-fLeft;
  fWidth:=value;
end;

procedure TMouseObject.fSetHeight(value:integer);
begin
  if value<0 then value:=32;
  if fTop+value>=PrimaryWindow.Height then value:=PrimaryWindow.Height-fTop;
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

