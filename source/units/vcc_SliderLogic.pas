{ -[Name]-------------------------------------------

   MKSZTSZ Visual Component Collection for SDL2

                                       Slider Logic

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

   Written by Gilby/MKSZTSZ                Freeware
   Hungary, 2023

  --------------------------------------------------

  -[Description]------------------------------------

   It is a slider visual component logic, doesn't
   offer drawing.
   For visible button see vcc_ARGBSlider.

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2023.03.01-02
//    * Initial creation.
//  V1.01: Gilby - 2023.03.03
//    * Added vertical slider.
//  V1.02: Gilby - 2023.03.03
//    * Added response to MouseWheel event in TVerticalSliderLogic.
//    * Added InvertWheel property to TVerticalSliderLogic.
//      Inverts scrolling direction on both axes.
//      Default setting is false.
//    * Added CrossWheels property to TVerticalSliderLogic.
//      Allows using both wheels for both type of sliders.
//      Default setting is true.
//    * OnChange only called when the fPosition value really changes.
//  V1.03: Gilby - 2023.03.09
//    * Following changes in MKMouse2

{$mode delphi}
{$smartlink on}

unit vcc_SliderLogic;

interface

uses Classes, MKMouse2, MKINIFile;

type

  TOnSliderPositionChangeEvent=procedure(Sender:TObject;newValue:integer) of object;

  TSliderMouseState=(csMouseDown,csMouseUp);

  { THorizontalSliderLogic }

  THorizontalSliderLogic=class(TMouseObject)
    constructor Create; overload;
    procedure MouseLeave(Sender:TObject);
    function MouseDown(Sender:TObject;x,y,buttons:integer):boolean;
    function MouseUp(Sender:TObject;x,y,buttons:integer):boolean;
    function MouseMove(Sender:TObject;x,y:integer):boolean;
    function MouseWheel(Sender:TObject;x,y,wheelx,wheely:integer):boolean;
//    function OnClick(x,y,buttons:integer):boolean;
  protected
    fState:TSliderMouseState;
    fMinValue,fMaxValue:integer;
    fPosition:integer;
    fDecClickAreaSize,fIncClickAreaSize:integer;
    fSlideAreaSize:integer;
    fOnChange:TOnSliderPositionChangeEvent;
    fInvertWheel,
    fCrossWheels:boolean;
    procedure fSetLeft(value:integer); virtual;
    procedure fSetTop(value:integer); virtual;
    procedure fSetWidth(value:integer);
    procedure fSetHeight(value:integer);
    procedure fSetDecClickAreaSize(value:integer);
    procedure fSetIncClickAreaSize(value:integer);
  public
    property Left:integer read fLeft write fSetLeft;
    property Top:integer read fTop write fSetTop;
    property Width:integer read fWidth write fSetWidth;
    property Height:integer read fHeight write fSetHeight;
    property DecClickAreaSize:integer read fDecClickAreaSize write fSetDecClickAreaSize;
    property IncClickAreaSize:integer read fIncClickAreaSize write fSetIncClickAreaSize;
    property MinValue:integer read fMinValue write fMinValue;
    property MaxValue:integer read fMaxValue write fMaxValue;
    property Position:integer read fPosition write fPosition;
    property InvertWheel:boolean read fInvertWheel write fInvertWheel;
    property CrossWheels:boolean read fCrossWheels write fCrossWheels;
    property OnChange:TOnSliderPositionChangeEvent read fOnChange write fOnChange;
  end;
     
  { TVerticalSliderLogic }

  TVerticalSliderLogic=class(TMouseObject)
    constructor Create; overload;
    procedure MouseLeave(Sender:TObject);
    function MouseDown(Sender:TObject;x,y,buttons:integer):boolean;
    function MouseUp(Sender:TObject;x,y,buttons:integer):boolean;
    function MouseMove(Sender:TObject;x,y:integer):boolean;
    function MouseWheel(Sender:TObject;x,y,wheelx,wheely:integer):boolean;
//    function OnClick(x,y,buttons:integer):boolean;
  protected
    fState:TSliderMouseState;
    fMinValue,fMaxValue:integer;
    fPosition:integer;
    fDecClickAreaSize,fIncClickAreaSize:integer;
    fSlideAreaSize:integer;
    fOnChange:TOnSliderPositionChangeEvent;
    fInvertWheel,
    fCrossWheels:boolean;
    procedure fSetLeft(value:integer); virtual;
    procedure fSetTop(value:integer); virtual;
    procedure fSetWidth(value:integer);
    procedure fSetHeight(value:integer);
    procedure fSetDecClickAreaSize(value:integer);
    procedure fSetIncClickAreaSize(value:integer);
  public
    property Left:integer read fLeft write fSetLeft;
    property Top:integer read fTop write fSetTop;
    property Width:integer read fWidth write fSetWidth;
    property Height:integer read fHeight write fSetHeight;
    property DecClickAreaSize:integer read fDecClickAreaSize write fSetDecClickAreaSize;
    property IncClickAreaSize:integer read fIncClickAreaSize write fSetIncClickAreaSize;
    property MinValue:integer read fMinValue write fMinValue;
    property MaxValue:integer read fMaxValue write fMaxValue;
    property Position:integer read fPosition write fPosition;
    property InvertWheel:boolean read fInvertWheel write fInvertWheel;
    property CrossWheels:boolean read fCrossWheels write fCrossWheels;
    property OnChange:TOnSliderPositionChangeEvent read fOnChange write fOnChange;
  end;

implementation

uses SysUtils, Font2Unit, MKToolBox, Logger;
     
const
  Fstr={$I %FILE%}+', ';
  Version='1.03';


{ THorizontalSliderLogic }

constructor THorizontalSliderLogic.Create;
begin
  inherited Create;
  fLeft:=0;
  fTop:=0;
  fWidth:=128;
  fHeight:=24;
  fMinValue:=0;
  fMaxValue:=100;
  fPosition:=0;
  fDecClickAreaSize:=16;
  fIncClickAreaSize:=16;
  fSlideAreaSize:=fWidth-fDecClickAreaSize-fIncClickAreaSize;
  fInvertWheel:=false;
  fCrossWheels:=true;

  OnMouseLeave:=Self.MouseLeave;
  OnMouseDown:=Self.MouseDown;
  OnMouseUp:=Self.MouseUp;
  OnMouseMove:=Self.MouseMove;
  OnMouseWheel:=Self.MouseWheel;
  fOnChange:=nil;

  fState:=csMouseUp;
end;

procedure THorizontalSliderLogic.MouseLeave(Sender:TObject);
begin
  fState:=csMouseUp;
end;

function THorizontalSliderLogic.MouseDown(Sender:TObject;x,y,buttons:integer):boolean;
var pre:integer;
begin
  x-=Left;
  pre:=fPosition;
  if (x>=0) and (x<fDecClickAreaSize) then begin
    if (fPosition>fMinValue) then dec(fPosition);
  end
  else if (x>=fDecClickAreaSize) and (x<fDecClickAreaSize+fSlideAreaSize) then begin
    fPosition:=(fMinValue)+(fMaxValue-fMinValue)*(x-fDecClickAreaSize) div (fSlideAreaSize-1);
    fState:=csMouseDown;
  end
  else if (x>=fDecClickAreaSize+fSlideAreaSize) and (x<fWidth) then begin
    if (fPosition<fMaxValue) then inc(fPosition);
  end;
  if (pre<>fPosition) and Assigned(fOnChange) then fOnChange(Sender,fPosition);
  Result:=true;
end;

function THorizontalSliderLogic.MouseUp(Sender:TObject;x,y,buttons:integer):boolean;
begin
  fState:=csMouseUp;
  Result:=true;
end;

function THorizontalSliderLogic.MouseMove(Sender:TObject; x,y:integer):boolean;
var pre:integer;
begin
  x-=Left;
  if (fState=csMouseDown) and (x>=fDecClickAreaSize) and (x<fDecClickAreaSize+fSlideAreaSize) then begin
    pre:=fPosition;
    x:=x-fDecClickAreaSize;
    fPosition:=(fMinValue)+(fMaxValue-fMinValue)*x div (fSlideAreaSize-1);
    if (pre<>fPosition) and Assigned(fOnChange) then fOnChange(Sender,fPosition);
  end;
  Result:=true;
end;

function THorizontalSliderLogic.MouseWheel(Sender:TObject;x,y,wheelx,wheely:integer):boolean;
var pre:integer;
begin
  // If only the y wheel rolled and CrossWheels enabled, use that.
  if (wheelx=0) and fCrossWheels then wheelx:=wheely;
  if fInvertWheel then wheelx:=-wheelx;
  pre:=fPosition;
  fPosition+=wheelx;
  if fPosition>fMaxValue then fPosition:=fMaxValue;
  if fPosition<fMinValue then fPosition:=fMinValue;
  if (pre<>fPosition) and Assigned(fOnChange) then fOnChange(Self,fPosition);
  Result:=true;
end;

procedure THorizontalSliderLogic.fSetLeft(value:integer);
begin
  fLeft:=value;
end;

procedure THorizontalSliderLogic.fSetTop(value:integer);
begin
  fTop:=value;
end;

procedure THorizontalSliderLogic.fSetWidth(value:integer);
begin
  if (value>0) and (value-fDecClickAreaSize-fIncClickAreaSize>0) then begin
    fWidth:=value;
    fSlideAreaSize:=fWidth-fDecClickAreaSize-fIncClickAreaSize;
  end;
end;

procedure THorizontalSliderLogic.fSetHeight(value:integer);
begin
  if fHeight>0 then fHeight:=value;
end;

procedure THorizontalSliderLogic.fSetDecClickAreaSize(value:integer);
begin
  if (value>=0) and (fWidth-value-fIncClickAreaSize>0) then begin
    fDecClickAreaSize:=value;
    fSlideAreaSize:=fWidth-fDecClickAreaSize-fIncClickAreaSize;
  end;
end;

procedure THorizontalSliderLogic.fSetIncClickAreaSize(value:integer);
begin
  if (value>=0) and (fWidth-fDecClickAreaSize-value>0) then begin
    fIncClickAreaSize:=value;
    fSlideAreaSize:=fWidth-fDecClickAreaSize-fIncClickAreaSize;
  end;
end;

{ TVerticalSliderLogic }

constructor TVerticalSliderLogic.Create;
begin
  inherited Create;
  fLeft:=0;
  fTop:=0;
  fWidth:=24;
  fHeight:=128;
  fMinValue:=0;
  fMaxValue:=100;
  fPosition:=0;
  fDecClickAreaSize:=16;
  fIncClickAreaSize:=16;
  fSlideAreaSize:=fHeight-fDecClickAreaSize-fIncClickAreaSize;
  fInvertWheel:=false;
  fCrossWheels:=true;

  OnMouseLeave:=Self.MouseLeave;
  OnMouseDown:=Self.MouseDown;
  OnMouseUp:=Self.MouseUp;
  OnMouseMove:=Self.MouseMove;
  OnMouseWheel:=Self.MouseWheel;
  fOnChange:=nil;

  fState:=csMouseUp;
end;

procedure TVerticalSliderLogic.MouseLeave(Sender:TObject);
begin
  fState:=csMouseUp;
end;

function TVerticalSliderLogic.MouseDown(Sender:TObject; x,y,buttons:integer):boolean;
var pre:integer;
begin
  y-=Top;
  pre:=fPosition;
  if (y>=0) and (y<fDecClickAreaSize) then begin
    if (fPosition>fMinValue) then dec(fPosition);
  end
  else if (y>=fDecClickAreaSize) and (y<fDecClickAreaSize+fSlideAreaSize) then begin
    fPosition:=(fMinValue)+(fMaxValue-fMinValue)*(x-fDecClickAreaSize) div (fSlideAreaSize-1);
    fState:=csMouseDown;
  end
  else if (y>=fDecClickAreaSize+fSlideAreaSize) and (y<fHeight) then begin
    if (fPosition<fMaxValue) then inc(fPosition);
  end;
  if (pre<>fPosition) and Assigned(fOnChange) then fOnChange(Sender,fPosition);
  Result:=true;
end;

function TVerticalSliderLogic.MouseUp(Sender:TObject; x,y,buttons:integer):boolean;
begin
  fState:=csMouseUp;
  Result:=true;
end;

function TVerticalSliderLogic.MouseMove(Sender:TObject; x,y:integer):boolean;
var pre:integer;
begin
  y-=Top;
  if (fState=csMouseDown) and (y>=fDecClickAreaSize) and (y<fDecClickAreaSize+fSlideAreaSize) then begin
    pre:=fPosition;
    y:=y-fDecClickAreaSize;
    fPosition:=(fMinValue)+(fMaxValue-fMinValue)*y div (fSlideAreaSize-1);
    if (pre<>fPosition) and Assigned(fOnChange) then fOnChange(Sender,fPosition);
  end;
  Result:=true;
end;

function TVerticalSliderLogic.MouseWheel(Sender:TObject;x,y,wheelx,wheely:integer):boolean;
var pre:integer;
begin
  // If only the x wheel rolled and CrossWheels enabled, use that.
  if (wheely=0) and fCrossWheels then wheely:=wheelx;
  if fInvertWheel then wheely:=-wheely;
  pre:=fPosition;
  fPosition-=wheely;
  if fPosition>fMaxValue then fPosition:=fMaxValue;
  if fPosition<fMinValue then fPosition:=fMinValue;
  if (pre<>fPosition) and Assigned(fOnChange) then fOnChange(Self,fPosition);
  Result:=true;
end;

procedure TVerticalSliderLogic.fSetLeft(value:integer);
begin
  fLeft:=value;
end;

procedure TVerticalSliderLogic.fSetTop(value:integer);
begin
  fTop:=value;
end;

procedure TVerticalSliderLogic.fSetWidth(value:integer);
begin
  if value>0 then fWidth:=value;
end;

procedure TVerticalSliderLogic.fSetHeight(value:integer);
begin
  if (value>0) and (value-fDecClickAreaSize-fIncClickAreaSize>0) then begin
    fHeight:=value;
    fSlideAreaSize:=fHeight-fDecClickAreaSize-fIncClickAreaSize;
  end;
end;

procedure TVerticalSliderLogic.fSetDecClickAreaSize(value:integer);
begin
  if (value>=0) and (fHeight-fIncClickAreaSize-value>0) then begin
    fdecClickAreaSize:=value;
    fSlideAreaSize:=fHeight-fDecClickAreaSize-fIncClickAreaSize;
  end;
end;

procedure TVerticalSliderLogic.fSetIncClickAreaSize(value:integer);
begin
  if (value>=0) and (fHeight-fDecClickAreaSize-value>0) then begin
    fIncClickAreaSize:=value;
    fSlideAreaSize:=fHeight-fDecClickAreaSize-fIncClickAreaSize;
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
