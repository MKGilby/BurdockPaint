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
//    * Initial creation

{$mode delphi}
{$smartlink on}

unit vcc_SliderLogic;

interface

uses Classes, MKMouse2, MKINIFile;

type

  TOnSliderPositionChangeEvent=procedure(Sender:TObject;newValue:integer) of object;

  { TSliderLogic }

  TSliderLogic=class(TMouseObject)
    constructor Create; overload;
    function DefaultOnMouseEnter(Sender:TObject;x,y:integer):boolean;
    function DefaultOnMouseLeave(Sender:TObject;x,y:integer):boolean;
    function MouseDown(Sender:TObject;x,y,buttons:integer):boolean;
    function MouseUp(Sender:TObject;x,y,buttons:integer):boolean;
    function MouseMove(Sender:TObject;x,y:integer):boolean;
//    function OnClick(x,y,buttons:integer):boolean;
  protected
    fState:(csMouseDown,csMouseUp);
    fMinValue,fMaxValue:integer;
    fPosition:integer;
    fDecClickAreaSize,fIncClickAreaSize:integer;
    fSlideAreaSize:integer;
    fOrgX:integer;
    fOnChange:TOnSliderPositionChangeEvent;
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
    property OnChange:TOnSliderPositionChangeEvent read fOnChange write fOnChange;
  end;
     
implementation

uses SysUtils, Font2Unit, MKToolBox, Logger;
     
const
  Fstr={$I %FILE%}+', ';
  Version='1.00';

constructor TSliderLogic.Create;
begin
  inherited Create;
  fLeft:=0;
  fTop:=0;
  fWidth:=64;
  fHeight:=24;
  fMinValue:=0;
  fMaxValue:=100;
  fPosition:=0;
  fDecClickAreaSize:=16;
  fIncClickAreaSize:=16;
  fSlideAreaSize:=fWidth-fDecClickAreaSize-fIncClickAreaSize;

  OnMouseEnter:=Self.DefaultOnMouseEnter;
  OnMouseLeave:=Self.DefaultOnMouseLeave;
  OnMouseDown:=Self.MouseDown;
  OnMouseUp:=Self.MouseUp;
  OnMouseMove:=Self.MouseMove;
  fOnChange:=nil;

  fState:=csMouseUp;
end;

procedure TSliderLogic.fSetLeft(value:integer);
begin
  fLeft:=value;
end;

procedure TSliderLogic.fSetTop(value:integer);
begin
  fTop:=value;
end;

procedure TSliderLogic.fSetWidth(value:integer);
begin
  if (value>0) and (value-fDecClickAreaSize-fIncClickAreaSize>0) then begin
    fWidth:=value;
    fSlideAreaSize:=fWidth-fDecClickAreaSize-fIncClickAreaSize;
  end;
end;

procedure TSliderLogic.fSetHeight(value:integer);
begin
  if fHeight>0 then fHeight:=value;
end;

procedure TSliderLogic.fSetDecClickAreaSize(value:integer);
begin
  if (value>=0) and (fWidth-value-fIncClickAreaSize>0) then begin
    fDecClickAreaSize:=value;
    fSlideAreaSize:=fWidth-fDecClickAreaSize-fIncClickAreaSize;
  end;
end;

procedure TSliderLogic.fSetIncClickAreaSize(value:integer);
begin
  if (value>=0) and (fWidth-fDecClickAreaSize-value>0) then begin
    fIncClickAreaSize:=value;
    fSlideAreaSize:=fWidth-fDecClickAreaSize-fIncClickAreaSize;
  end;
end;

function TSliderLogic.DefaultOnMouseEnter(Sender:TObject;x,y:integer):boolean;
begin
  Result:=true;
end;

function TSliderLogic.DefaultOnMouseLeave(Sender:TObject;x,y:integer):boolean;
begin
  fState:=csMouseUp;
  Result:=true;
end;

function TSliderLogic.MouseDown(Sender:TObject;x,y,buttons:integer):boolean;
begin
  x-=Left;
  if (x>=0) and (x<fDecClickAreaSize) then begin
    if (fPosition>fMinValue) then begin
      dec(fPosition);
      if Assigned(fOnChange) then fOnChange(Sender,fPosition);
    end;
  end
  else if (x>=fDecClickAreaSize) and (x<fDecClickAreaSize+fSlideAreaSize) then begin
    fPosition:=(fMinValue)+(fMaxValue-fMinValue)*(x-fDecClickAreaSize) div (fSlideAreaSize-1);
    if Assigned(fOnChange) then fOnChange(Sender,fPosition);
    fState:=csMouseDown;
  end
  else if (x>=fDecClickAreaSize+fSlideAreaSize) and (x<fWidth) then begin
    if (fPosition<fMaxValue) then begin
      inc(fPosition);
      if Assigned(fOnChange) then fOnChange(Sender,fPosition);
    end;
  end;
  Result:=true;
end;

function TSliderLogic.MouseUp(Sender:TObject;x,y,buttons:integer):boolean;
begin
  fState:=csMouseUp;
  Result:=true;
end;

function TSliderLogic.MouseMove(Sender:TObject; x,y:integer):boolean;
begin
  x-=Left;
  if (fState=csMouseDown) and (x>=fDecClickAreaSize) and (x<fDecClickAreaSize+fSlideAreaSize) then begin
    x:=x-fDecClickAreaSize;
    fPosition:=(fMinValue)+(fMaxValue-fMinValue)*x div (fSlideAreaSize-1);
    if Assigned(fOnChange) then fOnChange(Sender,fPosition);
  end;
  Result:=true;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
