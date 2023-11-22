{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPSlider2;

{$mode Delphi}

interface

uses
  vcc2_VisibleControlStatic, ARGBImageUnit;

type

  TOnSliderPositionChangeEvent=procedure(Sender:TObject;newValue:integer) of object;

  TSliderMouseState=(csMouseDown,csMouseUp);

  { TBDSlider2 }

  TBDSlider2=class(TVisibleControlStatic)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer);
  protected
    procedure ReDraw; override;
  private
    fKnob:TARGBImage;
    fState:TSliderMouseState;
    fMinValue,fMaxValue:integer;
    fKnobPosition,fSavedPosition:integer;
    fOnChange:TOnSliderPositionChangeEvent;
    fInvertWheel,
    fCrossWheels:boolean;
    procedure MouseLeave(Sender:TObject);
    procedure MouseDown(Sender:TObject;x,y,buttons:integer);
    procedure MouseUp(Sender:TObject;x,y,buttons:integer);
    procedure MouseMove(Sender:TObject;x,y:integer);
    procedure MouseWheel(Sender:TObject;x,y,wheelx,wheely:integer);
    function fGetPosition:integer;
    procedure fSetPosition(value:integer);
    procedure fSetMinValue(value:integer);
    procedure fSetMaxValue(value:integer);
  public
    property MinValue:integer read fMinValue write fSetMinValue;
    property MaxValue:integer read fMaxValue write fSetMaxValue;
    property Position:integer read fGetPosition write fSetPosition;
    property InvertWheel:boolean read fInvertWheel write fInvertWheel;
    property CrossWheels:boolean read fCrossWheels write fCrossWheels;
    property OnChange:TOnSliderPositionChangeEvent read fOnChange write fOnChange;
  end;

implementation

uses BDPShared;

{ TBDSlider2 }

constructor TBDSlider2.Create(iLeft,iTop,iWidth,iHeight:integer);
begin
  inherited Create;
  Left:=iLeft;
  Top:=iTop;
  Height:=iHeight;
  Width:=iWidth;
  Visible:=true;
  Enabled:=true;
  fMinValue:=0;
  fMaxValue:=100;
  fKnobPosition:=0;

  fKnob:=MM.Images.ItemByName['Knob'];
  fInvertWheel:=false;
  fCrossWheels:=true;
  OnMouseLeave:=Self.MouseLeave;
  OnMouseDown:=Self.MouseDown;
  OnMouseUp:=Self.MouseUp;
  OnMouseMove:=Self.MouseMove;
  OnMouseWheel:=Self.MouseWheel;
  fOnChange:=nil;
  fState:=csMouseUp;
  fNeedRedraw:=true;
end;

procedure TBDSlider2.ReDraw;
var RealTop:integer;
begin
  with fImage do begin
    bar(0,0,Width,Height,SystemPalette[SYSTEMCOLORMID]);
    RealTop:=(fHeight-15) div 2;
    bar(4,RealTop+5,Width-8,3,SystemPalette[SYSTEMCOLORDARK]);
    bar(4,RealTop+8,Width-8,3,SystemPalette[SYSTEMCOLORBLACK]);
    PutImage(fKnobPosition,RealTop,fKnob);
  end;
end;

procedure TBDSlider2.MouseLeave(Sender:TObject);
var pre:integer;
begin
  if fState=csMouseDown then begin
    pre:=fKnobPosition;
    fKnobPosition:=fSavedPosition;
    if (pre<>fKnobPosition) then begin
      if Assigned(fOnChange) then fOnChange(Self,Position);
      fNeedRedraw:=true;
    end;
  end;
  fState:=csMouseUp;
end;

procedure TBDSlider2.MouseDown(Sender:TObject; x,y,buttons:integer);
var pre:integer;
begin
  x-=Left;
  pre:=fKnobPosition;
  if (x>=4) and (x<Width-4) then begin
    fSavedPosition:=fKnobPosition;
    fKnobPosition:=x-4;
    fState:=csMouseDown;
  end;
  if (pre<>fKnobPosition) then begin
    if Assigned(fOnChange) then fOnChange(Self,Position);
    fNeedRedraw:=true;
  end;
end;

procedure TBDSlider2.MouseUp(Sender:TObject; x,y,buttons:integer);
begin
  fState:=csMouseUp;
end;

procedure TBDSlider2.MouseMove(Sender:TObject; x,y:integer);
var pre:integer;
begin
  x-=Left;
  if (fState=csMouseDown) and (x>=4) and (x<Width-4) then begin
    pre:=fKnobPosition;
    fSavedPosition:=fKnobPosition;
    fKnobPosition:=x-4;
    if (pre<>fKnobPosition) then begin
      if Assigned(fOnChange) then fOnChange(Self,Position);
      fNeedRedraw:=true;
    end;
  end;
end;

procedure TBDSlider2.MouseWheel(Sender:TObject; x,y,wheelx,wheely:integer);
var pre:integer;
begin
  // If only the y wheel rolled and CrossWheels enabled, use that.
  if (wheelx=0) and fCrossWheels then wheelx:=wheely;
  if fInvertWheel then wheelx:=-wheelx;
  pre:=fKnobPosition;
  fKnobPosition+=wheelx;
  if fKnobPosition>=Width-8 then fKnobPosition:=Width-8-1;
  if fKnobPosition<0 then fKnobPosition:=0;
  if (pre<>fKnobPosition) then begin
    if Assigned(fOnChange) then fOnChange(Self,Position);
    fNeedRedraw:=true;
  end;
end;

function TBDSlider2.fGetPosition;
begin
  Result:=Round((fKnobPosition/(Width-8-1))*(MaxValue-MinValue+1));
end;

procedure TBDSlider2.fSetPosition(value:integer);
begin
  if value<MinValue then value:=MinValue
  else if value>MaxValue then value:=MaxValue;
  fKnobPosition:=4+round(value/(MaxValue-MinValue)*(Width-8-1));
  Refresh;
end;

procedure TBDSlider2.fSetMinValue(value:integer);
var p:integer;
begin
  if (value<MaxValue) and (fMinValue<>value) then begin
    p:=Position;
    fMinValue:=value;
    Position:=p;
  end;
end;

procedure TBDSlider2.fSetMaxValue(value:integer);
var p:integer;
begin
  if (value>MinValue) and (fMaxValue<>value) then begin
    p:=Position;
    fMaxValue:=value;
    Position:=p;
  end;
end;

end.

