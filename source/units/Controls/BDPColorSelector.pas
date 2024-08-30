{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPColorSelector;

{$mode Delphi}

interface

uses vcc2_VisibleControlStatic, BDPGradient;

type

  { TBDColorSelector }

  TBDColorSelector=class(TVisibleControlStatic)
    constructor Create(iLeft,iTop,fWidth,fHeight:integer);
    destructor Destroy; override;
  protected
    procedure ReDraw; override;
    procedure fSetWidth(value:integer); override;
    procedure fSetHeight(value:integer); override;
  private
    fLeftGradientLeft,fLeftGradientWidth:integer;
    fMainColorLeft:integer;
    fRightGradientLeft,fRightGradientWidth:integer;
    fRightColorLeft:integer;
    fLeftGradient,fRightGradient:TGradient;
    procedure Click(Sender:TObject;x, y, buttons: integer);
    function KeyDown(Sender:TObject;key:integer):boolean;
    procedure RecalculatePositions;
  public
    property Width:integer read fWidth write fSetWidth;
    property Height:integer read fHeight write fSetHeight;
  end;

implementation

uses BDPShared, BDPSettings, sdl2, mk_sdl2, BDPKeyMapping;

{ TBDColorSelector }

constructor TBDColorSelector.Create(iLeft,iTop,fWidth,fHeight:integer);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  Width:=fWidth;
  Height:=fHeight;
  OnClick:=Self.Click;
  OnKeyDown:=Self.KeyDown;
  fLeftGradient:=TGradient.Create(
    Settings.ColorSelectorLeftColor,
    Settings.ColorSelectorMainColor);
  fRightGradient:=TGradient.Create(
    Settings.ColorSelectorMainColor,
    Settings.ColorSelectorRightColor);
end;

destructor TBDColorSelector.Destroy;
begin
  if Assigned(fLeftGradient) then fLeftGradient.Free;
  if Assigned(fRightGradient) then fRightGradient.Free;
  inherited Destroy;
end;

procedure TBDColorSelector.Click(Sender:TObject; x,y,buttons:integer);
begin
  x-=fLeft;
  if buttons=SDL_BUTTON_LEFT then begin
    if (x>=0) and (x<fLeftGradientLeft) then begin
      Settings.ActiveColor:=Settings.ColorSelectorLeftColor;
      Refresh;
    end else
    if (x>=fLeftGradientLeft) and (x<fMainColorLeft) then begin
      Settings.ActiveColor:=
        fLeftGradient.GetColorAt((x-fLeftGradientLeft)/fLeftGradientWidth);
      Refresh;
    end else
    if (x>=fMainColorLeft) and (x<fRightGradientLeft) then begin
      Settings.ActiveColor:=Settings.ColorSelectorMainColor;
      Refresh;
    end else
    if (x>=fRightGradientLeft) and (x<fRightColorLeft) then begin
      Settings.ActiveColor:=
        fRightGradient.GetColorAt((x-fRightGradientLeft)/fRightGradientWidth);
      Refresh;
    end else
    if (x>=fRightColorLeft) then begin
      Settings.ActiveColor:=Settings.ColorSelectorRightColor;
      Refresh;
    end;
  end else if buttons=SDL_BUTTON_MIDDLE then begin
  end else if buttons=SDL_BUTTON_RIGHT then begin
    if (x>=0) and (x<fLeftGradientLeft) then begin
      MessageQueue.AddMessage(
        MSG_OPENCOLOREDITOR,
        PARM_COL_SELECTOR_LEFT,
        Settings.ColorSelectorLeftColor);
    end else
    if (x>=fMainColorLeft) and (x<fRightGradientLeft) then begin
      MessageQueue.AddMessage(
        MSG_OPENCOLOREDITOR,
        PARM_COL_SELECTOR_MAIN,
        Settings.ColorSelectorMainColor);
    end else
    if (x>=fRightColorLeft) then begin
      MessageQueue.AddMessage(
        MSG_OPENCOLOREDITOR,
        PARM_COL_SELECTOR_RIGHT,
        Settings.ColorSelectorRightColor);
    end;
  end;
end;

function TBDColorSelector.KeyDown(Sender:TObject; key:integer):boolean;
begin
  Result:=false;
  if (key=KeyMap[KEY_GETCOLOR]) then begin
    MessageQueue.AddMessage(MSG_SELECTCOLOR);
    Result:=true;
  end;
end;

procedure TBDColorSelector.ReDraw;
var i:integer;c:uint32;
begin
  with fImage do begin
    Bar(0,0,Width,Height,SystemPalette[SYSTEMCOLORDARK]);
    if Settings.ActiveColor=Settings.ColorSelectorLeftColor then
      Bar(0,0,Height,Height,SystemPalette[SYSTEMCOLORHIGHLIGHT]);
    if Settings.ActiveColor=Settings.ColorSelectorMainColor then
      Bar(fMainColorLeft,0,Height,Height,SystemPalette[SYSTEMCOLORHIGHLIGHT]);
    if Settings.ActiveColor=Settings.ColorSelectorRightColor then
      Bar(fRightColorLeft,0,Height,Height,SystemPalette[SYSTEMCOLORHIGHLIGHT]);
    Bar(3,3,Height-6,Height-6,Settings.ColorSelectorLeftColor);
    Bar(fMainColorLeft+3,3,Height-6,Height-6,Settings.ColorSelectorMainColor);
    Bar(fRightColorLeft+3,3,Height-6,Height-6,Settings.ColorSelectorRightColor);
    fLeftGradient.Colors[1]:=Settings.ColorSelectorLeftColor;
    fLeftGradient.Colors[2]:=Settings.ColorSelectorMainColor;
    fRightGradient.Colors[1]:=Settings.ColorSelectorMainColor;
    fRightGradient.Colors[2]:=Settings.ColorSelectorRightColor;
    for i:=0 to fLeftGradientWidth-1 do begin
      c:=fLeftGradient.GetColorAtRaw(i/fLeftGradientWidth);
      VLine(fLeftGradientLeft+i,3,Height-6,c);
      if Settings.ActiveColor=c then begin
        VLine(fLeftGradientLeft+i,Height div 2-3,3,SystemPalette[SYSTEMCOLORLIGHT]);
        VLine(fLeftGradientLeft+i,Height div 2,3,SystemPalette[SYSTEMCOLORBLACK]);
      end;
    end;
    for i:=0 to fRightGradientWidth-1 do begin
      c:=fRightGradient.GetColorAtRaw(i/fRightGradientWidth);
      VLine(fRightGradientLeft+i,3,Height-6,c);
      if Settings.ActiveColor=c then begin
        VLine(fRightGradientLeft+i,Height div 2-3,3,SystemPalette[SYSTEMCOLORLIGHT]);
        VLine(fRightGradientLeft+i,Height div 2,3,SystemPalette[SYSTEMCOLORBLACK]);
      end;
    end;
  end;
end;

procedure TBDColorSelector.RecalculatePositions;
begin
  fLeftGradientLeft:=Height;
  fLeftGradientWidth:=(Width-3*Height) div 2;
  fMainColorLeft:=fLeftGradientLeft+fLeftGradientWidth;
  fRightGradientLeft:=fMainColorLeft+Height;
  fRightGradientWidth:=(Width-3*Height)-fLeftGradientWidth;
  fRightColorLeft:=fRightGradientLeft+fRightGradientWidth;
end;

procedure TBDColorSelector.fSetWidth(value:integer);
begin
  inherited fSetWidth(value);
  RecalculatePositions;
end;

procedure TBDColorSelector.fSetHeight(value:integer);
begin
  inherited fSetHeight(value);
  RecalculatePositions;
end;

end.

