{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPHSBox;

{$mode Delphi}

interface

uses SysUtils, vcc2_VisibleControlStatic, MKMouse2;

type

  { TBDHSBox }

  TBDHSBox=class(TVisibleControlStatic)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer);
    procedure SetColor(pH:word;pS:byte);
  protected
    procedure ReDraw; override;
  private
    fColor:uint32;
    fX,fY:integer;   // current crosshair center position
    fIsMouseDown:boolean;
    fOnChange:TSimpleEvent;
    procedure MouseDown(Sender:TObject;x,y,button:integer);
    procedure MouseMove(Sender:TObject;x,y:integer);
    procedure MouseUp(Sender:TObject;x,y,button:integer);
    procedure MouseLeave(Sender:TObject);
    function GetColorByH(pX:integer):uint32;
    function ModifyColorByS(pColor:uint32;pY:integer):uint32;
    function fGetH:word;
    function fGetS:byte;
  public
    property Color:uint32 read fColor;
    property H:word read fGetH;
    property S:byte read fGetS;
    property OnChange:TSimpleEvent read fOnChange write fOnChange;
  end;

implementation

uses BDPShared;

{ TBDHSBox }

constructor TBDHSBox.Create(iLeft,iTop,iWidth,iHeight:integer);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  Width:=iWidth;
  Height:=iHeight;
  fX:=3;
  fY:=3;
  fColor:=$ffff0000;
  fVisible:=true;
  fNeedRedraw:=true;
  OnMouseDown:=MouseDown;
  OnMouseUp:=MouseUp;
  OnMouseMove:=MouseMove;
  OnMouseLeave:=MouseLeave;
  fIsMouseDown:=false;
end;

procedure TBDHSBox.SetColor(pH:word;pS:byte);
begin
  fX:=3+((pH mod 360)*(fWidth-6) div 360);
  if ps>100 then ps:=100;
  fY:=3+((100-pS)*(fHeight-6) div 100);
  Refresh;
  fColor:=ModifyColorByS(GetColorByH(fX),fY);
end;

procedure TBDHSBox.ReDraw;
var i,j:integer;c:uint32;
begin
  with fImage do begin
    Bar(0,0,Width,3,SystemPalette[SYSTEMCOLORDARK]);
    Bar(0,Height-3,fWidth,3,SystemPalette[SYSTEMCOLORDARK]);
    Bar(0,3,3,Height-6,SystemPalette[SYSTEMCOLORDARK]);
    Bar(Width-3,3,3,Height-6,SystemPalette[SYSTEMCOLORDARK]);
    for i:=3 to fWidth-3-1 do begin
      c:=GetColorByH(i);
      for j:=3 to fHeight-3-1 do
        PutPixel(i,j,ModifyColorByS(c,j));
    end;
    Bar(fX+3,fY,4,2,0,0,0);
    Bar(fX,fY+3,2,4,0,0,0);
    Bar(fX-5,fY,4,2,0,0,0);
    Bar(fX,fY-5,2,4,0,0,0);
  end;
end;

procedure TBDHSBox.MouseDown(Sender:TObject; x,y,button:integer);
begin
  x-=fLeft;y-=fTop;
  if x<3 then x:=3
  else if x>fWidth-3-1 then x:=fWidth-3-1;
  if y<3 then y:=3
  else if y>fHeight-3-1 then y:=fHeight-3-1;
  fX:=x;fY:=y;
  fColor:=ModifyColorByS(GetColorByH(fX),fY);
  fIsMouseDown:=true;
  Refresh;
  if Assigned(fOnChange) then fOnChange(Sender);
end;

procedure TBDHSBox.MouseMove(Sender:TObject; x,y:integer);
begin
  if fIsMouseDown then begin
    x-=fLeft;y-=fTop;
    if x<3 then x:=3
    else if x>fWidth-3-1 then x:=fWidth-3-1;
    if y<3 then y:=3
    else if y>fHeight-3-1 then y:=fHeight-3-1;
    fX:=x;fY:=y;
    fColor:=ModifyColorByS(GetColorByH(fX),fY);
    Refresh;
    if Assigned(fOnChange) then fOnChange(Sender);
  end;
end;

procedure TBDHSBox.MouseUp(Sender:TObject; x,y,button:integer);
begin
  fIsMouseDown:=false;
end;

procedure TBDHSBox.MouseLeave(Sender:TObject);
begin
  fIsMouseDown:=false;
end;

function TBDHSBox.GetColorByH(pX:integer):uint32;
var w:integer;r,g,b:byte;

  function lerp1(value,lo,hi:integer):integer;
  begin
    Result:=(value-lo)*255 div (hi-lo);
  end;

begin
  w:=fWidth-6;
  pX-=3;
  if pX<0 then pX:=0
  else if pX>w-1 then pX:=w-1;
  if (pX>=0) and (pX<w div 6) then begin
    r:=255;
    g:=lerp1(pX,0,w div 6-1);
    b:=0;
  end else
  if (pX>=w div 6) and (pX<w div 3) then begin
    r:=255-lerp1(pX,w div 6,w div 3-1);
    g:=255;
    b:=0;
  end else
  if (pX>=w div 3) and (pX<w div 2) then begin
    r:=0;
    g:=255;
    b:=lerp1(pX,w div 3,w div 2-1);
  end else
  if (pX>=w div 2) and (pX<w*2 div 3) then begin
    r:=0;
    g:=255-lerp1(pX,w div 2,w*2 div 3-1);
    b:=255;
  end else
  if (pX>=w*2 div 3) and (pX<w*5 div 6) then begin
    r:=lerp1(pX,w*2 div 3,w*5 div 6-1);
    g:=0;
    b:=255;
  end;
  if (pX>=w*5 div 6) and (pX<w) then begin
    r:=255;
    g:=0;
    b:=255-lerp1(pX,w*5 div 6,w-1);
  end;
  Result:=$ff000000+(r<<16)+(g<<8)+b;
end;

function TBDHSBox.ModifyColorByS(pColor:uint32; pY:integer):uint32;
var h:integer;r,g,b:byte;

  function lerp2(value,max,start:integer):integer;
  begin
    Result:=start+value*(128-start) div max;
  end;

begin
  h:=fHeight-6;
  pY-=3;
  if pY<0 then py:=0
  else if pY>h-1 then py:=h-1;
  r:=lerp2(pY,h-1,(pColor and $FF0000)>>16);
  g:=lerp2(pY,h-1,(pColor and $FF00)>>8);
  b:=lerp2(pY,h-1,pColor and $FF);
  Result:=pColor and $FF000000+(r<<16)+(g<<8)+b;
end;

function TBDHSBox.fGetH:word;
begin
  Result:=(fX-3)*360 div (fWidth-7);
end;

function TBDHSBox.fGetS:byte;
begin
  Result:=100-((fY-3)*100 div (fHeight-7));
end;

end.

