unit BDPToolsUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, Lists;

type
  TBDTool=class
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Draw; virtual; // Draw helping lines, refresh infobar
    procedure Clear; virtual;  // Clear helping lines!
    procedure Move(x,y:integer);
    function MouseDown({%H-}x,{%H-}y,{%H-}buttons:integer):boolean; virtual;
    function MouseMove({%H-}x,{%H-}y,{%H-}buttons:integer):boolean; virtual;
    function MouseUp({%H-}x,{%H-}y,{%H-}buttons:integer):boolean; virtual;
    function Click({%H-}x,{%H-}y,{%H-}buttons:integer):boolean; virtual;   // Handle clicks
  protected
    fState:integer;  // 0 - Waiting for first click, 1 - waiting for second click, etc.
    fX,fY:integer;  // Current position from Move
    fName,fHint:string;
  public
    property Name:string read fName;
    property Hint:string read fHint;
  end;

  TBDTools=class(TNamedList<TBDTool>)
    constructor Create;
  end;

  { TBDToolBox }

  TBDToolBox=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
    procedure Draw; override;
    procedure Clear; override;
  private
    fSX,fSY:integer;
    fColorIndex:integer;
    procedure DrawBarWithInk(x1,y1,x2,y2:integer);
    procedure DrawRectangleWithInk(x1,y1,x2,y2:integer);
  end;

  { TBDToolCircle }

  TBDToolCircle=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
    procedure Draw; override;
    procedure Clear; override;
  private
    fSX,fSY:integer;
    fColorIndex:integer;
    procedure DrawCircleWithInk(cx,cy,r:integer);
    procedure DrawFilledCircleWithInk(cx,cy,r:integer);
  end;

  TBDToolDraw=class(TBDTool)
    constructor Create; override;
    function MouseDown(x,y,button:integer):boolean; override;
    function MouseUp({%H-}x,{%H-}y,{%H-}button:integer):boolean; override;
    function MouseMove(x,y,{%H-}button:integer):boolean; override;
  private
    fDown:boolean;
  end;

  TBDToolFill=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
  private
    fLeft,fTop,fRight,fBottom:integer;
    procedure FloodFillWithPostProcessColor(x,y:integer);
  end;

  { TBDToolFillTo }

  TBDToolFillTo=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
    procedure Draw; override;
  private
    fLeft,fTop,fRight,fBottom:integer;
    fSourceColor:integer;
    procedure FillToWithPostProcessColor(x,y:integer);
  end;

  { TBDToolLine }

  TBDToolLine=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
    procedure Draw; override;
    procedure Clear; override;
  private
    fSX,fSY:integer;
    fColorIndex:integer;
    procedure DrawLineWithInk(x1,y1,x2,y2:integer);
  end;

  TBDToolSep=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
  end;

implementation

uses
  BDPSharedUnit, MKToolbox, Logger;

// ------------------------------------------------------------ [ TBDTool ] ---

constructor TBDTool.Create;
begin
  fState:=0;
end;

destructor TBDTool.Destroy;
begin
  Log.Trace('Destroying '+fName+'...');
  inherited ;
end;

procedure TBDTool.Draw;
begin
  // Do nothing. Tools with only 1 click operations (Draw, Fill) does not need
  // additional drawing. They don't need to override this.
end;

procedure TBDTool.Clear;
begin
  // Do nothing. Tools with only 1 click operations (Draw, Fill) does not need
  // additional clearing. They don't need to override this.
end;

procedure TBDTool.Move(x,y:integer);
begin
  fX:=x;
  fY:=y;
end;

function TBDTool.MouseDown(x,y,buttons:integer):boolean;
begin
  Result:=false;
end;

function TBDTool.MouseUp(x,y,buttons:integer):boolean;
begin
  Result:=false;
end;

function TBDTool.MouseMove(x,y,buttons:integer):boolean;
begin
  Result:=false;
end;

function TBDTool.Click(x,y,buttons:integer):boolean;
begin
  Result:=false;
end;

// ----------------------------------------------------------- [ TBDTools ] ---

constructor TBDTools.Create;
begin
  AddObject('BOX',TBDToolBox.Create);
  AddObject('CIRCLE',TBDToolCircle.Create);
  AddObject('DRAW',TBDToolDraw.Create);
  AddObject('FILL',TBDToolFill.Create);
  AddObject('FILLTO',TBDToolFillTo.Create);
  AddObject('LINE',TBDToolLine.Create);
  AddObject('SEP.',TBDToolSep.Create);
end;

// --------------------------------------------------------- [ TBDToolBox ] ---

constructor TBDToolBox.Create;
begin
  inherited ;
  fName:='BOX';
  fHint:=uppercase('Draw a rectangle (filled or not).');
end;

function TBDToolBox.Click(x,y,button:integer):boolean;
begin
  if button=1 then begin
    case fState of
      0:begin
          fSX:=x;
          fSY:=y;
          Result:=true;
          fState:=1;
          fColorIndex:=0;
        end;
      1:begin
          ActiveInk.InitializeArea(fSX,fSY,x,y);
          if FillShapes then begin
            if ActiveInk.SupportsOnTheFly then
              DrawBarWithInk(fSX,fSY,x,y)
            else begin
              MainImage.Bar(fSX,fSY,fX,fY,POSTPROCESSCOLOR);
              ActiveInk.PostProcess;
            end;
          end else begin
            if ActiveInk.SupportsOnTheFly then
              DrawRectangleWithInk(fSX,fSY,x,y)
            else begin
              MainImage.Rectangle(fSX,fSY,fX,fY,POSTPROCESSCOLOR);
              ActiveInk.PostProcess;
            end;
          end;
          Result:=true;
          fState:=0;
        end;
    end;
  end
  else if Button=3 then
    if fState>0 then begin  // Right button
      fState:=0;
      Result:=true;
    end else Result:=false
  else Result:=false;
end;

procedure TBDToolBox.Draw;
begin
  case fState of
    0:;
    1:begin
        inc(fColorIndex);
        if fColorIndex=length(VibroColors) then fColorIndex:=0;
        OverlayImage.Rectangle(fSX,fSY,fX,fY,VibroColors[fColorIndex]);

        InfoBar.ShowText('('+inttostr(fSX)+','+inttostr(fSY)+') '+
          'WI='+inttostr(abs(fSX-fX)+1)+' HE='+inttostr(abs(fSY-fY)+1)+' '+
          '('+inttostr(fX)+','+inttostr(fY)+') ');
      end;
  end;
end;

procedure TBDToolBox.Clear;
begin
  case fState of
    0:;
    1:begin
        OverlayImage.Rectangle(fSX,fSY,fX,fY,0);
      end;
  end;
end;

procedure TBDToolBox.DrawBarWithInk(x1,y1,x2,y2:integer);
var
  i,j:integer;
begin
  if x1>x2 then begin i:=x1;x1:=x2;x2:=i;end;
  if y1>y2 then begin i:=y1;y1:=y2;y2:=i;end;
  for j:=y1 to y2 do
    for i:=x1 to x2 do
      MainImage.PutPixel(i,j,ActiveInk.GetColorIndexAt(i,j));
end;

procedure TBDToolBox.DrawRectangleWithInk(x1,y1,x2,y2:integer);
var
  i:integer;
begin
  if x1>x2 then begin i:=x1;x1:=x2;x2:=i;end;
  if y1>y2 then begin i:=y1;y1:=y2;y2:=i;end;
  for i:=y1 to y2 do begin
    MainImage.PutPixel(x1,i,ActiveInk.GetColorIndexAt(x1,i));
    MainImage.PutPixel(x2,i,ActiveInk.GetColorIndexAt(x2,i));
  end;
  for i:=x1+1 to x2-1 do begin
    MainImage.PutPixel(i,y1,ActiveInk.GetColorIndexAt(i,y1));
    MainImage.PutPixel(i,y2,ActiveInk.GetColorIndexAt(i,y2));
  end;
end;

// ------------------------------------------------------ [ TBDToolCircle ] ---

constructor TBDToolCircle.Create;
begin
  inherited ;
  fName:='CIRCLE';
  fHint:=uppercase('Draw a circle. Right click to select method.');
end;

function TBDToolCircle.Click(x,y,button:integer):boolean;
var r:integer;
begin
  if button=1 then begin
    case fState of
      0:begin
          fSX:=x;
          fSY:=y;
          Result:=true;
          fState:=1;
          fColorIndex:=0;
        end;
      1:begin
//          if abs(fSX-x)>abs(fSY-y) then r:=abs(fSX-x) else r:=abs(fSY-y);
          r:=round(sqrt(sqr(fSX-x)+sqr(fSY-y)));
          ActiveInk.InitializeArea(fSX-r,fSY-r,fSX+r,fSY+r);
          if ActiveInk.SupportsOnTheFly then begin
            if FillShapes then
              DrawFilledCircleWithInk(fSX,fSY,r)
            else
              DrawCircleWithInk(fSX,fSY,r)
          end else begin
            if FillShapes then
              MainImage.FilledCircle(fSX,fSY,r,POSTPROCESSCOLOR)
            else
              MainImage.Circle(fSX,fSY,r,POSTPROCESSCOLOR);
            ActiveInk.PostProcess;
          end;
          Result:=true;
          fState:=0;
        end;
    end;
  end
  else if Button=3 then begin  // Right button
    if fState>0 then begin  // Right button
      fState:=0;
      Result:=true;
    end else Result:=false
  end else Result:=false;
end;

procedure TBDToolCircle.Draw;
var r:integer;
begin
  case fState of
    0:;
    1:begin
        inc(fColorIndex);
        if fColorIndex=length(VibroColors) then fColorIndex:=0;
        r:=round(sqrt(sqr(fSX-fX)+sqr(fSY-fY)));
//        if abs(fSX-fX)>abs(fSY-fY) then r:=abs(fSX-fX) else r:=abs(fSY-fY);
        OverlayImage.Circle(fSX,fSY,r,VibroColors[fColorIndex]);

        InfoBar.ShowText('('+inttostr(fSX)+','+inttostr(fSY)+') '+
          'RADIUS='+inttostr(r)+' '+
          '('+inttostr(fX)+','+inttostr(fY)+') ');
      end;
  end;
end;

procedure TBDToolCircle.Clear;
var r:integer;
begin
  case fState of
    0:;
    1:begin
        r:=round(sqrt(sqr(fSX-fX)+sqr(fSY-fY)));
        OverlayImage.Circle(fSX,fSY,r,0);
      end;
  end;
end;

// Taken from https://www.geeksforgeeks.org/bresenhams-circle-drawing-algorithm/
procedure TBDToolCircle.DrawCircleWithInk(cx,cy,r:integer);

  procedure PutPixel8(x,y:integer);
  begin
    MainImage.PutPixel(cx+x, cy+y, ActiveInk.GetColorIndexAt(cx+x, cy+y));
    MainImage.PutPixel(cx-x, cy+y, ActiveInk.GetColorIndexAt(cx-x, cy+y));
    MainImage.PutPixel(cx+x, cy-y, ActiveInk.GetColorIndexAt(cx+x, cy-y));
    MainImage.PutPixel(cx-x, cy-y, ActiveInk.GetColorIndexAt(cx-x, cy-y));
    MainImage.PutPixel(cx+y, cy+x, ActiveInk.GetColorIndexAt(cx+y, cy+x));
    MainImage.PutPixel(cx-y, cy+x, ActiveInk.GetColorIndexAt(cx-y, cy+x));
    MainImage.PutPixel(cx+y, cy-x, ActiveInk.GetColorIndexAt(cx+y, cy-x));
    MainImage.PutPixel(cx-y, cy-x, ActiveInk.GetColorIndexAt(cx-y, cy-x));
  end;

var x,y,d:integer;

begin
  x:=0;
  y:=r;
  d:=3-2*r;
  PutPixel8(x,y);
  while (y>=x) do begin
    inc(x);

    // check for decision parameter and correspondingly update d, x, y
    if d>0 then begin
      dec(y);
      d:=d+4*(x-y)+10;
    end else
      d:=d+4*x+6;
    PutPixel8(x,y);
  end;
end;

// Taken from https://www.geeksforgeeks.org/bresenhams-circle-drawing-algorithm/
// Modified to draw filled circle
procedure TBDToolCircle.DrawFilledCircleWithInk(cx,cy,r:integer);

  procedure HLine(x,y:integer);
  var i:integer;
  begin
    for i:=-x to +x do begin
      MainImage.PutPixel(cx+i,cy+y,ActiveInk.GetColorIndexAt(cx+i,cy+y));
      MainImage.PutPixel(cx+i,cy-y,ActiveInk.GetColorIndexAt(cx+i,cy-y));
    end;
    for i:=-y to +y do begin
      MainImage.PutPixel(cx+i,cy+x,ActiveInk.GetColorIndexAt(cx+i,cy+x));
      MainImage.PutPixel(cx+i,cy-x,ActiveInk.GetColorIndexAt(cx+i,cy-x));
    end;
  end;

var x,y,d:integer;

begin
  x:=0;
  y:=r;
  d:=3-2*r;
  HLine(x,y);
  while (y>=x) do begin
    inc(x);

    // check for decision parameter and correspondingly update d, x, y
    if d>0 then begin
      dec(y);
      d:=d+4*(x-y)+10;
    end else
      d:=d+4*x+6;
    HLine(x,y);
  end;
end;

// -------------------------------------------------------- [ TBDToolDraw ] ---

constructor TBDToolDraw.Create;
begin
  inherited ;
  fName:='DRAW';
  fHint:=uppercase('Freehand draw. Click and move mouse to draw.');
end;

function TBDToolDraw.MouseDown(x,y,button:integer):boolean;
begin
  if button=1 then begin
    MainImage.PutPixel(x,y,ActiveInk.GetColorIndexAt(x,y));
    Result:=true;
    fDown:=true;
  end else begin
    Result:=false;
    fDown:=false;
  end;
end;

function TBDToolDraw.MouseUp(x,y,button:integer):boolean;
begin
  if fDown then begin
    fDown:=false;
    Result:=true;
  end else Result:=false;
end;

function TBDToolDraw.MouseMove(x,y,button:integer):boolean;
begin
  if fDown then begin
    MainImage.PutPixel(x,y,ActiveInk.GetColorIndexAt(x,y));
    Result:=true;
  end else Result:=false;
end;

// -------------------------------------------------------- [ TBDToolFill ] ---

constructor TBDToolFill.Create;
begin
  inherited ;
  fName:='FILL';
  fHint:=uppercase('Apply ink to all pixels until stopped by a different color.');
end;

function TBDToolFill.Click(x,y,button:integer):boolean;
begin
  if button=1 then begin
    fLeft:=x;
    fTop:=y;
    fRight:=x;
    fBottom:=y;
    FloodFillWithPostProcessColor(x,y);
    ActiveInk.InitializeArea(fLeft,fTop,fRight,fBottom);
    ActiveInk.PostProcess;
    Result:=true;
  end else Result:=false;
end;

procedure TBDToolFill.FloodFillWithPostProcessColor(x,y:integer);
var i,j,ic,cc:integer;w:boolean;

  function FFCheckPixel(x,y,src:integer):boolean;
  begin
    Result:=false;
    if (y>0) and (MainImage.GetPixel(x,y-1)=src) then begin
      MainImage.PutPixel(x,y-1,POSTPROCESSCOLOR);
      if y-1<fTop then fTop:=y-1;
      Result:=true;
    end;
    if (x<MainImage.Width-1) and (MainImage.GetPixel(x+1,y)=src) then begin
      MainImage.PutPixel(x+1,y,POSTPROCESSCOLOR);
      if x+1>fRight then fRight:=x+1;
      Result:=true;
    end;
    if (y<MainImage.Height-1) and (MainImage.GetPixel(x,y+1)=src) then begin
      MainImage.PutPixel(x,y+1,POSTPROCESSCOLOR);
      if y+1>fBottom then fBottom:=y+1;
      Result:=true;
    end;
    if (x>0) and (MainImage.GetPixel(x-1,y)=src) then begin
      MainImage.PutPixel(x-1,y,POSTPROCESSCOLOR);
      if x-1<fLeft then fLeft:=x-1;
      Result:=true;
    end;
  end;

begin
  cc:=MainImage.GetPixel(x,y);
  MainImage.PutPixel(x,y,POSTPROCESSCOLOR);
  ic:=0;
  repeat
    w:=false;

//    Log.Trace(Format('--- Iteration %d. ---',[ic]));
//    Log.Trace(Format('Left=%d, Top=%d, Right=%d, Bottom=%d',[fLeft,fTop,fRight,fBottom]));
    case (ic mod 4) of
      0:begin
          j:=fTop;
          while j<=fBottom do begin
            i:=fLeft;
            while i<=fRight do begin
              if MainImage.GetPixel(i,j)=POSTPROCESSCOLOR then
                if FFCheckPixel(i,j,cc) then w:=true;
              inc(i);
            end;
            inc(j);
          end;
        end;
      1:begin
          j:=fBottom;
          while j>=fTop do begin
            i:=fLeft;
            while i<=fRight do begin
              if MainImage.GetPixel(i,j)=POSTPROCESSCOLOR then
                if FFCheckPixel(i,j,cc) then w:=true;
              inc(i);
            end;
            dec(j);
          end;
        end;
      2:begin
          j:=fBottom;
          while j>=fTop do begin
            i:=fRight;
            while i>=fLeft do begin
              if MainImage.GetPixel(i,j)=POSTPROCESSCOLOR then
                if FFCheckPixel(i,j,cc) then w:=true;
              dec(i);
            end;
            dec(j);
          end;
        end;
      3:begin
          j:=fTop;
          while j<=fBottom do begin
            i:=fRight;
            while i>=fLeft do begin
              if MainImage.GetPixel(i,j)=POSTPROCESSCOLOR then
                if FFCheckPixel(i,j,cc) then w:=true;
              dec(i);
            end;
            inc(j);
          end;
        end;
    end;
    inc(ic);
  until not w;
  Log.Trace(Format('FloodFill completed with %d iterations.',[ic]));
end;

// ------------------------------------------------------ [ TBDToolFillTo ] ---

constructor TBDToolFillTo.Create;
begin
  inherited ;
  fName:='FILLTO';
  fHint:=uppercase('Click to select boundary color. Click again inside to fill.');
end;

function TBDToolFillTo.Click(x,y,button:integer):boolean;
begin
  if button=1 then
    case fstate of
      0:begin
          fSourceColor:=MainImage.GetPixel(x,y);
          fState:=1;
          Result:=true;
        end;
      1:begin
          fLeft:=x;
          fTop:=y;
          fRight:=x;
          fBottom:=y;
          FillToWithPostProcessColor(x,y);
          ActiveInk.InitializeArea(fLeft,fTop,fRight,fBottom);
          ActiveInk.PostProcess;
          fState:=0;
          Result:=true;
        end;
    end
  else Result:=false;
end;

procedure TBDToolFillTo.Draw;
begin
  case fState of
    0:;
    1:begin
        InfoBar.ShowText('('+inttostr(fX)+','+inttostr(fY)+') '+
          'SELECTED COLOR INDEX: '+inttostr(fSourceColor)+'. '+
          'CLICK TO FILL');
      end;
  end;
end;

procedure TBDToolFillTo.FillToWithPostProcessColor(x,y:integer);
var i,j,ic:integer;w:boolean;

  function FFCheckPixel(x,y:integer):boolean;
  begin
    Result:=false;
    if (y>0) and (MainImage.GetPixel(x,y-1)<>fSourceColor) and
      (MainImage.GetPixel(x,y-1)<>POSTPROCESSCOLOR) then begin
      MainImage.PutPixel(x,y-1,POSTPROCESSCOLOR);
      if y-1<fTop then fTop:=y-1;
      Result:=true;
    end;
    if (x<MainImage.Width-1) and (MainImage.GetPixel(x+1,y)<>fSourceColor) and
      (MainImage.GetPixel(x+1,y)<>POSTPROCESSCOLOR) then begin
      MainImage.PutPixel(x+1,y,POSTPROCESSCOLOR);
      if x+1>fRight then fRight:=x+1;
      Result:=true;
    end;
    if (y<MainImage.Height-1) and (MainImage.GetPixel(x,y+1)<>fSourceColor) and
      (MainImage.GetPixel(x,y+1)<>POSTPROCESSCOLOR) then begin
      MainImage.PutPixel(x,y+1,POSTPROCESSCOLOR);
      if y+1>fBottom then fBottom:=y+1;
      Result:=true;
    end;
    if (x>0) and (MainImage.GetPixel(x-1,y)<>fSourceColor) and
      (MainImage.GetPixel(x-1,y)<>POSTPROCESSCOLOR) then begin
      MainImage.PutPixel(x-1,y,POSTPROCESSCOLOR);
      if x-1<fLeft then fLeft:=x-1;
      Result:=true;
    end;
  end;

begin
  MainImage.PutPixel(x,y,POSTPROCESSCOLOR);
  ic:=0;
  repeat
    w:=false;

//    Log.Trace(Format('--- Iteration %d. ---',[ic]));
//    Log.Trace(Format('Left=%d, Top=%d, Right=%d, Bottom=%d',[fLeft,fTop,fRight,fBottom]));
    case (ic mod 4) of
      0:begin
          j:=fTop;
          while j<=fBottom do begin
            i:=fLeft;
            while i<=fRight do begin
              if MainImage.GetPixel(i,j)=POSTPROCESSCOLOR then
                if FFCheckPixel(i,j) then w:=true;
              inc(i);
            end;
            inc(j);
          end;
        end;
      1:begin
          j:=fBottom;
          while j>=fTop do begin
            i:=fLeft;
            while i<=fRight do begin
              if MainImage.GetPixel(i,j)=POSTPROCESSCOLOR then
                if FFCheckPixel(i,j) then w:=true;
              inc(i);
            end;
            dec(j);
          end;
        end;
      2:begin
          j:=fBottom;
          while j>=fTop do begin
            i:=fRight;
            while i>=fLeft do begin
              if MainImage.GetPixel(i,j)=POSTPROCESSCOLOR then
                if FFCheckPixel(i,j) then w:=true;
              dec(i);
            end;
            dec(j);
          end;
        end;
      3:begin
          j:=fTop;
          while j<=fBottom do begin
            i:=fRight;
            while i>=fLeft do begin
              if MainImage.GetPixel(i,j)=POSTPROCESSCOLOR then
                if FFCheckPixel(i,j) then w:=true;
              dec(i);
            end;
            inc(j);
          end;
        end;
    end;
    inc(ic);
  until not w;
  Infobar.ShowText(Format('FLOODFILL COMPLETED WITH %d ITERATIONS.',[ic]));
end;

// -------------------------------------------------------- [ TBDToolLine ] ---

constructor TBDToolLine.Create;
begin
  inherited ;
  fName:='LINE';
  fHint:=uppercase('Click for start then click for the end.');
end;

function TBDToolLine.Click(x,y,button:integer):boolean;
begin
  if button=1 then begin
    case fState of
      0:begin
          fSX:=x;
          fSY:=y;
          Result:=true;
          fState:=1;
          fColorIndex:=0;
        end;
      1:begin
          ActiveInk.InitializeArea(fSX,fSY,x,y);
          if ActiveInk.SupportsOnTheFly then
            DrawLineWithInk(fSX,fSY,x,y)
          else begin
            MainImage.Line(fSX,fSY,fX,fY,POSTPROCESSCOLOR);
            ActiveInk.PostProcess;
          end;
          Result:=true;
          fState:=0;
        end;
    end;
  end
  else if Button=3 then begin  // Right button
    if fState>0 then begin  // Right button
      fState:=0;
      Result:=true;
    end else Result:=false
  end else Result:=false;
end;

procedure TBDToolLine.Draw;
var d:integer;
begin
  case fState of
    0:;
    1:begin
        inc(fColorIndex);
        if fColorIndex=length(VibroColors) then fColorIndex:=0;
        OverlayImage.Line(fSX,fSY,fX,fY,VibroColors[fColorIndex]);
        if (fSX>fX) then begin
          d:=trunc(arctan((fSY-fY)/(fSX-fX))*180/pi)+270;
        end else
        if (fSX<fX) then begin
          d:=trunc(arctan((fSY-fY)/(fSX-fX))*180/pi)+90;
        end else begin
          if (fSY>=fY) then begin
            d:=0;
          end else begin
            d:=180;
          end;
        end;

        InfoBar.ShowText('('+inttostr(fSX)+','+inttostr(fSY)+') '+
          'WI='+inttostr(abs(fSX-fX)+1)+' HE='+inttostr(abs(fSY-fY)+1)+' '+
          '('+inttostr(fX)+','+inttostr(fY)+') '+
          'DEG='+inttostr(d));
      end;
  end;
end;

procedure TBDToolLine.Clear;
var d:integer;
begin
  case fState of
    0:;
    1:OverlayImage.Line(fSX,fSY,fX,fY,0);
  end;
end;

// Taken from http://www.efg2.com/Lab/Library/Delphi/Graphics/Bresenham.txt
// Stripped a few comments, variable names changed here and there...
procedure TBDToolLine.DrawLineWithInk(x1,y1,x2,y2:integer);
var
  a,b,d : integer;
  diag_inc, nondiag_inc : integer;
  dx_diag, dx_nondiag, dy_diag, dy_nondiag : integer;
  i,swap,x,y : integer;
begin
  x := x1;
  y := y1;
  {Determine drawing direction and step to the next pixel.}
  a := x2 - x1;
  b := y2 - y1;
  {Determine whether end point lies to right or left of start point.}
  if a < 0 then begin
    a := -a;
    dx_diag := -1;
  end else
    dx_diag := 1;
  {Determine whether end point lies above or below start point.}
  if b < 0 then begin
    b := -b;
    dy_diag := -1
  end else
    dy_diag := 1;
  {Identify octant containing end point.}
  if a < b then begin
    swap := a;
    a := b;
    b := swap;
    dx_nondiag := 0;
    dy_nondiag := dy_diag
  end else begin
    dx_nondiag := dx_diag;
    dy_nondiag := 0
  end;
  d := b + b - a;
  nondiag_inc := b + b;
  diag_inc    := b + b - a - a;
  for i := 0 to a do begin   {draw the a+1 pixels}
    MainImage.PutPixel(x,y,ActiveInk.GetColorIndexAt(x,y));
    if d < 0 then begin
      x := x + dx_nondiag;
      y := y + dy_nondiag;
      d := d + nondiag_inc
    end else begin
      x := x + dx_diag;
      y := y + dy_diag;
      d := d + diag_inc
    end;
  end;
end;

// --------------------------------------------------------- [ TBDToolSep ] ---

constructor TBDToolSep.Create;
begin
  inherited ;
  fName:='SEP.';
  fHint:=uppercase('Change the clicked color to ink color.');
end;

function TBDToolSep.Click(x,y,button:integer):boolean;
var i,j,sc:integer;
  fLeft,fRight,fTop,fBottom:integer;
begin
  if button=1 then begin
    fLeft:=MainImage.Width;
    fRight:=-1;
    fTop:=MainImage.Height;
    fBottom:=-1;
    sc:=MainImage.GetPixel(x,y);
    for j:=0 to MainImage.Height-1 do
      for i:=0 to MainImage.Width-1 do
        if MainImage.GetPixel(i,j)=sc then begin
          MainImage.Putpixel(i,j,POSTPROCESSCOLOR);
          if i>fRight then fRight:=i;
          if i<fLeft then fLeft:=i;
          if j>fBottom then fBottom:=j;
          if j<fTop then fTop:=j;
        end;
    ActiveInk.InitializeArea(fLeft,fTop,fRight,fBottom);
    ActiveInk.PostProcess;
    Result:=true;
  end else Result:=false;
end;

end.


