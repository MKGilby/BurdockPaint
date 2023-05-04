unit BDPTools;

{$mode delphi}

interface

uses
  Classes, SysUtils, Lists, BDPImage;

type

  { TBDTool }

  TBDTool=class
    constructor Create; virtual;
    procedure Initialize; virtual; // If the tool needs some initialization before every use
    procedure Draw; virtual; // Draw helping lines, refresh infobar
    procedure Clear; virtual;  // Clear helping lines!
    procedure Move(x,y:integer); virtual;
    function MouseDown(x,y,buttons:integer):boolean; virtual;
    function MouseMove(x,y,buttons:integer):boolean; virtual;
    function MouseUp(x,y,buttons:integer):boolean; virtual;
    function Click(x,y,buttons:integer):boolean; virtual;   // Handle clicks
  protected
    fState:integer;  // 0 - Waiting for first click, 1 - waiting for second click, etc.
    fX,fY:integer;  // Current position from Move
    fName,fHint:string;
    fPinnable:boolean;
  public
    property Name:string read fName;
    property Hint:string read fHint;
    property Pinnable:boolean read fPinnable;
  end;

  TBDTools=class(TNamedList<TBDTool>)
    constructor Create;
  end;

  { TBDToolBox }

  TBDToolBox=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
    function MouseUp(x,y,button:integer):boolean; override;
    procedure Draw; override;
    procedure Clear; override;
  private
    fSX,fSY:integer;
    procedure DrawBarWithInk(x1,y1,x2,y2:integer);
    procedure DrawRectangleWithInk(x1,y1,x2,y2:integer);
  end;

  { TBDToolCircle }

  TBDToolCircle=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
    function MouseUp(x,y,button:integer):boolean; override;
    procedure Draw; override;
    procedure Clear; override;
  private
    fSX,fSY:integer;
    procedure DrawCircleWithInk(cx,cy,r:integer);
    procedure DrawFilledCircleWithInk(cx,cy,r:integer);
  end;

  { TBDToolDraw }

  TBDToolDraw=class(TBDTool)
    constructor Create; override;
    function MouseDown(x,y,button:integer):boolean; override;
    function MouseUp(x,y,button:integer):boolean; override;
    function MouseMove(x,y,button:integer):boolean; override;
  private
    fTempImage:TBDImage;
    fLeft,fTop,fRight,fBottom:integer;
    fDown:boolean;
  end;

  { TBDToolFill }

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
    function MouseUp(x,y,button:integer):boolean; override;
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
    function MouseUp(x,y,button:integer):boolean; override;
    procedure Draw; override;
    procedure Clear; override;
  private
    fSX,fSY:integer;
    procedure DrawLineWithInk(x1,y1,x2,y2:integer);
  end;

  { TBDToolSep }

  TBDToolSep=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
  end;

  { TBDToolGetCel }

  TBDToolGetCel=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
    function MouseUp(x,y,button:integer):boolean; override;
    procedure Draw; override;
    procedure Clear; override;
  private
    fSX,fSY:integer;
  end;

  { TBDToolPutCel }

  TBDToolPutCel=class(TBDTool)
    constructor Create; override;
    procedure Initialize; override;
    function Click(x,y,button:integer):boolean; override;
    function MouseUp(x,y,button:integer):boolean; override;
    procedure Draw; override;
    procedure Clear; override;
  private
    fSX,fSY:integer;
  end;

  { TBDToolPickColor }

  TBDToolPickColor=class(TBDTool)
    constructor Create; override;
    procedure Move(x,y:integer); override;
    function Click(x,y,button:integer):boolean; override;
    function MouseUp(x,y,button:integer):boolean; override;
    procedure SetColor(colorindex:integer);
  private
    fColorIndex:integer;
  end;

  { TBDToolSelectColor }

  TBDToolSelectColor=class(TBDTool)
    constructor Create; override;
    procedure Move(x,y:integer); override;
    function Click(x,y,button:integer):boolean; override;
    procedure SetColor(colorindex:integer);
  private
    fColorIndex:integer;
  end;

  { TBDToolShowCEL }

  TBDToolShowCEL=class(TBDTool)
    constructor Create; override;
    procedure Initialize; override;
    procedure Draw; override;
    procedure Clear; override;
  private
    fStartTime:UInt64;
  end;

implementation

uses
  BDPShared, MKToolbox, Logger, BDPMessage, BDPSettings, SDL2;

// ------------------------------------------------------------ [ TBDTool ] ---

constructor TBDTool.Create;
begin
  fState:=0;
  fPinnable:=false;
end;

procedure TBDTool.Initialize;
begin
  // Do nothing. Override if the Tool needs initializing before every use.
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
  AddObject('GETCEL',TBDToolGetCel.Create);
  AddObject('PUTCEL',TBDToolPutCel.Create);
  AddObject('PICKCOL',TBDToolPickColor.Create);
  AddObject('SELCOL',TBDToolSelectColor.Create);
  AddObject('SHOWCEL',TBDToolShowCEL.Create);
end;

// --------------------------------------------------------- [ TBDToolBox ] ---

constructor TBDToolBox.Create;
begin
  inherited ;
  fName:='BOX';
  fHint:=uppercase('Draw a rectangle (filled or not).');
  fPinnable:=true;
end;

function TBDToolBox.Click(x,y,button:integer):boolean;
var i:integer;
begin
  if button=SDL_BUTTON_LEFT then begin
    case fState of
      0:begin
          fSX:=x;
          fSY:=y;
          Result:=true;
          fState:=1;
        end;
      1:begin
          if fSX>x then begin i:=x;x:=fSX;fSX:=i;end;
          if fSY>y then begin i:=y;y:=fSY;fSY:=i;end;
          Project.CurrentImage.ImageUndo.AddImageUndo(fSX,fSY,x-fSX+1,y-fSY+1);
          ActiveInk.InitializeArea(fSX,fSY,x,y);
          if Settings.FillShapes then begin
            if ActiveInk.SupportsOnTheFly then
              DrawBarWithInk(fSX,fSY,x,y)
            else begin
              Project.CurrentImage.Bar(fSX,fSY,fX,fY,POSTPROCESSCOLOR);
              ActiveInk.PostProcess;
            end;
          end else begin
            if ActiveInk.SupportsOnTheFly then
              DrawRectangleWithInk(fSX,fSY,x,y)
            else begin
              Project.CurrentImage.Rectangle(fSX,fSY,fX,fY,POSTPROCESSCOLOR);
              ActiveInk.PostProcess;
            end;
          end;
          Project.CurrentImage.ImageUndo.AddImageRedoToLastUndo(fSX,fSY,x-fSX+1,y-fSY+1);
          InfoBar.ShowText('');
          fState:=0;
          Result:=true;
        end;
    end;
  end
  else if Button=SDL_BUTTON_RIGHT then
    if fState>0 then begin
      fState:=0;
      InfoBar.ShowText('');
      Result:=true;
    end else Result:=false
  else Result:=false;
end;

function TBDToolBox.MouseUp(x,y,button:integer):boolean;
begin
  Result:=fState>0;
end;

procedure TBDToolBox.Draw;
begin
  case fState of
    0:;
    1:begin
        Project.OverlayImage.Rectangle(fSX,fSY,fX,fY,VibroColors.GetColorIndex);

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
        Project.OverlayImage.Rectangle(fSX,fSY,fX,fY,0);
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
      Project.CurrentImage.PutPixel(i,j,ActiveInk.GetColorIndexAt(i,j));
end;

procedure TBDToolBox.DrawRectangleWithInk(x1,y1,x2,y2:integer);
var
  i:integer;
begin
  if x1>x2 then begin i:=x1;x1:=x2;x2:=i;end;
  if y1>y2 then begin i:=y1;y1:=y2;y2:=i;end;
  for i:=y1 to y2 do begin
    Project.CurrentImage.PutPixel(x1,i,ActiveInk.GetColorIndexAt(x1,i));
    Project.CurrentImage.PutPixel(x2,i,ActiveInk.GetColorIndexAt(x2,i));
  end;
  for i:=x1+1 to x2-1 do begin
    Project.CurrentImage.PutPixel(i,y1,ActiveInk.GetColorIndexAt(i,y1));
    Project.CurrentImage.PutPixel(i,y2,ActiveInk.GetColorIndexAt(i,y2));
  end;
end;

// ------------------------------------------------------ [ TBDToolCircle ] ---

constructor TBDToolCircle.Create;
begin
  inherited ;
  fName:='CIRCLE';
  fHint:=uppercase('Draw a circle. Right click to select method.');
  fPinnable:=true;
end;

function TBDToolCircle.Click(x,y,button:integer):boolean;
var r:integer;
begin
  if button=SDL_BUTTON_LEFT then begin
    case fState of
      0:begin
          fSX:=x;
          fSY:=y;
          Result:=true;
          fState:=1;
        end;
      1:begin
          r:=round(sqrt(sqr(fSX-x)+sqr(fSY-y)));
          Project.CurrentImage.ImageUndo.AddImageUndo(fSX-r,fSY-r,r*2+1,r*2+1);
          ActiveInk.InitializeArea(fSX-r,fSY-r,fSX+r,fSY+r);
          if ActiveInk.SupportsOnTheFly then begin
            if Settings.FillShapes then
              DrawFilledCircleWithInk(fSX,fSY,r)
            else
              DrawCircleWithInk(fSX,fSY,r)
          end else begin
            if Settings.FillShapes then
              Project.CurrentImage.FilledCircle(fSX,fSY,r,POSTPROCESSCOLOR)
            else
              Project.CurrentImage.Circle(fSX,fSY,r,POSTPROCESSCOLOR);
            ActiveInk.PostProcess;
          end;
          Project.CurrentImage.ImageUndo.AddImageRedoToLastUndo(fSX-r,fSY-r,r*2+1,r*2+1);
          InfoBar.ShowText('');
          Result:=true;
          fState:=0;
        end;
    end;
  end
  else if Button=SDL_BUTTON_RIGHT then begin  // Right button
    if fState>0 then begin
      fState:=0;
      InfoBar.ShowText('');
      Result:=true;
    end else Result:=false
  end else Result:=false;
end;

function TBDToolCircle.MouseUp(x,y,button:integer):boolean;
begin
  Result:=fState>0;
end;

procedure TBDToolCircle.Draw;
var r:integer;
begin
  case fState of
    0:;
    1:begin
        r:=round(sqrt(sqr(fSX-fX)+sqr(fSY-fY)));
        Project.OverlayImage.Circle(fSX,fSY,r,VibroColors.GetColorIndex);

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
        Project.OverlayImage.Circle(fSX,fSY,r,0);
      end;
  end;
end;

// Taken from https://www.geeksforgeeks.org/bresenhams-circle-drawing-algorithm/
procedure TBDToolCircle.DrawCircleWithInk(cx,cy,r:integer);

  procedure PutPixel8(x,y:integer);
  begin
    Project.CurrentImage.PutPixel(cx+x, cy+y, ActiveInk.GetColorIndexAt(cx+x, cy+y));
    Project.CurrentImage.PutPixel(cx-x, cy+y, ActiveInk.GetColorIndexAt(cx-x, cy+y));
    Project.CurrentImage.PutPixel(cx+x, cy-y, ActiveInk.GetColorIndexAt(cx+x, cy-y));
    Project.CurrentImage.PutPixel(cx-x, cy-y, ActiveInk.GetColorIndexAt(cx-x, cy-y));
    Project.CurrentImage.PutPixel(cx+y, cy+x, ActiveInk.GetColorIndexAt(cx+y, cy+x));
    Project.CurrentImage.PutPixel(cx-y, cy+x, ActiveInk.GetColorIndexAt(cx-y, cy+x));
    Project.CurrentImage.PutPixel(cx+y, cy-x, ActiveInk.GetColorIndexAt(cx+y, cy-x));
    Project.CurrentImage.PutPixel(cx-y, cy-x, ActiveInk.GetColorIndexAt(cx-y, cy-x));
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
      Project.CurrentImage.PutPixel(cx+i,cy+y,ActiveInk.GetColorIndexAt(cx+i,cy+y));
      Project.CurrentImage.PutPixel(cx+i,cy-y,ActiveInk.GetColorIndexAt(cx+i,cy-y));
    end;
    for i:=-y to +y do begin
      Project.CurrentImage.PutPixel(cx+i,cy+x,ActiveInk.GetColorIndexAt(cx+i,cy+x));
      Project.CurrentImage.PutPixel(cx+i,cy-x,ActiveInk.GetColorIndexAt(cx+i,cy-x));
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
  fPinnable:=true;
end;

function TBDToolDraw.MouseDown(x,y,button:integer):boolean;
begin
  if button=SDL_BUTTON_LEFT then begin
    fTempImage:=TBDImage.Create(Project.CurrentImage.Width,Project.CurrentImage.Height);
    fTempImage.Palette.ResizeAndCopyColorsFrom(Project.CurrentImage.Palette);
    fTempImage.PutImage(0,0,Project.CurrentImage);
    Project.CurrentImage.PutPixel(x,y,ActiveInk.GetColorIndexAt(x,y));
    Result:=true;
    fLeft:=x;
    fTop:=y;
    fRight:=x;
    fBottom:=y;
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
    Project.CurrentImage.ImageUndo.AddImageUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1,fTempImage);
    FreeAndNil(fTempImage);
    Project.CurrentImage.ImageUndo.AddImageRedoToLastUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1);
    Result:=true;
  end else Result:=false;
end;

function TBDToolDraw.MouseMove(x,y,button:integer):boolean;
begin
  if fDown then begin
    Project.CurrentImage.PutPixel(x,y,ActiveInk.GetColorIndexAt(x,y));
    if fLeft>x then fLeft:=x;
    if fTop>y then fTop:=y;
    if fRight<x then fRight:=x;
    if fBottom<y then fBottom:=y;
    Result:=true;
  end else Result:=false;
end;

// -------------------------------------------------------- [ TBDToolFill ] ---

constructor TBDToolFill.Create;
begin
  inherited ;
  fName:='FILL';
  fHint:=uppercase('Apply ink to all pixels until stopped by a different color.');
  fPinnable:=true;
end;

function TBDToolFill.Click(x,y,button:integer):boolean;
var fTempImage:TBDImage;
begin
  if button=SDL_BUTTON_LEFT then begin
    fLeft:=x;
    fTop:=y;
    fRight:=x;
    fBottom:=y;
    fTempImage:=TBDImage.Create(Project.CurrentImage.Width,Project.CurrentImage.Height);
    fTempImage.Palette.ResizeAndCopyColorsFrom(Project.CurrentImage.Palette);
    fTempImage.PutImage(0,0,Project.CurrentImage);

    FloodFillWithPostProcessColor(x,y);

    Project.CurrentImage.ImageUndo.AddImageUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1,fTempImage);
    FreeAndNil(fTempImage);
    ActiveInk.InitializeArea(fLeft,fTop,fRight,fBottom);
    ActiveInk.PostProcess;
    Project.CurrentImage.ImageUndo.AddImageRedoToLastUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1);
    Result:=true;
  end else Result:=false;
end;

procedure TBDToolFill.FloodFillWithPostProcessColor(x,y:integer);
var i,j,ic,cc:integer;w:boolean;

  function FFCheckPixel(x,y,src:integer):boolean;
  begin
    Result:=false;
    if (y>0) and (Project.CurrentImage.GetPixel(x,y-1)=src) then begin
      Project.CurrentImage.PutPixel(x,y-1,POSTPROCESSCOLOR);
      if y-1<fTop then fTop:=y-1;
      Result:=true;
    end;
    if (x<Project.CurrentImage.Width-1) and (Project.CurrentImage.GetPixel(x+1,y)=src) then begin
      Project.CurrentImage.PutPixel(x+1,y,POSTPROCESSCOLOR);
      if x+1>fRight then fRight:=x+1;
      Result:=true;
    end;
    if (y<Project.CurrentImage.Height-1) and (Project.CurrentImage.GetPixel(x,y+1)=src) then begin
      Project.CurrentImage.PutPixel(x,y+1,POSTPROCESSCOLOR);
      if y+1>fBottom then fBottom:=y+1;
      Result:=true;
    end;
    if (x>0) and (Project.CurrentImage.GetPixel(x-1,y)=src) then begin
      Project.CurrentImage.PutPixel(x-1,y,POSTPROCESSCOLOR);
      if x-1<fLeft then fLeft:=x-1;
      Result:=true;
    end;
  end;

begin
  cc:=Project.CurrentImage.GetPixel(x,y);
  Project.CurrentImage.PutPixel(x,y,POSTPROCESSCOLOR);
  ic:=0;
  repeat
    w:=false;
    case (ic mod 4) of
      0:begin
          j:=fTop;
          while j<=fBottom do begin
            i:=fLeft;
            while i<=fRight do begin
              if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
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
              if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
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
              if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
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
              if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
                if FFCheckPixel(i,j,cc) then w:=true;
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

// ------------------------------------------------------ [ TBDToolFillTo ] ---

constructor TBDToolFillTo.Create;
begin
  inherited ;
  fName:='FILLTO';
  fHint:=uppercase('Click to select boundary color. Click again inside to fill.');
  fPinnable:=true;
end;

function TBDToolFillTo.Click(x,y,button:integer):boolean;
var fTempImage:TBDImage;
begin
  if button=SDL_BUTTON_LEFT then
    case fstate of
      0:begin
          fSourceColor:=Project.CurrentImage.GetPixel(x,y);
          fState:=1;
          Result:=true;
        end;
      1:begin
          fLeft:=x;
          fTop:=y;
          fRight:=x;
          fBottom:=y;

          fTempImage:=TBDImage.Create(Project.CurrentImage.Width,Project.CurrentImage.Height);
          fTempImage.Palette.ResizeAndCopyColorsFrom(Project.CurrentImage.Palette);
          fTempImage.PutImage(0,0,Project.CurrentImage);

          FillToWithPostProcessColor(x,y);
          Project.CurrentImage.ImageUndo.AddImageUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1,fTempImage);
          FreeAndNil(fTempImage);

          ActiveInk.InitializeArea(fLeft,fTop,fRight,fBottom);
          ActiveInk.PostProcess;
          Project.CurrentImage.ImageUndo.AddImageRedoToLastUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1);
          fState:=0;
          Result:=true;
        end;
    end
  else Result:=false;
end;

function TBDToolFillTo.MouseUp(x,y,button:integer):boolean;
begin
  Result:=fState>0;
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
    if (y>0) and (Project.CurrentImage.GetPixel(x,y-1)<>fSourceColor) and
      (Project.CurrentImage.GetPixel(x,y-1)<>POSTPROCESSCOLOR) then begin
      Project.CurrentImage.PutPixel(x,y-1,POSTPROCESSCOLOR);
      if y-1<fTop then fTop:=y-1;
      Result:=true;
    end;
    if (x<Project.CurrentImage.Width-1) and (Project.CurrentImage.GetPixel(x+1,y)<>fSourceColor) and
      (Project.CurrentImage.GetPixel(x+1,y)<>POSTPROCESSCOLOR) then begin
      Project.CurrentImage.PutPixel(x+1,y,POSTPROCESSCOLOR);
      if x+1>fRight then fRight:=x+1;
      Result:=true;
    end;
    if (y<Project.CurrentImage.Height-1) and (Project.CurrentImage.GetPixel(x,y+1)<>fSourceColor) and
      (Project.CurrentImage.GetPixel(x,y+1)<>POSTPROCESSCOLOR) then begin
      Project.CurrentImage.PutPixel(x,y+1,POSTPROCESSCOLOR);
      if y+1>fBottom then fBottom:=y+1;
      Result:=true;
    end;
    if (x>0) and (Project.CurrentImage.GetPixel(x-1,y)<>fSourceColor) and
      (Project.CurrentImage.GetPixel(x-1,y)<>POSTPROCESSCOLOR) then begin
      Project.CurrentImage.PutPixel(x-1,y,POSTPROCESSCOLOR);
      if x-1<fLeft then fLeft:=x-1;
      Result:=true;
    end;
  end;

begin
  Project.CurrentImage.PutPixel(x,y,POSTPROCESSCOLOR);
  ic:=0;
  repeat
    w:=false;

    case (ic mod 4) of
      0:begin
          j:=fTop;
          while j<=fBottom do begin
            i:=fLeft;
            while i<=fRight do begin
              if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
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
              if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
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
              if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
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
              if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
                if FFCheckPixel(i,j) then w:=true;
              dec(i);
            end;
            inc(j);
          end;
        end;
    end;
    inc(ic);
  until not w;
  Infobar.ShowText(Format('FILLTO COMPLETED WITH %d ITERATIONS.',[ic]));
end;

// -------------------------------------------------------- [ TBDToolLine ] ---

constructor TBDToolLine.Create;
begin
  inherited ;
  fName:='LINE';
  fHint:=uppercase('Click for start then click for the end.');
  fPinnable:=true;
end;

function TBDToolLine.Click(x,y,button:integer):boolean;
var Left,Top,Width,Height:integer;
begin
  if button=SDL_BUTTON_LEFT then begin
    case fState of
      0:begin
          fSX:=x;
          fSY:=y;
          Result:=true;
          fState:=1;
        end;
      1:begin
          if fSX>x then begin
            Left:=x;
            Width:=fSX-x+1;
          end else begin
            Left:=fSX;
            Width:=x-fSX+1;
          end;
          if fSY>y then begin
            Top:=y;
            Height:=fSY-y+1;
          end else begin
            Top:=fSY;
            Height:=y-fSY+1;
          end;
          Project.CurrentImage.ImageUndo.AddImageUndo(Left,Top,Width,Height);
          ActiveInk.InitializeArea(fSX,fSY,x,y);
          if ActiveInk.SupportsOnTheFly then
            DrawLineWithInk(fSX,fSY,x,y)
          else begin
            Project.CurrentImage.Line(fSX,fSY,fX,fY,POSTPROCESSCOLOR);
            ActiveInk.PostProcess;
          end;
          Project.CurrentImage.ImageUndo.AddImageRedoToLastUndo(Left,Top,Width,Height);
          Result:=true;
          fState:=0;
          InfoBar.ShowText('');
        end;
    end;
  end
  else if Button=SDL_BUTTON_RIGHT then begin
    if fState>0 then begin
      fState:=0;
      Result:=true;
      InfoBar.ShowText('');
    end else Result:=false
  end else Result:=false;
end;

function TBDToolLine.MouseUp(x,y,button:integer):boolean;
begin
  Result:=fState>0;
end;

procedure TBDToolLine.Draw;
var d:integer;
begin
  case fState of
    0:;
    1:begin
        Project.OverlayImage.Line(fSX,fSY,fX,fY,VibroColors.GetColorIndex);
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
begin
  case fState of
    0:;
    1:Project.OverlayImage.Line(fSX,fSY,fX,fY,0);
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
    Project.CurrentImage.PutPixel(x,y,ActiveInk.GetColorIndexAt(x,y));
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
  fPinnable:=true;
end;

function TBDToolSep.Click(x,y,button:integer):boolean;
var i,j,sc:integer;
  fLeft,fRight,fTop,fBottom:integer;
  fTempImage:TBDImage;
begin
  if button=SDL_BUTTON_LEFT then begin
    fTempImage:=TBDImage.Create(Project.CurrentImage.Width,Project.CurrentImage.Height);
    fTempImage.Palette.ResizeAndCopyColorsFrom(Project.CurrentImage.Palette);
    fTempImage.PutImage(0,0,Project.CurrentImage);
    fLeft:=Project.CurrentImage.Width;
    fRight:=-1;
    fTop:=Project.CurrentImage.Height;
    fBottom:=-1;
    sc:=Project.CurrentImage.GetPixel(x,y);
    for j:=0 to Project.CurrentImage.Height-1 do
      for i:=0 to Project.CurrentImage.Width-1 do
        if Project.CurrentImage.GetPixel(i,j)=sc then begin
          Project.CurrentImage.Putpixel(i,j,POSTPROCESSCOLOR);
          if i>fRight then fRight:=i;
          if i<fLeft then fLeft:=i;
          if j>fBottom then fBottom:=j;
          if j<fTop then fTop:=j;
        end;
    Project.CurrentImage.ImageUndo.AddImageUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1,fTempImage);
    FreeAndNil(fTempImage);
    ActiveInk.InitializeArea(fLeft,fTop,fRight,fBottom);
    ActiveInk.PostProcess;
    Project.CurrentImage.ImageUndo.AddImageRedoToLastUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1);
    Result:=true;
  end else Result:=false;
end;

// ------------------------------------------------------ [ TBDToolGetCEL ] ---

constructor TBDToolGetCel.Create;
begin
  inherited Create;
  fName:='GETCEL';
  fHint:=uppercase('Get a part of the image into a temporary image.');
end;

function TBDToolGetCel.Click(x,y,button:integer):boolean;
var i:integer;
begin
  if button=SDL_BUTTON_LEFT then begin
    case fState of
      0:begin
          fSX:=x;
          fSY:=y;
          Result:=true;
          fState:=1;
//          Log.Trace(Format('GETCEL1 fSX=%d, fSY=%d',[fSX,fSY]));
        end;
      1:begin
//          Log.Trace(Format('GETCEL2 fSX=%d, fSY=%d, x=%d, y=%d',[fSX,fSY,x,y]));
          if x<fSX then begin i:=x;x:=fSX;fSX:=i;end;
          if y<fSY then begin i:=y;y:=fSY;fSY:=i;end;
          if Assigned(Project.CELImage) then Project.CELImage.Free;
          Project.CELImage:=TBDImage.Create(x-fSX+1,y-fSY+1);
          Project.CELImage.Palette.ResizeAndCopyColorsFrom(Project.CurrentImage.Palette);
          Project.CELImage.PutImagePart(0,0,fSX,fSY,x-fSX+1,y-fSY+1,Project.CurrentImage);
          Project.CELImage.Left:=fSX;
          Project.CELImage.Top:=fSY;
          fState:=-1;
          MessageQueue.AddMessage(MSG_GETCELFINISHED);
          Result:=true;
        end;
    end;
  end
  else if Button=SDL_BUTTON_RIGHT then begin
    fState:=0;
    MessageQueue.AddMessage(MSG_RESTORECONTROLS);
    Result:=true;
  end
  else Result:=false;
end;

function TBDToolGetCel.MouseUp(x,y,button:integer):boolean;
begin
  Result:=true;
end;

procedure TBDToolGetCel.Draw;
begin
  case fState of
    -1:fState:=0;  // One frame where we don't draw anything before resetting fState.
                   // This is needed to remove flickering.
    0:begin
        Project.OverlayImage.VLine(fX,0,Project.OverlayImage.Height,VibroColors.GetColorIndex);
        Project.OverlayImage.HLine(0,fY,Project.OverlayImage.Width,VibroColors.GetColorIndex);
        InfoBar.ShowText(inttostr(fX)+','+inttostr(fY));
      end;
    1:begin
        Project.OverlayImage.Rectangle(fSX,fSY,fX,fY,VibroColors.GetColorIndex);

        InfoBar.ShowText('('+inttostr(fSX)+','+inttostr(fSY)+') '+
          'WI='+inttostr(abs(fSX-fX)+1)+' HE='+inttostr(abs(fSY-fY)+1)+' '+
          '('+inttostr(fX)+','+inttostr(fY)+') ');
      end;
  end;
end;

procedure TBDToolGetCel.Clear;
begin
  case fState of
    0:begin
        Project.OverlayImage.VLine(fX,0,Project.OverlayImage.Height,0);
        Project.OverlayImage.HLine(0,fY,Project.OverlayImage.Width,0);
      end;
    1:begin
        Project.OverlayImage.Rectangle(fSX,fSY,fX,fY,0);
      end;
  end;
end;

// ------------------------------------------------------ [ TBDToolPutCEL ] ---

constructor TBDToolPutCel.Create;
begin
  inherited Create;
  fName:='PUTCEL';
  fHint:=uppercase('Paste the temporary image to the image.');
end;

procedure TBDToolPutCel.Initialize;
begin
  CELHelperImage.Palette.ResizeAndCopyColorsFrom(Project.CELImage.Palette);
  CELHelperImage.Palette.Resize(CELHelperImage.Palette.Size+1);
  CELHelperImage.Palette[CELHelperImage.Palette.Size]:=0;
  CELHelperImage.Bar(0,0,CELHelperImage.Width,CELHelperImage.Height,CELHelperImage.Palette.Size);
end;

function TBDToolPutCel.Click(x,y,button:integer):boolean;
begin
  if button=SDL_BUTTON_LEFT then begin
    case fState of
      0:begin
          fSX:=x;
          fSY:=y;
          inc(fState);
        end;
      1:begin
          fState:=-1;
          Project.CelImage.Left:=Project.CelImage.Left+fX-fSX;
          Project.CelImage.Top:=Project.CelImage.Top+fY-fSY;
          Project.CurrentImage.ImageUndo.AddImageUndo(
            Project.CelImage.Left,Project.Celimage.Top,Project.CelImage.Width,Project.CELImage.Height);
          if Settings.ClearKeyColor then
            Project.CurrentImage.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage,Settings.SelectedColors[0])
          else
            Project.CurrentImage.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage);
          Project.CurrentImage.ImageUndo.AddImageRedoToLastUndo(Project.CelImage.Left,Project.Celimage.Top,Project.CelImage.Width,Project.CELImage.Height);
          MessageQueue.AddMessage(MSG_RESTORECONTROLS);
        end;
    end;
  end else
  if button=SDL_BUTTON_RIGHT then begin
    fState:=-1;
    MessageQueue.AddMessage(MSG_RESTORECONTROLS);
  end;
  Result:=true;
end;

function TBDToolPutCel.MouseUp(x,y,button:integer):boolean;
begin
  Result:=true;
end;

procedure TBDToolPutCel.Draw;
begin
  case fState of
    -1:fState:=0;  // One frame where we don't draw anything before resetting fState.
                   // This is needed to remove flickering.
    0:begin
        Project.OverlayImage.RectangleWH(Project.CelImage.Left,Project.CelImage.Top,Project.CELImage.Width,Project.CELImage.Height,VibroColors.GetColorIndex);
        if Settings.ClearKeyColor then
          CELHelperImage.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage,Settings.SelectedColors[0])
        else
          CELHelperImage.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage);
        InfoBar.ShowText(inttostr(fX)+','+inttostr(fY));
      end;
    1:begin
        if Settings.ClearKeyColor then
          CELHelperImage.PutImage(Project.CELImage.Left+fX-fSX,Project.CELImage.Top+fY-fSY,Project.CELImage,Settings.SelectedColors[0])
        else
          CELHelperImage.PutImage(Project.CELImage.Left+fX-fSX,Project.CELImage.Top+fY-fSY,Project.CELImage);
        InfoBar.ShowText(Format('%d,%d (%d,%d)',[Project.CELImage.Left+fX-fSX,Project.CELImage.Top+fY-fSY,fX-fSX,fY-fSY]));
      end;
  end;
end;

procedure TBDToolPutCel.Clear;
begin
  case fState of
    0:begin
        Project.OverlayImage.RectangleWH(Project.CelImage.Left,Project.CelImage.Top,Project.CELImage.Width,Project.CELImage.Height,0);
        CELHelperImage.BarWH(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage.Width,Project.CELImage.Height,CELHelperImage.Palette.Size);
      end;
    1:begin
        CELHelperImage.BarWH(Project.CELImage.Left+fX-fSX,Project.CELImage.Top+fY-fSY,Project.CELImage.Width,Project.CELImage.Height,CELHelperImage.Palette.Size);
      end;

  end;
end;

// --------------------------------------------------- [ TBDToolPickColor ] ---

constructor TBDToolPickColor.Create;
begin
  inherited Create;
  fName:='PICKCOL';
  fHint:=uppercase('Pick color for the selected color slot.');
end;

procedure TBDToolPickColor.Move(x,y:integer);
begin
  inherited Move(x,y);
  if (fX>=0) and (fX<Project.CurrentImage.Width) and (fY>=0) and (fY<Project.CurrentImage.Height) then
    SetColor(Project.CurrentImage.GetPixel(fX,fY))
  else
    SetColor(-1);
end;

function TBDToolPickColor.Click(x,y,button:integer):boolean;
begin
  if button=SDL_BUTTON_LEFT then begin
    MessageQueue.AddMessage(MSG_PICKEDCOLOR,fColorIndex);
  end else
  if button=SDL_BUTTON_RIGHT then begin
    MessageQueue.AddMessage(MSG_PICKEDCOLOR,-1);  // -1 means no change
  end;
  Result:=true;
end;

function TBDToolPickColor.MouseUp(x,y,button:integer):boolean;
begin
  Result:=true;
end;

procedure TBDToolPickColor.SetColor(colorindex:integer);
begin
  if (colorindex>=0) and (colorindex<Project.CurrentImage.Palette.Size) then begin
    fColorIndex:=colorindex;
    InfoBar.ShowText(Format('COLOR INDEX=%d (R=%d, G=%d, B=%d, A=%d) '#132'PICK '#133'CANCEL',
      [fColorIndex,
       Project.CurrentImage.Palette.ColorR[fColorIndex],
       Project.CurrentImage.Palette.ColorG[fColorIndex],
       Project.CurrentImage.Palette.ColorB[fColorIndex],
       Project.CurrentImage.Palette.ColorA[fColorIndex]]))
  end else
  if colorindex=-1 then begin
    fColorIndex:=-1;
    InfoBar.ShowText('');
  end;
end;

// ------------------------------------------------- [ TBDToolSelectColor ] ---

constructor TBDToolSelectColor.Create;
begin
  inherited Create;
  fName:='SELCOL';
  fHint:=uppercase('Select color to draw.');
  fColorIndex:=-1;
end;

procedure TBDToolSelectColor.Move(x,y:integer);
begin
  inherited Move(x,y);
  if (fX>=0) and (fX<Project.CurrentImage.Width) and (fY>=0) and (fY<Project.CurrentImage.Height) then
    SetColor(Project.CurrentImage.GetPixel(fX,fY))
  else
    SetColor(-1);
end;

function TBDToolSelectColor.Click(x,y,button:integer):boolean;
begin
  if button=SDL_BUTTON_LEFT then begin
    if (x>=0) and (x<Project.CurrentImage.Width) and (y>=0) and (y<Project.CurrentImage.Height) then
      Settings.ActiveColorIndex:=Project.CurrentImage.GetPixel(x,y);
  end else
  if button=SDL_BUTTON_RIGHT then begin
    MessageQueue.AddMessage(MSG_DEACTIVATEPALETTEEDITOR);
  end;
  Result:=true;
end;

procedure TBDToolSelectColor.SetColor(colorindex:integer);
begin
  if (colorindex>=-1) and (colorindex<Project.CurrentImage.Palette.Size) then begin
    fColorIndex:=colorindex;
    if colorindex>-1 then begin
      InfoBar.ShowText(Format('COLOR INDEX=%d (R=%d, G=%d, B=%d, A=%d) '#132'SELECT '#133'CLOSE PALETTE ED.',
        [fColorIndex,
         Project.CurrentImage.Palette.ColorR[fColorIndex],
         Project.CurrentImage.Palette.ColorG[fColorIndex],
         Project.CurrentImage.Palette.ColorB[fColorIndex],
         Project.CurrentImage.Palette.ColorA[fColorIndex]]))
    end else begin
      InfoBar.ShowText('');
    end;
  end;
end;

// ----------------------------------------------------- [ TBDToolShowCEL ] ---

constructor TBDToolShowCEL.Create;
begin
  inherited Create;
  fName:='SHOWCEL';
  fHint:=uppercase('Shows CEL for 2 secs.');
end;

procedure TBDToolShowCEL.Initialize;
begin
  CELHelperImage.Palette.ResizeAndCopyColorsFrom(Project.CELImage.Palette);
  CELHelperImage.Palette.Resize(CELHelperImage.Palette.Size+1);
  CELHelperImage.Palette[CELHelperImage.Palette.Size]:=0;
  CELHelperImage.Bar(0,0,CELHelperImage.Width,CELHelperImage.Height,CELHelperImage.Palette.Size);
  fStartTime:=GetTickCount64;
end;

procedure TBDToolShowCEL.Draw;
begin
  Project.OverlayImage.RectangleWH(Project.CelImage.Left,Project.CelImage.Top,Project.CELImage.Width,Project.CELImage.Height,VibroColors.GetColorIndex);
  if Settings.ClearKeyColor then
    CELHelperImage.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage,Settings.SelectedColors[0])
  else
    CELHelperImage.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage);
  if GetTickCount64-fStartTime>1000 then begin
    MessageQueue.AddMessage(MSG_RESTORECONTROLS);
  end;
end;

procedure TBDToolShowCEL.Clear;
begin
  Project.OverlayImage.RectangleWH(Project.CelImage.Left,Project.CelImage.Top,Project.CELImage.Width,Project.CELImage.Height,0);
  CELHelperImage.BarWH(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage.Width,Project.CELImage.Height,CELHelperImage.Palette.Size);
end;

end.


