{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPToolCircle;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase, BDPConfigureCircleDialog;

type

  { TBDToolCircle }

  TBDToolCircle=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
    function MouseUp(x,y,button:integer):boolean; override;
    procedure Draw; override;
    procedure Clear; override;
    procedure Configure; override;
  private
    fSX,fSY:integer;
    procedure DrawCircleWithInk(cx,cy,r:integer);
    procedure DrawFilledCircleWithInk(cx,cy,r:integer);
  end;

implementation

uses sdl2, BDPShared;

{ TBDToolCircle }

constructor TBDToolCircle.Create;
begin
  inherited ;
  fName:='CIRCLE';
  fHint:=uppercase('Draw a circle. Right click to select method.');
  fPinnable:=true;
end;

function TBDToolCircle.Click(x,y,button:integer):boolean;
var r,cx,cy,wi,he:integer;
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
          case Settings.CircleMode of
            0:begin
                r:=round(sqrt(sqr(fSX-x)+sqr(fSY-y)));
                Project.CurrentImage.RegionUndo.AddImageUndo(fSX-r,fSY-r,r*2+1,r*2+1);
                ActiveInk.InitializeArea(fSX-r,fSY-r,fSX+r,fSY+r);
                if ActiveInk.SupportsOnTheFly then begin
                  if Settings.FillShapes then
                    DrawFilledCircleWithInk(fSX,fSY,r)
                  else
                    DrawCircleWithInk(fSX,fSY,r)
                end else begin
                  if Settings.FillShapes then
                    Project.CurrentRegion.FilledCircle(fSX,fSY,r,POSTPROCESSCOLOR)
                  else
                    Project.CurrentRegion.Circle(fSX,fSY,r,POSTPROCESSCOLOR);
                  ActiveInk.PostProcess;
                end;
                Project.CurrentImage.RegionUndo.AddImageRedoToLastUndo(fSX-r,fSY-r,r*2+1,r*2+1);
                InfoBar.ShowText('');
                Result:=true;
                fState:=0;
              end;
            1:begin
                if fSX>x then begin r:=x;x:=fSX;fSX:=r;end;
                if fSY>y then begin r:=y;y:=fSY;fSY:=r;end;
                wi:=fX-fSX+1;
                he:=fY-fSY+1;
                Project.CurrentImage.RegionUndo.AddImageUndo(fSX,fSY,wi,he);
                cx:=(fSX+fX+1) div 2;
                cy:=(fSY+fY+1) div 2;
                if (wi<he) then
                  r:=wi div 2
                else
                  r:=he div 2;
                if r>0 then dec(r);
                ActiveInk.InitializeArea(fSX,fSY,x,y);
                if ActiveInk.SupportsOnTheFly then begin
                  if Settings.FillShapes then
                    DrawFilledCircleWithInk(cx,cy,r)
                  else
                    DrawCircleWithInk(cx,cy,r)
                end else begin
                  if Settings.FillShapes then
                    Project.CurrentRegion.FilledCircle(cx,cy,r,POSTPROCESSCOLOR)
                  else
                    Project.CurrentRegion.Circle(cx,cy,r,POSTPROCESSCOLOR);
                  ActiveInk.PostProcess;
                end;
                Project.CurrentImage.RegionUndo.AddImageRedoToLastUndo(fSX,fSY,x-fSX+1,y-fSY+1);
                InfoBar.ShowText('');
                Result:=true;
                fState:=0;
              end;
          end;
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
var r,cx,cy,wi,he:integer;
begin
  case fState of
    0:;
    1:begin
        case Settings.CircleMode of
          0:begin
              r:=round(sqrt(sqr(fSX-fX)+sqr(fSY-fY)));
              OverlayImage.Circle(fSX,fSY,r,VibroColors.GetColor);

              InfoBar.ShowText('('+inttostr(fSX)+','+inttostr(fSY)+') '+
                'RADIUS='+inttostr(r)+' '+
                '('+inttostr(fX)+','+inttostr(fY)+') ');

            end;
          1:begin
              wi:=abs(fSX-fX)+1;
              he:=abs(fSY-fY)+1;
              cx:=(fSX+fX+1) div 2;
              cy:=(fSY+fY+1) div 2;
              if (wi<he) then
                r:=wi div 2
              else
                r:=he div 2;
              if r>0 then dec(r);
              OverlayImage.RectangleXY(fSX,fSY,fX,fY,VibroColors.GetHelperColor);
              if r>0 then
                OverlayImage.Circle(cx,cy,r,VibroColors.GetColor);

              InfoBar.ShowText('('+inttostr(fSX)+','+inttostr(fSY)+') '+
                'RADIUS='+inttostr(r)+' '+
                'WI='+inttostr(abs(fSX-fX)+1)+' HE='+inttostr(abs(fSY-fY)+1)+' '+
                '('+inttostr(fX)+','+inttostr(fY)+') ');
            end;
        end;
      end;
  end;
end;

procedure TBDToolCircle.Clear;
var r,sx,sy,x,y:integer;
begin
  case fState of
    0:;
    1:begin
        case Settings.CircleMode of
          0:begin
              r:=round(sqrt(sqr(fSX-fX)+sqr(fSY-fY)));
              OverlayImage.Circle(fSX,fSY,r,0);
            end;
          1:begin
              sx:=fSX; sy:=fSY; x:=fX; y:=fY;
              if sx>x then begin r:=x;x:=sx;sx:=r;end;
              if sy>y then begin r:=y;y:=sy;sy:=r;end;
              OverlayImage.Bar(sx,sy,x-sx+1,y-sy+1,0);
            end;
        end;
      end;
  end;
end;

procedure TBDToolCircle.Configure;
begin
  MessageQueue.AddMessage(MSG_OPENCONFIGURECIRCLEDIALOG);
end;

// Taken from https://www.geeksforgeeks.org/bresenhams-circle-drawing-algorithm/
procedure TBDToolCircle.DrawCircleWithInk(cx,cy,r:integer);

  procedure PutPixel8(x,y:integer);
  begin
    Project.CurrentRegion.PutPixel(cx+x, cy+y, ActiveInk.GetColorAt(cx+x, cy+y));
    Project.CurrentRegion.PutPixel(cx-x, cy+y, ActiveInk.GetColorAt(cx-x, cy+y));
    Project.CurrentRegion.PutPixel(cx+x, cy-y, ActiveInk.GetColorAt(cx+x, cy-y));
    Project.CurrentRegion.PutPixel(cx-x, cy-y, ActiveInk.GetColorAt(cx-x, cy-y));
    Project.CurrentRegion.PutPixel(cx+y, cy+x, ActiveInk.GetColorAt(cx+y, cy+x));
    Project.CurrentRegion.PutPixel(cx-y, cy+x, ActiveInk.GetColorAt(cx-y, cy+x));
    Project.CurrentRegion.PutPixel(cx+y, cy-x, ActiveInk.GetColorAt(cx+y, cy-x));
    Project.CurrentRegion.PutPixel(cx-y, cy-x, ActiveInk.GetColorAt(cx-y, cy-x));
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
      Project.CurrentRegion.PutPixel(cx+i,cy+y,ActiveInk.GetColorAt(cx+i,cy+y));
      Project.CurrentRegion.PutPixel(cx+i,cy-y,ActiveInk.GetColorAt(cx+i,cy-y));
    end;
    for i:=-y to +y do begin
      Project.CurrentRegion.PutPixel(cx+i,cy+x,ActiveInk.GetColorAt(cx+i,cy+x));
      Project.CurrentRegion.PutPixel(cx+i,cy-x,ActiveInk.GetColorAt(cx+i,cy-x));
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

end.

