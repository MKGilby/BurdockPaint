{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPToolBox;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase;

type

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

implementation

uses sdl2, BDPShared;

{ TBDToolBox }

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
          Project.CurrentImage.RegionUndo.AddImageUndo(fSX,fSY,x-fSX+1,y-fSY+1);
          ActiveInk.InitializeArea(fSX,fSY,x,y);
          if Settings.FillShapes then begin
            if ActiveInk.SupportsOnTheFly then
              DrawBarWithInk(fSX,fSY,x,y)
            else begin
              Project.CurrentRegion.Bar(fSX,fSY,x-fSX+1,y-fSY+1,POSTPROCESSCOLOR);
              ActiveInk.PostProcess;
            end;
          end else begin
            if ActiveInk.SupportsOnTheFly then
              DrawRectangleWithInk(fSX,fSY,x,y)
            else begin
              Project.CurrentRegion.Rectangle(fSX,fSY,x-fSX+1,y-fSY+1,POSTPROCESSCOLOR);
              ActiveInk.PostProcess;
            end;
          end;
          Project.CurrentImage.RegionUndo.AddImageRedoToLastUndo(fSX,fSY,x-fSX+1,y-fSY+1);
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
        OverlayImage.RectangleXY(fSX,fSY,fX,fY,VibroColors.GetColor);

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
        OverlayImage.RectangleXY(fSX,fSY,fX,fY,0);
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
      Project.CurrentRegion.PutPixel(i,j,ActiveInk.GetColorAt(i,j));
end;

procedure TBDToolBox.DrawRectangleWithInk(x1,y1,x2,y2:integer);
var
  i:integer;
begin
  if x1>x2 then begin i:=x1;x1:=x2;x2:=i;end;
  if y1>y2 then begin i:=y1;y1:=y2;y2:=i;end;
  for i:=y1 to y2 do begin
    Project.CurrentRegion.PutPixel(x1,i,ActiveInk.GetColorAt(x1,i));
    Project.CurrentRegion.PutPixel(x2,i,ActiveInk.GetColorAt(x2,i));
  end;
  for i:=x1+1 to x2-1 do begin
    Project.CurrentRegion.PutPixel(i,y1,ActiveInk.GetColorAt(i,y1));
    Project.CurrentRegion.PutPixel(i,y2,ActiveInk.GetColorAt(i,y2));
  end;
end;

end.

