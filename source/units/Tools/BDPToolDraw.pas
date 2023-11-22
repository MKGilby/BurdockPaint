{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPToolDraw;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase, BDPRegion;

type

  { TBDToolDraw }

  TBDToolDraw=class(TBDTool)
    constructor Create; override;
    function MouseDown(x,y,button:integer):boolean; override;
    function MouseUp(x,y,button:integer):boolean; override;
    function MouseMove(x,y,button:integer):boolean; override;
  private
    fTempImage:TBDRegion;
    fLeft,fTop,fRight,fBottom:integer;
    fDown:boolean;
  end;

implementation

uses SDL2, BDPShared;

{ TBDToolDraw }

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
    fTempImage:=TBDRegion.Create(Project.CurrentRegion.Width,Project.CurrentRegion.Height);
    fTempImage.PutImage(0,0,Project.CurrentRegion);
    Project.CurrentRegion.PutPixel(x,y,ActiveInk.GetColorAt(x,y));
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
    Project.CurrentImage.RegionUndo.AddImageUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1,fTempImage);
    FreeAndNil(fTempImage);
    Project.CurrentImage.RegionUndo.AddImageRedoToLastUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1);
    Result:=true;
  end else Result:=false;
end;

function TBDToolDraw.MouseMove(x,y,button:integer):boolean;
begin
  if fDown then begin
    Project.CurrentRegion.PutPixel(x,y,ActiveInk.GetColorAt(x,y));
    if fLeft>x then fLeft:=x;
    if fTop>y then fTop:=y;
    if fRight<x then fRight:=x;
    if fBottom<y then fBottom:=y;
    Result:=true;
  end else Result:=false;
end;

end.

