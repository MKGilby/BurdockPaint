{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPToolFillTo;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase;

type

  { TBDToolFillTo }

  TBDToolFillTo=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
    function MouseUp(x,y,button:integer):boolean; override;
    procedure Draw; override;
  private
    fLeft,fTop,fRight,fBottom:integer;
    fSourceColor:uint32;
    procedure FillToWithPostProcessColor(x,y:integer);
  end;

implementation

uses sdl2, BDPShared, BDPRegion;

{ TBDToolFillTo }

constructor TBDToolFillTo.Create;
begin
  inherited ;
  fName:='FILLTO';
  fHint:=uppercase('Click to select boundary color. Click again inside to fill.');
  fPinnable:=true;
end;

function TBDToolFillTo.Click(x,y,button:integer):boolean;
var fTempImage:TBDRegion;
begin
  if button=SDL_BUTTON_LEFT then
    case fstate of
      0:begin
          fSourceColor:=Project.CurrentRegion.GetPixel(x,y);
          fState:=1;
          Result:=true;
        end;
      1:begin
          fLeft:=x;
          fTop:=y;
          fRight:=x;
          fBottom:=y;

          fTempImage:=TBDRegion.Create(Project.CurrentRegion.Width,Project.CurrentRegion.Height);
          fTempImage.PutImage(0,0,Project.CurrentRegion);

          FillToWithPostProcessColor(x,y);
          Project.CurrentImage.RegionUndo.AddImageUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1,fTempImage);
          FreeAndNil(fTempImage);

          ActiveInk.InitializeArea(fLeft,fTop,fRight,fBottom);
          ActiveInk.PostProcess;
          Project.CurrentImage.RegionUndo.AddImageRedoToLastUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1);
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
    if (y>0) and (Project.CurrentRegion.GetPixel(x,y-1)<>fSourceColor) and
      (Project.CurrentRegion.GetPixel(x,y-1)<>POSTPROCESSCOLOR) then begin
      Project.CurrentRegion.PutPixel(x,y-1,POSTPROCESSCOLOR);
      if y-1<fTop then fTop:=y-1;
      Result:=true;
    end;
    if (x<Project.CurrentRegion.Width-1) and (Project.CurrentRegion.GetPixel(x+1,y)<>fSourceColor) and
      (Project.CurrentRegion.GetPixel(x+1,y)<>POSTPROCESSCOLOR) then begin
      Project.CurrentRegion.PutPixel(x+1,y,POSTPROCESSCOLOR);
      if x+1>fRight then fRight:=x+1;
      Result:=true;
    end;
    if (y<Project.CurrentRegion.Height-1) and (Project.CurrentRegion.GetPixel(x,y+1)<>fSourceColor) and
      (Project.CurrentRegion.GetPixel(x,y+1)<>POSTPROCESSCOLOR) then begin
      Project.CurrentRegion.PutPixel(x,y+1,POSTPROCESSCOLOR);
      if y+1>fBottom then fBottom:=y+1;
      Result:=true;
    end;
    if (x>0) and (Project.CurrentRegion.GetPixel(x-1,y)<>fSourceColor) and
      (Project.CurrentRegion.GetPixel(x-1,y)<>POSTPROCESSCOLOR) then begin
      Project.CurrentRegion.PutPixel(x-1,y,POSTPROCESSCOLOR);
      if x-1<fLeft then fLeft:=x-1;
      Result:=true;
    end;
  end;

begin
  if fSourceColor=POSTPROCESSCOLOR then exit;
  Project.CurrentRegion.PutPixel(x,y,POSTPROCESSCOLOR);
  ic:=0;
  repeat
    w:=false;

    case (ic mod 4) of
      0:begin
          j:=fTop;
          while j<=fBottom do begin
            i:=fLeft;
            while i<=fRight do begin
              if Project.CurrentRegion.GetPixel(i,j)=POSTPROCESSCOLOR then
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
              if Project.CurrentRegion.GetPixel(i,j)=POSTPROCESSCOLOR then
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
              if Project.CurrentRegion.GetPixel(i,j)=POSTPROCESSCOLOR then
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
              if Project.CurrentRegion.GetPixel(i,j)=POSTPROCESSCOLOR then
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

end.

