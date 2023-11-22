{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPToolEdge;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase;

type

  { TBDToolEdge }

  TBDToolEdge=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
  end;

implementation

uses sdl2, BDPShared, BDPRegion;

{ TBDToolEdge }

constructor TBDToolEdge.Create;
begin
  inherited Create;
  fName:='EDGE';
  fHint:=uppercase('Draws the selected color''s edge with the current ink.');
  fPinnable:=true;
end;

function TBDToolEdge.Click(x,y,button:integer):boolean;
var i,j,cnt:integer;sc:uint32;
  fLeft,fRight,fTop,fBottom:integer;
  fTempImage:TBDRegion;
begin
  if button=SDL_BUTTON_LEFT then begin
    fTempImage:=TBDRegion.Create(Project.CurrentRegion.Width,Project.CurrentRegion.Height);
    fTempImage.PutImage(0,0,Project.CurrentRegion);
    fLeft:=Project.CurrentRegion.Width;
    fRight:=-1;
    fTop:=Project.CurrentRegion.Height;
    fBottom:=-1;
    sc:=Project.CurrentRegion.GetPixel(x,y);
    for j:=0 to Project.CurrentRegion.Height-1 do
      for i:=0 to Project.CurrentRegion.Width-1 do
        if Project.CurrentRegion.GetPixel(i,j)=sc then begin
          cnt:=0;
          if (i>0) then begin
            if (j>0) and (fTempImage.GetPixel(i-1,j-1)=sc) then inc(cnt);
            if (fTempImage.GetPixel(i-1,j)=sc) then inc(cnt);
            if (j<fTempImage.Height-1) and (fTempImage.GetPixel(i-1,j+1)=sc) then inc(cnt);
          end;
          if (j>0) and (fTempImage.GetPixel(i,j-1)=sc) then inc(cnt);
          if (j<fTempImage.Height-1) and (fTempImage.GetPixel(i,j+1)=sc) then inc(cnt);
          if (i<fTempImage.Width-1) then begin
            if (j>0) and (fTempImage.GetPixel(i+1,j-1)=sc) then inc(cnt);
            if (fTempImage.GetPixel(i+1,j)=sc) then inc(cnt);
            if (j<fTempImage.Height-1) and (fTempImage.GetPixel(i+1,j+1)=sc) then inc(cnt);
          end;
          if cnt<8 then begin
            Project.CurrentRegion.Putpixel(i,j,POSTPROCESSCOLOR);
            if i>fRight then fRight:=i;
            if i<fLeft then fLeft:=i;
            if j>fBottom then fBottom:=j;
            if j<fTop then fTop:=j;
          end;
        end;
    Project.CurrentImage.RegionUndo.AddImageUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1,fTempImage);
    FreeAndNil(fTempImage);
    ActiveInk.InitializeArea(fLeft,fTop,fRight,fBottom);
    ActiveInk.PostProcess;
    Project.CurrentImage.RegionUndo.AddImageRedoToLastUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1);
    Result:=true;
  end else Result:=false;
end;

end.

