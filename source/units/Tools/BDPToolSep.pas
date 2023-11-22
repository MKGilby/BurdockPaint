{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPToolSep;

{$mode Delphi}

interface

uses
  SysUtils, BDPToolBase;

type

  { TBDToolSep }

  TBDToolSep=class(TBDTool)
    constructor Create; override;
    function Click(x,y,button:integer):boolean; override;
  end;

implementation

uses sdl2, BDPShared, BDPRegion;

{ TBDToolSep }

constructor TBDToolSep.Create;
begin
  inherited ;
  fName:='SEP.';
  fHint:=uppercase('Change the clicked color to ink color.');
  fPinnable:=true;
end;

function TBDToolSep.Click(x,y,button:integer):boolean;
var i,j:integer;sc:uint32;
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
          Project.CurrentRegion.Putpixel(i,j,POSTPROCESSCOLOR);
          if i>fRight then fRight:=i;
          if i<fLeft then fLeft:=i;
          if j>fBottom then fBottom:=j;
          if j<fTop then fTop:=j;
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

