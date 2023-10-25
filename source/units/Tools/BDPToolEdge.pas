{
  BurdockPaint - Copyright 2023 MKSZTSZ
  Written by Szabó "Gilby" Zsolt / MKSZTSZ

  This file is part of the source code of BurdockPaint.

  BurdockPaint is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation, either version 3 of the License,
  or (at your option) any later version.

  BurdockPaint is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  BurdockPaint. If not, see <https://www.gnu.org/licenses/>.
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
    fTempImage:=TBDRegion.Create(Project.CurrentImage.Width,Project.CurrentImage.Height);
    fTempImage.PutImage(0,0,Project.CurrentImage);
    fLeft:=Project.CurrentImage.Width;
    fRight:=-1;
    fTop:=Project.CurrentImage.Height;
    fBottom:=-1;
    sc:=Project.CurrentImage.GetPixel(x,y);
    for j:=0 to Project.CurrentImage.Height-1 do
      for i:=0 to Project.CurrentImage.Width-1 do
        if Project.CurrentImage.GetPixel(i,j)=sc then begin
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
            Project.CurrentImage.Putpixel(i,j,POSTPROCESSCOLOR);
            if i>fRight then fRight:=i;
            if i<fLeft then fLeft:=i;
            if j>fBottom then fBottom:=j;
            if j<fTop then fTop:=j;
          end;
        end;
    Project.CurrentExtImage.ImageUndo.AddImageUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1,fTempImage);
    FreeAndNil(fTempImage);
    ActiveInk.InitializeArea(fLeft,fTop,fRight,fBottom);
    ActiveInk.PostProcess;
    Project.CurrentExtImage.ImageUndo.AddImageRedoToLastUndo(fLeft,fTop,fRight-fLeft+1,fBottom-fTop+1);
    Result:=true;
  end else Result:=false;
end;

end.

