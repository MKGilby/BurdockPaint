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

unit BDPInks;

{$mode delphi}

interface

uses
  Classes, SysUtils, Lists, BDPImage, BDPModalDialogs;

type

  { TBDInk }

  TBDInk=class
    constructor Create; virtual;
    procedure InitializeAreaWH(pLeft,pTop,pWidth,pHeight:integer); virtual;
    procedure InitializeArea(pX1,pY1,pX2,pY2:integer); virtual;
    function GetColorIndexAt(pX,pY:integer):integer; virtual;
    procedure PostProcess; virtual;
    procedure Configure; virtual;
    procedure ProcessWithCEL(pX,pY:integer); virtual;
  protected
    fLeft,fTop,fWidth,fHeight:integer;
    fSupportsOnTheFly:boolean;
    fName,fHint:string;
    fConfigureDialog:TBDModalDialog;
  public
    property Name:string read fName;
    property Hint:string read fHint;
    property SupportsOnTheFly:boolean read fSupportsOnTheFly;
    property ConfigureDialog:TBDModalDialog read fConfigureDialog write fConfigureDialog;
  end;

  { TBDInks }

  TBDInks=class(TNamedList<TBDInk>)
    constructor Create;
  end;

  { TBDInkHGrad }

  TBDInkHGrad=class(TBDInk)
    constructor Create; override;
    function GetColorIndexAt(pX,pY: integer):integer; override;
  end;

  { TBDInkLGrad }

  TBDInkLGrad=class(TBDInk)
    constructor Create; override;
    procedure PostProcess; override;
  private
    function ProcessSegment(i,j:integer):integer;
  end;

  { TBDInkOpaque }

  TBDInkOpaque=class(TBDInk)
    constructor Create; override;
    function GetColorIndexAt(pX, pY: integer):integer; override;
    procedure ProcessWithCEL(pX,pY:integer); override;
  end;

  { TBDInkVGrad }

  TBDInkVGrad=class(TBDInk)
    constructor Create; override;
    function GetColorIndexAt(pX,pY: integer):integer; override;
  end;

  { TBDInkRGrad }

  TBDInkRGrad=class(TBDInk)
    constructor Create; override;
    function GetColorIndexAt(pX,pY: integer):integer; override;
    procedure Configure; override;
  end;

  { TBDInkRandom }

  TBDInkRandom=class(TBDInk)
    constructor Create; override;
    function GetColorIndexAt(pX,pY: integer):integer; override;
  end;

  { TBDInkSoften }

  TBDInkSoften=class(TBDInk)
    constructor Create; override;
    procedure InitializeAreaWH(pLeft,pTop,pWidth,pHeight:integer); override;
    procedure InitializeArea(pX1,pY1,pX2,pY2:integer); override;
    function GetColorIndexAt(pX,pY: integer):integer; override;
    procedure PostProcess; override;
  private
    fTempImage:TBDImage;
  end;

  { TBDInkAdd }

  TBDInkAdd=class(TBDInk)
    constructor Create; override;
    function GetColorIndexAt(pX,pY: integer):integer; override;
    procedure ProcessWithCEL(pX,pY:integer); override;
  end;

implementation

uses MKToolBox, BDPShared, Logger;


// ------------------------------------------------------------- [ TBDInk ] ---

constructor TBDInk.Create;
begin
  fLeft:=0;
  fTop:=0;
  fWidth:=WindowWidth;
  fHeight:=WindowHeight;
  fConfigureDialog:=nil;
end;

procedure TBDInk.InitializeAreaWH(pLeft,pTop,pWidth,pHeight:integer);
begin
  if pWidth<0 then begin
    fLeft:=pLeft+pWidth;
    fWidth:=-pWidth;
  end else begin
    fLeft:=pLeft;
    fWidth:=pWidth;
  end;
  if pHeight<0 then begin
    fTop:=pTop+pHeight;
    fHeight:=-pHeight;
  end else begin
    fTop:=pTop;
    fHeight:=pHeight;
  end;
end;

procedure TBDInk.InitializeArea(pX1,pY1,pX2,pY2:integer);
begin
  if pX1>pX2 then begin
    fLeft:=pX2;
    fWidth:=pX1-pX2+1;
  end else begin
    fLeft:=pX1;
    fWidth:=pX2-pX1+1;
  end;
  if pY1>pY2 then begin
    fTop:=pY2;
    fHeight:=pY1-pY2+1;
  end else begin
    fTop:=pY1;
    fHeight:=pY2-pY1+1;
  end;
end;

function TBDInk.GetColorIndexAt(pX,pY:integer):integer;
begin
  Result:=-1;
  if fSupportsOnTheFly then
    raise Exception.Create('This ink ('+fName+') needs to override the GetColorIndexAt method!')
  else
    raise Exception.Create('This ink does not support OnTheFly, GetColorIndexAt method shouldn''t be called! ('+fName+')');
end;

procedure TBDInk.PostProcess;
var i,j:integer;
begin
  for j:=fTop to fTop+fHeight-1 do
    for i:=fLeft to fLeft+fWidth-1 do
      if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
        Project.CurrentImage.PutPixel(i,j,GetColorIndexAt(i,j));
end;

procedure TBDInk.Configure;
begin
  if Assigned(fConfigureDialog) then fConfigureDialog.Show;
end;

procedure TBDInk.ProcessWithCEL(pX,pY:integer);
var i,j:integer;
begin
  InitializeAreaWH(pX,pY,Project.CELImage.Width,Project.CELImage.Height);
  if Assigned(Project.CELImage) then begin
    for j:=0 to Project.CELImage.Height-1 do
      for i:=0 to Project.CELImage.Width-1 do
        if not Settings.ClearKeyColor or
           (Settings.ClearKeyColor and (Project.CELImage.GetPixel(i,j)<>Settings.SelectedColors[0])) then
        Project.CurrentImage.PutPixel(px+i,py+j,POSTPROCESSCOLOR);
    PostProcess;
  end;
end;

// ------------------------------------------------------------- [ TBDInk ] ---

constructor TBDInks.Create;
begin
  AddObject('OPAQUE',TBDInkOpaque.Create);
  AddObject('H GRAD',TBDInkHGrad.Create);
  AddObject('L GRAD',TBDInkLGrad.Create);
  AddObject('V GRAD',TBDInkVGrad.Create);
  AddObject('R GRAD',TBDInkRGrad.Create);
  AddObject('RANDOM',TBDInkRandom.Create);
  AddObject('SOFTEN',TBDInkSoften.Create);
  AddObject('ADD',TBDInkAdd.Create);
end;

// -------------------------------------------------------- [ TBDInkHGrad ] ---

constructor TBDInkHGrad.Create;
begin
  inherited ;
  fName:='H GRAD';
  fHint:='HORIZONTAL GRADIENT.';
  fSupportsOnTheFly:=true;
end;

function TBDInkHGrad.GetColorIndexAt(pX,pY: integer):integer;
begin
  if fWidth>1 then begin
    if Settings.DitherGradients then
      Result:=Project.CurrentImage.ColorClusters.ActiveColorCluster.GetIndexAtDithered(px-fLeft,fWidth-1,Settings.DitherStrength)
    else
      Result:=Project.CurrentImage.ColorClusters.ActiveColorCluster.GetIndexAt(px-fLeft,fWidth-1)
  end else
    Result:=Project.CurrentImage.ColorClusters.ActiveColorCluster.GetIndexAt(1,2);
end;

// -------------------------------------------------------- [ TBDInkLGrad ] ---

constructor TBDInkLGrad.Create;
begin
  inherited ;
  fName:='L GRAD';
  fHint:='LINEAR GRADIENT.';
  fSupportsOnTheFly:=false;
end;

function TBDInkLGrad.ProcessSegment(i,j:integer):integer;
var SegmentLeft,SegmentRight:integer;
begin
  SegmentLeft:=i;
  SegmentRight:=i;
  repeat
    inc(SegmentRight);
  until (SegmentRight>=Project.CurrentImage.Width) or (Project.CurrentImage.GetPixel(SegmentRight,j)<>POSTPROCESSCOLOR);
  Result:=SegmentRight-i-1;
  dec(SegmentRight);
  if Result>0 then begin
    if Settings.DitherGradients then begin
      while i<SegmentRight+1 do begin
        Project.CurrentImage.PutPixel(i,j,Project.CurrentImage.ColorClusters.ActiveColorCluster.GetIndexAtDithered(i-SegmentLeft,Result+1,Settings.DitherStrength));
        inc(i);
      end;
    end else begin
      while i<SegmentRight+1 do begin
        Project.CurrentImage.PutPixel(i,j,Project.CurrentImage.ColorClusters.ActiveColorCluster.GetIndexAt(i-SegmentLeft,Result+1));
        inc(i);
      end;
    end;
  end else begin
    Project.CurrentImage.PutPixel(i,j,Project.CurrentImage.ColorClusters.ActiveColorCluster.GetIndexAt(1,2));
  end;
end;

procedure TBDInkLGrad.PostProcess;
var i,j:integer;
begin
  for j:=fTop to fTop+fHeight-1 do begin
    i:=fLeft;
    while i<=fLeft+fWidth-1 do begin
      if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
        i+=ProcessSegment(i,j);
      inc(i);
    end;
  end;
end;

// ------------------------------------------------------- [ TBDInkOpaque ] ---

constructor TBDInkOpaque.Create;
begin
  inherited ;
  fName:='OPAQUE';
  fHint:='DRAWS WITH THE SELECTED COLOR.';
  fSupportsOnTheFly:=true;
end;

function TBDInkOpaque.GetColorIndexAt(pX,pY: integer):integer;
begin
  Result:=Settings.ActiveColorIndex;
end;

procedure TBDInkOpaque.ProcessWithCEL(pX,pY:integer);
begin
  if Settings.ClearKeyColor then
    Project.CurrentImage.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage,Settings.SelectedColors[0])
  else
    Project.CurrentImage.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage);
end;

// -------------------------------------------------------- [ TBDInkVGrad ] ---

constructor TBDInkVGrad.Create;
begin
  inherited ;
  fName:='V GRAD';
  fHint:='VERTICAL GRADIENT.';
  fSupportsOnTheFly:=true;
end;

function TBDInkVGrad.GetColorIndexAt(pX,pY: integer):integer;
begin
  if fHeight>1 then begin
    if Settings.DitherGradients then
      Result:=Project.CurrentImage.ColorClusters.ActiveColorCluster.GetIndexAtDithered(pY-fTop,fHeight-1,Settings.DitherStrength)
    else
      Result:=Project.CurrentImage.ColorClusters.ActiveColorCluster.GetIndexAt(pY-fTop,fHeight-1)
  end else
    Result:=Project.CurrentImage.ColorClusters.ActiveColorCluster.GetIndexAt(1,2);
end;

// -------------------------------------------------------- [ TBDInkRGrad ] ---

constructor TBDInkRGrad.Create;
begin
  inherited ;
  fName:='R GRAD';
  fHint:='ROUND GRADIENT. '#132'SELECT '#133'CONFIGURE';
  fSupportsOnTheFly:=true;
end;

function TBDInkRGrad.GetColorIndexAt(pX, pY: integer): integer;
var r:integer;
begin
  if Settings.RGradRadius>1 then begin
    r:=trunc(Sqrt(sqr(px-Settings.RGradCenterX)+sqr(py-Settings.RGradCenterY))) mod Settings.RGradRadius;
    if Settings.DitherGradients then
      Result:=Project.CurrentImage.ColorClusters.ActiveColorCluster.
        GetIndexAtDithered(r,Settings.RGradRadius,Settings.DitherStrength)
    else
      Result:=Project.CurrentImage.ColorClusters.ActiveColorCluster.
        GetIndexAt(r,Settings.RGradRadius)
  end else
    Result:=Project.CurrentImage.ColorClusters.ActiveColorCluster.GetIndexAt(1,2);
end;

procedure TBDInkRGrad.Configure;
begin
  MessageQueue.AddMessage(MSG_TOGGLECONTROLS);
  ActiveTool:=Tools.ItemByName['CONFRG'];
end;

// ------------------------------------------------------- [ TBDInkRandom ] ---

constructor TBDInkRandom.Create;
begin
  inherited ;
  fName:='RANDOM';
  fHint:='RANDOM COLORS FROM THE SELECTED COLOR CLUSTER.';
  fSupportsOnTheFly:=true;
end;

function TBDInkRandom.GetColorIndexAt(pX, pY: integer): integer;
begin
  Result:=Project.CurrentImage.ColorClusters.ActiveColorCluster.
    GetIndexAt(random(2048),2048);
end;

// ------------------------------------------------------- [ TBDInkSoften ] ---

constructor TBDInkSoften.Create;
begin
  inherited Create;
  fName:='SOFTEN';
  fHint:='AVERAGES PIXEL COLORS WITH NEIGHBOURS.';
  fSupportsOnTheFly:=false;
end;

procedure TBDInkSoften.InitializeAreaWH(pLeft,pTop,pWidth,pHeight:integer);
begin
  inherited ;
  fTempImage:=TBDImage.Create(fWidth,fHeight);
  fTempImage.PutImagePart(0,0,fLeft,fTop,fWidth,fHeight,Project.CurrentImage);
end;

procedure TBDInkSoften.InitializeArea(pX1,pY1,pX2,pY2:integer);
begin
  inherited ;
  fTempImage:=TBDImage.Create(fWidth,fHeight);
  fTempImage.PutImagePart(0,0,fLeft,fTop,fWidth,fHeight,Project.CurrentImage);
end;

function TBDInkSoften.GetColorIndexAt(pX, pY: integer): integer;
var r,g,b,c:integer;
begin
  pX-=fLeft;pY-=fTop;
  c:=1;
  r:=Project.CurrentImage.Palette.ColorR[fTempImage.GetPixel(px,py)];
  g:=Project.CurrentImage.Palette.ColorG[fTempImage.GetPixel(px,py)];
  b:=Project.CurrentImage.Palette.ColorB[fTempImage.GetPixel(px,py)];
  if px>0 then begin
    r+=Project.CurrentImage.Palette.ColorR[fTempImage.GetPixel(px-1,py)];
    g+=Project.CurrentImage.Palette.ColorG[fTempImage.GetPixel(px-1,py)];
    b+=Project.CurrentImage.Palette.ColorB[fTempImage.GetPixel(px-1,py)];
    inc(c);
  end;
  if px<fTempImage.Width-1 then begin
    r+=Project.CurrentImage.Palette.ColorR[fTempImage.GetPixel(px+1,py)];
    g+=Project.CurrentImage.Palette.ColorG[fTempImage.GetPixel(px+1,py)];
    b+=Project.CurrentImage.Palette.ColorB[fTempImage.GetPixel(px+1,py)];
    inc(c);
  end;
  if py>0 then begin
    r+=Project.CurrentImage.Palette.ColorR[fTempImage.GetPixel(px,py-1)];
    g+=Project.CurrentImage.Palette.ColorG[fTempImage.GetPixel(px,py-1)];
    b+=Project.CurrentImage.Palette.ColorB[fTempImage.GetPixel(px,py-1)];
    inc(c);
  end;
  if py<fTempImage.Height-1 then begin
    r+=Project.CurrentImage.Palette.ColorR[fTempImage.GetPixel(px,py+1)];
    g+=Project.CurrentImage.Palette.ColorG[fTempImage.GetPixel(px,py+1)];
    b+=Project.CurrentImage.Palette.ColorB[fTempImage.GetPixel(px,py+1)];
    inc(c);
  end;
  Result:=Project.CurrentImage.Palette.GetClosestColor(r div c,g div c,b div c);
end;

procedure TBDInkSoften.PostProcess;
var i,j:integer;
begin
  for j:=fTop to fTop+fHeight-1 do
    for i:=fLeft to fLeft+fWidth-1 do
      if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
        Project.CurrentImage.PutPixel(i,j,GetColorIndexAt(i,j));
  if Assigned(fTempImage) then FreeAndNil(fTempImage);
end;

// ---------------------------------------------------------- [ TBDInkAdd ] ---

constructor TBDInkAdd.Create;
begin
  inherited Create;
  fName:='ADD';
  fHint:='ADDS SELECTED COLOR INDEX TO THE IMAGE''S COLOR INDEX.';
  fSupportsOnTheFly:=true;
end;

function TBDInkAdd.GetColorIndexAt(pX,pY:integer):integer;
var c:integer;
begin
  c:=Project.CurrentImage.GetPixel(pX,pY);
  Result:=(c and $FF00)+((c+Settings.ActiveColorIndex) and $FF);
end;

procedure TBDInkAdd.ProcessWithCEL(pX,pY:integer);
var i,j,c:integer;
begin
  if Assigned(Project.CELImage) then begin
    for j:=0 to Project.CELImage.Height-1 do
      for i:=0 to Project.CELImage.Width-1 do
        if not Settings.ClearKeyColor or
           (Settings.ClearKeyColor and (Project.CELImage.GetPixel(i,j)<>Settings.SelectedColors[0])) then begin
          c:=Project.CurrentImage.GetPixel(pX+i,pY+j);
          Project.CurrentImage.PutPixel(pX+i,pY+j,
            (c and $FF00)+((c+Project.CELImage.GetPixel(i,j)) and $FF));
        end;
  end;
end;

end.


