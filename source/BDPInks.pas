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
  Classes, SysUtils, Lists, BDPModalDialog, BDPImage;

type

  { TBDInk }

  TBDInk=class
    constructor Create; virtual;
    procedure InitializeAreaWH(pLeft,pTop,pWidth,pHeight:integer); virtual;
    procedure InitializeArea(pX1,pY1,pX2,pY2:integer); virtual;
    function GetColorAt(pX,pY:integer):uint32; virtual;
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
    function GetColorAt(pX,pY:integer):uint32; override;
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
    function GetColorAt(pX,pY:integer):uint32; override;
    procedure ProcessWithCEL(pX,pY:integer); override;
  end;

  { TBDInkVGrad }

  TBDInkVGrad=class(TBDInk)
    constructor Create; override;
    function GetColorAt(pX,pY:integer):uint32; override;
  end;

  { TBDInkCGrad }

  TBDInkCGrad=class(TBDInk)
    constructor Create; override;
    function GetColorAt(pX,pY:integer):uint32; override;
    procedure Configure; override;
  end;

  { TBDInkRandom }

  TBDInkRandom=class(TBDInk)
    constructor Create; override;
    function GetColorAt(pX,pY:integer):uint32; override;
  end;

  { TBDInkSoften }

  TBDInkSoften=class(TBDInk)
    constructor Create; override;
    procedure InitializeAreaWH(pLeft,pTop,pWidth,pHeight:integer); override;
    procedure InitializeArea(pX1,pY1,pX2,pY2:integer); override;
    function GetColorAt(pX,pY:integer):uint32; override;
    procedure PostProcess; override;
  private
    fTempImage:TBDRegion;
  end;

  { TBDInkRGrad }

  TBDInkRGrad=class(TBDInk)
    constructor Create; override;
    function GetColorAt(pX,pY:integer):uint32; override;
    procedure Configure; override;
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

function TBDInk.GetColorAt(pX,pY:integer):uint32;
begin
  Result:=0;
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
        Project.CurrentImage.PutPixel(i,j,GetColorAt(i,j));
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
           (Settings.ClearKeyColor and (Project.CELImage.GetPixel(i,j)<>$FF000000)) then
        Project.CurrentImage.PutPixel(px+i,py+j,POSTPROCESSCOLOR);
    PostProcess;
  end;
end;

// ------------------------------------------------------------ [ TBDInks ] ---

constructor TBDInks.Create;
begin
  AddObject('OPAQUE',TBDInkOpaque.Create);
  AddObject('H GRAD',TBDInkHGrad.Create);
  AddObject('L GRAD',TBDInkLGrad.Create);
  AddObject('V GRAD',TBDInkVGrad.Create);
  AddObject('C GRAD',TBDInkCGrad.Create);
  AddObject('RANDOM',TBDInkRandom.Create);
  AddObject('SOFTEN',TBDInkSoften.Create);
  AddObject('R GRAD',TBDInkRGrad.Create);
end;

// -------------------------------------------------------- [ TBDInkHGrad ] ---

constructor TBDInkHGrad.Create;
begin
  inherited ;
  fName:='H GRAD';
  fHint:='HORIZONTAL GRADIENT.';
  fSupportsOnTheFly:=true;
end;

function TBDInkHGrad.GetColorAt(pX,pY:integer):uint32;
begin
  if fWidth>1 then begin
    if Settings.DitherGradients then
      Result:=Project.CurrentColorClusters.ActiveColorCluster.GetColorAtDithered(px-fLeft,fWidth-1,Settings.DitherStrength)
    else
      Result:=Project.CurrentColorClusters.ActiveColorCluster.GetColorAt(px-fLeft,fWidth-1)
  end else
    Result:=Project.CurrentColorClusters.ActiveColorCluster.GetColorAt(1,2);
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
        Project.CurrentImage.PutPixel(i,j,Project.CurrentColorClusters.ActiveColorCluster.GetColorAtDithered(i-SegmentLeft,Result+1,Settings.DitherStrength));
        inc(i);
      end;
    end else begin
      while i<SegmentRight+1 do begin
        Project.CurrentImage.PutPixel(i,j,Project.CurrentColorClusters.ActiveColorCluster.GetColorAt(i-SegmentLeft,Result+1));
        inc(i);
      end;
    end;
  end else begin
    Project.CurrentImage.PutPixel(i,j,Project.CurrentColorClusters.ActiveColorCluster.GetColorAt(1,2));
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

function TBDInkOpaque.GetColorAt(pX,pY:integer):uint32;
begin
  Result:=Settings.ActiveColor;
end;

procedure TBDInkOpaque.ProcessWithCEL(pX,pY:integer);
begin
  if Settings.ClearKeyColor then
    Project.CurrentImage.PutImage(Project.CELImage.Left,Project.CELImage.Top,Project.CELImage,$FF000000)
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

function TBDInkVGrad.GetColorAt(pX,pY:integer):uint32;
begin
  if fHeight>1 then begin
    if Settings.DitherGradients then
      Result:=Project.CurrentColorClusters.ActiveColorCluster.GetColorAtDithered(pY-fTop,fHeight-1,Settings.DitherStrength)
    else
      Result:=Project.CurrentColorClusters.ActiveColorCluster.GetColorAt(pY-fTop,fHeight-1)
  end else
    Result:=Project.CurrentColorClusters.ActiveColorCluster.GetColorAt(1,2);
end;

// -------------------------------------------------------- [ TBDInkCGrad ] ---

constructor TBDInkCGrad.Create;
begin
  inherited ;
  fName:='C GRAD';
  fHint:='CIRCULAR GRADIENT. '#132'SELECT '#133'CONFIGURE';
  fSupportsOnTheFly:=true;
end;

function TBDInkCGrad.GetColorAt(pX,pY:integer):uint32;
var r:integer;
begin
  if Settings.CGradRadius>1 then begin
    r:=trunc(Sqrt(sqr(px-Settings.CGradCenterX)+sqr(py-Settings.CGradCenterY))) mod Settings.CGradRadius;
    if Settings.DitherGradients then
      Result:=Project.CurrentColorClusters.ActiveColorCluster.
        GetColorAtDithered(r,Settings.CGradRadius,Settings.DitherStrength)
    else
      Result:=Project.CurrentColorClusters.ActiveColorCluster.
        GetColorAt(r,Settings.CGradRadius)
  end else
    Result:=Project.CurrentColorClusters.ActiveColorCluster.GetColorAt(1,2);
end;

procedure TBDInkCGrad.Configure;
begin
  MessageQueue.AddMessage(MSG_TOGGLECONTROLS);
  ActiveTool:=Tools.ItemByName['CONFCG'];
end;

// ------------------------------------------------------- [ TBDInkRandom ] ---

constructor TBDInkRandom.Create;
begin
  inherited ;
  fName:='RANDOM';
  fHint:='RANDOM COLORS FROM THE SELECTED COLOR CLUSTER.';
  fSupportsOnTheFly:=true;
end;

function TBDInkRandom.GetColorAt(pX,pY:integer):uint32;
begin
  Result:=Project.CurrentColorClusters.ActiveColorCluster.
    GetColorAt(random(256),256);
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
  fTempImage:=TBDRegion.Create(fWidth,fHeight);
  fTempImage.PutImagePart(0,0,fLeft,fTop,fWidth,fHeight,Project.CurrentImage);
end;

procedure TBDInkSoften.InitializeArea(pX1,pY1,pX2,pY2:integer);
begin
  inherited ;
  fTempImage:=TBDRegion.Create(fWidth,fHeight);
  fTempImage.PutImagePart(0,0,fLeft,fTop,fWidth,fHeight,Project.CurrentImage);
end;

function TBDInkSoften.GetColorAt(pX,pY:integer):uint32;
var r,g,b,c:integer;p:uint32;
begin
  pX-=fLeft;pY-=fTop;
  c:=1;
  p:=fTempImage.GetPixel(px,py);
  r:=(p and $FF0000)>>16;
  g:=(p and $FF00)>>8;
  b:=(p and $FF);
  if px>0 then begin
    p:=fTempImage.GetPixel(px-1,py);
    r+=(p and $FF0000)>>16;
    g+=(p and $FF00)>>8;
    b+=(p and $FF);
    inc(c);
  end;
  if px<fTempImage.Width-1 then begin
    p:=fTempImage.GetPixel(px+1,py);
    r+=(p and $FF0000)>>16;
    g+=(p and $FF00)>>8;
    b+=(p and $FF);
    inc(c);
  end;
  if py>0 then begin
    p:=fTempImage.GetPixel(px,py-1);
    r+=(p and $FF0000)>>16;
    g+=(p and $FF00)>>8;
    b+=(p and $FF);
    inc(c);
  end;
  if py<fTempImage.Height-1 then begin
    p:=fTempImage.GetPixel(px,py+1);
    r+=(p and $FF0000)>>16;
    g+=(p and $FF00)>>8;
    b+=(p and $FF);
    inc(c);
  end;
  Result:=($FF000000)+
          (((r div c) and $ff)<<16)+
          (((g div c) and $ff)<<8)+
          (((b div c) and $ff));
end;

procedure TBDInkSoften.PostProcess;
var i,j:integer;
begin
  for j:=fTop to fTop+fHeight-1 do
    for i:=fLeft to fLeft+fWidth-1 do
      if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
        Project.CurrentImage.PutPixel(i,j,GetColorAt(i,j));
  if Assigned(fTempImage) then FreeAndNil(fTempImage);
end;

// -------------------------------------------------------- [ TBDInkRGrad ] ---

constructor TBDInkRGrad.Create;
begin
  inherited ;
  fName:='R GRAD';
  fHint:='RADIAL GRADIENT. '#132'SELECT '#133'CONFIGURE';
  fSupportsOnTheFly:=true;
end;

function TBDInkRGrad.GetColorAt(pX,pY:integer):uint32;
var d:integer;
begin
  if (Settings.RGradCenterX>pX) then begin
    d:=trunc(arctan((Settings.RGradCenterY-pY)/(Settings.RGradCenterX-pX))*180/pi)+270;
  end else
  if (Settings.RGradCenterX<pX) then begin
    d:=trunc(arctan((Settings.RGradCenterY-pY)/(Settings.RGradCenterX-pX))*180/pi)+90;
  end else begin
    if (Settings.RGradCenterY>=pY) then begin
      d:=0;
    end else begin
      d:=180;
    end;
  end;
  if Settings.DitherGradients then
    Result:=Project.CurrentColorClusters.ActiveColorCluster.GetColorAtDithered((round((d+Settings.RGradRotation)*Settings.RGradRepetitions)) mod 360,359,Settings.DitherStrength)
  else
    Result:=Project.CurrentColorClusters.ActiveColorCluster.GetColorAt((round((d+Settings.RGradRotation)*Settings.RGradRepetitions)) mod 360,359)
end;

procedure TBDInkRGrad.Configure;
begin
  MessageQueue.AddMessage(MSG_OPENCONFIGURERGRADDIALOG);
end;

end.

