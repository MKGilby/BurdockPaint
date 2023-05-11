unit BDPInks;

{$mode delphi}

interface

uses
  Classes, SysUtils, Lists, BDPModalDialogs;

type

  // It is required to call InitializeArea[WH] before using GetColorIndexAt
  // or PostProcess! (Area must be set to the smallest rectangle containing
  // pixels need to be drawn!)

  { TBDInk }

  TBDInk=class
    constructor Create; virtual;
    procedure InitializeAreaWH(pLeft,pTop,pWidth,pHeight:integer);
    procedure InitializeArea(pX1,pY1,pX2,pY2:integer);
    function GetColorIndexAt(pX,pY:integer):integer; virtual;
    procedure PostProcess; virtual; abstract;
    procedure Configure; virtual;
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
    procedure PostProcess; override;
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
    procedure PostProcess; override;
  end;

  { TBDInkVGrad }

  TBDInkVGrad=class(TBDInk)
    constructor Create; override;
    function GetColorIndexAt(pX,pY: integer):integer; override;
    procedure PostProcess; override;
  end;

  { TBDInkRGrad }

  TBDInkRGrad=class(TBDInk)
    constructor Create; override;
    function GetColorIndexAt(pX,pY: integer):integer; override;
    procedure PostProcess; override;
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

function TBDInk.GetColorIndexAt(pX,pY:integer):integer;
begin
  Result:=-1;
  if fSupportsOnTheFly then
    raise Exception.Create('This ink ('+fName+') needs to override the GetColorIndexAt method!')
  else
    raise Exception.Create('This ink does not support OnTheFly, GetColorIndexAt method shouldn''t be called! ('+fName+')');
end;

procedure TBDInk.Configure;
begin
  if Assigned(fConfigureDialog) then fConfigureDialog.Show;
end;

// ------------------------------------------------------------- [ TBDInk ] ---

constructor TBDInks.Create;
begin
  AddObject('OPAQUE',TBDInkOpaque.Create);
  AddObject('H GRAD',TBDInkHGrad.Create);
  AddObject('L GRAD',TBDInkLGrad.Create);
  AddObject('V GRAD',TBDInkVGrad.Create);
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

function TBDInkHGrad.GetColorIndexAt(pX,pY: integer):integer;
begin
  if fWidth>1 then begin
    if Settings.DitherGradients then
      Result:=Project.CurrentImage.ColorClusters[ActiveColorClusterIndex].GetIndexAtDithered(px-fLeft,fWidth-1,Settings.DitherStrength)
    else
      Result:=Project.CurrentImage.ColorClusters[ActiveColorClusterIndex].GetIndexAt(px-fLeft,fWidth-1)
  end else
    Result:=Project.CurrentImage.ColorClusters[ActiveColorClusterIndex].GetIndexAt(1,2);
end;

procedure TBDInkHGrad.PostProcess;
var i,j:integer;
begin
  for j:=fTop to fTop+fHeight-1 do
    for i:=fLeft to fLeft+fWidth-1 do
      if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
        Project.CurrentImage.PutPixel(i,j,GetColorIndexAt(i,j));
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
var SegmentRight:integer;
begin
  SegmentRight:=i;
  repeat
    inc(SegmentRight);
  until (SegmentRight>=Project.CurrentImage.Width) or (Project.CurrentImage.GetPixel(SegmentRight,j)<>POSTPROCESSCOLOR);
  Result:=SegmentRight-i-1;
  dec(SegmentRight);
  if Result>0 then begin
    if Settings.DitherGradients then begin
      while i<SegmentRight+1 do begin
        Project.CurrentImage.PutPixel(i,j,Project.CurrentImage.ColorClusters[ActiveColorClusterIndex].GetIndexAtDithered(SegmentRight-i,Result+1,Settings.DitherStrength));
        inc(i);
      end;
    end else begin
      while i<SegmentRight+1 do begin
        Project.CurrentImage.PutPixel(i,j,Project.CurrentImage.ColorClusters[ActiveColorClusterIndex].GetIndexAt(SegmentRight-i,Result+1));
        inc(i);
      end;
    end;
  end else begin
    Project.CurrentImage.PutPixel(i,j,Project.CurrentImage.ColorClusters[ActiveColorClusterIndex].GetIndexAt(1,2));
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

procedure TBDInkOpaque.PostProcess;
var i,j:integer;
begin
  for j:=fTop to fTop+fHeight-1 do
    for i:=fLeft to fLeft+fWidth-1 do
      if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
        Project.CurrentImage.PutPixel(i,j,Settings.ActiveColorIndex);
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
      Result:=Project.CurrentImage.ColorClusters[ActiveColorClusterIndex].GetIndexAtDithered(pY-fTop,fHeight-1,Settings.DitherStrength)
    else
      Result:=Project.CurrentImage.ColorClusters[ActiveColorClusterIndex].GetIndexAt(pY-fTop,fHeight-1)
  end else
    Result:=Project.CurrentImage.ColorClusters[ActiveColorClusterIndex].GetIndexAt(1,2);
end;

procedure TBDInkVGrad.PostProcess;
var i,j:integer;
begin
  for j:=fTop to fTop+fHeight-1 do
    for i:=fLeft to fLeft+fWidth-1 do
      if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
        Project.CurrentImage.PutPixel(i,j,GetColorIndexAt(i,j));
end;

// -------------------------------------------------------- [ TBDInkRGrad ] ---

constructor TBDInkRGrad.Create;
begin
  inherited ;
  fName:='R GRAD';
  fHint:='ROUND GRADIENT.';
  fSupportsOnTheFly:=true;
end;

function TBDInkRGrad.GetColorIndexAt(pX, pY: integer): integer;
var r:integer;
begin
  if Settings.RGradRadius>1 then begin
    r:=trunc(Sqrt(sqr(px-Settings.RGradCenterX)+sqr(py-Settings.RGradCenterY))) mod Settings.RGradRadius;
    if Settings.DitherGradients then
      Result:=Project.CurrentImage.ColorClusters[ActiveColorClusterIndex].
        GetIndexAtDithered(r,Settings.RGradRadius,Settings.DitherStrength)
    else
      Result:=Project.CurrentImage.ColorClusters[ActiveColorClusterIndex].
        GetIndexAt(r,Settings.RGradRadius)
  end else
    Result:=Project.CurrentImage.ColorClusters[ActiveColorClusterIndex].GetIndexAt(1,2);
end;

procedure TBDInkRGrad.PostProcess;
var i,j:integer;
begin
  for j:=fTop to fTop+fHeight-1 do
    for i:=fLeft to fLeft+fWidth-1 do
      if Project.CurrentImage.GetPixel(i,j)=POSTPROCESSCOLOR then
        Project.CurrentImage.PutPixel(i,j,GetColorIndexAt(i,j));
end;

procedure TBDInkRGrad.Configure;
begin
  MessageQueue.AddMessage(MSG_TOGGLECONTROLS);
  ActiveTool:=Tools.ItemByName['CONFRG'];
end;

end.


