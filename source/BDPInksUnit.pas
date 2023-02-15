unit BDPInksUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, Lists;

type
  // It is required to call InitializeArea[WH] before using GetColorIndexAt
  // or PostProcess! (Area must be set to the smallest rectangle containing
  // pixels need to be drawn!)

  TBDInk=class
    constructor Create; virtual;
    procedure InitializeAreaWH(pLeft,pTop,pWidth,pHeight:integer);
    procedure InitializeArea(pX1,pY1,pX2,pY2:integer);
    function GetColorIndexAt({%H-}pX,{%H-}pY:integer):integer; virtual;
    procedure PostProcess; virtual; abstract;
  protected
    fLeft,fTop,fWidth,fHeight:integer;
    fSupportsOnTheFly:boolean;
    fName,fHint:string;
  public
    property Name:string read fName;
    property Hint:string read fHint;
    property SupportsOnTheFly:boolean read fSupportsOnTheFly;
  end;

  TBDInks=class(TNamedList<TBDInk>)
    constructor Create;
  end;

  TBDInkHGrad=class(TBDInk)
    constructor Create; override;
    function GetColorIndexAt(pX,{%H-}pY: integer):integer; override;
    procedure PostProcess; override;
  end;

  TBDInkLGrad=class(TBDInk)
    constructor Create; override;
    procedure PostProcess; override;
  private
    function ProcessSegment(i,j:integer):integer;
  end;

  TBDInkOpaque=class(TBDInk)
    constructor Create; override;
    function GetColorIndexAt({%H-}pX, {%H-}pY: integer):integer; override;
    procedure PostProcess; override;
  end;

  TBDInkVGrad=class(TBDInk)
    constructor Create; override;
    function GetColorIndexAt({%H-}pX,pY: integer):integer; override;
    procedure PostProcess; override;
  end;

implementation

uses MKToolBox, BDPSharedUnit, Logger;

// ------------------------------------------------------------- [ TBDInk ] ---

constructor TBDInk.Create;
begin
  fLeft:=0;
  fTop:=0;
  fWidth:=WindowWidth;
  fHeight:=WindowHeight;
  ActiveCluster.startindex:=128-32;
  ActiveCluster.endindex:=143-32;
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
//  Log.LogDebug(inttostr(pLeft)+', '+inttostr(pTop)+', '+inttostr(pWidth)+', '+inttostr(pHeight));
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
//  Log.LogDebug(inttostr(pX1)+', '+inttostr(pY1)+', '+inttostr(pX2)+', '+inttostr(pY2));
end;

function TBDInk.GetColorIndexAt(pX,pY:integer):integer;
begin
  Result:=-1;
  if fSupportsOnTheFly then
    raise Exception.Create('This ink ('+fName+') needs to override the GetColorIndexAt method!')
  else
    raise Exception.Create('This ink does not support OnTheFly, GetColorIndexAt method shouldn''t be called! ('+fName+')');
end;

// ------------------------------------------------------------- [ TBDInk ] ---

constructor TBDInks.Create;
begin
  AddObject('OPAQUE',TBDInkOpaque.Create);
  AddObject('H GRAD',TBDInkHGrad.Create);
  AddObject('L GRAD',TBDInkLGrad.Create);
  AddObject('V GRAD',TBDInkVGrad.Create);
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
//  Log.LogDebug('pX='+inttostr(pX)+', pY='+inttostr(pY)+', fLeft='+inttostr(fLeft)+', fWidth='+inttostr(fWidth));
  if fWidth>1 then
    Result:=trunc(0.5+ActiveCluster.startindex+(ActiveCluster.endindex-ActiveCluster.startindex)*(pX-fLeft)/(fWidth-1))
  else
    Result:=ActiveCluster.startindex+(ActiveCluster.endindex-ActiveCluster.startindex) div 2;
end;

procedure TBDInkHGrad.PostProcess;
var i,j:integer;
begin
  for j:=fTop to fTop+fHeight-1 do
    for i:=fLeft to fLeft+fWidth-1 do
      if MainImage.GetPixel(i,j)=POSTPROCESSCOLOR then
        MainImage.PutPixel(i,j,GetColorIndexAt(i,j));
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
var e:integer;
begin
  e:=i;
  repeat
    inc(e);
  until (e>=MainImage.Width) or (MainImage.GetPixel(e,j)<>POSTPROCESSCOLOR);
  Result:=e-i-1;
  dec(e);
  if Result>0 then begin
    while i<e+1 do begin
      MainImage.PutPixel(i,j,
        trunc(0.5+ActiveCluster.endindex-(ActiveCluster.endindex-ActiveCluster.startindex)*(e-i)/(Result+1))
      );
      inc(i);
    end;
  end else begin
    MainImage.PutPixel(i,j,
      ActiveCluster.startindex+(ActiveCluster.endindex-ActiveCluster.startindex) div 2
    );
  end;
end;

procedure TBDInkLGrad.PostProcess;
var i,j:integer;
begin
  for j:=fTop to fTop+fHeight-1 do begin
    i:=fLeft;
    while i<=fLeft+fWidth-1 do begin
      if MainImage.GetPixel(i,j)=POSTPROCESSCOLOR then
        i+=ProcessSegment(i,j);
//        Image.CurrentFrame.PutPixel(i,j,GetColorIndexAt(i,j));
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
      if MainImage.GetPixel(i,j)=POSTPROCESSCOLOR then
        MainImage.PutPixel(i,j,Settings.ActiveColorIndex);
end;

// -------------------------------------------------------- [ TBDInkHGrad ] ---

constructor TBDInkVGrad.Create;
begin
  inherited ;
  fName:='V GRAD';
  fHint:='VERTICAL GRADIENT.';
  fSupportsOnTheFly:=true;
end;

function TBDInkVGrad.GetColorIndexAt(pX,pY: integer):integer;
begin
  if fHeight>1 then
    Result:=trunc(0.5+ActiveCluster.startindex+(ActiveCluster.endindex-ActiveCluster.startindex)*(pY-fTop)/(fHeight-1))
  else
    Result:=ActiveCluster.startindex+(ActiveCluster.endindex-ActiveCluster.startindex) div 2;
end;

procedure TBDInkVGrad.PostProcess;
var i,j:integer;
begin
  for j:=fTop to fTop+fHeight-1 do
    for i:=fLeft to fLeft+fWidth-1 do
      if MainImage.GetPixel(i,j)=POSTPROCESSCOLOR then
        MainImage.PutPixel(i,j,GetColorIndexAt(i,j));
end;

end.


