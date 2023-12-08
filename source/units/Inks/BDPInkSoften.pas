{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPInkSoften;

{$mode Delphi}

interface

uses BDPInkBase, BDPRegion;

type

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

implementation

uses BDPShared;

{ TBDInkSoften }

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
  fTempImage.PutImagePart(0,0,fLeft,fTop,fWidth,fHeight,Project.CurrentRegion);
end;

procedure TBDInkSoften.InitializeArea(pX1,pY1,pX2,pY2:integer);
begin
  inherited ;
  fTempImage:=TBDRegion.Create(fWidth,fHeight);
  fTempImage.PutImagePart(0,0,fLeft,fTop,fWidth,fHeight,Project.CurrentRegion);
end;

function TBDInkSoften.GetColorAt(pX,pY:integer):uint32;
var r,g,b,a,c:integer;p:uint32;
begin
  pX-=fLeft;pY-=fTop;
  c:=Settings.SoftenCenterWeight;
  p:=fTempImage.GetPixel(px,py);
  if not Settings.SoftenAlphaToo then Result:=(p and $FF000000)>>24;
  a:=Settings.SoftenCenterWeight*(p and $FF000000)>>24;
  r:=Settings.SoftenCenterWeight*(p and $FF0000)>>16;
  g:=Settings.SoftenCenterWeight*(p and $FF00)>>8;
  b:=Settings.SoftenCenterWeight*(p and $FF);
  if px>0 then begin
    p:=fTempImage.GetPixel(px-1,py);
    a+=(p and $FF000000)>>24;
    r+=(p and $FF0000)>>16;
    g+=(p and $FF00)>>8;
    b+=(p and $FF);
    inc(c);
  end;
  if px<fTempImage.Width-1 then begin
    p:=fTempImage.GetPixel(px+1,py);
    a+=(p and $FF000000)>>24;
    r+=(p and $FF0000)>>16;
    g+=(p and $FF00)>>8;
    b+=(p and $FF);
    inc(c);
  end;
  if py>0 then begin
    p:=fTempImage.GetPixel(px,py-1);
    a+=(p and $FF000000)>>24;
    r+=(p and $FF0000)>>16;
    g+=(p and $FF00)>>8;
    b+=(p and $FF);
    inc(c);
  end;
  if py<fTempImage.Height-1 then begin
    p:=fTempImage.GetPixel(px,py+1);
    a+=(p and $FF000000)>>24;
    r+=(p and $FF0000)>>16;
    g+=(p and $FF00)>>8;
    b+=(p and $FF);
    inc(c);
  end;
  if Settings.SoftenAlphaToo then
    Result:=(((a div c) and $ff)<<24)+
            (((r div c) and $ff)<<16)+
            (((g div c) and $ff)<<8)+
            (((b div c) and $ff))
  else
    Result+=(((r div c) and $ff)<<16)+
            (((g div c) and $ff)<<8)+
            (((b div c) and $ff));
end;

procedure TBDInkSoften.PostProcess;
var i,j:integer;
begin
  for j:=fTop to fTop+fHeight-1 do
    for i:=fLeft to fLeft+fWidth-1 do
      if Project.CurrentRegion.GetPixel(i,j)=POSTPROCESSCOLOR then
        Project.CurrentRegion.PutPixel(i,j,GetColorAt(i,j));
  if Assigned(fTempImage) then fTempImage.Free;
end;

end.

