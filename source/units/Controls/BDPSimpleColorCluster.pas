unit BDPSimpleColorCluster;

{$mode Delphi}

interface

uses vcc2_VisibleControl, BDPColorCluster;

type

  { TBDSimpleColorCluster }

  TBDSimpleColorCluster=class(TVisibleControl)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer;iColorCluster:TColorCluster);
  protected
    procedure ReDraw; override;
  private
    fColorCluster:TColorCluster;
    fColorsWidth:integer;
    function fGetColorsWidth:integer;
  public
    property ColorsWidth:integer read fGetColorsWidth;
  end;


implementation

uses BDPShared;

{ TBDSimpleColorCluster }

constructor TBDSimpleColorCluster.Create(iLeft,iTop,iWidth,iHeight:integer;iColorCluster:TColorCluster);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  fColorCluster:=iColorCluster;
  Width:=iWidth;
  Height:=iHeight;
  fNeedRedraw:=true;
//  OnClick:=Click;
end;

procedure TBDSimpleColorCluster.ReDraw;
var i:integer;
begin
  if Assigned(fTexture) then begin
    fColorsWidth:=Width-6;
    with fTexture.ARGBImage do begin
      // Outer border
      Bar(0,0,Width,3,SystemPalette[SYSTEMCOLORDARK]);
      Bar(0,Height-3,Width,3,SystemPalette[SYSTEMCOLORDARK]);
      Bar(0,3,3,Height-6,SystemPalette[SYSTEMCOLORDARK]);
      Bar(Width-3,3,3,Height-6,SystemPalette[SYSTEMCOLORDARK]);
      // Color cluster bar
      if Assigned(fColorCluster) then begin
        for i:=0 to fColorsWidth-1 do
          VLine(i+3,3,Height-6,fColorCluster.GetColorAt(i,fColorsWidth-1));
      end;
    end;
    fTexture.Update;
  end;
end;

function TBDSimpleColorCluster.fGetColorsWidth: integer;
begin
  Result:=fColorsWidth+6;
end;

end.

