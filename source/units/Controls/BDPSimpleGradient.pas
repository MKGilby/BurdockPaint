unit BDPSimpleGradient;

{$mode Delphi}

interface

uses vcc2_VisibleControl, BDPGradient;

type

  { TBDSimpleGradient }

  TBDSimpleGradient=class(TVisibleControl)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer;iGradient:TGradient);
  protected
    procedure ReDraw; override;
  private
    fGradient:TGradient;
    fColorsWidth:integer;
    function fGetColorsWidth:integer;
  public
    property ColorsWidth:integer read fGetColorsWidth;
  end;


implementation

uses BDPShared;

{ TBDSimpleGradient }

constructor TBDSimpleGradient.Create(iLeft,iTop,iWidth,iHeight:integer;iGradient:TGradient);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  fGradient:=iGradient;
  Width:=iWidth;
  Height:=iHeight;
  fNeedRedraw:=true;
//  OnClick:=Click;
end;

procedure TBDSimpleGradient.ReDraw;
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
      // Gradient bar
      if Assigned(fGradient) then begin
        for i:=0 to fColorsWidth-1 do
          VLine(i+3,3,Height-6,fGradient.GetColorAt(i/(fColorsWidth-1)));
      end;
    end;
    fTexture.Update;
  end;
end;

function TBDSimpleGradient.fGetColorsWidth: integer;
begin
  Result:=fColorsWidth+6;
end;

end.

