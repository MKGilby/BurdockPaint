{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPInfoBar;

{$mode Delphi}{$H+}

interface

uses SysUtils, vcc2_VisibleControlStatic;

type

  { TBDInfoBar }

  TBDInfoBar=class(TVisibleControlStatic)
    constructor Create;
    procedure ShowText(text:string);
  protected
    procedure ReDraw; override;
  private
    fTextTop:integer;
    fText:string;
    procedure fSetTop(aValue:integer);
  public
    property Top:integer read fTop write fSetTop;
  end;

implementation

uses BDPShared, mk_sdl2, Font2Unit;

{ TBDInfoBar }

constructor TBDInfoBar.Create;
begin
  inherited Create;
  Width:=Settings.WindowWidth;
  Height:=INFOBARHEIGHT;
  fTop:=Settings.WindowHeight-CONTROLSHEIGHT-INFOBARHEIGHT;
  fText:='';
  if fTop=0 then fTextTop:=3 else fTextTop:=6;
  fNeedRedraw:=true;
end;

procedure TBDInfoBar.ShowText(text:string);
begin
  if fText<>text then begin
    fText:=text;
    fNeedRedraw:=true;
  end;
end;

procedure TBDInfoBar.ReDraw;
begin
  if fText<>'' then begin
    if fTop>0 then begin
      fImage.Bar(0,0,Width,3,SystemPalette.Colors[SYSTEMCOLORDARK]);
      fImage.Bar(0,3,Width,Height-3,SystemPalette.Colors[SYSTEMCOLORMID]);
    end else begin
      fImage.Bar(0,0,Width,Height-3,SystemPalette.Colors[SYSTEMCOLORMID]);
      fImage.Bar(0,Height-3,Width,3,SystemPalette.Colors[SYSTEMCOLORDARK]);
    end;
    MM.Fonts['Black'].OutText(fImage,fText,8,fTextTop,mjLeft);
  end else
    fImage.Bar(0,0,Width,Height,0,0,0,0);
end;

procedure TBDInfoBar.fSetTop(aValue:integer);
begin
  fTop:=aValue;
  if fTop<0 then fTop:=0
  else if fTop>Settings.WindowHeight-INFOBARHEIGHT then fTop:=Settings.WindowHeight-INFOBARHEIGHT;
  if fTop=0 then fTextTop:=3 else fTextTop:=6;
end;

end.

