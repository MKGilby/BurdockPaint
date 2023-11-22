{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPColorBox;

{$mode Delphi}

interface

uses SysUtils, ARGBImageUnit, vcc2_VisibleControlStatic;

type

  { TBDColorBox }

  TBDColorBox=class(TVisibleControlStatic)
    constructor Create(iLeft,iTop,iWidth,iHeight:integer);
    procedure ColorChanged;
  protected
    procedure ReDraw; override;
  private
    fColor:uint32;
    fTLImage,fTRImage,fBLImage,fBRImage:TARGBImage;
    procedure fSetColor(value:uint32);
  public
    property Color:uint32 read fColor write fSetColor;
  end;

implementation

uses BDPShared, mk_sdl2;

{ TBDColorBox }

constructor TBDColorBox.Create(iLeft,iTop,iWidth,iHeight:integer);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  Width:=iWidth;
  Height:=iHeight;
  fTLImage:=MM.Images.ItemByName['ArchTopLeft'];
  fTRImage:=MM.Images.ItemByName['ArchTopRight'];
  fBLImage:=MM.Images.ItemByName['ArchBottomLeft'];
  fBRImage:=MM.Images.ItemByName['ArchBottomRight'];
  fColor:=$FF000000;
  fVisible:=true;

  fNeedRedraw:=true;
end;

procedure TBDColorBox.ColorChanged;
begin
  fNeedRedraw:=true;
end;

procedure TBDColorBox.ReDraw;
begin
  with fImage do begin
    Bar(3,3,Width-6,Height-6,fColor);
    Bar(8,0,Width-16,3,SystemPalette[SYSTEMCOLORDARK]);
    Bar(8,Height-3,fWidth-16,3,SystemPalette[SYSTEMCOLORDARK]);
    Bar(0,8,3,Height-16,SystemPalette[SYSTEMCOLORDARK]);
    Bar(Width-3,8,3,Height-16,SystemPalette[SYSTEMCOLORDARK]);
  end;
  if Assigned(fTLImage) then
    fTLImage.CopyTo(0,0,fTLImage.Width,fTLImage.Height,0,0,fImage,true);
  if Assigned(fTRImage) then
    fTRImage.CopyTo(0,0,fTRImage.Width,fTRImage.Height,fWidth-8,0,fImage,true);
  if Assigned(fBLImage) then
    fBLImage.CopyTo(0,0,fBLImage.Width,fBLImage.Height,0,fHeight-8,fImage,true);
  if Assigned(fBRImage) then
    fBRImage.CopyTo(0,0,fBRImage.Width,fBRImage.Height,fWidth-8,fHeight-8,fImage,true);
end;

procedure TBDColorBox.fSetColor(value:uint32);
begin
  if (fColor<>value) then begin fColor:=value;fNeedRedraw:=true;end;
end;

end.

