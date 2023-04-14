unit BDPColorBoxUnit;

{$mode Delphi}

interface

uses vcc2_VisibleControl, ARGBImageUnit;

type

  { TBDColorBox }

  TBDColorBox=class(TVisibleControl)
    constructor Create(iLeft,iTop:integer);
    procedure ColorChanged;
  protected
    procedure ReDraw; override;
  private
    fColorIndex:word;
    fTLImage,fTRImage,fBLImage,fBRImage:TARGBImage;
    procedure fSetColorIndex(value:word);
  public
    property ColorIndex:word read fColorIndex write fSetColorIndex;
  end;

implementation

uses BDPSharedUnit;

{ TBDColorBox }

constructor TBDColorBox.Create(iLeft,iTop:integer);
begin
  inherited Create;
  fLeft:=iLeft;
  fTop:=iTop;
  Width:=COLORBOXWIDTH;
  Height:=COLORBOXHEIGHT;
  fTLImage:=MM.Images.ItemByName['ArchTopLeft'];
  fTRImage:=MM.Images.ItemByName['ArchTopRight'];
  fBLImage:=MM.Images.ItemByName['ArchBottomLeft'];
  fBRImage:=MM.Images.ItemByName['ArchBottomRight'];
  fColorIndex:=65535;
  fVisible:=true;

  fNeedRedraw:=true;
end;

procedure TBDColorBox.ColorChanged;
begin
  fNeedRedraw:=true;
end;

procedure TBDColorBox.ReDraw;
begin
  if Assigned(fTexture) then begin
    with fTexture.ARGBImage do begin
      if (fColorIndex<Project.CurrentImage.Palette.Size) then
        Bar(3,3,Width-6,Height-6,Project.CurrentImage.Palette[fColorIndex]);
      Bar(8,0,Width-16,3,SystemPalette[2]);
      Bar(8,Height-3,fWidth-16,3,SystemPalette[2]);
      Bar(0,8,3,Height-16,SystemPalette[2]);
      Bar(Width-3,8,3,Height-16,SystemPalette[2]);
    end;
    if Assigned(fTLImage) then
      fTLImage.CopyTo(0,0,fTLImage.Width,fTLImage.Height,0,0,fTexture.ARGBImage,true);
    if Assigned(fTRImage) then
      fTRImage.CopyTo(0,0,fTRImage.Width,fTRImage.Height,fWidth-8,0,fTexture.ARGBImage,true);
    if Assigned(fBLImage) then
      fBLImage.CopyTo(0,0,fBLImage.Width,fBLImage.Height,0,fHeight-8,fTexture.ARGBImage,true);
    if Assigned(fBRImage) then
      fBRImage.CopyTo(0,0,fBRImage.Width,fBRImage.Height,fWidth-8,fHeight-8,fTexture.ARGBImage,true);
    fTexture.Update;
  end;
end;

procedure TBDColorBox.fSetColorIndex(value:word);
begin
  fColorIndex:=value;
  fNeedRedraw:=true;
end;

end.

