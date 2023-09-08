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

// This is a stripped version of the original BDPImage!

unit BDPImage;

{$mode Delphi}

interface

uses
  Classes, SysUtils, BDPPalette, ARGBImageUnit;

type

  { TBDRegion }

  TBDRegion=class(TARGBImage)
    // Creates region from stream (fileformats.txt - R-block)
    constructor CreateFromStream(iStream:TStream);

    // ------------- File/Stream operations ----------------
    // Saves image to file, including palette data.
    procedure SaveToFile(pFilename:string);
    // Saves image to stream, including palette data.
    procedure SaveToStream(Target:TStream);
    // Loads image from file, including palette data.
    procedure LoadFromFile(pFilename:string);
    // Loads image from stream, including palette data.
    procedure LoadFromStream(Source:TStream);
    // Read a region as whole image from stream.
    procedure LoadRegionFromStream(Source:TStream;Palette:TBDPalette=nil);
  protected
    // Image position (used for CELImage, leave on 0,0 otherwise)
    fLeft,fTop:integer;
    // Is the pixel data changed?
    fChanged:boolean;
  public
    property Left:integer read fLeft write fLeft;
    property Top:integer read fTop write fTop;
    property Changed:boolean read fChanged write fChanged;
  end;

implementation

uses MyZStreamUnit, Logger, MKToolbox, FastPaletteUnit;

const
  IMAGEDATAID=$49;
  REGIONDATAID=$52;


{ TBDRegion }

constructor TBDRegion.CreateFromStream(iStream:TStream);
begin
  LoadFromStream(iStream);
  fChanged:=false;
end;

procedure TBDRegion.SaveToFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmCreate);
  SaveToStream(Xs);
  FreeAndNil(Xs);
end;

procedure TBDRegion.SaveToStream(Target:TStream);
var i:integer;curr:int64;Xs:TStream;
begin
  i:=REGIONDATAID;
  Target.Write(i,1);
  curr:=Target.Position;
  i:=0;
  Target.Write(i,4);
  Xs:=TMemoryStream.Create;
  i:=2;  // Version
  Xs.Write(i,1);
  Xs.Write(fLeft,2);
  Xs.Write(fTop,2);
  Xs.Write(Width,2);
  Xs.Write(Height,2);
  Xs.Write(Rawdata^,Width*Height*4);
  Xs.Position:=0;
  CompressStream(Xs,Target,Xs.Size);
  FreeAndNil(Xs);
  i:=Target.Position-curr-4;
  Target.Position:=curr;
  Target.write(i,4);
  Target.Position:=Target.Position+i;
end;

procedure TBDRegion.LoadFromFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmOpenRead or fmShareDenyNone);
  LoadFromStream(Xs);
  FreeAndNil(Xs);
end;

procedure TBDRegion.LoadFromStream(Source:TStream);
var curr,size:int64;b:byte;
    fPalette:TBDPalette;
begin
  b:=0;
  Source.Read(b,1);
  if b=IMAGEDATAID then begin  // It is a legacy I-block.
    size:=0;
    Source.Read(size,4);
    curr:=Source.Position;
    Source.Read(b,1);
    if b=1 then begin  // version 1, C-block and R-block
      fPalette:=TBDPalette.CreateFromStream(Source);
      LoadRegionFromStream(Source,fPalette);
      fPalette.Free;
    end
    else if b=2 then begin  // version 2, only region
      LoadRegionFromStream(Source,nil);
    end
    else raise Exception.Create(Format('Invalid image version! (%d)',[b]));
    Source.Position:=curr+size;
  end else
  if b=REGIONDATAID then begin  // It is an R-block
    Source.Position:=Source.Position-1;  // Step back
    LoadRegionFromStream(Source,nil);  // and load as region
  end;
  fChanged:=false;
end;

procedure TBDRegion.LoadRegionFromStream(Source:TStream; Palette:TBDPalette);
var curr,size:int64;b:byte;Xs:TStream;i,j:integer;
begin
  b:=0;
  Source.Read(b,1);
  if b<>REGIONDATAID then raise Exception.Create(Format('Region data ID expected (0x%.2x), got 0x%.2x!',[REGIONDATAID,b]));
  size:=0;
  Source.Read(size,4);
  curr:=Source.Position;
  Xs:=TMemoryStream.Create;
  UnCompressStream(Source,Xs);
  Source.Position:=curr+size;
  Xs.Position:=0;

  Xs.Read(b,1);  // Version
  fLeft:=0;fTop:=0;fWidth:=0;fHeight:=0;
  if b=1 then begin
    if not Assigned(Palette) then raise Exception.Create('Loading v1 region without palette!');
    Xs.Read(fLeft,2);
    Xs.Read(fTop,2);
    Xs.Read(fWidth,2);
    Xs.Read(fHeight,2);
    Freemem(fRawdata);
    fRawdata:=Getmem(fWidth*fHeight*4);
    j:=0;
    for i:=0 to fWidth*fHeight-1 do begin
      Xs.Read(j,2);
      uint32((fRawdata+i*4)^):=Palette.Colors[j];
    end;
    FreeAndNil(Xs);
  end else if b=2 then begin
    Xs.Read(fLeft,2);
    Xs.Read(fTop,2);
    Xs.Read(fWidth,2);
    Xs.Read(fHeight,2);
    Freemem(fRawdata);
    fRawdata:=Getmem(fWidth*fHeight*4);
    Xs.Read(fRawdata^,fWidth*fHeight*4);
    FreeAndNil(Xs);
  end else
    Exception.Create(Format('Unknown PixelData version! (%d)',[b]));

end;

end.

