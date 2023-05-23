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

unit BDPProject;

{$mode Delphi}

interface

uses
  Classes, SysUtils, fgl, BDPImage, BDPUndo, BDPColorCluster;

type

  { TBDExtendedImage }

  TBDExtendedImage=class(TBDImage)
    // Creates a new empty 320x200 image with the NTSC.COL palette,
    // creates empty UndoSystems and default color clusters.
    constructor Create; overload;

    // Creates a new empty image with the NTSC.COL palette,
    // creates empty UndoSystems and default color clusters.
    constructor Create(iWidth,iHeight:integer); overload;

    // Loads everything from stream. (fileformats.txt - E-block)
    constructor CreateFromStream(iStream:TStream);

    // Free assigned entities
    destructor Destroy; override;

    // Writes everything to stream. (fileformats.txt - E-block)
    procedure SaveToStream(pStream:TStream);

    // Clears undo/redo data
    procedure ClearUndoData;
  private
//    fImage:TBDImage;
    fImageUndoSystem:TBDImageUndoSystem;
    fPaletteUndoSystem:TBDPaletteUndoSystem;
    fColorClusters:TColorClusters;
    procedure LoadFromStreamV1(pStream:TStream);
  public
//    property Image:TBDImage read fImage;
    property ImageUndo:TBDImageUndoSystem read fImageUndoSystem;
    property PaletteUndo:TBDPaletteUndoSystem read fPaletteUndoSystem;
    property ColorClusters:TColorClusters read fColorClusters;
  end;

  TBDExtendedImages=class(TFPGObjectList<TBDExtendedImage>);

  { TBDProject }

  TBDProject=class
    // Creates a project with a new empty 320x200 image
    constructor Create;

    // Creates a project from file. (fileformats.txt - P-block)
    constructor CreateFromFile(iFilename:String);

    // Creates a project from stream. (fileformats.txt - P-block)
    constructor CreateFromStream(iStream:TStream);

    // Free assigned entities
    destructor Destroy; override;

    // Saves project to stream. (fileformats.txt - P-block)
    procedure SaveToFile(pFilename:string);

    // Saves project to stream. (fileformats.txt - P-block)
    procedure SaveToStream(pStream:TStream);

    // Clears undo data and releases CEL
    procedure Clean;
  private
    fCurrentImageIndex:integer;
    fImages:TBDExtendedImages;
    // Only needed in-program, but must be the same size as the current image.
    fOverlayImage:TBDImage;
    fCELImage:TBDImage;
    procedure LoadFromStreamV1(pStream:TStream);
    procedure SetOverlayPalette;
    procedure fSetCurrentImageIndex(value:integer);
    function fGetCurrentImage:TBDExtendedImage;
  public
    property Images:TBDExtendedImages read fImages;
    property CurrentImageIndex:integer read fCurrentImageIndex write fSetCurrentImageIndex;
    property OverlayImage:TBDImage read fOverlayImage;
    property CurrentImage:TBDExtendedImage read fGetCurrentImage;
    property CELImage:TBDImage read fCELImage write fCELImage;
  end;

implementation

uses BDPShared, BDPPalette;

const
  EXTENDEDIMAGEID=$45;
  PROJECTDATAID=$50;

{ TBDExtendedImage }

constructor TBDExtendedImage.Create;
begin
  Create(320,200);
end;

constructor TBDExtendedImage.Create(iWidth,iHeight:integer);
begin
  inherited Create(iWidth,iHeight);
  fImageUndoSystem:=TBDImageUndoSystem.Create;
  fPaletteUndoSystem:=TBDPaletteUndoSystem.Create;
  fColorClusters:=TColorClusters.Create;
end;

constructor TBDExtendedImage.CreateFromStream(iStream:TStream);
var curr,size:int64;b:byte;
begin
  b:=0;
  iStream.Read(b,1);
  if b<>EXTENDEDIMAGEID then raise Exception.Create(Format('Extended image ID expected, got 0x%.2x!',[b]));
  size:=0;
  iStream.Read(size,4);
  curr:=iStream.Position;
  iStream.Read(b,1);  // Version
  if b=1 then LoadFromStreamV1(iStream)
  else raise Exception.Create(Format('Unknown extended image data version! (%d)',[b]));
  iStream.Position:=curr+size;

  if not Assigned(fImageUndoSystem) then fImageUndoSystem:=TBDImageUndoSystem.Create;
  if not Assigned(fPaletteUndoSystem) then fPaletteUndoSystem:=TBDPaletteUndoSystem.Create;
  if not Assigned(fColorClusters) then fColorClusters:=TColorClusters.Create;

end;

destructor TBDExtendedImage.Destroy;
begin
  if Assigned(fColorClusters) then FreeAndNil(fColorClusters);
  if Assigned(fImageUndoSystem) then FreeAndNil(fImageUndoSystem);
  if Assigned(fPaletteUndoSystem) then FreeAndNil(fPaletteUndoSystem);
  inherited Destroy;
end;

procedure TBDExtendedImage.SaveToStream(pStream:TStream);
var i,curr:int64;flags:byte;
begin
  i:=EXTENDEDIMAGEID;
  pStream.Write(i,1);
  curr:=pStream.Position;
  i:=0;  pStream.Write(i,4);  // Size placeholder
  i:=1;  pStream.Write(i,1);  // version

  flags:=0;
  if fImageUndoSystem.Count>0 then flags:=flags or 2;
  if fPaletteUndoSystem.Count>0 then flags:=flags or 4;
  if fColorClusters.Count>0 then flags:=flags or 8;
  pStream.Write(flags,1);

  Palette.SaveToStream(pStream);
  SaveWholeImageDataToStream(pStream);
  if fImageUndoSystem.Count>0 then fImageUndoSystem.SaveToStream(pStream);
  if fPaletteUndoSystem.Count>0 then fPaletteUndoSystem.SaveToStream(pStream);
  if fColorClusters.Count>0 then fColorClusters.SaveToStream(pStream);

  i:=pStream.Position-curr-4;
  pStream.Position:=curr;
  pStream.write(i,4);
  pStream.Position:=pStream.Position+i;
end;

procedure TBDExtendedImage.ClearUndoData;
begin
  fImageUndoSystem.Clear;
  fPaletteUndoSystem.Clear;
end;

procedure TBDExtendedImage.LoadFromStreamV1(pStream:TStream);
var flags:byte;
begin
  flags:=0;
  pStream.Read(flags,1);
  fPalette:=TBDPalette.Create;
  Palette.LoadFromStream(pStream);
  LoadWholeImageDataFromStream(pStream);
  if flags and 2<>0 then fImageUndoSystem:=TBDImageUndoSystem.CreateFromStream(pStream);
  if flags and 4<>0 then fPaletteUndoSystem:=TBDPaletteUndoSystem.CreateFromStream(pStream);
  if flags and 8<>0 then fColorClusters:=TColorClusters.CreateFromStream(pStream);
end;


{ TBDProject }

constructor TBDProject.Create;
begin
  fImages:=TBDExtendedImages.Create;
  fImages.Add(TBDExtendedImage.Create);
  fCurrentImageIndex:=0;
  fOverlayImage:=TBDImage.Create(fImages[0].Width,fImages[0].Height);
  SetOverlayPalette;
  fOverlayImage.Bar(0,0,fOverlayImage.Width,fOverlayImage.Height,0);
  fCELImage:=nil;
end;

constructor TBDProject.CreateFromFile(iFilename:String);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(iFilename,fmOpenRead or fmShareDenyWrite);
  CreateFromStream(Xs);
  FreeAndNil(Xs);
end;

constructor TBDProject.CreateFromStream(iStream:TStream);
var curr,size:int64;b:byte;
begin
  fImages:=TBDExtendedImages.Create;
  b:=0;
  iStream.Read(b,1);
  if b<>PROJECTDATAID then raise Exception.Create(Format('Project image ID expected, got 0x%.2x!',[b]));
  size:=0;
  iStream.Read(size,4);
  curr:=iStream.Position;
  iStream.Read(b,1);  // Version
  if b=1 then LoadFromStreamV1(iStream)
  else raise Exception.Create(Format('Unknown project data version! (%d)',[b]));
  iStream.Position:=curr+size;

  if fImages.Count=0 then fImages.Add(TBDExtendedImage.Create);

  if (fCurrentImageIndex<0) or (fCurrentImageIndex>=fImages.Count) then
    fCurrentImageIndex:=0;
  fOverlayImage:=TBDImage.Create(fImages[fCurrentImageIndex].Width,fImages[fCurrentImageIndex].Height);
//  SetOverlayPalette;
  fOverlayImage.Palette.ResizeAndCopyColorsFrom(SystemPalette);
  fOverlayImage.Bar(0,0,fOverlayImage.Width,fOverlayImage.Height,0);
end;

destructor TBDProject.Destroy;
begin
  if Assigned(fCELImage) then FreeAndNil(fCELImage);
  if Assigned(fOverlayImage) then FreeAndNil(fOverlayImage);
  if Assigned(fImages) then FreeAndNil(fImages);
  inherited Destroy;
end;

procedure TBDProject.SaveToFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmCreate);
  SaveToStream(Xs);
  FreeAndNil(Xs);
end;

procedure TBDProject.SaveToStream(pStream:TStream);
var curr:int64;flags:byte;i:integer;
begin
  i:=PROJECTDATAID;
  pStream.Write(i,1);
  curr:=pStream.Position;
  i:=0;  pStream.Write(i,4);  // Size placeholder
  i:=1;  pStream.Write(i,1);  // version

  flags:=0;
  if Assigned(fCELImage) then flags:=flags or 1;
  pStream.Write(flags,1);
  i:=fImages.Count;
  pStream.Write(i,2);
  pStream.Write(fCurrentImageIndex,2);
  for i:=0 to fImages.Count-1 do
    fImages[i].SaveToStream(pStream);

  if Assigned(fCELImage) then fCELImage.SaveToStream(pStream);

  i:=pStream.Position-curr-4;
  pStream.Position:=curr;
  pStream.write(i,4);
  pStream.Position:=pStream.Position+i;
end;

procedure TBDProject.Clean;
var i:integer;
begin
  if Assigned(fCELImage) then FreeAndNil(fCELImage);
  for i:=0 to fImages.Count-1 do fImages[i].ClearUndoData;
end;

procedure TBDProject.LoadFromStreamV1(pStream:TStream);
var flags:byte;count:integer;
begin
  flags:=0;
  pStream.Read(flags,1);
  count:=0;
  pStream.Read(count,2);
  fCurrentImageIndex:=0;
  pStream.Read(fCurrentImageIndex,2);
  while count>0 do begin
    fImages.Add(TBDExtendedImage.CreateFromStream(pStream));
    dec(count);
  end;
  if flags and 1<>0 then fCELImage:=TBDImage.CreateFromStream(pStream);
end;

procedure TBDProject.SetOverlayPalette;
begin
  with fOverlayImage do begin
    Palette.Colors[0]:=$00000000;
    Palette.Colors[1]:=$ff040404;
    Palette.Colors[2]:=$ff5d5d5d;
    Palette.Colors[3]:=$ff9a9a9a;
    Palette.Colors[4]:=$ffc7c7c7;
    Palette.Colors[5]:=$ffc70404;
    Palette.Colors[6]:=$ff202020;
    Palette.Colors[7]:=$ff505050;
    Palette.Colors[8]:=$ff808080;
    Palette.Colors[9]:=$ffb0b0b0;
    Palette.Colors[10]:=$ffe0e0e0;
  end;
end;

procedure TBDProject.fSetCurrentImageIndex(value:integer);
begin
  if (value<>fCurrentImageIndex) and (value>=0) and (value<fImages.Count) then begin
    fCurrentImageIndex:=value;
    fOverlayImage.Recreate(fImages[fCurrentImageIndex].Width,fImages[fCurrentImageIndex].Height);
    fOverlayImage.Bar(0,0,fOverlayImage.Width,fOverlayImage.Height,0);
  end;
end;

function TBDProject.fGetCurrentImage:TBDExtendedImage;
begin
  Result:=fImages[fCurrentImageIndex];
end;

end.

