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
  Classes, SysUtils, fgl, BDPRegion, BDPGradient, BDPPalette,
  BDPRegionUndo;

type

  { TBDImage }

  TBDImage=class
    // Creates a new empty 320x200 image with the NTSC.COL palette,
    // creates empty UndoSystems and default gradients.
    constructor Create; overload;

    // Creates a new empty image with the NTSC.COL palette,
    // creates empty UndoSystems and default gradients.
    constructor Create(iWidth,iHeight:integer); overload;

    // Loads everything from stream. (fileformats.txt - IMG-block)
    constructor CreateFromStream(iStream:TStream);

    // Free assigned entities
    destructor Destroy; override;

    // Writes everything to stream. (fileformats.txt - IMG-block)
    procedure SaveToStream(pStream:TStream);

    // Clears undo/redo data
    procedure ClearUndoData;
  private
    fRegion:TBDRegion;
    fPalette:TBDPalette;
    fRegionUndoSystem:TBDRegionUndoSystem;
//    fPaletteUndoSystem:TBDPaletteUndoSystem;
    fGradients:TGradientList;
    procedure LoadFromStreamV1(pStream:TStream);
  public
    property Region:TBDRegion read fRegion;
    property Palette:TBDPalette read fPalette;
    property RegionUndo:TBDRegionUndoSystem read fRegionUndoSystem;
//    property PaletteUndo:TBDPaletteUndoSystem read fPaletteUndoSystem;
    property Gradients:TGradientList read fGradients;
  end;

  TBDImageList=class(TFPGObjectList<TBDImage>);

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
    fImages:TBDImageList;
    // Only needed in-program, but must be the same size as the current image.
    fOverlayImage:TBDRegion;
    fCELImage:TBDRegion;
    procedure LoadFromStreamV1(pStream:TStream);
    procedure fSetCurrentImageIndex(value:integer);
    function fGetCurrentRegion:TBDRegion;
    function fGetCurrentPalette:TBDPalette;
    function fGetCurrentGradientList:TGradientList;
    function fGetCurrentImage:TBDImage;
  public
    property Images:TBDImageList read fImages;
    property CurrentImageIndex:integer read fCurrentImageIndex write fSetCurrentImageIndex;
    property OverlayImage:TBDRegion read fOverlayImage;
    property CurrentImage:TBDImage read fGetCurrentImage;
    property CurrentRegion:TBDRegion read fGetCurrentRegion;
    property CurrentPalette:TBDPalette read fGetCurrentPalette;
    property CurrentGradientList:TGradientList read fGetCurrentGradientList;
    property CELImage:TBDRegion read fCELImage write fCELImage;
  end;

implementation

uses BDPShared;

const
  IMAGEBLOCKID='IMG';
  PROJECTBLOCKID='PRJ';

{ TBDImage }

constructor TBDImage.Create;
begin
  Create(320,200);
end;

constructor TBDImage.Create(iWidth,iHeight:integer);
begin
  fRegion:=TBDRegion.Create(iWidth,iHeight);
  fPalette:=TBDPalette.Create(256);
  fRegionUndoSystem:=TBDRegionUndoSystem.Create;
//  fPaletteUndoSystem:=TBDPaletteUndoSystem.Create;
  fGradients:=TGradientList.Create;
end;

constructor TBDImage.CreateFromStream(iStream:TStream);
var curr,size:int64;b:byte;s:string;
begin
  s:=#0#0#0#0;
  iStream.Read(s[1],4);
  if uppercase(copy(s,1,3))<>IMAGEBLOCKID then raise
    Exception.Create(Format('Image block ID expected, got %s!',[copy(s,1,3)]));
  if ord(s[1]) and $20<>0 then Exception.Create('Image block cannot be compressed!');
  size:=0;
  iStream.Read(size,4);
  curr:=iStream.Position;
  b:=ord(s[4]);
  if b=1 then LoadFromStreamV1(iStream)
  else raise Exception.Create(Format('Unknown image data version! (%d)',[b]));
  iStream.Position:=curr+size;

  if not Assigned(fRegionUndoSystem) then fRegionUndoSystem:=TBDRegionUndoSystem.Create;
//  if not Assigned(fPaletteUndoSystem) then fPaletteUndoSystem:=TBDPaletteUndoSystem.Create;
  if not Assigned(fGradients) then fGradients:=TGradientList.Create;

end;

destructor TBDImage.Destroy;
begin
  if Assigned(fRegion) then fRegion.Free;
  if Assigned(fPalette) then fPalette.Free;
  if Assigned(fGradients) then fGradients.Free;
  if Assigned(fRegionUndoSystem) then fRegionUndoSystem.Free;
//  if Assigned(fPaletteUndoSystem) then fPaletteUndoSystem.Free;
  inherited Destroy;
end;

procedure TBDImage.SaveToStream(pStream:TStream);
var i,curr:int64;flags:byte;s:string;
begin
  s:=IMAGEBLOCKID+#1;
  pStream.Write(s[1],4);
  curr:=pStream.Position;
  i:=0;
  pStream.Write(i,4);  // Size placeholder

  flags:=0;
  if fRegionUndoSystem.Count>0 then flags:=flags or 2;
//  if fPaletteUndoSystem.Count>0 then flags:=flags or 4;
  if fGradients.Count>0 then flags:=flags or 8;
  pStream.Write(flags,1);

  fPalette.SaveToStream(pStream);
  fRegion.SaveToStream(pStream);
  if fRegionUndoSystem.Count>0 then fRegionUndoSystem.SaveToStream(pStream);
//  if fPaletteUndoSystem.Count>0 then fPaletteUndoSystem.SaveToStream(pStream);
  if fGradients.Count>0 then fGradients.SaveToStream(pStream);

  i:=pStream.Position-curr-4;
  pStream.Position:=curr;
  pStream.write(i,4);
  pStream.Position:=pStream.Position+i;
end;

procedure TBDImage.ClearUndoData;
begin
  fRegionUndoSystem.Clear;
//  fPaletteUndoSystem.Clear;
end;

procedure TBDImage.LoadFromStreamV1(pStream:TStream);
var flags:byte;
begin
  flags:=0;
  pStream.Read(flags,1);
  fPalette:=TBDPalette.CreateFromStream(pStream);
  fRegion:=TBDRegion.CreateFromStream(pStream);
  if flags and 2<>0 then fRegionUndoSystem:=TBDRegionUndoSystem.CreateFromStream(pStream);
//  if flags and 4<>0 then fPaletteUndoSystem:=TBDPaletteUndoSystem.CreateFromStream(pStream);
  if flags and 8<>0 then fGradients:=TGradientList.CreateFromStream(pStream);
end;


{ TBDProject }

constructor TBDProject.Create;
begin
  fImages:=TBDImageList.Create;
  fImages.Add(TBDImage.Create);
  fCurrentImageIndex:=0;
  fOverlayImage:=TBDRegion.Create(fImages[0].Region.Width,fImages[0].Region.Height);
  fOverlayImage.Bar(0,0,fOverlayImage.Width,fOverlayImage.Height,0);
  fCELImage:=nil;
end;

constructor TBDProject.CreateFromFile(iFilename:String);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(iFilename,fmOpenRead or fmShareDenyWrite);
  try
    CreateFromStream(Xs);
  finally
    Xs.Free;
  end;
end;

constructor TBDProject.CreateFromStream(iStream:TStream);
var curr,size:int64;b:byte;s:String;
begin
  fImages:=TBDImageList.Create;
  s:=#0#0#0#0;
  iStream.Read(s[1],4);
  if uppercase(copy(s,1,3))<>PROJECTBLOCKID then raise Exception.Create(Format('Project block ID expected, got %s!',[copy(s,1,3)]));
  if ord(s[1]) and $20<>0 then Exception.Create('Project block cannot be compressed!');
  size:=0;
  iStream.Read(size,4);
  curr:=iStream.Position;
  b:=ord(s[4]);
  if b=1 then LoadFromStreamV1(iStream)
  else raise Exception.Create(Format('Unknown project data version! (%d)',[b]));
  iStream.Position:=curr+size;

  if fImages.Count=0 then fImages.Add(TBDImage.Create);

  if (fCurrentImageIndex<0) or (fCurrentImageIndex>=fImages.Count) then
    fCurrentImageIndex:=0;
  fOverlayImage:=TBDRegion.Create(fImages[fCurrentImageIndex].Region.Width,fImages[fCurrentImageIndex].Region.Height);
  fOverlayImage.Bar(0,0,fOverlayImage.Width,fOverlayImage.Height,0);
end;

destructor TBDProject.Destroy;
begin
  if Assigned(fCELImage) then fCELImage.Free;
  if Assigned(fOverlayImage) then fOverlayImage.Free;
  if Assigned(fImages) then fImages.Free;
  inherited Destroy;
end;

procedure TBDProject.SaveToFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmCreate);
  try
    SaveToStream(Xs);
  finally
    Xs.Free;
  end;
end;

procedure TBDProject.SaveToStream(pStream:TStream);
var curr:int64;flags:byte;i:integer;s:string;
begin
  s:=PROJECTBLOCKID+#1;
  pStream.Write(s[1],4);
  curr:=pStream.Position;
  i:=0;
  pStream.Write(i,4);  // Size placeholder

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
    fImages.Add(TBDImage.CreateFromStream(pStream));
    dec(count);
  end;
  if flags and 1<>0 then fCELImage:=TBDRegion.CreateFromStream(pStream);
end;

procedure TBDProject.fSetCurrentImageIndex(value:integer);
begin
  if (value<>fCurrentImageIndex) and (value>=0) and (value<fImages.Count) then begin
    fCurrentImageIndex:=value;
    fOverlayImage.Recreate(fImages[fCurrentImageIndex].Region.Width,fImages[fCurrentImageIndex].Region.Height);
    fOverlayImage.Bar(0,0,fOverlayImage.Width,fOverlayImage.Height,0);
  end;
end;

function TBDProject.fGetCurrentRegion:TBDRegion;
begin
  Result:=fImages[fCurrentImageIndex].Region;
end;

function TBDProject.fGetCurrentPalette:TBDPalette;
begin
  Result:=fImages[fCurrentImageIndex].Palette;
end;

function TBDProject.fGetCurrentGradientList:TGradientList;
begin
  Result:=fImages[fCurrentImageIndex].Gradients;
end;

function TBDProject.fGetCurrentImage:TBDImage;
begin
  Result:=fImages[fCurrentImageIndex];
end;

end.

