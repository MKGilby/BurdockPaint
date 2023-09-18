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

// This is a stripped version of the original BDPProject!

unit BDPProject;

{$mode Delphi}

interface

uses
  Classes, SysUtils, fgl, BDPRegion, BDPUndo, BDPColorCluster, BDPPalette;

type

  { TBDExtendedImage }

  TBDExtendedImage=class
    // Loads everything from stream. (fileformats.txt - E-block)
    constructor CreateFromStream(iStream:TStream);

    // Free assigned entities
    destructor Destroy; override;

    // Writes everything to stream. (fileformats.txt - E-block)
    procedure SaveToStream(pStream:TStream);

    // Writes everything to stream. (fileformats.txt - IMG-block)
    procedure SaveToStream2(pStream:TStream);

    // Clears undo/redo data
    procedure ClearUndoData;
  private
    fRegion:TBDRegion;
    fPalette:TBDPalette;
    fImageUndoSystem:TBDImageUndoSystem;
    fPaletteUndoSystem:TBDPaletteUndoSystem;
    fColorClusters:TColorClusters;
    procedure LoadFromStreamV1(pStream:TStream);
    procedure LoadFromStreamV2(pStream:TStream);
  end;

  TBDExtendedImages=class(TFPGObjectList<TBDExtendedImage>);

  { TBDProject }

  TBDProject=class
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

    // Saves project to stream. (fileformats.txt - PRJ-block)
    procedure SaveToStream2(pStream:TStream);

    // Clears undo data and releases CEL
    procedure Clean;
  private
    fCurrentImageIndex:integer;
    fImages:TBDExtendedImages;
    fCELImage:TBDRegion;
    procedure LoadFromStreamV1(pStream:TStream);
  end;

implementation

uses BDPShared;

const
  EXTENDEDIMAGEID=$45;
  PROJECTDATAID=$50;

  PROJECTBLOCKID='PRJ';
  IMAGEBLOCKID='IMG';

{ TBDExtendedImage }

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
  else if b=2 then LoadFromStreamV2(iStream)
  else raise Exception.Create(Format('Unknown extended image data version! (%d)',[b]));
  iStream.Position:=curr+size;

  if not Assigned(fImageUndoSystem) then fImageUndoSystem:=TBDImageUndoSystem.Create;
  if not Assigned(fPaletteUndoSystem) then fPaletteUndoSystem:=TBDPaletteUndoSystem.Create;
  if not Assigned(fColorClusters) then fColorClusters:=TColorClusters.Create;

end;

destructor TBDExtendedImage.Destroy;
begin
  if Assigned(fRegion) then fRegion.Free;
  if Assigned(fPalette) then begin
    fPalette.Free;
    GlobalV1Palette:=nil;
  end;
  if Assigned(fColorClusters) then fColorClusters.Free;
  if Assigned(fImageUndoSystem) then fImageUndoSystem.Free;
  if Assigned(fPaletteUndoSystem) then fPaletteUndoSystem.Free;
  inherited Destroy;
end;

procedure TBDExtendedImage.SaveToStream(pStream:TStream);
var i,curr:int64;flags:byte;
begin
  i:=EXTENDEDIMAGEID;
  pStream.Write(i,1);
  curr:=pStream.Position;
  i:=0;  pStream.Write(i,4);  // Size placeholder
  i:=2;  pStream.Write(i,1);  // version

  flags:=0;
  if fImageUndoSystem.Count>0 then flags:=flags or 2;
  if fPaletteUndoSystem.Count>0 then flags:=flags or 4;
  if fColorClusters.Count>0 then flags:=flags or 8;
  pStream.Write(flags,1);

  fPalette.SaveToStream(pStream);
  fRegion.SaveToStream(pStream);
  if fImageUndoSystem.Count>0 then fImageUndoSystem.SaveToStream(pStream);
  if fPaletteUndoSystem.Count>0 then fPaletteUndoSystem.SaveToStream(pStream);
  if fColorClusters.Count>0 then fColorClusters.SaveToStream(pStream);

  i:=pStream.Position-curr-4;
  pStream.Position:=curr;
  pStream.write(i,4);
  pStream.Position:=pStream.Position+i;
end;

procedure TBDExtendedImage.SaveToStream2(pStream: TStream);
var i,curr:int64;flags:byte;s:string;
begin
  s:=IMAGEBLOCKID+#1;
  pStream.Write(s[1],4);
  curr:=pStream.Position;
  i:=0;  pStream.Write(i,4);  // Size placeholder

  flags:=0;
  if fColorClusters.Count>0 then flags:=flags or 8;
  pStream.Write(flags,1);

  fPalette.SaveToStream2(pStream);
  fRegion.SaveToStream2(pStream);
  if fColorClusters.Count>0 then fColorClusters.SaveToStream2(pStream);

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
  fPalette:=TBDPalette.CreateFromStream(pStream);
  GlobalV1Palette:=fPalette;
  fRegion:=TBDRegion.Create(16,16);
  fRegion.LoadRegionFromStream(pStream,fPalette);
  if flags and 2<>0 then fImageUndoSystem:=TBDImageUndoSystem.CreateFromStream(pStream);
  if flags and 4<>0 then fPaletteUndoSystem:=TBDPaletteUndoSystem.CreateFromStream(pStream);
  if flags and 8<>0 then fColorClusters:=TColorClusters.CreateFromStream(pStream);
end;

procedure TBDExtendedImage.LoadFromStreamV2(pStream:TStream);
var flags:byte;
begin
  flags:=0;
  pStream.Read(flags,1);
  fPalette:=TBDPalette.CreateFromStream(pStream);
  fRegion:=TBDRegion.CreateFromStream(pStream);
  if flags and 2<>0 then fImageUndoSystem:=TBDImageUndoSystem.CreateFromStream(pStream);
  if flags and 4<>0 then fPaletteUndoSystem:=TBDPaletteUndoSystem.CreateFromStream(pStream);
  if flags and 8<>0 then fColorClusters:=TColorClusters.CreateFromStream(pStream);
end;


{ TBDProject }

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
end;

destructor TBDProject.Destroy;
begin
  if Assigned(fCELImage) then fCELImage.Free;
  if Assigned(fImages) then fImages.Free;
  inherited Destroy;
end;

procedure TBDProject.SaveToFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmCreate);
  SaveToStream2(Xs);
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

procedure TBDProject.SaveToStream2(pStream:TStream);
var curr:int64;flags:byte;i:integer;s:String;
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
    fImages[i].SaveToStream2(pStream);

  if Assigned(fCELImage) then fCELImage.SaveToStream2(pStream);

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
  if flags and 1<>0 then fCELImage:=TBDRegion.CreateFromStream(pStream);
end;

end.

