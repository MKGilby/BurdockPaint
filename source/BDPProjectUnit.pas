unit BDPProjectUnit;

{$mode Delphi}

interface

uses
  Classes, SysUtils, fgl, BDPImageUnit, BDPUndoUnit, BDPColorClusterUnit;

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

    // Creates a project from stream. (fileformats.txt - P-block)
    constructor CreateFromStream(iStream:TStream);

    // Free assigned entities
    destructor Destroy; override;

    // Saves project to stream. (fileformats.txt - P-block)
    procedure SaveToStream(pStream:TStream);
  private
    fActiveImageIndex:integer;
    fImages:TBDExtendedImages;
    fCELImage:TBDImage;
  public
    property Images:TBDExtendedImages read fImages;
    property ActiveImageIndex:integer read fActiveImageIndex;
  end;

implementation

{$i ntsccol.inc}

const
  EXTENDEDIMAGEID=$45;
  PROJECTDATAID=$50;

{ TBDExtendedImage }

constructor TBDExtendedImage.Create;
begin
  Create(320,200);
end;

constructor TBDExtendedImage.Create(iWidth,iHeight:integer);
var Xs:TStream;
begin
  inherited Create(iWidth,iHeight);
  Xs:=TStringStream.Create(NTSCCOL);
  Palette.LoadCOL(Xs,0);
  FreeAndNil(Xs);
  fImageUndoSystem:=TBDImageUndoSystem.Create;
  fPaletteUndoSystem:=TBDPaletteUndoSystem.Create;
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
  fColorClusters:=TColorClusters.Create;
  if b=1 then LoadFromStreamV1(iStream)
  else raise Exception.Create(Format('Unknown extended image data version! (%d)',[b]));
  iStream.Position:=curr+size;
end;

destructor TBDExtendedImage.Destroy;
begin
//  if Assigned(fImage) then FreeAndNil(fImage);
  if Assigned(fImageUndoSystem) then FreeAndNil(fImageUndoSystem);
  if Assigned(fPaletteUndoSystem) then FreeAndNil(fPaletteUndoSystem);
//  if Assigned(fCELImage) then FreeAndNil(fCELImage);
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

procedure TBDExtendedImage.LoadFromStreamV1(pStream:TStream);
var flags:byte;
begin
  flags:=0;
  pStream.Read(flags,1);
  Palette.LoadFromStream(pStream);
  LoadWholeImageDataFromStream(pStream);
  if flags and 2<>0 then fImageUndoSystem:=TBDImageUndoSystem.CreateFromStream(pStream);
  if flags and 4<>0 then fPaletteUndoSystem:=TBDPaletteUndoSystem.CreateFromStream(pStream);
  if flags and 8<>0 then fColorClusters.LoadFromStream(pStream);
end;


{ TBDProject }

constructor TBDProject.Create;
begin
  fImages:=TBDExtendedImages.Create;
  fImages.Add(TBDExtendedImage.Create);
  fActiveImageIndex:=0;
  fCELImage:=nil;
end;

constructor TBDProject.CreateFromStream(iStream:TStream);
begin

end;

destructor TBDProject.Destroy;
begin
  if Assigned(fImages) then FreeAndNil(fImages);
  inherited Destroy;
end;

procedure TBDProject.SaveToStream(pStream:TStream);
var i,curr:int64;flags:byte;
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
  pStream.Write(fActiveImageIndex,2);
  for i:=0 to fImages.Count-1 do
    fImages[i].SaveToStream(pStream);

  if Assigned(fCELImage) then fCELImage.SaveToStream(pStream);

  i:=pStream.Position-curr-4;
  pStream.Position:=curr;
  pStream.write(i,4);
  pStream.Position:=pStream.Position+i;
end;

end.

