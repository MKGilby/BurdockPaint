unit BDPPalette;

{$mode Delphi}

interface

uses Classes, SysUtils, BDPColorCluster;

type

  { TBDVibroColors }

  TBDVibroColors=class
    constructor Create(iMinIndex,iMaxIndex:integer);
    function GetColorIndex:integer;
  private
    fMinIndex,fLength:integer;
  end;

  { TBDPalette }

  TBDPalette=class
    // Create with MAXPALETTEENTRIES entry count. (currently 2048)
    constructor Create; overload;

    // Create with specified entry count. Cannot be more than MAXPALETTEENTRIES.
    constructor Create(iMaxEntries:integer); overload;

    // Create from stream.
    constructor CreateFromStream(iStream:TStream);

    // Destructor.
    destructor Destroy; override;

    // Save palette to a file.
    procedure SaveToFile(pFilename:string);

    // Save palette to a stream.
    procedure SaveToStream(Target:TStream); overload;

    // Load palette from a file.
    procedure LoadFromFile(pFilename:string);

    // Identify palette file and call appropiate loader method.
    procedure LoadFromStream(Source:TStream);

    // Load an entire .COL format palette (256 colors) from file.
    // Loading entry 0 to startindex, entry 1 to startindex+1 and so on.
    procedure LoadCOL(filename:string;startindex:integer); overload;

    // Load an entire .COL format palette (256 colors) from stream.
    // Loading entry 0 to startindex, entry 1 to startindex+1 and so on.
    procedure LoadCOL(Source:TStream;startindex:integer); overload;

    // Copies colors from the specified palette. Omit count to copy all
    // colors from SourceStart.
    procedure CopyColorsFrom(Source:TBDPalette;SourceStart,TargetStart:integer;count:integer=-1);

    // Resizes the palette, retaining color data that fits into new size.
    procedure Resize(newSize:integer);

    // Resize the palette to fit the colors
    procedure ResizeAndCopyColorsFrom(Source:TBDPalette;start:integer=0;count:integer=-1);

    // Creates a color ramp from the start color to the end color.
    procedure Ramp(pColorCluster:TColorCluster);
  private
    fEntries:pointer;
    fMaxEntries:integer;
    fChanged:boolean;
    function fGetColor(index:integer):uint32;
    function fGetColorA(index:integer):uint8;
    function fGetColorR(index:integer):uint8;
    function fGetColorG(index:integer):uint8;
    function fGetColorB(index:integer):uint8;
    procedure fSetColor(index:integer; value:uint32);
    procedure fSetColorA(index:integer; value:uint8);
    procedure fSetColorR(index:integer; value:uint8);
    procedure fSetColorG(index:integer; value:uint8);
    procedure fSetColorB(index:integer; value:uint8);
    // Creates a compressed stream that contains the data in V1 format.
    function CreateDataV1:TStream;
    // Creates a compressed stream that contains the data in V2 format.
    function CreateDataV2:TStream;
    // Creates a compressed stream that contains the data in V3 format.
    function CreateDataV3:TStream;
    // Load palette from stream format V1 (see below).
    procedure LoadFromStreamV1(Source:TStream);
    // Load palette from stream format V2 (see below).
    procedure LoadFromStreamV2(Source:TStream);
    // Load palette from stream format V3 (see below).
    procedure LoadFromStreamV3(Source:TStream);

  public
    property Colors[index:integer]:uint32 read fGetColor write fSetColor; default;
    property ColorR[index:integer]:uint8 read fGetColorR write fSetColorR;
    property ColorG[index:integer]:uint8 read fGetColorG write fSetColorG;
    property ColorB[index:integer]:uint8 read fGetColorB write fSetColorB;
    property ColorA[index:integer]:uint8 read fGetColorA write fSetColorA;
    property Size:integer read fMaxEntries;
    property Changed:boolean read fChanged write fChanged;
  end;

implementation

uses BDPShared, MKStream, Logger, MyZStreamUnit, CodingUnit;

const
  PALETTEDATAID=$43;

{ TBDVibroColors }

constructor TBDVibroColors.Create(iMinIndex, iMaxIndex: integer);
begin
  fMinIndex:=iMinIndex;
  fLength:=iMaxIndex-iMinIndex+1;
end;

function TBDVibroColors.GetColorIndex: integer;
var i:integer;
begin
  i:=GetTickCount64 mod 1000;
  if i>500 then i:=1000-i;
  if i=500 then i:=499;
  Result:=fMinIndex+(fLength*i) div 500;
end;


{ TBDPalette }

constructor TBDPalette.Create;
begin
  Create(MAXPALETTEENTRIES);
  fChanged:=false;
end;

constructor TBDPalette.Create(iMaxEntries:integer);
begin
  if iMaxEntries>MAXPALETTEENTRIES then iMaxEntries:=MAXPALETTEENTRIES;
  fMaxEntries:=iMaxEntries;
  fEntries:=getmem(fMaxEntries*4);
  fillchar(fEntries^,fMaxEntries*4,0);
  fChanged:=false;
end;

constructor TBDPalette.CreateFromStream(iStream:TStream);
begin
  LoadFromStream(iStream);
  fChanged:=false;
end;

destructor TBDPalette.Destroy;
begin
  freemem(fEntries);
  inherited Destroy;
end;

procedure TBDPalette.SaveToFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmCreate);
  SaveToStream(Xs);
  FreeAndNil(Xs);
end;

procedure TBDPalette.SaveToStream(Target:TStream);
var i,curr:integer;Xs,Ys:TStream;
begin
  i:=PALETTEDATAID;
  Target.Write(i,1);
  curr:=Target.Position;
  i:=0;
  Target.Write(i,4);
  Xs:=CreateDataV1;
  Ys:=CreateDataV2;
  if Xs.Size<=Ys.Size then begin
    FreeAndNil(Ys);
  end else begin
    FreeAndNil(Xs);
    Xs:=Ys;
  end;
  Ys:=CreateDataV3;
  if Xs.Size<=Ys.Size then begin
    Target.CopyFrom(Xs,Xs.Size);
  end else begin
    Target.CopyFrom(Ys,Ys.Size);
  end;
  FreeAndNil(Xs);
  FreeAndNil(Ys);
  i:=Target.Position-curr-4;
  Target.Position:=curr;
  Target.write(i,4);
  Target.Position:=Target.Position+i;
end;

function TBDPalette.CreateDataV1:TStream;
var Xs:TMemoryStream;i:integer;
begin
  Result:=TMemoryStream.Create;
  Xs:=TMemoryStream.Create;
  i:=1;
  Xs.Write(i,1);
  i:=fMaxEntries;
  Xs.Write(i,2);
  Xs.Write(fEntries^,i*4);
  Xs.Position:=0;
  CompressStream(Xs,Result,Xs.Size);
  FreeAndNil(Xs);
  Result.Position:=0;
end;

function TBDPalette.CreateDataV2:TStream;
var Xs:TMemoryStream;i,j:integer;
begin
  Result:=TMemoryStream.Create;
  Xs:=TMemoryStream.Create;
  i:=2;
  Xs.Write(i,1);
  i:=fMaxEntries;
  Xs.Write(i,2);
  for j:=0 to 3 do
    for i:=0 to fMaxEntries-1 do
      Xs.Write((fEntries+i*4+j)^,1);
  Xs.Position:=0;
  CompressStream(Xs,Result,Xs.Size);
  FreeAndNil(Xs);
  Result.Position:=0;
end;

function TBDPalette.CreateDataV3:TStream;
var Xs:TMemoryStream;i,j,b:integer;
begin
  Result:=TMemoryStream.Create;
  Xs:=TMemoryStream.Create;
  i:=3;
  Xs.Write(i,1);
  i:=fMaxEntries;
  Xs.Write(i,2);
  for j:=0 to 3 do begin
    b:=0;
    for i:=0 to fMaxEntries-1 do begin
      b:=byte((fEntries+i*4+j)^)-b;
      Xs.Write(b,1);
      b:=byte((fEntries+i*4+j)^);
    end;
  end;
  Xs.Position:=0;
  CompressStream(Xs,Result,Xs.Size);
  FreeAndNil(Xs);
  Result.Position:=0;
end;

procedure TBDPalette.LoadFromFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=MKStreamOpener.OpenStream(pFilename);
  LoadFromStream(Xs);
  FreeAndNil(Xs);
end;

procedure TBDPalette.LoadFromStream(Source:TStream);
var size,curr:int64;b:byte;Xs:TMemoryStream;
begin
  b:=0;
  Source.Read(b,1);
  if b<>PALETTEDATAID then raise Exception.Create(Format('ID is not for palette data! (%.2x)',[b]));
  size:=0;
  Source.Read(Size,4);
  curr:=Source.Position;
  Xs:=TMemoryStream.Create;
  UnCompressStream(Source,Xs);
  Xs.Position:=0;
  b:=0;
  Xs.Read(b,1);
  if b=1 then LoadFromStreamV1(Xs)
  else if b=2 then LoadFromStreamV2(Xs)
  else if b=3 then LoadFromStreamV3(Xs)
  else raise Exception.Create(Format('Unknown palette data version! (%d)',[b]));
  FreeAndNil(Xs);
  Source.Position:=curr+size;
end;

procedure TBDPalette.LoadFromStreamV1(Source:TStream);
var count:integer;
begin
  count:=0;
  Source.Read(count,2);
  if count>MAXPALETTEENTRIES then begin
    Log.LogWarning(Format('Too many palette entries in file! Only the first %d entries will be loaded.',[MAXPALETTEENTRIES]));
    count:=MAXPALETTEENTRIES;
  end;
  if count<>fMaxEntries then begin
    Freemem(fEntries);
    fMaxEntries:=count;
    fEntries:=GetMem(fMaxEntries*4);
  end;
  Source.Read(fEntries^,fMaxEntries*4);
end;

procedure TBDPalette.LoadFromStreamV2(Source:TStream);
var i,j,count:integer;
begin
  count:=0;
  Source.Read(count,2);
  if count>MAXPALETTEENTRIES then begin
    Log.LogWarning(Format('Too many palette entries in file! Only the first %d entries will be loaded.',[MAXPALETTEENTRIES]));
    count:=MAXPALETTEENTRIES;
  end;
  if count<>fMaxEntries then begin
    Freemem(fEntries);
    fMaxEntries:=count;
    fEntries:=GetMem(fMaxEntries*4);
  end;
  for j:=0 to 3 do
    for i:=0 to fMaxEntries-1 do
      Source.Read((fEntries+i*4+j)^,1);
end;

procedure TBDPalette.LoadFromStreamV3(Source:TStream);
var i,j,count:integer;b,b2:byte;
begin
  count:=0;
  Source.Read(count,2);
  if count>MAXPALETTEENTRIES then begin
    Log.LogWarning(Format('Too many palette entries in file! Only the first %d entries will be loaded.',[MAXPALETTEENTRIES]));
    count:=MAXPALETTEENTRIES;
  end;
  if count<>fMaxEntries then begin
    Freemem(fEntries);
    fMaxEntries:=count;
    fEntries:=GetMem(fMaxEntries*4);
  end;
  b2:=0;
  for j:=0 to 3 do begin
    b:=0;
    for i:=0 to fMaxEntries-1 do begin
      Source.Read(b2,1);
      b:=(b+b2) and $ff;
      byte((fEntries+i*4+j)^):=b;
    end;
  end;
end;

procedure TBDPalette.LoadCOL(filename:string; startindex:integer);
var Xs:TStream;
begin
  Xs:=MKStreamOpener.OpenStream(filename);
  LoadCOL(Xs,startindex);
  FreeAndNil(Xs);
end;

procedure TBDPalette.LoadCOL(Source:TStream; startindex:integer);
var buf:array[0..767] of byte;i:integer;p:pointer;
begin
  if (startindex+256<=fMaxEntries) then begin
    buf[0]:=0;
    Source.Read(buf[0],768);
    for i:=0 to 767 do buf[i]:=buf[i]<<2;
    p:=fEntries+startindex*4;
    for i:=0 to 255 do begin
      byte(p^):=buf[i*3+2];
      inc(p);
      byte(p^):=buf[i*3+1];
      inc(p);
      byte(p^):=buf[i*3];
      inc(p);
      byte(p^):=255;
      inc(p);
    end;
  end;
  fChanged:=true;
end;

procedure TBDPalette.CopyColorsFrom(Source:TBDPalette;SourceStart,TargetStart:integer; count:integer);
var i,c:Integer;
begin
  if (SourceStart>=0) and (SourceStart<Source.Size) and
     (TargetStart>=0) and (TargetStart<Size) then begin
    c:=count;
    if (c=-1) or (SourceStart+c>Source.Size) then c:=Source.Size-SourceStart;
    if (TargetStart+c>Size) then c:=Size-TargetStart;
    for i:=0 to c-1 do Colors[TargetStart+i]:=Source.Colors[SourceStart+i];
    fChanged:=true;
  end;
end;

procedure TBDPalette.Resize(newSize:integer);
var p:pointer;i:integer;
begin
  p:=fEntries;
  getmem(fEntries,newSize*4);
  if fMaxEntries>newSize then
    move(p^,fEntries^,newSize*4)
  else begin
    move(p^,fEntries^,fMaxEntries*4);
    for i:=fMaxEntries to newSize-1 do
      uint32((fEntries+i*4)^):=0;
  end;

  fMaxEntries:=newSize;
  Freemem(p);
  fChanged:=true;
end;

procedure TBDPalette.ResizeAndCopyColorsFrom(Source:TBDPalette; start:integer; count:integer);
var i,c:Integer;
begin
  if (start>=0) and (start<Source.Size) then begin
    c:=count;
    if (c=-1) or (start+c>Source.Size) then c:=Source.Size-start;
    if fMaxEntries<>c then begin
      Freemem(fEntries);
      fMaxEntries:=c;
      fEntries:=Getmem(fMaxEntries*4);
    end;
    for i:=0 to fMaxEntries do Colors[i]:=Source.Colors[start+i];
    fChanged:=true;
  end;  // Start is out of range so nothing to copy.
end;

procedure TBDPalette.Ramp(pColorCluster:TColorCluster);
var i,pi1,pi2,count:integer;
begin
  if pColorCluster.StartIndex>pColorCluster.EndIndex then begin
    pi1:=pColorCluster.EndIndex;
    pi2:=pColorCluster.StartIndex;
  end else begin
    pi1:=pColorCluster.StartIndex;
    pi2:=pColorCluster.EndIndex;
  end;
  count:=pi2-pi1+1;
  for i:=1 to Count-2 do begin
    Self.ColorR[pi1+i]:=Self.ColorR[pi1]+(Self.ColorR[pi2]-Self.ColorR[pi1])*i div (count-1);
    Self.ColorG[pi1+i]:=Self.ColorG[pi1]+(Self.ColorG[pi2]-Self.ColorG[pi1])*i div (count-1);
    Self.ColorB[pi1+i]:=Self.ColorB[pi1]+(Self.ColorB[pi2]-Self.ColorB[pi1])*i div (count-1);
    Self.ColorA[pi1+i]:=Self.ColorA[pi1]+(Self.ColorA[pi2]-Self.ColorA[pi1])*i div (count-1);
  end;
end;

function TBDPalette.fGetColor(index:integer):uint32;
begin
  if (index>=0) and (index<fMaxEntries) then
    Result:=uint32((fEntries+index*4)^)
  else
    Result:=$ffffffff;
end;

function TBDPalette.fGetColorA(index:integer):uint8;
begin
  if (index>=0) and (index<fMaxEntries) then
    Result:=uint8((fEntries+index*4+3)^)
  else
    Result:=0;
end;

function TBDPalette.fGetColorR(index:integer):uint8;
begin
  if (index>=0) and (index<fMaxEntries) then
    Result:=uint8((fEntries+index*4+2)^)
  else
    Result:=0;
end;

function TBDPalette.fGetColorG(index:integer):uint8;
begin
  if (index>=0) and (index<fMaxEntries) then
    Result:=uint8((fEntries+index*4+1)^)
  else
    Result:=0;
end;

function TBDPalette.fGetColorB(index:integer):uint8;
begin
  if (index>=0) and (index<fMaxEntries) then
    Result:=uint8((fEntries+index*4)^)
  else
    Result:=0;
end;

procedure TBDPalette.fSetColor(index:integer; value:uint32);
begin
  if (index>=0) and (index<fMaxEntries) then begin
    uint32((fEntries+index*4)^):=value;
    fChanged:=true;
  end;
end;

procedure TBDPalette.fSetColorA(index:integer; value:uint8);
begin
  if (index>=0) and (index<fMaxEntries) then begin
    uint8((fEntries+index*4+3)^):=value;
    fChanged:=true;
  end;
end;

procedure TBDPalette.fSetColorR(index:integer; value:uint8);
begin
  if (index>=0) and (index<fMaxEntries) then begin
    uint8((fEntries+index*4+2)^):=value;
    fChanged:=true;
  end;
end;

procedure TBDPalette.fSetColorG(index:integer; value:uint8);
begin
  if (index>=0) and (index<fMaxEntries) then begin
    uint8((fEntries+index*4+1)^):=value;
    fChanged:=true;
  end;
end;

procedure TBDPalette.fSetColorB(index:integer; value:uint8);
begin
  if (index>=0) and (index<fMaxEntries) then begin
    uint8((fEntries+index*4)^):=value;
    fChanged:=true;
  end;
end;

end.

