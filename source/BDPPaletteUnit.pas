unit BDPPaletteUnit;

{$mode Delphi}

interface

uses Classes;

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
    // colors from start, omit start to copy whole palette.
    procedure CopyColorsFrom(Source:TBDPalette;start:integer=0;count:integer=-1);

    // Resizes the palette, retaining color data that fits into new size.
    procedure Resize(newSize:integer);
  private
    fEntries:pointer;
    fMaxEntries:integer;
    function fGetColor(index:integer):uint32;
    function fGetColorA(index:integer):uint8;
    function fGetColorB(index:integer):uint8;
    function fGetColorG(index:integer):uint8;
    function fGetColorR(index:integer):uint8;
    procedure fSetColor(index:integer; value:uint32);
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
    property ColorR[index:integer]:uint8 read fGetColorR;
    property ColorG[index:integer]:uint8 read fGetColorG;
    property ColorB[index:integer]:uint8 read fGetColorB;
    property ColorA[index:integer]:uint8 read fGetColorA;
    property Size:integer read fMaxEntries;
  end;

implementation

uses SysUtils, BDPSharedUnit, MKStream, Logger, MyZStreamUnit, CodingUnit;

const
  PALETTEID=$43;

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
end;

constructor TBDPalette.Create(iMaxEntries:integer);
begin
  if iMaxEntries>MAXPALETTEENTRIES then iMaxEntries:=MAXPALETTEENTRIES;
  fMaxEntries:=iMaxEntries;
  fEntries:=getmem(fMaxEntries*4);
  fillchar(fEntries^,fMaxEntries*4,0);
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
  i:=PALETTEID;
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
//  Xs.SaveToFile('ntsc1.dat');
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
//  Xs.SaveToFile('ntsc2.dat');
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
//  Xs.SaveToFile('ntsc3.dat');
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
  if b<>PALETTEID then raise Exception.Create(Format('ID is not for palette data! (%.2x)',[b]));
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
  Freemem(fEntries);
  fMaxEntries:=count;
  fEntries:=GetMem(fMaxEntries*4);
  Source.Read(fEntries^,fMaxEntries*4);
end;

procedure TBDPalette.LoadFromStreamV2(Source:TStream);
begin

end;

procedure TBDPalette.LoadFromStreamV3(Source:TStream);
begin

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
end;

procedure TBDPalette.CopyColorsFrom(Source:TBDPalette; start:integer;
  count:integer);
var
  i,c:Integer;
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
  end;  // Start is out of range so nothing to copy.
end;

procedure TBDPalette.Resize(newSize:integer);
var p:pointer;i:integer;
begin
  p:=fEntries;
  getmem(fEntries,newSize*4);
  if fMaxEntries>newSize then
    move(p^,fEntries^,newSize*4)
  else begin
    move(p^,fEntries^,fMaxEntries);
    for i:=fMaxEntries to newSize-1 do
      uint32((fEntries+i*4)^):=0;
  end;

  fMaxEntries:=newSize;
  Freemem(p);
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
  if (index>=0) and (index<fMaxEntries) then
    uint32((fEntries+index*4)^):=value;
end;

end.

{
  Palette file format
  -------------------
    Name       Size   Content / Description
    ID          1     'C'  (0x43)   Shows that serialized TPalette data follows.
    Size        4     Size of data
    Data       ???    ZLib compressed data

  Version 1 uncompressed data structure
  ---------------------------
    Name        Size               Content / Description
    Version     1                  0x01
    ColorCount  2                  Count of colors
    Colors      ColorCount*uint32  Color data

  Version 2 uncompressed data structure
  ---------------------------
    Name        Size               Content / Description
    Version     1                  0x02
    ColorCount  2                  Count of colors
    ColorsB     ColorCount*byte    Blue color data
    ColorsG     ColorCount*byte    Green color data
    ColorsR     ColorCount*byte    Red color data
    ColorsA     ColorCount*byte    Alpha color data

  Version 2 uncompressed data structure
  ---------------------------
    Name        Size               Content / Description
    Version     1                  0x03
    ColorCount  2                  Count of colors
    ColorsB     ColorCount*byte    Blue color data delta encoded
    ColorsG     ColorCount*byte    Green color data delta encoded
    ColorsR     ColorCount*byte    Red color data delta encoded
    ColorsA     ColorCount*byte    Alpha color data delta encoded

  Delta encoding
  --------------
    First byte is the same as original data
    Second and other bytes are difference from the previous original data.

    Example:
      Original data:  01 02 04 05 01 02 01 01
      Delta encoded:  01 01 02 01 FC 01 FF 00

      Encoding:       First=01
                      02-01=01
                      04-02=02
                      05-04=01
                      (01-05) and FF=FC
                      02-01=01
                      (01-02) and FF=FF
                      01-01=00

      Decoding:       First=01
                      01+01=02
                      02+02=04
                      04+01=05
                      (05+FC) and FF=01
                      01+01=02
                      (02+FF) and FF=01
                      01+00=01
                      04+01=05
}

