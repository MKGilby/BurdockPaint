{
  This file is part of the source code of BurdockPaint.
  See "copyright.txt" for details.
}

unit BDPPalette;

{$mode Delphi}

interface

uses Classes, SysUtils;

type

  { TBDVibroColors }

  TBDVibroColors=class
    constructor Create(iColor1,iColor2:uint32);
    function GetColor:uint32;
    function GetHelperColor:uint32;
  private
    R1,G1,B1,A1,R2,G2,B2,A2:integer;
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
    procedure SaveToStream(pStream:TStream); overload;

    // Load palette from a file.
    procedure LoadFromFile(pFilename:string);

    // Identify palette file and call appropiate loader method.
    procedure LoadFromStream(pStream:TStream);

    // Load an entire .COL format palette (256 colors) from file.
    // Loading entry 0 to startindex, entry 1 to startindex+1 and so on.
    procedure LoadCOL(filename:string;startindex:integer); overload;

    // Load an entire .COL format palette (256 colors) from stream.
    // Loading entry 0 to startindex, entry 1 to startindex+1 and so on.
    procedure LoadCOL(pStream:TStream;startindex:integer); overload;

    // Copies colors from the specified palette. Omit count to copy all
    // colors from SourceStart.
    procedure CopyColorsFrom(Source:TBDPalette;SourceStart,TargetStart:integer;count:integer=-1);

    // Resizes the palette, retaining color data that fits into new size.
    procedure Resize(newSize:integer);

    // Resize the palette to fit the colors
    procedure ResizeAndCopyColorsFrom(Source:TBDPalette;start:integer=0;count:integer=-1);

    // Returns the closest color to the specified r,g,b values.
    // Difference is calculated weighted: r*0.3, g*0.59 and b*0.11
    function GetClosestColor(r,g,b:byte):word;
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

uses BDPShared, MKStream, Logger, MyZStreamUnit, CodingUnit,
  BDPInternalFileFormat;

const
  COLORBLOCKID='CLR';

{ TBDVibroColors }

constructor TBDVibroColors.Create(iColor1,iColor2:uint32);
begin
  A1:=(iColor1 and $FF000000)>>24;
  R1:=(iColor1 and $FF0000)>>16;
  G1:=(iColor1 and $FF00)>>8;
  B1:=(iColor1 and $FF);
  A2:=(iColor2 and $FF000000)>>24;
  R2:=(iColor2 and $FF0000)>>16;
  G2:=(iColor2 and $FF00)>>8;
  B2:=(iColor2 and $FF);
end;

function TBDVibroColors.GetColor:uint32;
var i:integer;
begin
  i:=GetTickCount64 mod 1000;
  if i>500 then i:=1000-i;
  Result:=
    ((A1+(A2-A1)*i div 500) and $FF)<<24+
    ((R1+(R2-R1)*i div 500) and $FF)<<16+
    ((G1+(G2-G1)*i div 500) and $FF)<<8+
    ((B1+(B2-B1)*i div 500) and $FF);
end;

function TBDVibroColors.GetHelperColor:uint32;
var i:integer;
begin
  i:=GetTickCount64 mod 1000;
  if i>500 then i:=1000-i;
  Result:=
    ((A1+(A2-A1)*i div 500) and $FF)<<24+
    ((R1+(R2-R1)*i div 500) and $FE)<<15+
    ((G1+(G2-G1)*i div 500) and $FE)<<7+
    ((B1+(B2-B1)*i div 500) and $FE)>>1;
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

procedure TBDPalette.SaveToStream(pStream:TStream);
var Xs,Ys:TStream;
begin
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
    pStream.CopyFrom(Xs,Xs.Size);
  end else begin
    pStream.CopyFrom(Ys,Ys.Size);
  end;
  FreeAndNil(Xs);
  FreeAndNil(Ys);
end;

function TBDPalette.CreateDataV1:TStream;
var Xs:TStream;
begin
  Result:=TMemoryStream.Create;
  Xs:=TMemoryStream.Create;
  try
    Xs.Write(fMaxEntries,2);
    Xs.Write(fEntries^,fMaxEntries*4);
    TInternalFileFormat.WriteBlock(Result,COLORBLOCKID,1,Xs);
  finally
    Xs.Free;
  end;
  Result.Position:=0;
end;

function TBDPalette.CreateDataV2:TStream;
var Xs:TMemoryStream;i,j:integer;
begin
  Result:=TMemoryStream.Create;
  Xs:=TMemoryStream.Create;
  try
    Xs.Write(fMaxEntries,2);
    for j:=0 to 3 do
      for i:=0 to fMaxEntries-1 do
        Xs.Write((fEntries+i*4+j)^,1);
    TInternalFileFormat.WriteBlock(Result,COLORBLOCKID,2,Xs);
  finally
    Xs.Free;
  end;
  Result.Position:=0;
end;

function TBDPalette.CreateDataV3:TStream;
var Xs:TMemoryStream;i,j,b:integer;
begin
  Result:=TMemoryStream.Create;
  Xs:=TMemoryStream.Create;
  try
    Xs.Write(fMaxEntries,2);
    for j:=0 to 3 do begin
      b:=0;
      for i:=0 to fMaxEntries-1 do begin
        b:=byte((fEntries+i*4+j)^)-b;
        Xs.Write(b,1);
        b:=byte((fEntries+i*4+j)^);
      end;
    end;
    TInternalFileFormat.WriteBlock(Result,COLORBLOCKID,3,Xs);
  finally
    Xs.Free;
  end;
  Result.Position:=0;
end;

procedure TBDPalette.LoadFromFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=MKStreamOpener.OpenStream(pFilename);
  try
    LoadFromStream(Xs);
  finally
    Xs.Free;
  end;
end;

procedure TBDPalette.LoadFromStream(pStream:TStream);
var tmp:TInternalBlock;
begin
  tmp:=TInternalFileFormat.ReadBlock(pStream);
  try
    if tmp.BlockID<>COLORBLOCKID then
      raise Exception.Create(Format('Color block expected, got %s.',[tmp.BlockID]));
    case tmp.Version of
      1:LoadFromStreamV1(tmp.Data);
      2:LoadFromStreamV2(tmp.Data);
      3:LoadFromStreamV3(tmp.Data);
      else raise Exception.Create(Format('Unknown color block version! (%d)',[tmp.Version]));
    end;
  finally
    tmp.Free;
  end;

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
  try
    LoadCOL(Xs,startindex);
  finally
    Xs.Free;
  end;
end;

procedure TBDPalette.LoadCOL(pStream:TStream; startindex:integer);
var buf:array[0..767] of byte;i:integer;p:pointer;
begin
  if (startindex+256<=fMaxEntries) then begin
    buf[0]:=0;
    pStream.Read(buf[0],768);
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

function TBDPalette.GetClosestColor(r, g, b: byte): word;
var i,min,diff:integer;
begin
  Result:=0;min:=10000;
  for i:=0 to Size-1 do begin
    diff:=abs(fGetColorR(i)-r)*30+abs(fGetColorG(i)-g)*59+abs(fGetColorB(i)-b)*11;
    if diff<min then begin
      min:=diff;
      Result:=i;
    end;
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
    MessageQueue.AddMessage(MSG_PALETTECHANGED,index,value);
    fChanged:=true;
  end;
end;

procedure TBDPalette.fSetColorA(index:integer; value:uint8);
begin
  if (index>=0) and (index<fMaxEntries) then begin
    uint8((fEntries+index*4+3)^):=value;
    MessageQueue.AddMessage(MSG_PALETTECHANGED,index,value);
    fChanged:=true;
  end;
end;

procedure TBDPalette.fSetColorR(index:integer; value:uint8);
begin
  if (index>=0) and (index<fMaxEntries) then begin
    uint8((fEntries+index*4+2)^):=value;
    MessageQueue.AddMessage(MSG_PALETTECHANGED,index,value);
    fChanged:=true;
  end;
end;

procedure TBDPalette.fSetColorG(index:integer; value:uint8);
begin
  if (index>=0) and (index<fMaxEntries) then begin
    uint8((fEntries+index*4+1)^):=value;
    MessageQueue.AddMessage(MSG_PALETTECHANGED,index,value);
    fChanged:=true;
  end;
end;

procedure TBDPalette.fSetColorB(index:integer; value:uint8);
begin
  if (index>=0) and (index<fMaxEntries) then begin
    uint8((fEntries+index*4)^):=value;
    MessageQueue.AddMessage(MSG_PALETTECHANGED,index,value);
    fChanged:=true;
  end;
end;

end.

